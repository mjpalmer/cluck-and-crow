;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Copyright (c) 2013, Michael J. Palmer
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without 
;; modification, are permitted provided that the following conditions are met:
;;
;;      Redistributions of source code must retain the above copyright notice, 
;;      this list of conditions and the following disclaimer.
;;
;;      Redistributions in binary form must reproduce the above copyright 
;;      notice, this list of conditions and the following disclaimer in the 
;;      documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE 
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN 
;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE 
;; POSSIBILITY OF SUCH DAMAGE.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ql:quickload "plain-odbc")

(require :utils-lang)

(provide :utils-db)

(defpackage :utils-db
    (:use :common-lisp)
    (:use :plain-odbc)
    (:import-from :utils-lang 
        :mapcargs
        :strcat
        )
    ;(:import-from :plain-odbc :connect :exec-query :exec-command :exec-update)
    (:export
        :db-connect
        :db-close
        :db-commit
        :db-set-schema
        :db-proc-to-list
        :db-proc
        ))

(in-package :utils-db)

;; Was using defparameter but kept getting caught out when reloading file during 
;; development. defvar only sets default value if var is not yet set.
(defvar *sql-dialect* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; See:
;       http://common-lisp.net/project/plain-odbc/documentation.html
;
; plain-odbc uses cffi, but it seems that recently an update to cffi introduced
; a deprecation warning:
;
;       http://lists.common-lisp.net/pipermail/cffi-devel/2012-March/003702.html
;
; Presumably, plain-odbc will  be fixed at some point to avoid this warning.
; However, in the meantime, the following site provides a way of supressing
; this warning:
;
;      http://comments.gmane.org/gmane.lisp.cffi.devel/2417#
;
; An example:
;
; (handler-bind
;       ((alexandria:simple-style-warning
;          (lambda (warning)
;            (when (alexandria:starts-with-subseq
;                   "bare references to struct types are deprecated."
;                   (simple-condition-format-control warning))
;              (muffle-warning warning)))))
;    (exec-query *con* "select * from common.dictionary_domains")) 
; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(defun db-connect-old-1 (dsn dialect username password )
;    (setf *sql-dialect* (string-upcase dialect))
;    (connect dsn username password) )

;(defun db-connect-old-2 (dialect dsn username &optional password)
;    (setf *sql-dialect* (string-upcase dialect))
;    (if password
;        (connect-generic :dsn dsn :uid username :pwd password)
;        (connect-generic :dsn dsn :uid username)))

(defun db-connect (driver server port database &optional username password)
    (cond 
        ((string-equal "PostgreSQL" (subseq driver 0 10))
            (unless (and username password) (error "username and password required with PostgreSQL driver"))
            (setf *sql-dialect* (string-upcase "PLPGSQL"))
            (connect-generic :driver driver :server server :port port :database database :uid username :pwd password))
        ((string-equal "SQL Server" (subseq driver 0 10))
            (setf *sql-dialect* (string-upcase "T-SQL"))
            (if (and username password)
                (connect-generic :driver driver :server (strcat server "," port) :database database :uid username :pwd password)
                (connect-generic :driver driver :server (strcat server "," port) :database database :Trusted_Connection "yes")))
        (t (error "SQL procedural dialect not supported: ~A" *sql-dialect*))))

(defun db-commit (con)
    (commit con))

(defun db-close (con)
    (setf *sql-dialect* nil)
    (close-connection con))

(defun db-set-schema (con schema &optional username)
;;    (exec-command con (format nil "set search_path to ~A" schema)))
    (cond
        ((string-equal "PLPGSQL" *sql-dialect*)
            (exec-command con (format nil "set search_path to ~A" schema)))
        ((string-equal "T-SQL" *sql-dialect*)
            ;(error "db-set-schema is not currently supportedwith SQL Server"))
            ;; ToDo
            ;; The only way in SQL Server is to do this:
            ;;     "ALTER USER [User] WITH DEFAULT_SCHEMA=[Schema]"
            ;; However, the connection to SQL Server may have been made with the Trusted_Connection string
            ;; and so there is no immediate way to know the user. There is probably a way to get this info.
            (assert (stringp username) (username) "username is required with  SQL Server" a)
            (exec-command con (format nil "ALTER USER \"~A\" WITH DEFAULT_SCHEMA=~A" username schema))
            (commit con))
        (t
            (error "SQL procedural dialect not supported: ~A" *sql-dialect*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Returns a comma-seperated list of ? for each parameter in param-list.
;     e.g. "?, ? ?"
; Doesn't include parentheses because T-SQL's exec does not expect stored procedure params within
; braces. On the other hand PLPGSQL does, however these can be added by calling form.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun odbc-params (param-list &optional (tempstr ""))
    (cond 
        ((null param-list)
            tempstr)
        ((null (cdr param-list))
            (concatenate 'string tempstr "?"))
        (t
            (odbc-params (cdr param-list) (concatenate 'string tempstr "?, ")))))

(defmacro db-proc-to-list (con procname &rest args)
    ;; exec-query returns two values, a list of rwos and a list of column names.
    ;; We are not interested in knowing the names of the columns so ignore them
    ;; and just take the rows 
    `(multiple-value-bind
        (rows columns)
        (cond
            ((string-equal "PLPGSQL" *sql-dialect*)
                ;; Postgres expects function parameters to be within parenthesese...
                (exec-query ,con (format nil "select * from ~A (~A)" ,procname (odbc-params ',args)) ,@args))
            ((string-equal "T-SQL" *sql-dialect*)
                ;; T-SQL does not expect function parameters to be within parenthesese...
                (exec-query ,con (format nil "exec ~A ~A" ,procname (odbc-params ',args)) ,@args))
            (t
                (error "SQL procedural dialect not supported: ~A" *sql-dialect*)
                ))
        rows))

(defmacro db-proc (con procname &rest args)
    ;; Note that exec-query, exec-command and exec-update differ only in the 
    ;; values they return. Thought about using exec-update as this returns the
    ;; number of rows affected but it generates an error when a PLPGSQL proc raises an
    ;; exception. Could also use exec-query or even exec-sql directly but 
    ;; why not just use db-proc-to-list?
    (let ((res (gensym)))
        `(let ((,res (car (car (db-proc-to-list ,con ,procname ,@args)))))
            (commit ,con)
            ,res)))


