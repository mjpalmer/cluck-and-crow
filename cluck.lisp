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

(require :utils-lang)
(require :utils-cgi)

(provide :cluck)

(defpackage :cluck
    (:use :common-lisp)
    (:import-from :ext
        :getenv
        )
    (:import-from :utils-lang
        :strcat
        :split
        :nl
        :bytes-to-string 
        :printenv
        :read-up-to-char
        :list-to-string
        )
    (:import-from :utils-cgi
        :send-response
        :with-env-params
        :env
        :params
        :qs
        )
    (:export
        :with-scgi
        :with-cgi
        :with-gi
        :cluck
        ))

(in-package :cluck)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun setenv-scgi-headers (headers) 
    ;; There will be an uneven number of items in headers due to last char being the comma.
    ;; Hence, need stop after length of headers falls below 2 - we'll end up stopping when length
    ;; is 1.
    (if (< (length headers) 2)
        t
        (progn
            (setf (getenv (first headers)) (second headers))
            (setenv-scgi-headers (cdr (cdr headers))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro with-scgi ( port &rest body)
    (let ((server (gensym)) (socket (gensym)) (str (gensym)) (header-length (gensym)))
        `(LET ((,server (SOCKET:SOCKET-SERVER ,port)))
            (unwind-protect
                (loop
                    (FORMAT t "~&Waiting for a connection on ~S:~D~%" 
                        (SOCKET:SOCKET-SERVER-HOST ,server) (SOCKET:SOCKET-SERVER-PORT ,server))
                    ;;
                    ;; TODO
                    ;; Need ignore-errors because I was getting  the following error:
                    ;;      *** - Winsock error 10054 (ECONNRESET): Connection reset by peer
                    ;; This was happeining only with Chrome when double clicking quickly on the 
                    ;; submit button. Using  :bt showed this happened when we were writing to the 
                    ;; socket. I'm guessing that for some reason Chrome was causing Apache to 
                    ;; close the first socket connection after receiving the second
                    ;; from the browser. Not sure why Chrome should do this and not other
                    ;; browsers, but the only way I found to avoid this app from breaking out
                    ;; into the debugger was to use ignore-errors. The downside of this is that
                    ;; ctrl-c on the terminal will cause the clisp repl to quit but the clisp
                    ;; server socket server process is still running. Don't suppose this is a big
                    ;; deal. It can probably be fixed by creating a specific condition handler for
                    ;; the above reset error.
                    ;;

                    ;; Replace (progn with (ignore-errors...
                    ;(ignore-errors
                    (progn
                    ;; Alternative possibility for handling these errors 
                    ;; - see http://comments.gmane.org/gmane.lisp.cffi.devel/2417
                    ;(handler-bind
                    ;    ((alexandria:simple-style-warning
                    ;        (lambda (warning)
                    ;            (when 
                    ;                (or
                    ;                    (alexandria:starts-with-subseq
                    ;                        ;"bare references to struct types are deprecated."
                    ;                        "Connection reset by peer"
                    ;                        (simple-condition-format-control warning))
                    ;                    (alexandria:starts-with-subseq
                    ;                        "Software caused connection abort"
                    ;                        (simple-condition-format-control warning))
                    ;                )
                    ;                (progn
                    ;                    (format *terminal-io* "Muffling warning ~A" warning)
                    ;                    (muffle-warning warning))))))

                      (WITH-OPEN-STREAM (,socket (SOCKET:SOCKET-ACCEPT ,server))
                        (let ((,header-length (read-up-to-char #\: ,socket))) 
                            (setenv-scgi-headers 
                                (split 
                                    (let ((,str (make-string (parse-integer ,header-length)))) 
                                        (read-sequence ,str ,socket) 
                                        ,str)
                                    #\null))

                            ;; Read the comma terminating the headers netstring...
                            (read-char ,socket)

                            ;; Our socket should now be ready for reading the request body.
                            ;; We bind *standard-input* and *standard-output* to socket so that 
                            ;; code called hereafter can behave like a stanard CGI application.
                            ;; 
                            (let ( (*standard-input* ,socket) (*standard-output* ,socket) )
                                ,@body))))
                    )

                    ;; Never get here if using ignore-errors above
                    (FORMAT t "Closing server...~%")
                    (SOCKET:SOCKET-SERVER-CLOSE ,server)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro with-cgi ( &rest body)
    `(progn ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro with-gi (gi &rest body) 
    ;; Construct the name of the macro refering to the gateway that we are going to use, e.g. with-scgi.
    ;; Need to do this in two steps, first create the macro's name in string form, then find its symbol.
    (let ((mac-name-string (strcat "with-" gi)))
        (let ((mac-name-symbol (find-symbol (string-upcase mac-name-string))))
            ;; Call the macro with our body.
            ;; Note that body should start with port if gi is "scgi".
            `(,mac-name-symbol
                ,@body)))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; There are a couple of things to note here about the lexical var state:
;; 1. It is defined outside of with-gi so that it persists beyond
;;    each connection (assuming we are using scgi).
;; 2. It is passed to func with the intension that func can modify it.
;; 3. It is a hash rather than a list. This is not so much for performance
;;    as usability - I can pass an empty hash as a parameter and have the function
;;    add key/value pairs. With a list it was not possible to pass () as this just
;;    evaluates to the symbol nil and cannot then be modified by the function to
;;    which it is passed. Note that, although Common Lisp function arguments are passed by value,
;;    the contents of a compound data type such as a list or hash are effectively passed
;;    by reference. An empty hash is setf-able, although an empty list is not.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro cluck (func gi &rest args) 
    (let ((state (make-hash-table)))
        `(with-gi
            ,gi
            ,@args ; should be port if gi is 'scgi
            (send-response 
                (with-env-params
                    ; state is intended to be changed by func - can be used e.g. to store session vars...
                    (let 
                        ;; Now we pass the env to the app function as an association list, so the app function
                        ;; has everything passed to it via its arg list. 

                        ;; Note that params, like ext:getenv, is an association list, i.e. :
                        ;;     ((p1 v1a v1b) (p2 v2a v2b) ...)
                        ;; The only difference is that the value of any pair is itself a list of values.
                        ;; We have to do it this way since, in the case where the parameter name represents a file,
                        ;; the first param value is the file name and the second param value is the file contents.
                        ;; This is another way of saying the cons pair would be (p1 . (v1a v1b)). 
                        ;; i.e. The cdr is the list of param values, with one value in the non-file case and 
                        ;; two values for file params.

                        ((app-data (funcall ,func ',state env params qs) ))
                        (strcat 
                            (first app-data) ; status
                            (nl)
                            (second app-data) ; content type
                            (nl)
                            (nl)
;                            (third app-data)) ; body
                            ;; Reduce the remainder of the list to as single string.
                            ;; This means the application function does not have to 
                            ;; concatenate it's body into the third item in its list.
                            (list-to-string (cdr (cdr app-data))))
                    ))))))

