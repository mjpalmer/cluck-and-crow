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

(ql:quickload "cl-ppcre")
;(ql:quickload "flexi-streams")

(provide :utils-lang)

(defpackage :utils-lang
;    (:use :common-lisp :ext)
    (:use :common-lisp)
    (:import-from :ext
        :getenv
        :make-encoding
        )
    (:import-from :cl-ppcre
        :scan
    ;    :scan-to-strings
        )
    (:export
        ;; String stuff...
        :strcat
        :strlet
        :mkstr
        :splitstr
        :rptstr
        :spaces
        :nl
        ;; Higher order...
        :mapcargs
        ;; Association lists, hashes etc...
        :lookup
        :assoc-str
        :assoc-str-val
        ;; Sequences...
        :subseq-safe
        :strleft
        :strright
        :strmiddle
        :split
        :splitarr
        :remove-last
        :string-to-lists
        :list-to-string
        :get-first-record
        :fields-to-lists
        :slice
        ;; Regular expression stuff...
        :match-pattern
        :match-pattern-before-after
        :match-before
        :match-after
        :match-groups
        :with-misc-encoding
        ;; IO stuff...
        :read-charsn
        :read-chars-from-stdin
        :stdin-to-bytes
        :bytes-to-string
        :stdin-to-string
        :stream-to-bytes
        :string-to-bytes
        :bytes-to-file
        :string-to-file
        :stream-to-string
        :file-to-bytes
        :file-to-string
        :read-bytes
        :read-bytes-from-stdin
        :printenv
        :prettyenv
        :read-up-to-char
        :read-up-to-byte
        :write-bytes
        :read-byte-array-from-stream
        ))

(in-package :utils-lang)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro strcat (&rest strings)
    `(concatenate 'string ,@strings))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Like let but concatenates the expressions that make up the body.
;; Requires all body expressions to be strings. Ought to do some checking of 
;; this.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro strlet (assignment-lst &rest body)
    `(let* ,assignment-lst
        (concatenate 'string ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; mkstr copied from Doug Hoyte's Let Over Lamda book:
;       http://letoverlambda.com/lol-orig.lisp
; Actually, it was originally described by Paul Graham in "On Lisp":
;       http://www.paulgraham.com/onlisp.html
; Note that implementing strcat from mkstr doesn't work as expected.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; mapcargs takes a list and zero or more arguments which get passed to function
; call in addition to the list element. I'm sure there must be a standard 
; macro or function to do this but I couldn't find one.
; Changed implementation to use mapcar rather than loop - more lispy.
; Initially, was getting weird memory errors, a seg fault and also out of memory.
; However, suspect those were something weird with env/windows as I have not yet
; been able to reproduce them.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(defmacro mapcargs (f l &rest args)
;    `(loop for x in ,l collect (funcall ,f x ,@args)))

(defun mapcargs (f l &rest args)
    (mapcar #'(lambda (x) (apply f x args)) l))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(defun rptstr (str n)
;    (if (eq n 0)
;        ""
;        (strcat str (rptstr str (- n 1)))))

(defun rptstr (str n)
    (with-output-to-string (s) 
        (dotimes (i n) (format s "~A" str))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun spaces (n)
    (rptstr " " n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun nl () "~%")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun splitstr (str sep)
  (let ( (j 0) (lst ()) )
    (do ((i 0 (+ i 1)))
        ( (> i (- (length str) (length sep))) 
          (if (< j (length str)) (setf lst (cons (subseq str j) lst))) 
          ;(format t "Z: ~A ~A~%" j (length str) ) 
          (reverse lst))
            (if (string= sep (subseq str i (+ i (length sep))))
                (progn
                    ;(format t "Y: ~A ~A~%" j i) 
                    (if (< j i) (progn 
                        ;(format t "CONS~%") 
                        (setf lst (cons (subseq str j i) lst))))
                    (setf j (+ i (length sep)))
                    (setf i (- j 1)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun split (str ch &optional tempstr templist)
    (if (eq 0 (length str))
        (append templist (list tempstr))
        (if (eq ch (char str 0))
            (split (subseq str 1) ch "" (append templist (list tempstr)))
            (split (subseq str 1) ch (strcat tempstr (subseq str 0 1)) templist))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun splitarr (arr b &optional temparr templist)
    (if (eq 0 (length arr))
        (append templist (list temparr))
        (if (eq b (aref arr 0))
            (splitarr (subseq arr 1) b "" (append templist (list temparr)))
            (splitarr (subseq arr 1) b (concatenate 'array temparr (subseq arr 0 1)) templist))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun remove-last (l)
    (reverse (cdr (reverse l))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Splitting on strings instead of character allows character combinations such 
; as #\return #\newline.
; TODO
; - this could be generalised to have any number of delimeters which would be used
; to split the string into lists in the order they appear as args.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun string-to-lists (str d1 d2)
    (mapcargs #'splitstr (splitstr str d1) d2))

(defun list-to-string (l)
    (reduce 
        (lambda (a b) 
            (assert (stringp a) (a) "Not a string: ~A" a)
            (assert (stringp b) (b) "Not a string: ~A" b)
            (concatenate 'string a b)) 
        l))


(defun get-first-record (str d n &optional (c 0) (tempstr ""))
    (cond 
        ((= 0 (length str)) 
            (list tempstr str)
        )

        ((= c n) 
            (list (reverse (subseq (reverse tempstr) 1)) str)
        )

        ((string= d (subseq str 0 1))
            (get-first-record (subseq str 1) d n (+ c 1)(strcat tempstr (subseq str 0 1)))
        )

        (t
            (get-first-record (subseq str 1) d n c (strcat tempstr (subseq str 0 1)))
        )
    )
)

(defun fields-to-lists (str d n &optional (l nil))
    (cond
        ((string= "" str) 
            (reverse l))
        (t
            (let ((xxx (get-first-record str d n)))
                (fields-to-lists (second xxx) d n (cons (splitstr (first xxx) d) l))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Doesn't throw an exception if the string is shorter than the subseq being 
;; sought.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun subseq-safe (str i)
    (cond
        ((<= (length str) i) str)
        (t (subseq str 0 i))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lookup (key table)
    (second (assoc key table )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun assoc-str (key alist)
    (assoc key alist :test #'string=))

(defun assoc-str-val (key alist)
    (cdr (assoc key alist :test #'string=)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun strleft (str i)
    (if (and (integerp i)
             (<= 0 i)
             (<= i (length str)))
        (subseq str 0 i)
        nil))

(defun strright (str j)
    (if (and (integerp j)
             (<= 0 j)
             (<= j (length str)))
        (subseq str j)
        nil))

(defun strmiddle (str i j)
    (if (and (integerp i) 
             (integerp j)
             (<= 0 i)
             (<= j (length str))
             (<= i j))
        (subseq str i j)
        nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Regular expressions...
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; match-indices produces a list of lists. The first list in the list contains 
;; the start and end of the entire match, the second list contains the start and
;; end of the first capture group, the third list the start and end of the second
;; capture group etc.

(defun match-indices (str pattern)
     (let ((lst (multiple-value-bind (a b c d) (ppcre:scan pattern str) (list a b c d))))
        (append
            (list (list (first lst) (second lst)))
            (mapcar #'list (coerce (third lst) 'list) (coerce (fourth lst) 'list)))))

(defun match-pattern (str pattern)
    (let (( indices (car (match-indices str pattern)) ))
        (strmiddle str (first indices) (second indices))))

(defun match-before (str pattern)
    (let (( indices (first (match-indices str pattern)) ))
        (strleft str (first indices) )))

(defun match-after (str pattern)
    (let (( indices (first (match-indices str pattern)) ))
        (strright str (second indices) )))

(defun match-pattern-before-after (str pattern)
    (let ((indices (match-indices str pattern)))
        (list
            (strleft str (first (car indices)))
            (strmiddle str (first (car indices)) (second (car indices)))
            (strright str (second (car indices))))))

(defun match-groups (str pattern)
    (mapcar #'(lambda (x)
                (apply #'strmiddle (list str (first x) (second x))))
            (cdr (match-indices str pattern))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro with-misc-encoding (charset &body body) 
    `(let (( custom:*misc-encoding* (make-encoding :charset ,charset :line-terminator :dos) ))
        ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun read-charsn (n)
    (if (eq n 0)
        ""
        (strcat (format nil "~c" (read-char)) (read-charsn (- n 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun read-chars-from-stdin ()
    (let ((c (read-char nil nil nil)))
        (if (eq c nil)
            ""
            (strcat (format nil "~c" c) (read-chars-from-stdin)))))

;; Can't get ext:make-stream to work with a stream created by socket-accept, so workaround is to temporarily 
;; change the element type of standard input. Note that this can't be tested interractively as stdin will be 
;; bound to the terminal and would therefore be illegal.
;; Should really create a macro e.g.called "with-stream-element-type" which temporarily changes the element type
;; of a stream and executes the body before changeing it back and returns whatever the body returns.

;(defun stdin-to-bytes (len) 
;;    (WITH-OPEN-STREAM (instream (EXT:MAKE-STREAM :INPUT :DIRECTION :INPUT :ELEMENT-TYPE '(UNSIGNED-BYTE 8)))
;    (WITH-OPEN-STREAM (instream (EXT:MAKE-STREAM *standard-input* :DIRECTION :INPUT :ELEMENT-TYPE '(UNSIGNED-BYTE 8)))
;;        (let (( seq (make-array (+ 0 (PARSE-INTEGER (EXT:GETENV "CONTENT_LENGTH"))))))
;        (let (( seq (make-array (+ 0 (PARSE-INTEGER len)))))
;            (read-sequence seq instream)
;            seq)))


(defun stdin-to-bytes (len) 
    (let ( (stdin-prev-element-type (stream-element-type *standard-input*))
           ( seq (make-array (+ 0 (PARSE-INTEGER len)))))
        (setf (stream-element-type *standard-input*) '(UNSIGNED-BYTE 8) )
        (read-sequence seq *standard-input*)
        (setf (stream-element-type *standard-input*) stdin-prev-element-type )
        seq))

;; Tried using flexi-streams, but...
;; Even specifying element-type, read-sequence is returning an array of chars not an array of char codes.
;; Think it must be because flexi-streams are meant to be created on top of binary steams not character streams.
;; Standard input  is character-based and I'm guessing that socket:socket-accept defaults to character data too. 
;; Got a similar behaviour when tried making a flexi-stream on top of a 
;; character file stream - when the file stream was opened with element-type of (unsigned-byte 8) however, 
;; it was possible to flip between reading bytes and chars. For now, can't see any other option than using
;; clisp's feature of allowing setf on (stream-element-type ... )
;;
;(defun stdin-to-bytes (len) 
;    (let* ( (seq (make-array (+ 0 (PARSE-INTEGER len))))
;            (flexi (flexi-streams:make-flexi-stream *standard-input* :external-format :utf-8 :element-type '(unsigned-byte 8)))
;            (prev-element-type (flexi-streams:flexi-stream-element-type flexi))
;            )
;        (setf (flexi-streams:flexi-stream-element-type flexi) '(unsigned-byte 8))
;        (read-sequence seq flexi)
;        (setf (flexi-streams:flexi-stream-element-type flexi) prev-element-type)
;        seq))


;(defun stream-to-bytes (len &optional (strm (EXT:MAKE-STREAM :INPUT :DIRECTION :INPUT :ELEMENT-TYPE '(UNSIGNED-BYTE 8))))
(defun stream-to-bytes (len &optional (strm (EXT:MAKE-STREAM *standard-input* :DIRECTION :INPUT :ELEMENT-TYPE '(UNSIGNED-BYTE 8))))
    (WITH-OPEN-STREAM (instream strm)
        (let (( seq (make-array (+ 0 (PARSE-INTEGER len)))))
            (read-sequence seq instream)
            seq)))

(defun file-to-bytes (file) 
    (with-open-file (stream file :DIRECTION :INPUT :ELEMENT-TYPE '(UNSIGNED-BYTE 8)) 
        (let (( seq (make-array (+ 0 (file-length stream)))))
            (read-sequence seq stream)
            seq)))

;; Some factoring out needs to be done here...
(defun bytes-to-string (arr)
    (let ((str (make-string (+ 0 (length arr)))))
        (loop for i from 0 to (- (length arr) 1) 
            do (setf (aref str i) (code-char (if (null (aref arr i)) 0 (aref arr i)))))
        str))

(defun string-to-bytes (str)
    (let ((arr (make-array (+ 0 (length str)))))
        (loop for i from 0 to (- (length str) 1) 
            do (setf (aref arr i) (char-code (if (null (aref str i)) 0 (aref str i)))))
        arr))

(defun bytes-to-file (arr file) 
    (with-open-file (stream file 
            :DIRECTION :OUTPUT 
            :IF-EXISTS :SUPERSEDE
            :IF-DOES-NOT-EXIST :CREATE
            :ELEMENT-TYPE '(UNSIGNED-BYTE 8)) 
        (write-sequence arr stream)
        arr))

(defun string-to-file (str file)
    (bytes-to-file (string-to-bytes str) file))

(defun stdin-to-string (len)
   (bytes-to-string (stdin-to-bytes len)))

;(defun stream-to-string (len &optional (strm (EXT:MAKE-STREAM :INPUT :DIRECTION :INPUT :ELEMENT-TYPE '(UNSIGNED-BYTE 8))))
(defun stream-to-string (len &optional (strm (EXT:MAKE-STREAM *standard-input* :DIRECTION :INPUT :ELEMENT-TYPE '(UNSIGNED-BYTE 8))))
   (bytes-to-string (stream-to-bytes len strm)))

(defun file-to-string (file)
   (bytes-to-string (file-to-bytes file)))

(defun read-bytes (s n)
    (if (eq n 0)
        ""
        (strcat (format nil "~c" (code-char (read-byte s nil  nil))) (read-bytes s (- n 1)))))

(defun read-bytes-from-stdin (len)
  ;(WITH-OPEN-STREAM (in (EXT:MAKE-STREAM :INPUT :DIRECTION :INPUT :ELEMENT-TYPE '(UNSIGNED-BYTE 8)))
  (WITH-OPEN-STREAM (in (EXT:MAKE-STREAM *standard-input* :DIRECTION :INPUT :ELEMENT-TYPE '(UNSIGNED-BYTE 8)))
    (read-bytes in (PARSE-INTEGER len))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun printenv ()
    (loop for x in (getenv)
	    do (format t "~A = ~A~%"  (car x) (cdr x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun prettyenv ()
    (prettyenv_ (getenv) ""))

(defun prettyenv_ (env str) 
    (if (null (car env))
        str
        (prettyenv_ (cdr env) (concatenate 'string str (format nil "~A = ~A<BR/>~%"  (car (car env)) (cdr (car env)) )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun read-up-to-char (ch &optional (strm *standard-input*) (str ""))
    (let ((c (read-char strm)))
        (if (char= c ch)
            str
            (read-up-to-char ch strm (concatenate 'string str (string c))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun read-up-to-byte (bt &optional  
        ;(strm (EXT:MAKE-STREAM :INPUT :DIRECTION :INPUT :ELEMENT-TYPE '(UNSIGNED-BYTE 8))) 
        (strm (EXT:MAKE-STREAM *standard-input* :DIRECTION :INPUT :ELEMENT-TYPE '(UNSIGNED-BYTE 8))) 
        (arr #()))
    (let ((b (read-byte strm)))
        (if (= b bt)
            arr
            (read-up-to-byte bt strm (concatenate 'array arr (vector b))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun write-bytes (bytes strm &optional (n 0))
    (if (= (length bytes) 0)
        n
        (progn
            (write-byte (aref bytes 0) strm)
            (write-bytes (subseq bytes 1) strm (+ n 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun read-byte-array-from-stream (instream len)
    (let (( seq (make-array (+ 0 len))))
        (read-sequence seq instream)
        (print seq)
        seq))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Takes a list and any number of idices, returns a set of slices through the list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun slice (l &rest indices)
    (let ((res1 nil))
        (dolist (r l)
            (let ((res2 nil))
                (dolist (i indices)
                    (setf res2 (cons (nth i r) res2)))
                (setf res1 (cons (reverse res2) res1))))
        (reverse res1)))

