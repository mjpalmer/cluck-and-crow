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

(provide :utils-cgi)

(defpackage :utils-cgi
    (:use :common-lisp)
    (:import-from :ext
        :getenv
        )
    (:import-from :utils-lang 
        :with-misc-encoding
        :spaces
        :strcat 
        :mapcargs 
        :split 
        :splitstr 
        :remove-last
        :nl
        :match-pattern
        :match-pattern-before-after
        :match-after
        :match-groups
        :stdin-to-string
        )
    (:export
        :http-status
        :http-content-type
        :send-response
        :param
        :with-env-params
        ;; Need to export the variable names introduced by the anaphoric macro with-env-params,
        ;; otherwise the name belonging to the package where the variable names are used will be
        ;; prepended in the macro expansion.
        :env
        :params
        :qs ; export just for debugging
        ))

(in-package :utils-cgi)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun http-content-type (ct) 
    (cond 
        ((string-equal (symbol-name ct) "TEXT" ) "Content-Type: text/plain")
        ((string-equal (symbol-name ct) "JSON" ) "Content-Type: application/json")
        ((string-equal (symbol-name ct) "CSV" ) "Content-Type: text/csv")
        ;((string-equal (symbol-name ct) "EXCEL" ) "Content-Type: application/vnd.ms-excel")
        ((string-equal (symbol-name ct) "EXCEL" ) "Content-Type: application/msexcel")
        ;((string-equal (symbol-name ct) "EXCEL" ) "Content-Type: application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
        ((string-equal (symbol-name ct) "HTML" ) "Content-Type: text/html; charset=ISO-8859-1")
;        ((string-equal (symbol-name ct) "HTML" ) "Content-Type: text/html; charset=utf-8")
        (t (error "http-content-type: unknown content type: ~A" (princ-to-string ct)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HTTP status codes taken from here:
;;
;;     http://www.iana.org/assignments/http-status-codes/http-status-codes.xhtml
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun http-status (value)
    (cond 
        ((eql value 100) "Status: 100 Continue")
        ((eql value 101) "Status: 101 Switching Protocols")
        ((eql value 102) "Status: 102 Processing")
        ((eql value 200) "Status: 200 OK")
        ((eql value 201) "Status: 201 Created")
        ((eql value 202) "Status: 202 Accepted")
        ((eql value 203) "Status: 203 Non-Authoritative Information")
        ((eql value 204) "Status: 204 No Content")
        ((eql value 205) "Status: 205 Reset Content")
        ((eql value 206) "Status: 206 Partial Content")
        ((eql value 207) "Status: 207 Multi-Status")
        ((eql value 208) "Status: 208 Already Reported")
        ((eql value 226) "Status: 226 IM Used")
        ((eql value 300) "Status: 300 Multiple Choices")
        ((eql value 301) "Status: 301 Moved Permanently")
        ((eql value 302) "Status: 302 Found")
        ((eql value 303) "Status: 303 See Other")
        ((eql value 304) "Status: 304 Not Modified")
        ((eql value 305) "Status: 305 Use Proxy")
        ((eql value 306) "Status: 306 Reserved")
        ((eql value 307) "Status: 307 Temporary Redirect")
        ((eql value 308) "Status: 308 Permanent Redirect")
        ((eql value 400) "Status: 400 Bad Request")
        ((eql value 401) "Status: 401 Unauthorized")
        ((eql value 402) "Status: 402 Payment Required")
        ((eql value 403) "Status: 403 Forbidden")
        ((eql value 404) "Status: 404 Not Found")
        ((eql value 405) "Status: 405 Method Not Allowed")
        ((eql value 406) "Status: 406 Not Acceptable")
        ((eql value 407) "Status: 407 Proxy Authentication Required")
        ((eql value 408) "Status: 408 Request Timeout")
        ((eql value 409) "Status: 409 Conflict")
        ((eql value 410) "Status: 410 Gone")
        ((eql value 411) "Status: 411 Length Required")
        ((eql value 412) "Status: 412 Precondition Failed")
        ((eql value 413) "Status: 413 Request Entity Too Large")
        ((eql value 414) "Status: 414 Request-URI Too Long")
        ((eql value 415) "Status: 415 Unsupported Media Type")
        ((eql value 416) "Status: 416 Requested Range Not Satisfiable")
        ((eql value 417) "Status: 417 Expectation Failed")
        ((eql value 422) "Status: 422 Unprocessable Entity")
        ((eql value 423) "Status: 423 Locked")
        ((eql value 424) "Status: 424 Failed Dependency")
        ((eql value 426) "Status: 426 Upgrade Required")
        ((eql value 428) "Status: 428 Precondition Required")
        ((eql value 429) "Status: 429 Too Many Requests")
        ((eql value 431) "Status: 431 Request Header Fields Too Large")
        ((eql value 500) "Status: 500 Internal Server Error")
        ((eql value 501) "Status: 501 Not Implemented")
        ((eql value 502) "Status: 502 Bad Gateway")
        ((eql value 503) "Status: 503 Service Unavailable")
        ((eql value 504) "Status: 504 Gateway Timeout")
        ((eql value 505) "Status: 505 HTTP Version Not Supported")
        ((eql value 506) "Status: 506 Variant Also Negotiates (Experimental)")
        ((eql value 507) "Status: 507 Insufficient Storage")
        ((eql value 508) "Status: 508 Loop Detected")
        ((eql value 510) "Status: 510 Not Extended")
        ((eql value 511) "Status: 511 Network Authentication Required")
        (t (error "http-status: unknown status: ~A" (princ-to-string value)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun tagp (str)
    ;; CL-PPCRE version...
    (if (string= "" (match-pattern str "<[^>]+>")) nil T ))

(defun end-tagp (str)
    ;; CL-PPCRE version...
    (if (string= "" (match-pattern str "^</[^>]+>$")) nil T))

(defun empty-tagp (str)
    (cond 
        ;; CL-PPCRE version...
        ( (string> (match-pattern str "<.+/>") "" ) T )

        ;; Note that we use string-equal here to ignore case  because the use of 
        ;; def-tag-macros results in conversion to upper case (i.e. (def-empty-tag aaa)
        ;; will result in the tag <AAA>. We could have used upper case strings below
        ;; but string-equal is more robust.

        ( (and (> (length str) 5) (string-equal (subseq str 0 5) "<area")) T)
        ( (and (> (length str) 5) (string-equal (subseq str 0 5) "<base")) T)
        ( (and (> (length str) 9) (string-equal (subseq str 0 9) "<basefont")) T)
        ( (and (> (length str) 3) (string-equal (subseq str 0 3) "<br")) T)
        ( (and (> (length str) 4) (string-equal (subseq str 0 4) "<col")) T)
        ( (and (> (length str) 6) (string-equal (subseq str 0 6) "<frame")) T)
        ( (and (> (length str) 3) (string-equal (subseq str 0 3) "<hr")) T)
        ( (and (> (length str) 4) (string-equal (subseq str 0 4) "<img")) T)
        ( (and (> (length str) 6) (string-equal (subseq str 0 6) "<input")) T)
        ( (and (> (length str) 8) (string-equal (subseq str 0 8) "<isindex")) T)
        ( (and (> (length str) 5) (string-equal (subseq str 0 5) "<link")) T)
        ( (and (> (length str) 5) (string-equal (subseq str 0 5) "<meta")) T)
        ( (and (> (length str) 6) (string-equal (subseq str 0 6) "<param")) T)
        ( t nil) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun indent-html (str &optional (tempstr "") (tabstop 0))
    (let ((strings (match-pattern-before-after str "<[^>]+>")))
        (if (null (second strings)) 
            (strcat tempstr str)
            (if (end-tagp (second strings ))
                (indent-html 
                    (third strings) 
                    (strcat tempstr 
                            (if (first strings) ; i.e. there are chars before the tag in the string 
                                (strcat (spaces tabstop) 
                                        (first strings) 
                                        (nl) 
                                        (spaces (- tabstop 4))) 
                                (spaces (- tabstop 4))) 
                            (second strings) 
                            (nl)) 
                    (- tabstop 4))
                (indent-html 
                    (third strings)
                    (strcat tempstr 
                            (spaces tabstop) 
                            (first strings) 
                            (if (first strings) 
                                (strcat (nl) (spaces tabstop)) "" ) 
                            (second strings) (nl)) 
                    (if (empty-tagp(second strings))
                        tabstop
                        (+ tabstop 4) ))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro send-response (&rest html-strings)
    `(format t (indent-html (strcat ,@html-strings))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-multipart-name-value (str)
    (match-groups str "(?s)name=\"(.*?)\"(.*)" ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-content-type (str)
    (car (split str #\;)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gets the boundary string in a multipart/form-data form...
(defun multipart-boundary (str)
    (let ((a (car (splitstr str ";"))))
        (if (string= a "multipart/form-data")
            (subseq (second (splitstr str ";")) 10)
            "N/A")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Added a wrapping check for null str because for some reasone seem to be 
;; getting (()()) at the end of the list returned by call in get-name-val-pairs. 
;; Only happens when called via web server, not when pasting calls in to clisp. 
;; Have now commented out this check as we now remove the empty list from the 
;; end in get-name-val-pairs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun parse-value (str)
  ;(if (null str)

    ;; I've found that the structure of a multipart/form-data post is such that the value is preceded by two newlines and forllowed
    ;; by one - on Windows a newline consists of \r\n so the value is preceded by 4 bytes and followed by two.
    ;; Needs to work with DOS/Windows as well as Unix (and Mac) new lines.
    ;; i.e. when looking to match a newline we need to expect \r\n, \n or \r

    ;; First, try a match for a file upload.

    ;; This looks more complicated than it really is. Just need to make sure we deal with line breaks in a robust way:
    ;;     http://www.w3.org/Protocols/rfc2616/rfc2616-sec2.html#sec2.2
    ;;     http://www.w3.org/Protocols/rfc2616/rfc2616-sec19.html#sec19.3
    ;; The above suggests that although the correct representation of a newline is CRLF, a single LF should be recognised
    ;; asa the terminator and the leading CR ignored.
    ;; I tried some simpler approaches to matching CRLF or CR or LF but this was the only one that I found to work...

    ;; Notes on Perl regexps 
    ;;      (?:a|b|c) matches a, b or c but without capturing
    ;;      (?s) at beginning of pattern string switches on single-line mode 
    ;;      (.*?) does minimal matching

    ;; For more info see: 
    ;;      http://perldoc.perl.org/perlre.html
    ;;      http://weitz.de/cl-ppcre/
    ;;      http://www.regular-expressions.info/modifiers.html):

    ;; CL-PPCRE version...

    (let ((x (match-groups str "(?s); filename=\"(.*?)\"(?:\\r\\n|\\r|\\[^\\r]\\n)Content-Type: [a-zA-Z]*/[-.a-zA-Z]*(?:\\r\\n\\r\\n|\\n\\n|\\r\\r)(.*)(?:\\r\\n|\\r|[^\\r]\\n)$")))
        (if (null (car x))
            ;; If not a file upload then just strip off the newlines before and after and pick out the value...
            (match-groups str "(?s)(?:\\r\\n\\r\\n|\\n\\n|\\r\\r)(.*)(?:\\r\\n|\\r|[^\\r]\\n)$")
            ;; Otherwise, it was a file upload so return the match found above...
            x))
;  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun parse-name-value-pair (pair)
    (append (list (first pair)) (parse-value (second pair)) )
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun decode-string (url &optional tempstr)
    (if (or (string= "" url) (string= nil url))
        tempstr
        (cond
            ( (eq (char url 0) #\+) 
                (decode-string (subseq url 1) (strcat tempstr " ")))
            ( (eq (char url 0) #\%) 
                (decode-string (subseq url 3) (strcat tempstr (format nil "~c" (code-char (read-from-string (strcat "#x" (subseq url 1 3))))))))
            ( T
                (decode-string (subseq url 1) (strcat tempstr (subseq url 0 1)))))))

(defun decode-list (l)
    (mapcar #'decode-string l))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro get-name-val-pairs (query-string request-method content-type)
    `(cond 

        ;; multipart/form-data

        ;; Found that, at least with Apache using mod_proxy_scgi (doesn't happen with plain old CGI), it is possible for 
        ;; REQUEST_METHOD to be set to GET ;; but CONTENT_TYPE to be set to multipart/form-data. This happens after a form 
        ;; using POST is submitted but then instead of submitting the next form, the url is used to navigate to the page. 
        ;; This will cause errors in the code below unless we check that the REQUEST_METHOD is POST before executing the 
        ;; multipart/form-data code. Don't know if this is a bug in Apache or just my ignorance

        ((and (string= ,request-method "POST") (string= (get-content-type ,content-type) "multipart/form-data"))
            (mapcar 
                #'parse-name-value-pair 
                (mapcar 
                    #'get-multipart-name-value 
                    ;; Previously with-misc-encoding was used within match-after but has been moved out of there
                    ;; to here as match-after is a general utility not just for reading post data...
                    ;; It was needed when using clisp's regexp - probably no longer needed now that cl-ppcre is being used.
               ;     (with-misc-encoding charset:iso-8859-1 
               ;     (with-misc-encoding charset:utf-8 
                        (mapcargs 
                            #'match-after 
                            (remove-last 
                                ;; For some reason splitstr is adding on an empty list () at the end. 
                                ;; If we don't remove it here, we need to check for it in function parse-value...
                                (splitstr 
                                    (subseq ,query-string 0 (- (length ,query-string) 2)) 
                                    (strcat "--" (multipart-boundary ,content-type )))
                            )
                            "Content-Disposition: form-data; ")
               ;     )
                )))

        ;; If not POST and multipart/form-data then CONTENT_TYPE must be application/x-www-form-urlencoded.
        ;; Request method may be post or get - doesn't matter.
        (t
            (mapcar #'decode-list (mapcargs #'splitstr (splitstr ,query-string "&") "=")) )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun param (name params &optional templist)
    (if (null params) 
        (reverse templist)
;        (if (string= name (car (car params))) 
        (if (string-equal name (car (car params))) 
            (param name (cdr params) (cons (cdr (car params)) templist))
            (param name (cdr params) templist))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This is a purposefully anaphoric macro which captures the following  variables:
;;      env, 
;;      qs, 
;;      params
;; The following vars are now created as gensyms to avoid capture - they are not
;; required to be accessible from context within which the form is called:
;;      qs-get, 
;;      qs-post, 
;;      params-get, 
;;      params-post, 
;;
;; We get parameter name/value pairs from both the query strig and the request body if there is one.
;; Not sure how "correct" this is but seems that many frameworks do this, e.g. Plack
;; We prepend post params to get params.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro with-env-params (&rest body)
;    (let ((qs-get (gensym)) (qs-post (gensym)) (params-get (gensym)) (params-post (gensym)) (qs (gensym)) )
    (let ((qs-get (gensym)) (qs-post (gensym)) (params-get (gensym)) (params-post (gensym)) )
        `(let (( env (getenv)))
                ;; Can't get the setf to work here. Still doing it at top level.
                ;`(setf *misc-encoding* (ext:make-encoding :charset charset:iso-8859-1 :line-terminator :dos))
                (let 
                    ((,qs-get (first (param "QUERY_STRING" env)))
                    (,qs-post (if (string= (first (param "REQUEST_METHOD" env)) "POST")
                                    (stdin-to-string (getenv "CONTENT_LENGTH"))
                                    ())))

                    (let 
                        ((,params-get  (get-name-val-pairs ,qs-get "GET" (first (param "CONTENT_TYPE" env))))
                        (,params-post (get-name-val-pairs ,qs-post (getenv "REQUEST_METHOD") (first (param "CONTENT_TYPE" env)))))

                        (let 
;                            ((,qs (if (string= "GET" (getenv "REQUEST_METHOD")) ,qs-get ,qs-post))
                            ((qs (if (string= "GET" (getenv "REQUEST_METHOD")) ,qs-get ,qs-post))
                            ;(params (if (string= "GET" (getenv "REQUEST_METHOD")) ,params-get ,params-post)))
                            ;(params (if (string= "GET" (getenv "REQUEST_METHOD")) (append ,params-get ,params-post) (append ,params-post ,params-get))))
                            (params (append ,params-post ,params-get)))

                            ,@body))))))

