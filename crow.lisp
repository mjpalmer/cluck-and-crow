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

(provide :crow)

(defpackage :crow
    (:use :common-lisp)
    (:import-from :ext
        :getenv
        )
    (:import-from :utils-lang 
        :strcat 
        :nl 
        :subseq-safe
        :mkstr)
    (:export
        :tag
        :tagpair
        :emptytag
        :def-empty-tag
        :def-tag-pair
        :def-tag-macros
        :doctype
        :form-post-urlencoded
        :form-post-multipart
        :submit
        :hidden
        :anchor
        :options
        :env-to-string
        :html-body
        :page
        :prettyprint-params
        :list-to-table
        :stylesheet
        :def-tag-macros-
        :css-menu
        ))

(in-package :crow)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun list2paramvals (lst)
    (if (null (second lst))
        ""
        (strcat " " (first lst) "=" "\"" (second lst) "\"" "" (list2paramvals (cdr (cdr lst))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun tag_ (a tag args b)
    (strcat
        a
        tag
        (list2paramvals args)
        b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun tag (tagname &rest args)
    (tag_ "<" tagname args ">"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun emptytag (tagname &rest args)
    (tag_ "<" tagname args "/>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro tagpair (tag arglist &rest bodylst)
    `(strcat 
        (tag ,tag ,@arglist) 
        ,@bodylst 
        (tag (strcat "/" ,tag))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The following implementation was adapted from the examples in "On Lisp" by 
;; Paul Graham, Chapter 16.1, pp 213-215:
;;
;;      http://www.paulgraham.com/onlisptext.html
;;
;; Note that these macros use nested backquotes and nested unquote commas.
;;
;; There's a pattern here in these two macros 
;; - could be factored out even further...
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro def-empty-tag (tag-name macro-name)
    `(progn 
        (defmacro ,macro-name (&rest args)
            `(tag ,',(symbol-name tag-name) ,@args))
        (export ',macro-name)))

(defmacro def-tag-pair (tag-name macro-name)
    `(progn
        (defmacro ,macro-name (args &rest body)
            (assert 
                (listp args) 
                (args) 
                "~A: Expected first argument to be a list but got ~A: ~A" (symbol-name ',macro-name) (princ-to-string (type-of args)) (princ-to-string args))
            `(strcat
                (tag ,',(symbol-name tag-name) ,@args)
                ,@body
                (tag (strcat "/" ,',(symbol-name tag-name)))))
        (export ',macro-name)))

;; Looks like there might be variable capture going on here with tag but
;; there is not because of the nested backquotes...

(defmacro def-tag-macros (macro-name &rest tags)
  (let ((tag (gensym)))
    `(progn
        ,@(mapcar 
            #'(lambda (tag) 
                `(,macro-name ,tag ,(intern (concatenate 'string "<" (string tag) ">"))))
            tags))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Create void elements.
;;
;; The list of void elements here was taken from the following site:
;;
;;     http://www.htmlandcsswebdesign.com/articles/voidel.php
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-tag-macros def-empty-tag
    area base br col command embed hr img input keygen link meta param source
    track wbr
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Create non-void elements.
;;
;; The full list of HTML5 elements was taken from here:
;;
;;     http://www.w3.org/TR/html-markup/elements.html
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-tag-macros def-tag-pair
    a abbr address article aside audio b bdi bdo blockquote body button canvas 
    caption cite code colgroup datalist dd del details dfn dialog div dl dt em 
    fieldset figcaption figure footer form h1 head header html i iframe ins kbd 
    label legend li map mark menu meter nav noscript object ol optgroup option 
    output p pre progress q rp rt ruby s samp script section select small span 
    strong style sub summary sup table tbody td textarea tfoot th thead time 
    title tr u ul var video
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro form-post-urlencoded ( action &rest bodylist)
    `(tagpair "form" ("method" "POST" "action" ,action "enctype" "application/x-www-form-urlencoded") ,@bodylist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro form-post-multipart ( action &rest bodylist)
    `(tagpair "form" ("method" "POST" "action" ,action "enctype" "multipart/form-data") ,@bodylist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun submit (name value)
    (<input> "type" "submit" "name" name "value" value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun hidden (name value)
    (<input> "type" "hidden" "name" name "value" value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun anchor (url text)
    (tagpair "a" ("href" url) text))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun options (items)
    (let ((str ""))
        (dolist (i items)
            ;; Each item in items may contain just one value which is also used for the option text
            ;; or it can contain a value plus text to be displayed...
            (if (null (second i))
                (setf str (strcat str (<option> ("value" (mkstr (first i))) (mkstr (first i)))))
                (setf str (strcat str (<option> ("value" (mkstr (first i))) (mkstr (second i)))))))
        str))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun env-to-string ()
  (with-output-to-string (s)
    (dolist (a (getenv)) (princ (concatenate 'string "   " (car a) " = " (cdr a) (<br>)) s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun prettyprint-params (l &optional temp )
    (if (null l)
        temp
;        (prettyprint-params (cdr l) (strcat temp "^" (first (car l)) "^" (second (car l)) "^"(<br>) ))))
        (prettyprint-params (cdr l) (strcat temp "^" (first (car l)) "^" (second (car l)) "^" (third (car l)) "^" (<br>) ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun doctype (doctype) 
    (cond 
        ((string-equal doctype "html")
            (strcat
                "<!DOCTYPE html>" (nl)
                ))
        (t "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro html-body (title css arglist &rest body)
    `(strcat
        (doctype "html")
        (<html> ()
            (<head> ()
                (<title> () ,title)
                (<meta> "charset" "iso-8859-1")
;                (<meta> "charset" "utf-8")
                (stylesheet ,css)
                )
            (<body> ,arglist
                ,@body))))

(defmacro page (title action css arglist &rest body)
    `(strcat
        (html-body ,title ,css ,arglist
            (form-post-multipart ,action ,@body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(defmacro page (title action css body-arglist &rest body)
;    `(strcat
;        (doctype "html")
;        (<html> ()
;            (<head> ()
;                (<title> () ,title)
;                (<meta> "charset" "iso-8859-1")
;;                (<meta> "charset" "utf-8")
;                (stylesheet ,css)
;                )
;            (<body> ,body-arglist
;                (form-post-multipart ,action ,@body)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Experimented with truncating the data in the cells to cope with large amounts
;; of data...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun list-to-table (lst)
    (let ( (temp1 "") (temp2 "") )
        (dolist (line lst)
            (setf temp1 "")
            (dolist (element line)
                (setf temp1 (strcat temp1 (<td> () element))))
;                (setf temp1 (strcat temp1 (<td> () (subseq-safe element 10)))))
            (setf temp2 (strcat temp2 (<tr> () temp1))))
        (<table> ("border" "1") temp2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; <link rel="stylesheet" type="text/css" href="mystyle.css">
;;

(defmacro stylesheet (file)
    (<link> "rel" "stylesheet" "type" "text/css" "href" file))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro css-menu (css-class &rest items)
    `(<div> ("class" ,css-class)
        (<ul> ()
            ,@(mapcar 
                #'(lambda (item) 
                     `(<li> () ,item ))
                items))))

