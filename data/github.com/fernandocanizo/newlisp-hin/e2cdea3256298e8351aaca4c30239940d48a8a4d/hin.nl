#!/usr/bin/env newlisp
;; Creation Date: Wed, 08 May 2013 09:41:37 -0300
;; @author Fernando Canizo (aka conan) - http://conan.muriandre.com/
;; @module hin
;; @description DSL for HTML tags
;; @version 2013.05.13

; TODO properly indent output would be nice

(context 'hin)

(constant 'doctype "<!DOCTYPE html>\n")

(define (comment str-comment)
	(if (< (length str-comment) 80)
		(string "\n<-- " str-comment " -->\n")
		; else
		(string "\n<--\n" str-comment "\n-->\n")))

(setq globalAttributes '(
	"class"
	"contenteditable"
	"contextmenu"
	"dir"
	"draggable"
	"hidden"
	"id"
	"lang"
	"style"
	"title"
	))

(setq specialAttributes '(
	("html" ("manifest"))
	("base" ("href" "target"))
	))

(setq tags '(
	"address"
	"body"
	"div"
	"h1"
	"h2"
	"h3"
	"h4"
	"h5"
	"h6"
	"hr"
	"html"
	"base"
	"head"
	))

(setq allAttributes (copy globalAttributes)) ; start with globalAttributes...
(dolist (a specialAttributes) ; ... then add the rest
	(dolist (b (last a))
		(if (not (member b allAttributes))
			(push b hin:allAttributes -1))))

; TODO if allAttributes is not needed elsewhere, then build constants in one step instead of this
(dolist (a allAttributes)
	(constant (sym (string a "=")) (string a "=")))

; allowed attributes for each tag
; (setq tags+attributes '(
; 	("address" ("_globals"))
; 	("body" ("_globals")) ; also only one body is allowed per html document, it would be nice to check that
; 	("div" ("_globals"))
; 	("h1" ("_globals"))
; 	("h2" ("_globals"))
; 	("h3" ("_globals"))
; 	("h4" ("_globals"))
; 	("h5" ("_globals"))
; 	("h6" ("_globals"))
; 	("hr" ("_globals"))
; 	("html" ("_globals" "manifest")) ; would be nice to set default lang="en", also there can be only one per document
; 	("base" ("_globals" "href" "target")) ; there can be only one
; 	("head" ("_globals")) ; head may contain only these elements: base, link, meta, script, style, and title

; 	))

;; build association list with all tags and allowed attributes for each one
(setq tags+attributes '())

(define (build-tag-attribute str-tag lst-attributes)
	(list str-tag lst-attributes))

(dolist (t tags)
	(let (auxSA (assoc t specialAttributes))
		(if (true? auxSA)
			(push (build-tag-attribute t (flat (list globalAttributes (last auxSA)))) tags+attributes)
			; else put only global attributes
			(push (build-tag-attribute t globalAttributes) tags+attributes))))


; macro which builds every htmltag-function
; TODO maybe I don't need to test tagName, maybe just have to use the evaluated result
; TODO modify macro to build a function which test if attr is allowed for tag
(define-macro (hin:hin tagName)
	(eval (list 'define (list (sym (if (symbol? tagName) (string "." (eval tagName)) (string "." tagName))))
		(list 'let (list
			(list 'innerCode (list 'string "<" (eval tagName))) ; holds attributes and values
			(list 'betweenTags "") ; holds content
			(list 'myArgs '$args)) ; can't touch $idx, so to consume arbitrarily the arguments, I need to copy argument list and manage it myself

			(list 'do-until (list 'null? 'myArgs)
				(list 'let (list (list 'currentArg (list 'pop 'myArgs)))
					(list 'if (list 'protected? (list 'sym 'currentArg))
						; get next arg (which is value for attribute) and append everything into innerCode
						(list 'setq 'innerCode (list 'string 'innerCode " " 'currentArg (list 'pop 'myArgs)))
						; else
						(list 'setq 'betweenTags (list 'string 'betweenTags 'currentArg)))))

			(list 'string 'innerCode ">" 'betweenTags "</" (eval tagName) ">")))))


; create the htmltag-functions
(dolist (tag tags)
	(hin tag))
