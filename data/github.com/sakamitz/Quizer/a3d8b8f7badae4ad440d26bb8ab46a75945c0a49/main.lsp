#!/usr/local/bin/newlisp

;;; initialization
(load (append (env "NEWLISPDIR")
		"/guiserver.lsp"))
(gs:init)

(setf lib-list '()
	  lib-len 0)

;;; widget definitions

;; frame
(gs:frame 'MyFrame 300 150 380 290 "Quiz Gamer")
(gs:set-resizable 'MyFrame nil)
(gs:set-flow-layout 'MyFrame "center" 2 15)

(gs:label 'MsgLabel "Tell me your class first." "center" 400 20)
; --------------------------------------------------------------
;; panel-1
(gs:panel 'CommitPanel)
(gs:set-flow-layout 'CommitPanel "left" 10 10)
(gs:set-titled-border 'CommitPanel "I'm in Class ...")

(gs:text-field 'aTextField 'do-nothing 4)

(gs:button 'ButtonCommit 'commit-handler "Commit" "center")

(gs:add-to 'CommitPanel 'aTextField 'ButtonCommit)
; --------------------------------------------------------
;; panel-2
(gs:panel 'GenPanel)
(gs:set-flow-layout 'GenPanel "left" 10 10)
(gs:set-titled-border 'GenPanel "Name Here")

(gs:label 'NameLabel "-------" "center" 300 30)
(gs:set-font 'NameLabel "Monospaced" 30 "bold")

(gs:add-to 'GenPanel 'NameLabel)
; ------------------------------------------------------
;; bottom of frame
(gs:button 'ButtonNext 'next-handler "Next" "center")
(gs:disable 'ButtonNext)


;;; function definitions

(define (do-nothing))
(define (commit-handler)
	(set 'lib-file-name (append "lib/" (gs:get-text 'aTextField) ".txt"))

	(if (file? lib-file-name)
		(begin
			(gs:set-text 'MsgLabel "OK, let's begin.")
			(gs:set-foreground 'MsgLabel gs:gray)
			(read-lib lib-file-name)
			(gs:enable 'ButtonNext))
		(begin
			(gs:set-text 'MsgLabel "You must be joking.")
			(gs:set-foreground 'MsgLabel gs:black)
			(gs:set-text 'aTextField ""))))

(define (next-handler)
	(gs:disable 'ButtonNext)
	(gs:set-text 'NameLabel "? ? ?")
	(sleep 1500)
	(setf next-index (rand lib-len))
	(gs:set-text 'NameLabel (lib-list next-index))
	(gs:enable 'ButtonNext))

(define (read-lib file-name)
	(setf lib-list '())
	(set 'lib-file (open file-name "read"))
	(while (read-line lib-file)
		(if (char (current-line))
			(push (current-line) lib-list)))
	(close lib-file)
	(setf lib-len (length lib-list)))


(gs:add-to 'MyFrame 'MsgLabel 'CommitPanel 'GenPanel 'ButtonNext)
(gs:set-visible 'MyFrame true)
(gs:listen)