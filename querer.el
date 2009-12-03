;;; querer.el --- Display menu/message, QUERy for options, and select one. 
;; Time-stamp: <2005-08-07 17:26:29 deego>
;; Copyright (C) 2002 D. Goel
;; Emacs Lisp Archive entry
;; Filename: querer.el
;; Package: querer
;; Author: D. Goel <deego@glue.umd.edu>
;; Keywords: query ask menu options y-or-n-p read-char select choice
;; Version: 0.1.4
;; Author's homepage: http://www.glue.umd.edu/~deego
;; For latest version: 

(defconst querer-home-page
  "http://www.gnufans.org/~deego/emacspub/lisp-mine/querer")


 
;; This file is NOT (yet) part of GNU Emacs.
 
;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
 
;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.


(defconst querer-version "0.1.4")

;;; New since 0.0:
;;   Name change to querer.el (there's already a query.el)
;;   Provide a recursive edit feature. 
 
;;; Commentary:
;; querer is a small utility to query the user for options and select
;; one of the options.  So, meant for elisp authors.  
;; Main function is: querer.   
;; Tested with emacs >= 21.2, breaks on 20.7.

;; This example says it all:
(defun querer-example ()
  (interactive)
  (querer 
    '((?s save-some-buffers)
      (?S save-buffer))
    "s: Type this to save some buffers, 
S: Type this to save this buffer,
q: to quit
e: recursive edit."
    ))
;; Thanks:
;;  Roman Belenov

;; Currently known user(s) of querer.el:  ges.el
;;==========================================
;;; Code:

(defgroup querer nil 
  "The group querer"
  :group 'applications)
(defcustom querer-after-load-hooks nil 
  "Hooks to run after loading querer."
  :group 'querer)
(run-hooks 'querer-before-load-hooks)

(defvar querer-bindings-last nil) 
(defvar querer-message-last nil) 
(defvar querer-prompt-last nil) 

(defcustom querer-buffer "*Querer-Message*"
  ""
  :type 'string
  :group 'querer)

(defcustom querer-prompt "Make a choice (e for recursive edit, q to quit): "
  ""
  :type 'string
  :group 'querer)


(defcustom querer-bindings-extra
  `((?e querer-recursive-edit)
    ;; C-l
    (12 querer-recenter)
    (?q querer-quit))
  ""
  :group 'querer)


(defun querer-recursive-edit (&optional inhibit-message)
  (interactive)
  (unwind-protect
      (progn
	(unless inhibit-message
	  (message "Entering recursive edit. Press C-M-c when done. "))
	;;(read-char "press something to continue")
	(recursive-edit))
    ;;(message "Done.. Resuming. ")
    (querer querer-bindings-last querer-message-last querer-prompt-last)))

;(defvar querer-window-original nil)
;(defvar querer-window-configuration nil
;;  "internal")
(defun querer-message-display (mesg)
  (with-output-to-temp-buffer  querer-buffer
    (princ mesg))
   ;; this one not needed, but trying... so that menu may show up at
   ;; bottom, hopefully..
  (delete-other-windows)
  (split-window-vertically)
  (select-window 
   (car (last (window-list))))
  (switch-to-buffer querer-buffer)
  (fit-window-to-buffer
   (selected-window))

  ;(fit-window-to-buffer
  ; (get-buffer-window querer-buffer)
   )

(defun querer-message-undisplay ()
  ;; bury rather than kill.. the user might need it..
  (bury-buffer querer-buffer)
  ;;(set-window-configuration querer-window-configuration)
  ;;(select-window querer-window-original)
  )

(defun querer-quit (&rest args)
  (interactive)
  'querer-quit)


(defvar querer-args-internal nil)
(defun querer-recenter ()
  (interactive)
  (recenter)
  (apply 'querer querer-args-internal))


;;;###autoload
(defun querer (bindings &optional message prompt)
  "Query for a key, accept one of the choices. 
PROMPT gets displayed in the minibuffer. 
MESSAGE is the long passage displayed in the dialog box. 
q quits--don't forget to mention that in your message. 
MESSAGE can also be nil or 'auto

Each of BINDINGS is of the form: 
 (key function &rest arguments)
"
  (let (fnargs
	(querer-bindings-last bindings)
	(querer-message-last message)
	(querer-prompt-last prompt)
	(querer-args-internal (list bindings message prompt))
	)
    (save-window-excursion
      (unless prompt (setq prompt querer-prompt))
      (setq bindings
	    (append bindings
		    querer-bindings-extra))
      
      (when (equal message 'auto)
	(setq message 
	      (mapconcat 'identity 
			 (mapcar 
			  '(lambda (binding)
			     (format "%c: %S" (car binding) (cadr
							     binding)))
			  bindings)
			 
			 "\n")))
      (when message
	;;(setq querer-window-configuration
	 ;;     (current-window-configuration))
	;;(setq querer-window-original
	 ;;     (selected-window))
	(querer-message-display message))
      
      (let ((donep nil)
	    input key-fn fn
	    )
	(while (not donep)
	  (setq input (let ((cursor-in-echo-area t))
			(read-event prompt)))
	  (setq key-fn (assoc input 
			      bindings))
	  
	  (when key-fn
	    (setq donep t)
	    (setq fnargs (cdr key-fn))))
	(when message (querer-message-undisplay))))
    (apply (car
	    fnargs)
	   (cdr fnargs))))


;;;###autoload
(defun* querer-auto-eval (expressions &optional (message 'auto) prompt
				      nicks)
  "Like `querer', but generate bindings automatically.


In other words, this function is a bit more high level than
`querer'. The author just provides us the various possible EXPRESSIONS
that can be executed at this point, and we shall take care of the rest
here---we shall convert those expressions into functions and then
automatically come up with bindings for those functions.  For an
example, see the implemantation of `synth-options-choose-speaker'.


 
When nicks are provided and message is 'auto, we generate the message
from nicks.
"
  (let*
      ((c1 (loop for i from ?0 to ?9 collect i))
       (c2 (loop for i from ?a to ?z collect i))
       (c3 (loop for i from ?A to ?Z collect i))
       (choices (append c1 c2 c3))
       (pointer choices)
       (remaining expressions)
       c e
       rbindings bindings)
    (while remaining
      (setq e (car pointer) pointer (cdr pointer))
      (while (member* (car choices)
		      querer-bindings-extra 
		      :key 'car)
	(setq choices (cdr choices)))
      (setq c (car choices) choices (cdr choices))
      (setq rbindings
	    (cons 
	     (copy-tree 
	      `(,c 
		(lambda ()
		  (interactive)
		  ,(copy-tree (car remaining)))))
	     rbindings))
      (setq remaining (cdr remaining))
      )
    (setq bindings (reverse rbindings))
    (querer bindings
	    (if (and nicks (eql message 'auto))
		(mapconcat 
		 'identity 
		 (mapcar*
		  #'(lambda (binding nick)
		      (format "%c: %s" (car binding) nick))
		  bindings nicks)
		 "\n")
	      message)
	    prompt)))






(provide 'querer)
(run-hooks 'querer-after-load-hooks)



;;; querer.el ends here
