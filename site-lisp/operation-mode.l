;; operation-mode
(provide "operation-mode")
(in-package "editor")

(export '(operation-mode
		  *operation-mode-hook*
		  *operation-mode-map*
		  *operation-mode-syntax-table*
		  copy-command-as-kill
		  clear-copyed-attribute
		  insert-log
		  operation-mode-set-prompt
		  operation-mode-reset-prompt
		  *operation-copyed-attribute-bold*
		  *operation-copyed-attribute-underline*
		  *operation-copyed-attribute-strike-out*
		  *operation-copyed-attribute-foreground*
		  *operation-copyed-attribute-background*
		  *operation-log-start-string*
		  *operation-log-end-string*
		  *operation-log-attribute-bold*
		  *operation-log-attribute-underline*
		  *operation-log-attribute-strike-out*
		  *operation-log-attribute-foreground*
		  *operation-log-attribute-background*
		  ))

; hook
(defvar *operation-mode-hook* nil)

; start mode command
(defun operation-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq buffer-mode 'operation-mode)
  (setq mode-name "Operation")
  (use-keymap *operation-mode-map*)
  
  ; var initialize
  (make-local-variable 'operation-prompt)
  (setq *operation-prompt* nil)
  
  ; load keyword
  (and *operation-keyword-file*
	   (null *operation-keyword-hash-table*)
	   (setq *operation-keyword-hash-table*
			 (load-keyword-file *operation-keyword-file* t)))
  (when *operation-keyword-hash-table*
	(make-local-variable 'keyword-hash-table)
	(setq keyword-hash-table *operation-keyword-hash-table*))

  ; use syntax
  (use-syntax-table *operation-mode-syntax-table*)

  ; add hooks
  (add-hook '*deactivate-hook* 'reset-this-command)
  
  (run-hooks '*operation-mode-hook*))

; key map
(defvar *operation-mode-map* nil)
(unless *operation-mode-map*
  (setq *operation-mode-map* (make-sparse-keymap))
  (define-key *operation-mode-map* #\M-c 'copy-command-as-kill)
  (define-key *operation-mode-map* #\M-n 'clear-copyed-attribute)
  (define-key *operation-mode-map* #\M-l 'insert-log))

; keyword
(defvar *operation-keyword-hash-table* nil)
(defvar *operation-keyword-file* "Operation")

; syntax table
(defvar *operation-mode-syntax-table* nil)
(unless *operation-mode-syntax-table*
  (setq *operation-mode-syntax-table* (make-syntax-table))
  (set-syntax-start-comment *operation-mode-syntax-table* #\#)
  (set-syntax-end-comment *operation-mode-syntax-table* #\LFD nil t))

; special var
(defvar *operation-copyed-attribute-bold* nil)
(defvar *operation-copyed-attribute-underline* nil)
(defvar *operation-copyed-attribute-strike-out* nil)
(defvar *operation-copyed-attribute-foreground* 0)
(defvar *operation-copyed-attribute-background* 9)
(defvar *operation-log-start-string* "--log--")
(defvar *operation-log-end-string* "--log--")
(defvar *operation-log-attribute-bold* nil)
(defvar *operation-log-attribute-underline* nil)
(defvar *operation-log-attribute-strike-out* nil)
(defvar *operation-log-attribute-foreground* 2)
(defvar *operation-log-attribute-background* 0)
(defvar *operation-prompt* nil)

; actions
(defun copy-command-as-kill ()
  (interactive)
  (if (eq *last-command* 'copy-command-as-kill)
	  (copy-command-continuously)
	(copy-command-first))
  (setq *this-command* 'copy-command-as-kill))

(defun copy-command-first ()
  (let ((start nil) (end nil) (line (current-line-number)))
	(beginning-of-line)
	(setq start (point))
	(end-of-line)
	(setq end (point))
	(if *operation-prompt*
		(progn (goto-char start)
		  (if (search-forward *operation-prompt* t)
			  (if (and (= line (current-line-number)) (not (= end (+ (point) 1))))
				  (setq start (+ (point) 1))
				(goto-char start)))))
	(copy-region-as-kill start end)
	(set-copyed-attribute start end)
	(next-line)
	(beginning-of-line)))

(defun copy-command-continuously ()
  (let ((start nil) (end nil))
	(beginning-of-line)
	(open-line)
	(setq start (point))
	(next-line)
	(end-of-line)
	(setq end (point))
	(append-next-kill)
	(copy-region-as-kill start end)
	(set-copyed-attribute start end)
	(goto-char start)
	(delete-char)
	(next-line)
	(beginning-of-line)))

(defun set-copyed-attribute (start end)
  (set-text-attribute start end 'operation-copyed
					  :bold *operation-copyed-attribute-bold*
					  :underline *operation-copyed-attribute-underline*
					  :strike-out *operation-copyed-attribute-strike-out*
					  :foreground *operation-copyed-attribute-foreground*
					  :background *operation-copyed-attribute-background*))

(defun clear-copyed-attribute ()
  (interactive)
  (delete-text-attributes 'operation-copyed))

(defun reset-this-command ()
  (when (eq *this-command* 'copy-command-as-kill)
	(setq *this-command* 'next-virtual-line)))

(defun insert-log ()
  (interactive)
  (new-line-if-need)
  (let ((start nil) (end nil))
	(setq start (point))
	(insert *operation-log-start-string* "\n\n" *operation-log-end-string* "\n")
	(backward-char)
	(setq end (point))
	(set-log-attribute start end)
	(previous-virtual-line)))

(defun new-line-if-need ()
  (let ((begin nil) (end nil))
	(beginning-of-line)
	(setq begin (point))
	(end-of-line)
	(setq end (point))
	(when (/= begin end)
	  (newline))))

(defun set-log-attribute (start end)
  (set-text-attribute start end 'operation-log
					  :bold *operation-log-attribute-bold*
					  :underline *operation-log-attribute-underline*
					  :strike-out *operation-log-attribute-strike-out*
					  :foreground *operation-log-attribute-foreground*
					  :background *operation-log-attribute-background*))

(defun operation-mode-set-prompt (prompt)
  (interactive "sInput Prompt: ")
  (if (stringp prompt)
	  (setq *operation-prompt* prompt)
	(setq *operation-prompt* nil)))

(defun operation-mode-reset-prompt ()
  (interactive)
	(setq *operation-prompt* nil))
