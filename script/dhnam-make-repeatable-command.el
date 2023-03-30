
;;;;; Some part of this code is should be called before <make-repeatable-command> should work.
;;;;; However, I couldn't find it...
;;;;;
(defadvice AAAAAAAAAAAAAAAAA (around vlf-if-file-too-large
                                           compile activate)
  "If file SIZE larger than `large-file-warning-threshold', \
allow user to view file with `vlf', open it normally, or abort.
OP-TYPE specifies the file operation being performed over FILENAME."
  (cond
   ((or (not size) (zerop size)))
   ((or (not vlf-application)
        (not filename)
        (memq (vlf-determine-major-mode filename)
              vlf-forbidden-modes-list))
    ad-do-it)
   ((eq vlf-application 'always)
    (vlf filename)
    (error ""))
   ((and large-file-warning-threshold
         (< large-file-warning-threshold size)
         (< vlf-batch-size size))
    (if (eq vlf-application 'dont-ask)
        (progn (vlf filename)
               (error ""))
      (let ((char nil))
        (while (not (memq (setq char
                                (read-event
                                 (propertize
                                  (format
                                   "File %s is large (%s): \
%s normally (o), %s with vlf (v) or abort (a)"
                                   (if filename
                                       (file-name-nondirectory filename)
                                     "")
                                   (file-size-human-readable size)
                                   op-type op-type)
                                  'face 'minibuffer-prompt)))
                          '(?o ?O ?v ?V ?a ?A))))
        (cond ((memq char '(?v ?V))
               (vlf filename)
               (error ""))
              ((memq char '(?a ?A))
               (error "Aborted"))))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(unless (fboundp 'make-repeatable-command)
  (require 'repeat)
  (defun make-repeatable-command (cmd)
    "Returns a new command that is a repeatable version of CMD.
The new command is named CMD-repeat.  CMD should be a quoted
command.

This allows you to bind the command to a compound keystroke and
repeat it with just the final key.  For example:

  (global-set-key (kbd \"C-c a\") (make-repeatable-command 'foo))

will create a new command called foo-repeat.  Typing C-c a will
just invoke foo.  Typing C-c a a a will invoke foo three times,
and so on.

See related discussion here: 
http://batsov.com/articles/2012/03/08/emacs-tip-number-4-repeat-last-command/#comment-459843643
https://groups.google.com/forum/?hl=en&fromgroups=#!topic/gnu.emacs.help/RHKP2gjx7I8"
    (fset (intern (concat (symbol-name cmd) "-repeat"))
          `(lambda ,(help-function-arglist cmd) ;; arg list
             ,(format "A repeatable version of `%s'." (symbol-name cmd)) ;; doc string
             ,(interactive-form cmd) ;; interactive form
             ;; see also repeat-message-function
             (setq last-repeatable-command ',cmd)
             (repeat nil)))
    (intern (concat (symbol-name cmd) "-repeat"))))


(provide 'dhnam-make-repeatable-command)
