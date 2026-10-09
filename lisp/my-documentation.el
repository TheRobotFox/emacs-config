;;; my-documentation.el --- One documentation interface, per-mode providers -*- lexical-binding: t; -*-

;;; Commentary:
;; `my/documentation-lookup' prompts and `my/documentation-at-point' uses the
;; symbol or word at point; both call the provider registered for the current
;; buffer in `my/documentation-providers'.  A provider is a function of one
;; argument: the thing to look up as a string, or nil to prompt.  Adapters for
;; common backends are below; language modules add their own.

;;; Code:
(require 'seq)
(declare-function devdocs-lookup "devdocs" (&optional ask-docs initial-input))
(declare-function dictionary-search "dictionary" (word &optional dictionary))
(declare-function info-lookup-symbol "info-look" (symbol &optional mode same-window))

(defgroup my/documentation nil "Contextual documentation." :group 'help)

(defcustom my/documentation-providers nil
  "Documentation providers by context; the first match wins.
Each key is a major mode symbol matched with `derived-mode-p', a predicate
function called with no arguments, or t for the fallback.  Each value is a
function receiving the thing to look up, or nil to prompt."
  :type '(alist :key-type sexp :value-type function))

(defun my/documentation-provider ()
  "Return the provider for the current buffer, or nil."
  (seq-some (lambda (entry)
              (let ((key (car entry)))
                (when (cond ((eq key t) t)
                            ((symbolp key) (derived-mode-p key))
                            (t (funcall key)))
                  (cdr entry))))
            my/documentation-providers))

(defun my/documentation-thing ()
  "Return the region, the symbol at point in code, or the word at point."
  (cond ((use-region-p)
         (string-trim (buffer-substring-no-properties (region-beginning) (region-end))))
        ((derived-mode-p 'prog-mode) (thing-at-point 'symbol t))
        (t (thing-at-point 'word t))))

(defun my/documentation--call (thing choose)
  "Pass THING to the current provider, or to one selected when CHOOSE."
  (let ((provider (if choose
                      (let ((names (mapcar (lambda (entry) (format "%s" (car entry)))
                                           my/documentation-providers)))
                        (cdr (nth (seq-position names (completing-read "Documentation provider: " names nil t))
                                  my/documentation-providers)))
                    (my/documentation-provider))))
    (unless provider (user-error "No documentation provider for %s" major-mode))
    (funcall provider thing)))

;;;###autoload
(defun my/documentation-lookup (&optional choose)
  "Look up documentation, prompting through the current mode's provider.
With prefix CHOOSE, select the provider."
  (interactive "P")
  (my/documentation--call nil choose))

;;;###autoload
(defun my/documentation-at-point (&optional choose)
  "Show documentation for the symbol or word at point.
With prefix CHOOSE, select the provider."
  (interactive "P")
  (my/documentation--call (or (my/documentation-thing) (user-error "Nothing at point")) choose))

;;;; Adapters

(defun my/documentation-devdocs (thing)
  "Look THING up in the buffer's DevDocs documents."
  (devdocs-lookup nil thing))

(defun my/documentation-dictionary (thing)
  "Show THING's dictionary definition."
  (if thing (dictionary-search thing) (call-interactively #'dictionary-search)))

(defun my/documentation-describe-symbol (thing)
  "Describe the Emacs Lisp symbol THING."
  (if thing
      (describe-symbol (or (intern-soft thing) (user-error "No symbol named %s" thing)))
    (call-interactively #'describe-symbol)))

(defun my/documentation-info (thing)
  "Look THING up in the mode's Info manual."
  (if thing (info-lookup-symbol thing) (call-interactively #'info-lookup-symbol)))

(provide 'my-documentation)
;;; my-documentation.el ends here
