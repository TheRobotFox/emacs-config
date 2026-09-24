;;; my-documentation.el --- Contextual documentation lookup -*- lexical-binding: t; -*-

(require 'seq)
(autoload 'devdocs-lookup "devdocs" nil t)
(autoload 'consult-hoogle "consult-hoogle" nil t)
(autoload 'dictionary-search "dictionary" nil t)

(defgroup my/documentation nil "Contextual documentation." :group 'help)
(defcustom my/documentation-functions
  '((haskell-mode . consult-hoogle)
    (haskell-ts-mode . consult-hoogle)
    (emacs-lisp-mode . describe-symbol)
    (text-mode . dictionary-search)
    (eww-mode . dictionary-search)
    (prog-mode . devdocs-lookup)
    (t . devdocs-lookup))
  "Documentation command symbols by derived mode; first match wins.
The mode t is the fallback."
  :type '(alist :key-type symbol :value-type symbol))

(defun my/documentation-command ()
  "Return the documentation command for the current major mode."
  (if (bound-and-true-p olivetti-mode)
      #'dictionary-search
    (seq-some (lambda (entry)
              (when (or (eq (car entry) t) (derived-mode-p (car entry)))
                (cdr entry)))
              my/documentation-functions)))

;;;###autoload
(defun my/dictionary-at-point ()
  "Display the definition of the word at point without prompting."
  (interactive)
  (let ((word (thing-at-point 'word t)))
    (unless word (user-error "No word at point"))
    (dictionary-search word)))

;;;###autoload
(defun my/documentation-lookup (&optional choose)
  "Look up documentation using the current mode's provider.
With prefix CHOOSE, select a provider for this lookup."
  (interactive "P")
  (let* ((default (my/documentation-command))
         (command
          (if choose
              (intern (completing-read
                       "Documentation provider: "
                       (mapcar #'symbol-name
                               (seq-uniq (mapcar #'cdr my/documentation-functions)))
                       nil t nil nil (and default (symbol-name default))))
            default)))
    (unless (commandp command) (user-error "No documentation command for %s" major-mode))
    (let ((current-prefix-arg nil))
      (call-interactively command))))

(provide 'my-documentation)
;;; my-documentation.el ends here
