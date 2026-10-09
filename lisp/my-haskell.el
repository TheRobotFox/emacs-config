;;; my-haskell.el --- Haskell adapters: sessions, REPL lookup, notation -*- lexical-binding: t; -*-

;;; Commentary:
;; Everything Haskell-specific that other components reach through hooks and
;; dispatch tables: session repair for haskell-mode, REPL lookup for the popup
;; panes, Hoogle as documentation provider, and Haskell-aware prettification.

;;; Code:
(require 'prog-mode)

(declare-function haskell-session "haskell-session")
(declare-function haskell-session-maybe "haskell-session")
(declare-function haskell-session-name "haskell-session")
(declare-function haskell-session-process "haskell-session")
(declare-function haskell-session-set-process "haskell-session")
(declare-function haskell-session-get "haskell-session")
(declare-function haskell-process-process "haskell-process")
(declare-function haskell-process-make "haskell-process")
(declare-function haskell-process-start "haskell-process")

;;;; Sessions

(defun my/haskell-ensure-process-for-load (&rest _)
  "Ensure the current Haskell session has a running process before loading.
A retained session can have missing process state or an exited GHCi child.
Rebuild broken state with an empty queue so stale commands are not replayed."
  (let* ((session (haskell-session))
         (state (haskell-session-process session))
         (process (and state (haskell-process-process state))))
    (unless (and (processp process) (process-live-p process))
      ;; Initialize state before starting: the upstream restart path writes
      ;; to it if an orphaned process still exists under the session name.
      (haskell-session-set-process session
                                   (haskell-process-make (haskell-session-name session)))
      (haskell-process-start session))))

(with-eval-after-load 'haskell
  (advice-add 'haskell-process-file-loadish :before #'my/haskell-ensure-process-for-load))

(defun my/haskell-repl-buffer ()
  "Return the running GHCi buffer of the current session, for `my/popup-repl'.
Never create one: `haskell-session-interactive-buffer' would start and select
a REPL even when the session has no process."
  (when (fboundp 'haskell-session-maybe)
    (let* ((session (haskell-session-maybe))
           (state (and session (haskell-session-process session)))
           (process (and state (haskell-process-process state)))
           (buffer (and session (haskell-session-get session 'interactive-buffer))))
      (unless (and (processp process) (process-live-p process) (buffer-live-p buffer))
        (user-error "No running GHCi session; start it with M-x haskell-process-load-file"))
      buffer)))

;;;; Documentation

(declare-function consult-hoogle "consult-hoogle" (arg &optional command))

(defun my/haskell-hoogle (thing)
  "Search Hoogle for THING, or prompt, for `my/documentation-providers'."
  (require 'consult-hoogle)
  (if thing
      (minibuffer-with-setup-hook (lambda () (insert thing)) (consult-hoogle nil))
    (consult-hoogle nil)))

;;;; Notation

(defface my/haskell-composition-face
  '((t (:height 1.4)))
  "Size of composition symbols."
  :group 'faces)

(defcustom my/haskell-composition-raise -0.1
  "Vertical offset for enlarged composition symbols, in character heights.
Negative values lower the symbol.  Refontify buffers after changing this."
  :type 'number
  :group 'faces)

(defconst my/haskell-composition-keywords
  '(("\\."
     (0 (when (get-text-property (match-beginning 0) 'composition)
          (put-text-property (match-beginning 0) (match-end 0)
                             'display (list 'raise my/haskell-composition-raise))
          'my/haskell-composition-face)
        prepend)))
  "Apply size after prettification has identified composition operators.")

(defun my/haskell-composition-font-lock-setup ()
  "Keep composition sizing after prettification, and remove it when disabled."
  (font-lock-remove-keywords nil my/haskell-composition-keywords)
  (setq-local font-lock-extra-managed-props
              (cons 'display (remq 'display font-lock-extra-managed-props)))
  (when prettify-symbols-mode
    (font-lock-add-keywords nil my/haskell-composition-keywords 'append))
  (font-lock-flush))

(defun my/haskell-prettify-compose-p (start end match)
  "Keep dots in qualified names, numbers and larger operators literal.
Prettify standalone dots, retaining the default string/comment checks."
  (and (prettify-symbols-default-compose-p start end match)
       (or (not (equal match "."))
           (not (or (memq (char-syntax (or (char-before start) ?\s)) '(?w ?_ ?. ?\\))
                    (memq (char-syntax (or (char-after end) ?\s)) '(?w ?_ ?. ?\\)))))))

(defun my/haskell-prettify-setup ()
  "Use Haskell-aware boundaries for symbolic substitutions."
  (setq-local prettify-symbols-compose-predicate #'my/haskell-prettify-compose-p)
  (add-hook 'prettify-symbols-mode-hook #'my/haskell-composition-font-lock-setup nil t)
  (my/haskell-composition-font-lock-setup))

(provide 'my-haskell)
;;; my-haskell.el ends here
