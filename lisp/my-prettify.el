;;; my-prettify.el --- One symbol table, per-mode prettification -*- lexical-binding: t; -*-

;;; Commentary:
;; Symbols are named once in `my/pretty-symbols'; modes declare which source
;; texts map to which names with `my/prettify-set'.  Optional math notation
;; (subscripts and deltas) is declared with `my/math-set' and toggled per
;; buffer by `my/math-symbols-mode'.  Font ligatures are configured separately.

;;; Code:
(require 'cl-lib)
(require 'prog-mode)
(require 'seq)

(defvar my/pretty-symbols
  '((composition . ?∘)
    (arrow . ?→)
    (left-arrow . ?←)

    (string . ?𝕊)
    (boolean . ?𝔹)
    (integer . ?ℤ)
    (real . ?ℝ)
    (natural . ?ℕ)

    (infinity . ?∞)
    (sqrt . ?√)

    (null . ?∅)
    (true . ?⊤)
    (false . ?⊥)
    (not . ?¬)
    (and . ?∧)
    (or . ?∨)
    (for . ?∀)
    (return . ?⟼)
    (yield . ?⟻)
    (delta . ?Δ)
    (epsilon . ?ε))
  "Shared characters for language declarations.
After editing, re-evaluate the declarations and run `my/prettify-setup'.")

(defun my/pretty-symbol (name)
  "Look up NAME in `my/pretty-symbols', reporting unknown names."
  (or (alist-get name my/pretty-symbols)
      (error "Unknown pretty symbol: %S" name)))

(defun my/prettify--symbols (entries)
  "Resolve (TEXT SYMBOL-NAME) ENTRIES into prettification pairs."
  (mapcar (lambda (entry)
            (pcase entry
              (`(,(and text (pred stringp)) ,(and name (pred symbolp)))
               (cons text (my/pretty-symbol name)))
              (_ (error "Expected (TEXT SYMBOL-NAME), got %S" entry))))
          entries))

(defvar my/prettify-rules nil
  "Alist of major modes and their ordinary `prettify-symbols-alist' entries.
Register complete tables with `my/prettify-set'.")
(defvar my/math-rules nil
  "Mode-specific notation enabled by `my/math-symbols-mode'.")
(defvar-local my/prettify--base nil)
(defvar-local my/prettify--installed nil)
(defvar-local my/prettify--initialized nil)
(defvar my/math-symbols-mode)

(defmacro my/prettify-set (modes &rest entries)
  "Declare ENTRIES as (TEXT SYMBOL-NAME) pairs for unquoted MODES.
MODES is a mode name or list of mode names.  No entries clears their tables."
  (declare (indent 1))
  `(my/prettify-register ',modes (my/prettify--symbols ',entries)))

(defun my/prettify-register (modes symbols)
  "Replace the SYMBOLS table for MODES (one mode or a list).
Nil removes the table.  Re-evaluating a declaration does not append rules.
Use `my/prettify-setup' to refresh an already open buffer."
  (dolist (mode (ensure-list modes))
    (setf (alist-get mode my/prettify-rules nil t) (copy-tree symbols))))

(defun my/math-register (modes bases indices deltas symbols)
  "Register subscript BASES, INDICES, DELTAS and explicit SYMBOLS for MODES.
Each base is a single-character string; source names use x_i and d_x."
  (let ((rules
         (append symbols
                 (cl-loop for base in deltas
                          unless (and (stringp base) (= (length base) 1))
                          do (error "Delta base must be one character: %S" base)
                          collect (cons (concat "d_" base)
                                        (list (my/pretty-symbol 'delta) '(Br . Bl)
                                              (aref base 0))))
                 (cl-loop for base in bases
                          unless (and (stringp base) (= (length base) 1))
                          do (error "Subscript base must be one character: %S" base)
                          append (cl-loop for index across indices
                                          collect (cons (concat base "_" (string index))
                                                        (list (aref base 0) '(Br . cl) index)))))))
    (dolist (mode (ensure-list modes))
      (setf (alist-get mode my/math-rules nil t) rules))))

(cl-defmacro my/math-set (modes &key subscripts (indices "ijkn0123456789") deltas symbols)
  "Declare optional notation for unquoted MODES.
SUBSCRIPTS lists single-character base strings; INDICES lists allowed suffixes.
DELTAS lists variables whose d_ prefix should display as delta.
SYMBOLS uses the same (TEXT SYMBOL-NAME) pairs as `my/prettify-set'."
  (declare (indent 1))
  `(my/math-register ',modes ',subscripts ,indices ',deltas
     (my/prettify--symbols ',symbols)))

(defun my/prettify--merge (&rest tables)
  "Merge TABLES, keeping the first entry for each text string."
  (seq-uniq (apply #'append tables)
            (lambda (a b) (equal (car a) (car b)))))

(defun my/prettify--mode-rules (table)
  "Merge TABLE entries from the current mode through its parents."
  (apply #'my/prettify--merge
         (mapcar (lambda (mode) (alist-get mode table))
                 (derived-mode-all-parents major-mode))))

(defun my/prettify-setup (&optional enable)
  "Refresh symbols, preserving major-mode defaults and the user's toggle.
Precedence is exact mode, nearest parent, math notation, then existing
defaults.  Enable prettification on first setup when rules apply, or when
ENABLE is non-nil."
  (interactive)
  (let* ((rules (my/prettify--mode-rules my/prettify-rules))
         (math (and my/math-symbols-mode (my/prettify--mode-rules my/math-rules)))
         (active (or prettify-symbols-mode enable
                     (and (not my/prettify--initialized) (or rules math)))))
    ;; Remove only entries we installed; retain later additions by packages.
    (setq my/prettify--base
          (my/prettify--merge
           (cl-remove-if (lambda (entry) (memq entry my/prettify--installed))
                         prettify-symbols-alist)
           my/prettify--base))
    (when prettify-symbols-mode (prettify-symbols-mode -1))
    (setq-local prettify-symbols-alist
                (my/prettify--merge rules math my/prettify--base))
    (setq my/prettify--installed
          (cl-remove-if (lambda (entry) (memq entry my/prettify--base))
                        prettify-symbols-alist)
          my/prettify--initialized t)
    (when active (prettify-symbols-mode 1))))

(define-minor-mode my/math-symbols-mode
  "Toggle the notation declared by `my/math-set' in this buffer.
For a project, enable this in its .dir-locals.el via a mode entry.
The ordinary `prettify-symbols-mode' command toggles all symbol display."
  :lighter " Math"
  (my/prettify-setup my/math-symbols-mode))

(provide 'my-prettify)
;;; my-prettify.el ends here
