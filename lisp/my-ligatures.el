;;; my-ligatures.el --- Personal configuration helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Loaded by config.org; edit this file directly.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defvar my/ligatures-extra-symbols
  '(;; org
    ;;     :name          "»"
    ;;     :src_block     "»"
    ;;     :src_block_end "«"
    ;;     :quote         "“"
    ;;     :quote_end     "”"

    ;; Functional
    :lambda        "λ"
    :def           "ƒ"
    :composition   "○"
    :map           "↦"
    :to            "→"
    :from          "←"

    ;; Types
    :null          "∅"
    :true          "⊤"
    :false         "⊥"
    :int           "ℤ"
    :float         "ℝ"
    :str           "𝕊"
    :bool          "𝔹"
    :list          "𝕃"

    ;; Flow
    :not           "¬"
    :in            "∈"
    :not-in        "∉"
    :and           "∧"
    :or            "∨"
    :for           "∀"
    :some          "∃"
    :return        "⟼"
    :yield         "⟻"

    ;; Other
    :sqrt          "√"
    :infinity      "∞"
    :uint          "ℕ"
    :union         "⋃"
    :intersect     "∩"
    :diff          "∖"
    :tuple         "⨂"
    :pipe          "" ;; FIXME: find a non-private char
    :dot           "•"))
;; "Maps identifiers to symbols, recognized by `set-ligatures'.

(defun my/cartesian-product-call (fn l1 l2)
  (mapcan (lambda (a)
            (mapcar (lambda (b) (funcall fn a b)) l2))
          l1))
(defvar my/fancy-vars
  (append (mapcar (lambda (character)
                    (cons (string ?d character)
                          (list ?Δ '(Br . cl) character)))
                  (string-to-list "xyzwts"))
          (my/cartesian-product-call (lambda (character subscript)
                                       (cons (string character subscript)
                                             (list character '(Br . cl) subscript)))
                                     (string-to-list "xyzktw")
                                     (string-to-list "nik0123456789"))))

;;; ui/ligatures/autoload/ligatures.el -*- lexical-binding: t; -*-

;;;###autodef
(defun my/set-ligatures (modes &rest plist)
  "Associates string patterns with icons in certain major-modes.

  MODES is a major mode symbol or a list of them.
  PLIST is a property list whose keys must match keys in
`my/ligatures-extra-symbols', and whose values are strings representing the text
to be replaced with that symbol.

If the car of PLIST is nil, then unset any
pretty symbols and ligatures previously defined for MODES.

For example, the rule for emacs-lisp-mode is very simple:

  (after! elisp-mode
    (my/set-ligatures \\='emacs-lisp-mode
      :lambda \"lambda\"))

This will replace any instances of \"lambda\" in emacs-lisp-mode with the symbol
associated with :lambda in `my/ligatures-extra-symbols'.

Pretty symbols can be unset by passing `nil':

  (after! rustic
    (my/set-ligatures \\='rustic-mode nil))

Note that this will keep all ligatures in `my/ligatures-prog-mode-list' active, as
`emacs-lisp-mode' is derived from `prog-mode'."
  (declare (indent defun))
  (if (null (car-safe plist))
      (dolist (mode (ensure-list modes))
        (setf (alist-get mode my/ligatures-extra-alist nil t) nil))
    (let ((results))
      (while plist
        (let ((key (pop plist)))
          (when-let (char (plist-get my/ligatures-extra-symbols key))
            (push (cons (pop plist) char) results))))
      (dolist (mode (ensure-list modes))
        (setf (alist-get mode my/ligatures-extra-alist)
              (if-let* ((old-results (alist-get mode my/ligatures-extra-alist)))
                  (dolist (cell results old-results)
                    (setf (alist-get (car cell) old-results) (cdr cell)))
                results))))))

;;;###autodef
(defun my/set-font-ligatures (modes &rest ligatures)
  "Associates string patterns with ligatures in certain major-modes.

  MODES is a major mode symbol or a list of them.
  LIGATURES is a list of ligatures that should be handled by the font,
    like \"==\" or \"-->\". LIGATURES is a list of strings.

For example, the rule for emacs-lisp-mode is very simple:

  (my/set-font-ligatures \\='emacs-lisp-mode \"->\")

This will ligate \"->\" into the arrow of choice according to your font.

All font ligatures for emacs-lisp-mode can be unset with:

  (my/set-font-ligatures \\='emacs-lisp-mode nil)

However, ligatures for any parent modes (like `prog-mode') will still be in
effect, as `emacs-lisp-mode' is derived from `prog-mode'."
  (declare (indent defun))
  (with-eval-after-load 'ligature
    (if (or (null ligatures) (equal ligatures '(nil)))
        (dolist (table ligature-composition-table)
          (let ((modes (ensure-list modes))
                (tmodes (car table)))
            (cond ((and (listp tmodes) (cl-intersection modes tmodes))
                   (let ((tmodes (cl-nset-difference tmodes modes)))
                     (setq ligature-composition-table
                           (if tmodes
                               (cons tmodes (cdr table))
                             (delete table ligature-composition-table)))))
                  ((memq tmodes modes)
                   (setq ligature-composition-table (delete table ligature-composition-table))))))
      (ligature-set-ligatures modes ligatures))))

(defvar my/ligatures-extra-alist '((t))
  "A map of major modes to symbol lists (for `prettify-symbols-alist').

To configure this variable, use `my/set-ligatures'.")

(defvar my/ligatures-extras-in-modes t
  "List of major modes where extra ligatures should be enabled.

Extra ligatures are mode-specific substituions, defined in
`my/ligatures-extra-symbols' and assigned with `my/set-ligatures'. This variable
controls where these are enabled.

  If t, enable it everywhere (except `fundamental-mode').
  If the first element is not, enable it in any mode besides what is listed.
  If nil, don't enable these extra ligatures anywhere (though it's more
efficient to remove the `+extra' flag from the :ui ligatures module instead).")

(defun my/ligatures--enable-p (modes)
  "Return t if ligatures should be enabled in this buffer depending on MODES."
  (unless (eq major-mode 'fundamental-mode)
    (or (eq modes t)
        (if (eq (car modes) 'not)
            (not (apply #'derived-mode-p (cdr modes)))
          (apply #'derived-mode-p modes)))))

(defun my/ligatures-init-extra-symbols-h ()
  "Set up `prettify-symbols-mode' for the current buffer.

Overwrites `prettify-symbols-alist' and activates `prettify-symbols-mode' if
(and only if) there is an associated entry for the current major mode (or a
parent mode) in `my/ligatures-extra-alist' AND the current mode (or a parent mode)
isn't disabled in `my/ligatures-extras-in-modes'."

  (when-let*
      (((my/ligatures--enable-p my/ligatures-extras-in-modes))
       (symbols
        (if-let* ((symbols (assq major-mode my/ligatures-extra-alist)))
            (cdr symbols)
          (cl-loop for (mode . symbols) in my/ligatures-extra-alist
                   if (derived-mode-p mode)
                   return symbols))))
    (setq prettify-symbols-alist
          (append symbols
                  ;; Don't overwrite global defaults
                  my/fancy-vars
                  (default-value 'prettify-symbols-alist)))
    (when (bound-and-true-p prettify-symbols-mode)
      (prettify-symbols-mode -1))
    (prettify-symbols-mode +1)))

(provide 'my-ligatures)
;;; my-ligatures.el ends here
