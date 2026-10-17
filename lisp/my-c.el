;;; my-c.el --- C/C++ tooling: comments, banners, Doxygen, source files -*- lexical-binding: t; -*-

;;; Commentary:
;; Comment continuation on newline (c-ts-mode's own counts columns in
;; characters, which breaks with tabs, and takes a // inside a block comment
;; for a line comment), comment filling that works with Doxygen highlighting,
;; section banners, Doxygen skeletons extracted with treesit and edited with
;; YASnippet, and switching between headers and sources, creating the latter.

;;; Code:
(require 'cl-lib)
(require 'subr-x)
(require 'treesit)
(declare-function yas-expand-snippet "yasnippet" (snippet &optional start end expand-env))
(declare-function yas-minor-mode "yasnippet" (&optional arg))
(defvar yas-indent-line)
(defvar auto-insert)
(declare-function ff-find-other-file "find-file" (&optional in-other-window ignore-include event))
(declare-function c-ts-common--fill-paragraph "c-ts-common" (&optional arg))

;;;; Comments

(defun my/c-doc-newline ()
  "Continue a comment under its start: a line comment with its slashes, a
block comment with its star prefix.  Code gets a plain `newline', with its
electric pairing and indentation."
  (interactive)
  (let* ((state (syntax-ppss))
         (start (and (nth 4 state) (nth 8 state)))
         (block (and start (eq (char-after (1+ start)) ?*))))
    (if (not start)
        (newline nil t)
      (pcase-let ((`(,column . ,prefix)
                   (save-excursion
                     (cond ((not block)
                            (goto-char start)
                            (looking-at "//[/!]*[ \t]*")
                            (cons (current-column) (match-string-no-properties 0)))
                           ((progn (back-to-indentation)
                                   (and (looking-at "\\*+[ \t]*")
                                        (not (eq (char-after (match-end 0)) ?/))))
                            (cons (current-column) (match-string-no-properties 0)))
                           (t (cons (1+ (progn (goto-char start) (current-column))) "* "))))))
        (delete-horizontal-space)
        (newline)
        (indent-to column)
        (insert prefix)
        ;; Keep a closing delimiter on its own line.
        (when (and block (looking-at "[ \t]*\\*/"))
          (save-excursion
            (delete-region (point) (progn (skip-chars-forward " \t") (point)))
            (newline)
            (indent-to column)))))))

(defun my/c-fill-paragraph (&optional arg)
  "Fill like c-ts-mode, finding comments with the primary parser; ARG as there.
With `c-ts-mode-enable-doxygen', point in a comment is in the embedded Doxygen
parser, where `c-ts-common--fill-paragraph' would find no comment to fill."
  (let ((node-at (symbol-function 'treesit-node-at)))
    (cl-letf (((symbol-function 'treesit-node-at)
               (lambda (pos &optional parser-or-lang named)
                 (funcall node-at pos (or parser-or-lang treesit-primary-parser) named))))
      (c-ts-common--fill-paragraph arg))))

;;;; Banners

(defun my/c-doc--banner (title width boxed)
  "Format TITLE to WIDTH as a line comment, or as a box when BOXED."
  (let ((title (string-trim title)))
    (when (or (string-empty-p title) (string-match-p "[\n\r]" title))
      (user-error "Enter a nonempty, single-line title"))
    (if boxed
        (let* ((inner (max (+ (string-width title) 2) (- width 5)))
               (padding (- inner (string-width title)))
               (left (/ padding 2))
               (border (make-string inner ?─)))
          (format "// ╭%s╮\n// │%s%s%s│\n// ╰%s╯"
                  border (make-string left ?\s) title (make-string (- padding left) ?\s) border))
      (let* ((padding (max 6 (- width (string-width title) 8)))
             (left (/ padding 2)))
        (format "// %s %s %s //" (make-string left ?-) title (make-string (- padding left) ?-))))))

(defun my/c-doc--insert-banner (boxed)
  "Insert a banner for the selected standalone title, or prompt for one.
A selected title is replaced; otherwise the banner goes above the current line."
  (barf-if-buffer-read-only)
  (let* ((selected (use-region-p))
         (start (if selected (region-beginning) (point)))
         (end (and selected (region-end)))
         (title (if selected (buffer-substring-no-properties start end)
                  (read-string "Banner title: "))))
    (save-excursion
      (goto-char start)
      (when (nth 8 (syntax-ppss (line-beginning-position)))
        (user-error "Insert the banner outside strings and comments"))
      (when (and selected
                 (or (not (string-blank-p (buffer-substring (line-beginning-position) start)))
                     (not (string-blank-p (buffer-substring end (save-excursion (goto-char end) (line-end-position)))))))
        (user-error "Select a standalone title, without surrounding code")))
    (goto-char start)
    (let* ((indent (buffer-substring-no-properties (line-beginning-position)
                                                   (save-excursion (back-to-indentation) (point))))
           (banner (my/c-doc--banner title (- fill-column (current-indentation)) boxed)))
      (atomic-change-group
        (beginning-of-line)
        (when selected
          (delete-region (point) (save-excursion (goto-char end) (line-beginning-position 2))))
        (insert indent (string-replace "\n" (concat "\n" indent) banner) "\n"))
      (setq deactivate-mark t))))

(defun my/c-doc-banner ()
  "Insert a one-line section banner for the selected text or a prompted title."
  (interactive)
  (my/c-doc--insert-banner nil))

(defun my/c-doc-box-banner ()
  "Insert a boxed section banner for the selected text or a prompted title."
  (interactive)
  (my/c-doc--insert-banner t))

;;;; Doxygen skeletons

(defun my/c-doc--declarator-chain (node)
  "Return NODE's declarator chain, without descending into parameter lists."
  (when node
    (cons node
          (my/c-doc--declarator-chain
           (or (treesit-node-child-by-field-name node "declarator")
               (when (member (treesit-node-type node)
                             '("parenthesized_declarator" "reference_declarator" "variadic_declarator"))
                 (treesit-node-child node 0 t)))))))

(defun my/c-doc--parameter-name (node)
  "Return the name the parameter NODE declares, or nil when it is unnamed."
  (let ((leaf (car (last (my/c-doc--declarator-chain
                          (treesit-node-child-by-field-name node "declarator"))))))
    (and leaf (not (string-prefix-p "abstract_" (treesit-node-type leaf)))
         (treesit-node-text leaf t))))

(defun my/c-doc--template-name (node)
  "Return the name of the template parameter NODE, or nil when it is unnamed."
  (or (my/c-doc--parameter-name node)
      (when (equal (treesit-node-type node) "template_template_parameter_declaration")
        (my/c-doc--template-name (car (last (treesit-node-children node t)))))
      (when-let* ((name (seq-find (lambda (child) (equal (treesit-node-type child) "type_identifier"))
                                  (treesit-node-children node t))))
        (treesit-node-text name t))))

(defun my/c-doc--target ()
  "Find a declaration at point and return (DECLARATION . OUTER-NODE).
OUTER-NODE includes enclosing template declarations."
  (unless (derived-mode-p 'c-ts-mode 'c++-ts-mode)
    (user-error "Use this command in c-ts-mode or c++-ts-mode"))
  (let* ((language (if (derived-mode-p 'c++-ts-mode) 'cpp 'c))
         (node (save-excursion (skip-chars-forward " \t\n") (treesit-node-at (point) language)))
         (types '("function_definition" "declaration" "field_declaration"
                  "struct_specifier" "class_specifier" "template_declaration")))
    (while (and node (not (member (treesit-node-type node) types)))
      (setq node (treesit-node-parent node)))
    (unless node (user-error "No supported declaration at point"))
    (when (equal (treesit-node-type node) "template_declaration")
      (setq node (car (last (treesit-node-children node t)))))
    (let ((outer node))
      (while (equal (treesit-node-type (treesit-node-parent outer)) "template_declaration")
        (setq outer (treesit-node-parent outer)))
      (cons node outer))))

(defun my/c-doc--template-parameters (node outer)
  "Return template parameter names between NODE and OUTER, outermost first."
  (let (names)
    (while (not (treesit-node-eq node outer))
      (setq node (treesit-node-parent node))
      (when-let* ((parameters (treesit-node-child-by-field-name node "parameters")))
        (setq names (append (delq nil (mapcar #'my/c-doc--template-name
                                              (treesit-node-children parameters t)))
                            names))))
    names))

(defun my/c-doc--returns-p (node function chain)
  "Whether FUNCTION declared by NODE through CHAIN returns a value."
  (let* ((trailing (seq-find (lambda (child) (equal (treesit-node-type child) "trailing_return_type"))
                             (treesit-node-children function t)))
         (type (treesit-node-child-by-field-name node "type"))
         ;; A macro before the type parses as the type, the type as an error:
         ;; the word before the declarator ends the return type.
         (text (if trailing
                   (string-trim (string-remove-prefix "->" (treesit-node-text trailing t)))
                 (and type (car (last (split-string
                                       (buffer-substring-no-properties
                                        (treesit-node-start type)
                                        (treesit-node-start
                                         (treesit-node-child-by-field-name node "declarator"))))))))))
    (when (and (not trailing) (member text '("auto" "decltype(auto)")))
      (user-error "Deduced return type needs an explicit trailing return type"))
    (and text
         (or (not (equal text "void"))
             (and (not trailing)
                  (seq-some (lambda (part) (member (treesit-node-type part)
                                                   '("pointer_declarator" "reference_declarator")))
                            chain))))))

(defun my/c-doc--description (node outer)
  "Return (:templates NAMES :parameters NAMES :returns BOOL) for NODE within OUTER."
  (when (seq-some (lambda (part) (equal (treesit-node-type part) ",")) (treesit-node-children node))
    (user-error "Document one declarator at a time"))
  (let* ((chain (my/c-doc--declarator-chain (treesit-node-child-by-field-name node "declarator")))
         ;; The declarator nearest the name: outer ones belong to a returned
         ;; function pointer's type.
         (function (car (last (seq-filter (lambda (part) (equal (treesit-node-type part) "function_declarator"))
                                          chain)))))
    (list :templates (my/c-doc--template-parameters node outer)
          :parameters (and function
                           (delq nil (mapcar #'my/c-doc--parameter-name
                                             (treesit-node-children
                                              (treesit-node-child-by-field-name function "parameters") t))))
          :returns (and function (my/c-doc--returns-p node function chain)))))

(defun my/c-doc--snippet (description)
  "Format DESCRIPTION as a Doxygen snippet with numbered editable fields."
  (let ((index 1)
        (lines (list " * @brief ${1:Summary.}" "/**"))) ; built in reverse
    (pcase-dolist (`(,tag . ,names) `(("tparam" . ,(plist-get description :templates))
                                      ("param" . ,(plist-get description :parameters))))
      (dolist (name names)
        (push (format " * @%s %s ${%d:Description.}" tag
                      (replace-regexp-in-string "[\\\\$`]" "\\\\\\&" name) (cl-incf index))
              lines)))
    (when (plist-get description :returns)
      (push (format " * @return ${%d:Description.}" (cl-incf index)) lines))
    (string-join (nreverse (append '("$0" " */") lines)) "\n")))

(defun my/c-doc-insert ()
  "Insert a Doxygen skeleton for the C/C++ declaration at point."
  (interactive)
  (barf-if-buffer-read-only)
  (pcase-let* ((`(,node . ,outer) (my/c-doc--target))
               (description (my/c-doc--description node outer))
               (start (treesit-node-start outer))
               (previous (treesit-node-prev-sibling outer t)))
    (when (and previous (equal (treesit-node-type previous) "comment")
               (string-match-p "\\`/\\(?:\\*[*!]\\|/[!/]\\)" (treesit-node-text previous t)))
      (user-error "Declaration is already documented"))
    (unless (save-excursion
              (goto-char start)
              (string-blank-p (buffer-substring-no-properties (line-beginning-position) start)))
      (user-error "Put the declaration on its own line before documenting it"))
    (require 'yasnippet)
    (unless (bound-and-true-p yas-minor-mode) (yas-minor-mode 1))
    (goto-char start)
    (let* ((yas-indent-line 'none)
           (indent (buffer-substring-no-properties (line-beginning-position) start))
           (snippet (string-replace "\n" (concat "\n" indent) (my/c-doc--snippet description))))
      (atomic-change-group
        (yas-expand-snippet snippet start start)))))

;;;; Source files

(defun my/c-other-file ()
  "Visit the other of this header and its source file, in another window.
A header without one gets a new implementation file, with its include and
namespaces."
  (interactive)
  (unless buffer-file-name (user-error "This buffer does not visit a file"))
  (let* ((file buffer-file-name)
         (extension (pcase (file-name-extension file) ("h" "c") ("hpp" "cpp")))
         (target (and extension (file-name-with-extension file extension))))
    (if (or (not target) (file-exists-p target) (get-file-buffer target))
        (ff-find-other-file t)
      (my/c--create-src-file file target extension))))

(defun my/c--create-src-file (file target extension)
  "Create TARGET, the EXTENSION implementation file for the header FILE."
  (let (namespaces)
    ;; Inspect the header before switching buffers; plain C needs no parser.
    (when (equal extension "cpp")
      (unless (treesit-parser-list)
        (user-error "C++ namespace detection requires a Tree-sitter parser"))
      (let ((node (treesit-node-at (point) 'cpp)))
        (while node
          (when (equal (treesit-node-type node) "namespace_definition")
            (when-let* ((name (treesit-node-child-by-field-name node "name")))
              (push (treesit-node-text name t) namespaces)))
          (setq node (treesit-node-parent node)))))
    (let ((namespace (string-join namespaces "::")))
      ;; The include is written here, so auto-insert's template stays out.
      (let ((auto-insert nil)) (find-file-other-window target))
      (atomic-change-group
        (insert "#include \"" (file-name-nondirectory file) "\"\n\n"
                (if (string-empty-p namespace) ""
                  (format "namespace %s {\n} // %s" namespace namespace)))))))

(provide 'my-c)
;;; my-c.el ends here
