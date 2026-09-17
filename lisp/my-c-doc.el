;;; my-c-doc.el --- Context-aware C/C++ documentation -*- lexical-binding: t; -*-

;;; Commentary:
;; Extract declarations with treesit; edit descriptions with YASnippet.

;;; Code:
(require 'cl-lib)
(require 'subr-x)
(require 'treesit)
(declare-function yas-expand-snippet "yasnippet" (snippet &optional start end expand-env))
(declare-function yas-minor-mode "yasnippet" (&optional arg))
(defvar yas-indent-line)

(defun my/c-doc--banner (title width &optional boxed)
  "Format TITLE to WIDTH as a line comment, or a box when BOXED."
  (let ((title (string-trim title)))
    (when (or (string-empty-p title) (string-match-p "[\n\r]" title))
      (user-error "Enter a nonempty, single-line title"))
    (if boxed
        (let* ((inner (max (+ (string-width title) 2) (- width 5)))
               (padding (- inner (string-width title)))
               (left (/ padding 2))
               (border (make-string inner ?─)))
          (format "// ╭%s╮\n// │%s%s%s│\n// ╰%s╯"
                  border (make-string left ?\s) title
                  (make-string (- padding left) ?\s) border))
      (let* ((padding (max 6 (- width (string-width title) 8)))
             (left (/ padding 2)))
        (format "// %s %s %s //"
                (make-string left ?-) title (make-string (- padding left) ?-))))))

(defun my/c-doc--banner-title ()
  "Read a banner title, using the active region when available."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (read-string "Banner title: ")))

(defun my/c-doc--insert-banner (title boxed)
  "Insert TITLE as a banner, using BOXED style when non-nil.
Replace a selected standalone title; otherwise insert above the current line."
  (barf-if-buffer-read-only)
  (let* ((selected (use-region-p))
         (start (if selected (region-beginning) (point)))
         (end (and selected (region-end))))
    (save-excursion
      (goto-char start)
      (when (nth 8 (syntax-ppss (line-beginning-position)))
        (user-error "Insert the banner outside strings and comments"))
      (when (and selected
                 (or (not (string-blank-p
                           (buffer-substring (line-beginning-position) start)))
                     (not (string-blank-p
                           (buffer-substring end (save-excursion
                                                   (goto-char end) (line-end-position)))))))
        (user-error "Select a standalone title, without surrounding code")))
    (goto-char start)
    (let* ((column (current-indentation))
           (indent (buffer-substring-no-properties
                    (line-beginning-position)
                    (save-excursion (back-to-indentation) (point))))
           (banner (my/c-doc--banner title (- fill-column column) boxed)))
      (atomic-change-group
        (beginning-of-line)
        (when selected
          (delete-region (point) (save-excursion (goto-char end) (line-beginning-position 2))))
        (insert indent
                (replace-regexp-in-string "\n" (concat "\n" indent) banner t t)
                "\n"))
      (setq deactivate-mark t))))

;;;###autoload
(defun my/c-doc-banner (title)
  "Insert a one-line section banner for TITLE or the selected standalone text."
  (interactive (list (my/c-doc--banner-title)))
  (my/c-doc--insert-banner title nil))

;;;###autoload
(defun my/c-doc-box-banner (title)
  "Insert a boxed section banner for TITLE or the selected standalone text."
  (interactive (list (my/c-doc--banner-title)))
  (my/c-doc--insert-banner title t))

(defun my/c-doc--prefix (start)
  "Return (COLUMN . PREFIX) for the comment starting at START."
  (let ((origin (point)))
    (save-excursion
      (save-match-data
        (goto-char start)
        (let ((column (current-column)))
          (if (looking-at "//[/!]*[ \t]*")
              (cons column (match-string-no-properties 0))
            (goto-char origin)
            (back-to-indentation)
            (if (and (looking-at "\\*+[ \t]*")
                     (not (eq (char-after (match-end 0)) ?/)))
                (cons (current-column) (match-string-no-properties 0))
              (cons (1+ column) "* "))))))))

(defun my/c-doc-newline ()
  "Continue comment prefixes at point, or insert an indented code line."
  (interactive)
  (let* ((state (syntax-ppss))
         (start (and (nth 4 state) (nth 8 state))))
    (if (not start)
        (newline-and-indent)
      (pcase-let ((`(,column . ,prefix) (my/c-doc--prefix start)))
        (delete-horizontal-space)
        (newline)
        (indent-to column)
        (insert prefix)
        (when (looking-at "[ \t]*\\*/")
          (save-excursion
            (delete-region (point) (progn (skip-chars-forward " \t") (point)))
            (newline)
            (indent-to column)))))))

(defun my/c-doc-fill-paragraph (&optional justify)
  "Reflow the current comment paragraph, preserving its delimiters."
  (let* ((language (if (derived-mode-p 'c++-ts-mode) 'cpp 'c))
         (node (treesit-node-at (point) language)))
    (when (equal (treesit-node-type node) "comment")
      (if (string-prefix-p "//" (treesit-node-text node t))
          (fill-comment-paragraph justify)
        (let ((start (treesit-node-start node))
              (end (treesit-node-end node))
              (fill-paragraph-function nil))
          (save-excursion
            (goto-char start)
            (skip-chars-forward "/*!" end)
            (setq start (if (looking-at "[ \t]*$")
                            (line-beginning-position 2) (point)))
            (goto-char end)
            (when (looking-back "\\*/" (- end 2)) (backward-char 2))
            (setq end (if (string-blank-p
                           (buffer-substring (line-beginning-position) (point)))
                          (line-beginning-position)
                        (skip-chars-backward " \t" start)
                        (point))))
          (when (< start end)
            (save-restriction
              (narrow-to-region start end)
              (fill-paragraph justify))))))
    t))

(defun my/c-doc--children (node)
  "Return the named children of NODE."
  (cl-loop for i below (treesit-node-child-count node t)
           collect (treesit-node-child node i t)))

(defun my/c-doc--declarator-chain (node)
  "Return NODE's declarator chain, without descending into parameter lists."
  (when node
    (cons node
          (my/c-doc--declarator-chain
           (or (treesit-node-child-by-field-name node "declarator")
               (when (member (treesit-node-type node)
                             '("parenthesized_declarator" "reference_declarator"
                               "variadic_declarator"))
                 (treesit-node-child node 0 t)))))))

(defun my/c-doc--parameter-name (node)
  "Extract a parameter name from NODE, or nil for an unnamed parameter."
  (let ((leaf (car (last (my/c-doc--declarator-chain
                          (treesit-node-child-by-field-name node "declarator"))))))
    (when (and leaf (not (string-prefix-p "abstract_" (treesit-node-type leaf))))
      (unless (member (treesit-node-type leaf) '("identifier" "field_identifier"))
        (user-error "Unsupported parameter declarator"))
      (treesit-node-text leaf t))))

(defun my/c-doc--template-name (node)
  "Extract a named template parameter from NODE."
  (or (my/c-doc--parameter-name node)
      (when (equal (treesit-node-type node) "template_template_parameter_declaration")
        (my/c-doc--template-name (car (last (my/c-doc--children node)))))
      (when-let* ((name (cl-find "type_identifier" (my/c-doc--children node)
                                 :key #'treesit-node-type :test #'equal)))
        (treesit-node-text name t))
      (user-error "Unsupported or unnamed template parameter")))

(defun my/c-doc--signature-error-p (node)
  "Whether NODE has parse errors in syntax needed for documentation."
  (and (treesit-node-check node 'has-error)
       (or (treesit-node-check node 'missing)
           (equal (treesit-node-type node) "ERROR")
           (cl-loop for index below (treesit-node-child-count node)
                    for child = (treesit-node-child node index)
                    for field = (treesit-node-field-name-for-child node index)
                    thereis
                    (and (not (member field '("body" "default_value")))
                         (not (equal (treesit-node-type child) "field_initializer_list"))
                         (my/c-doc--signature-error-p child))))))

(defun my/c-doc--target ()
  "Find a declaration at point and return (DECLARATION . OUTER-NODE)."
  (unless (derived-mode-p 'c-ts-mode 'c++-ts-mode)
    (user-error "Use this command in c-ts-mode or c++-ts-mode"))
  (let* ((language (if (derived-mode-p 'c++-ts-mode) 'cpp 'c))
         (node (save-excursion
                 (skip-chars-forward " \t\n")
                 (treesit-node-at (point) language)))
         (types '("function_definition" "declaration" "field_declaration"
                  "struct_specifier" "class_specifier" "template_declaration")))
    (while (and node (not (member (treesit-node-type node) types)))
      (setq node (treesit-node-parent node)))
    (unless node (user-error "No supported declaration at point"))
    (when (equal (treesit-node-type node) "template_declaration")
      (setq node (car (last (my/c-doc--children node)))))
    (let ((outer node))
      (while (member (treesit-node-type (treesit-node-parent outer))
                     '("template_declaration"))
        (setq outer (treesit-node-parent outer)))
      (when (my/c-doc--signature-error-p outer)
        (user-error "Declaration signature contains syntax errors"))
      (cons node outer))))

(defun my/c-doc--description (node outer)
  "Return documentation data for declaration NODE within OUTER."
  (let* ((type (treesit-node-type node))
         (aggregate (member type '("struct_specifier" "class_specifier")))
         (chain (my/c-doc--declarator-chain
                 (treesit-node-child-by-field-name node "declarator")))
         (functions (cl-remove-if-not
                     (lambda (part) (equal (treesit-node-type part) "function_declarator"))
                     chain))
         (function (car functions))
         (name (and function (treesit-node-child-by-field-name function "declarator")))
         (templates nil)
         (parent node))
    (unless (or aggregate
                (and (= (length functions) 1)
                     (member (treesit-node-type name)
                             '("identifier" "field_identifier" "qualified_identifier"
                               "destructor_name" "operator_name"))))
      (user-error "Expected a function, struct or class; complex declarators are unsupported"))
    (when (or (cl-some (lambda (part) (equal (treesit-node-type part) ","))
                       (cl-loop for i below (treesit-node-child-count node)
                                collect (treesit-node-child node i)))
              (and aggregate
                   (not (member (treesit-node-type (treesit-node-parent node))
                                '("translation_unit" "declaration_list"
                                  "field_declaration_list" "template_declaration")))))
      (user-error "Document a single standalone declaration"))
    (while (not (treesit-node-eq parent outer))
      (setq parent (treesit-node-parent parent))
      (when-let* ((params (treesit-node-child-by-field-name parent "parameters")))
        (setq templates (append (mapcar #'my/c-doc--template-name
                                        (my/c-doc--children params)) templates))))
    (let* ((return-type (treesit-node-child-by-field-name node "type"))
           (trailing (and function
                          (cl-find "trailing_return_type" (my/c-doc--children function)
                                   :key #'treesit-node-type :test #'equal)))
           (return-text (if trailing
                            (string-trim (string-remove-prefix "->" (treesit-node-text trailing t)))
                          (and return-type (treesit-node-text return-type t))))
           (returns (and function return-text
                         (or (not (equal return-text "void"))
                             (and (not trailing)
                                  (cl-some (lambda (part)
                                             (member (treesit-node-type part)
                                                     '("pointer_declarator" "reference_declarator")))
                                           chain))))))
      (when (and (not trailing) (member return-text '("auto" "decltype(auto)")))
        (user-error "Deduced return type needs an explicit trailing return type"))
      (list :templates templates
            :parameters (and function
                             (delq nil (mapcar #'my/c-doc--parameter-name
                                               (my/c-doc--children
                                                (treesit-node-child-by-field-name function "parameters")))))
            :returns returns))))

(defun my/c-doc--snippet (description)
  "Format DESCRIPTION as a Doxygen snippet with numbered editable fields."
  (let ((index 1)
        (lines '("/**" " * @brief ${1:Summary.}")))
    (dolist (entry `(("tparam" . ,(plist-get description :templates))
                     ("param" . ,(plist-get description :parameters))))
      (dolist (name (cdr entry))
        (setq lines (append lines
                            (list (format " * @%s %s ${%d:Description.}"
                                          (car entry)
                                          (replace-regexp-in-string "[\\\\$`]" "\\\\\\&" name)
                                          (cl-incf index)))))))
    (when (plist-get description :returns)
      (setq lines (append lines (list (format " * @return ${%d:Description.}" (cl-incf index))))))
    (string-join (append lines '(" */" "$0")) "\n")))

;;;###autoload
(defun my/c-doc-insert ()
  "Insert a Doxygen skeleton for the C/C++ declaration at point."
  (interactive)
  (barf-if-buffer-read-only)
  (pcase-let* ((`(,node . ,outer) (my/c-doc--target))
               (description (my/c-doc--description node outer))
               (start (treesit-node-start outer))
               (previous (treesit-node-prev-sibling outer t)))
    (when (and previous (equal (treesit-node-type previous) "comment")
               (or (string-match-p "\\`/\\(?:\\*[*!]\\|/[!/]\\)"
                                   (treesit-node-text previous t))
                   (string-match-p "\\`[ \t]*\n[ \t]*\\'"
                                   (buffer-substring-no-properties
                                    (treesit-node-end previous) start))))
      (user-error "Declaration already has a preceding comment"))
    (unless (save-excursion
              (goto-char start)
              (string-blank-p (buffer-substring-no-properties (line-beginning-position) start)))
      (user-error "Put the declaration on its own line before documenting it"))
    (require 'yasnippet)
    (unless (bound-and-true-p yas-minor-mode) (yas-minor-mode 1))
    (goto-char start)
    (let* ((yas-indent-line 'none)
           (indent (buffer-substring-no-properties (line-beginning-position) start))
           (snippet (replace-regexp-in-string
                     "\n" (concat "\n" indent) (my/c-doc--snippet description) t t)))
      (atomic-change-group
        (yas-expand-snippet snippet start start)))))

(provide 'my-c-doc)
;;; my-c-doc.el ends here
