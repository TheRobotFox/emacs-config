;;; my-c.el --- C/C++ tooling: comments, banners, Doxygen, source files -*- lexical-binding: t; -*-

;;; Commentary:
;; Block comment continuation on newline (c-ts-mode continues only line
;; comments natively), section banners, Doxygen skeletons extracted with
;; treesit and edited with YASnippet, and implementation files for headers.
;; Comment filling is native: c-ts-mode's `fill-paragraph' handles both styles.

;;; Code:
(require 'cl-lib)
(require 'subr-x)
(require 'treesit)
(require 'newcomment)
(declare-function yas-expand-snippet "yasnippet" (snippet &optional start end expand-env))
(declare-function yas-minor-mode "yasnippet" (&optional arg))
(defvar yas-indent-line)

;;;; Comments

(defun my/c-doc-newline ()
  "Continue a block comment with its star prefix.
Line comments and code go through `comment-indent-new-line'."
  (interactive)
  (let* ((state (syntax-ppss))
         (start (and (nth 4 state) (nth 8 state))))
    (if (not (and start (eq (char-after (1+ start)) ?*)))
        (comment-indent-new-line)
      (pcase-let ((`(,column . ,prefix)
                   (save-excursion
                     (back-to-indentation)
                     (if (and (looking-at "\\*+[ \t]*")
                              (not (eq (char-after (match-end 0)) ?/)))
                         (cons (current-column) (match-string-no-properties 0))
                       (cons (1+ (save-excursion (goto-char start) (current-column))) "* ")))))
        (delete-horizontal-space)
        (newline)
        (indent-to column)
        (insert prefix)
        ;; Keep a closing delimiter on its own line.
        (when (looking-at "[ \t]*\\*/")
          (save-excursion
            (delete-region (point) (progn (skip-chars-forward " \t") (point)))
            (newline)
            (indent-to column)))))))

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

;;;###autoload
(defun my/c-doc-banner ()
  "Insert a one-line section banner for the selected text or a prompted title."
  (interactive)
  (my/c-doc--insert-banner nil))

;;;###autoload
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
        (my/c-doc--template-name (car (last (treesit-node-children node t)))))
      (when-let* ((name (seq-find (lambda (child) (equal (treesit-node-type child) "type_identifier"))
                                  (treesit-node-children node t))))
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
                    thereis (and (not (member field '("body" "default_value")))
                                 (not (equal (treesit-node-type child) "field_initializer_list"))
                                 (my/c-doc--signature-error-p child))))))

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
      (when (my/c-doc--signature-error-p outer)
        (user-error "Declaration signature contains syntax errors"))
      (cons node outer))))

(defun my/c-doc--template-parameters (node outer)
  "Return template parameter names between NODE and OUTER, outermost first."
  (let (names)
    (while (not (treesit-node-eq node outer))
      (setq node (treesit-node-parent node))
      (when-let* ((parameters (treesit-node-child-by-field-name node "parameters")))
        (setq names (append (mapcar #'my/c-doc--template-name (treesit-node-children parameters t))
                            names))))
    names))

(defun my/c-doc--returns-p (node function chain)
  "Whether FUNCTION declared by NODE through CHAIN returns a value."
  (let* ((trailing (seq-find (lambda (child) (equal (treesit-node-type child) "trailing_return_type"))
                             (treesit-node-children function t)))
         (type (treesit-node-child-by-field-name node "type"))
         (text (if trailing
                   (string-trim (string-remove-prefix "->" (treesit-node-text trailing t)))
                 (and type (treesit-node-text type t)))))
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
  (let* ((aggregate (member (treesit-node-type node) '("struct_specifier" "class_specifier")))
         (chain (my/c-doc--declarator-chain (treesit-node-child-by-field-name node "declarator")))
         (functions (seq-filter (lambda (part) (equal (treesit-node-type part) "function_declarator")) chain))
         (function (car functions))
         (name (and function (treesit-node-child-by-field-name function "declarator"))))
    (unless (or aggregate
                (and (length= functions 1)
                     (member (treesit-node-type name)
                             '("identifier" "field_identifier" "qualified_identifier"
                               "destructor_name" "operator_name"))))
      (user-error "Expected a function, struct or class; complex declarators are unsupported"))
    (when (or (seq-some (lambda (part) (equal (treesit-node-type part) ",")) (treesit-node-children node))
              (and aggregate
                   (not (member (treesit-node-type (treesit-node-parent node))
                                '("translation_unit" "declaration_list"
                                  "field_declaration_list" "template_declaration")))))
      (user-error "Document a single standalone declaration"))
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
               (or (string-match-p "\\`/\\(?:\\*[*!]\\|/[!/]\\)" (treesit-node-text previous t))
                   (string-match-p "\\`[ \t]*\n[ \t]*\\'"
                                   (buffer-substring-no-properties (treesit-node-end previous) start))))
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
           (snippet (string-replace "\n" (concat "\n" indent) (my/c-doc--snippet description))))
      (atomic-change-group
        (yas-expand-snippet snippet start start)))))

;;;; Source files

;;;###autoload
(defun my/c-create-src-file ()
  "Create a new implementation buffer for the current C or C++ header."
  (interactive)
  (unless buffer-file-name (user-error "This buffer does not visit a file"))
  (let* ((file buffer-file-name)
         (extension (pcase (file-name-extension file)
                      ("h" "c") ("hpp" "cpp")
                      (_ (user-error "Expected a .h or .hpp header"))))
         (target (file-name-with-extension file extension))
         namespaces)
    (when (or (file-exists-p target) (get-file-buffer target))
      (user-error "Implementation file or buffer already exists: %s" target))
    ;; Inspect the header before switching buffers; plain C needs no parser.
    (when (equal extension "cpp")
      (unless (treesit-parser-list)
        (user-error "C++ namespace detection requires a Tree-sitter parser"))
      (let ((node (treesit-node-at (point))))
        (while node
          (when (equal (treesit-node-type node) "namespace_definition")
            (when-let* ((name (treesit-node-child-by-field-name node "name")))
              (push (treesit-node-text name t) namespaces)))
          (setq node (treesit-node-parent node)))))
    (let ((namespace (string-join namespaces "::")))
      (find-file-other-window target)
      (atomic-change-group
        (insert "#include \"" (file-name-nondirectory file) "\"\n\n"
                (if (string-empty-p namespace) ""
                  (format "namespace %s {\n} // %s" namespace namespace)))))))

(provide 'my-c)
;;; my-c.el ends here
