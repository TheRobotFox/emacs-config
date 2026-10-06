;;; my-org-editing.el --- Personal configuration helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Loaded by config.org; edit this file directly.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-element)
(require 'org-list)
(require 'org-fold)
(require 'ox)
(require 'face-remap)

(defun my/org-beispiel-export-options (options backend)
  "Define the Beispiel environment when exporting with LaTeX."
  (if (org-export-derived-backend-p backend 'latex)
      (plist-put options :latex-header-extra
                 (concat (plist-get options :latex-header-extra)
                         "\n\\newenvironment{beispiel}{\\begin{quote}\\textbf{Beispiel.}\\quad}{\\end{quote}}\n"))
    options))

(defun my/org-typography ()
  "Apply note typography without changing fonts in other buffers."
  (setq-local line-spacing 0.12)
  (add-hook 'text-scale-mode-hook #'my/org-scale-inline-images nil t))

(defun my/org-scale-image (image)
  "Return IMAGE with the buffer's text zoom, without changing its base dimensions."
  (if (eq (car-safe image) 'image)
      (cons 'image
            (plist-put (copy-sequence (cdr image)) :scale
                       (expt text-scale-mode-step
                             (if text-scale-mode text-scale-mode-amount 0))))
    image))

(defun my/org-scale-inline-images ()
  "Update displayed images and their alignment to match text zoom."
  (dolist (overlay org-link-preview-overlays)
    (let ((image (overlay-get overlay 'display)))
      (when (eq (car-safe image) 'image)
        (let* ((scaled (my/org-scale-image image))
               (before (overlay-get overlay 'before-string))
               (alignment (and (stringp before) (> (length before) 0)
                               (get-text-property 0 'display before))))
          (overlay-put overlay 'display scaled)
          (when (eq (car-safe alignment) 'space)
            (let ((spacer (copy-sequence before)))
              (put-text-property
               0 1 'display
               (cl-subst-if scaled
                            (lambda (form) (eq (car-safe form) 'image))
                            alignment)
               spacer)
              (overlay-put overlay 'before-string spacer))))))))

(defun my/org-inline-image (image)
  "Apply text zoom to IMAGE and keep D2 math independent of the buffer font."
  (let* ((image (my/org-scale-image image))
         (properties (cdr image))
         (file (plist-get properties :file)))
    (if (and (eq (plist-get properties :type) 'svg)
             file (not (file-remote-p file))
             (with-temp-buffer
               (insert-file-contents file nil 0 512)
               (search-forward "data-d2-version=" nil t)))
        (cons 'image
              (plist-put (copy-sequence properties) :css
                         (concat (plist-get properties :css)
                                 "\nsvg { font-size: 16px; }")))
      image)))

(defun my/org--block-fold-state (element)
  "Return `hide', `off', or nil for ELEMENT's initial folding."
  (pcase (org-export-read-attribute :attr_org element :fold)
    ("yes" 'hide)
    ("no" 'off)
    (_ (when (pcase (org-element-type element)
               ('src-block
                (member (org-element-property :language element) '("d2" "dot" "plantuml")))
               ('special-block
                (member (downcase (org-element-property :type element)) '("beispiel" "proof"))))
         'hide))))

(defun my/org-fold-note-blocks ()
  "Fold diagram sources, Beispiel and proof blocks by default.
Put #+ATTR_ORG: :fold yes or :fold no immediately above any block to
override its initial visibility.  Other blocks are left alone by default."
  (interactive)
  (org-with-wide-buffer
   (org-block-map
    (lambda ()
      (let* ((element (org-element-at-point))
             (state (my/org--block-fold-state element)))
        (when state (org-fold-hide-block-toggle state nil element)))))))

(defvar-local my/org--startup-visibility-pending nil
  "Whether background loading skipped Org's initial visibility setup.")

(defun my/org--fold-note-blocks-on-display (window)
  "Apply pending startup visibility and block folding on first display in WINDOW."
  ;; Window change hooks can also run in the buffer being switched away from.
  (when (eq (window-buffer window) (current-buffer))
    (when my/org--startup-visibility-pending
      (org-with-wide-buffer
       (org-cycle-set-startup-visibility)
       (unless (org-before-first-heading-p)
         (org-fold-show-context 'agenda)))
      (setq my/org--startup-visibility-pending nil))
    (my/org-fold-note-blocks)
    (remove-hook 'window-buffer-change-functions
                 #'my/org--fold-note-blocks-on-display t)))

(defun my/org-defer-note-folding ()
  "Arrange initial folding on first display, including agenda-loaded notes.
Do not scan undisplayed agenda files or temporary export/metadata buffers."
  (setq my/org--startup-visibility-pending org-inhibit-startup)
  (add-hook 'window-buffer-change-functions
            #'my/org--fold-note-blocks-on-display nil t))

;; Doom-Emacs-insert
(defun my/org--insert-item (direction)
  (let ((context (org-element-lineage
                  (org-element-context)
                  '(table table-row headline inlinetask item plain-list)
                  t)))
    (pcase (org-element-type context)
      ;; Add a new list item (carrying over checkboxes if necessary)
      ((or `item `plain-list)
       (let ((orig-point (point)))
         ;; Position determines where org-insert-todo-heading and `org-insert-item'
         ;; insert the new list item.
         (if (eq direction 'above)
             (org-beginning-of-item)
           (end-of-line))
         (let* ((ctx-item? (eq 'item (org-element-type context)))
                (ctx-cb (org-element-property :contents-begin context))
                ;; Hack to handle edge case where the point is at the
                ;; beginning of the first item
                (beginning-of-list? (and (not ctx-item?)
                                         (= ctx-cb orig-point)))
                (item-context (if beginning-of-list?
                                  (org-element-context)
                                context))
                ;; Horrible hack to handle edge case where the
                ;; line of the bullet is empty
                (ictx-cb (org-element-property :contents-begin item-context))
                (empty? (and (eq direction 'below)
                             ;; in case contents-begin is nil, or contents-begin
                             ;; equals the position end of the line, the item is
                             ;; empty
                             (or (not ictx-cb)
                                 (= ictx-cb
                                    (1+ (point))))))
                (pre-insert-point (point)))
           ;; Insert dummy content, so that `org-insert-item'
           ;; inserts content below this item
           (when empty?
             (insert " "))
           (org-insert-item (org-element-property :checkbox context))
           ;; Remove dummy content
           (when empty?
             (delete-region pre-insert-point (1+ pre-insert-point))))))
      ;; Add a new table row
      ((or `table `table-row)
       (pcase direction
         ('below (org-table-next-row t))
         ('above (org-table-insert-row))))

      ;; Otherwise, add a new heading, carrying over any todo state, if
      ;; necessary.
      (_
       (let ((level (or (org-current-level) 1)))
         ;; I intentionally avoid `org-insert-heading' and the like because they
         ;; impose unpredictable whitespace rules depending on the cursor
         ;; position. It's simpler to express this command's responsibility at a
         ;; lower level than work around all the quirks in org's API.
         (pcase direction
           (`below
            (let (org-insert-heading-respect-content)
              (goto-char (line-end-position))
              (org-end-of-subtree)
              (insert "\n" (make-string level ?*) " ")))
           (`above
            (org-back-to-heading)
            (insert (make-string level ?*) " ")
            (save-excursion (insert "\n"))))
         (run-hooks 'org-insert-heading-hook)
         (when-let* ((todo-keyword (org-element-property :todo-keyword context))
                     (todo-type    (org-element-property :todo-type context)))
           (org-todo (unless (eq todo-type 'done) todo-keyword))))))

    (when (org-invisible-p)
      (org-show-hidden-entry))))
(defun my/org-insert-item-below (count)
  "Inserts a new heading, table cell or item below the current one."
  (interactive "p")
  (dotimes (_ count) (my/org--insert-item 'below)))

(defun my/org-insert-item-above (count)
  "Inserts a new heading, table cell or item above the current one."
  (interactive "p")
  (dotimes (_ count) (my/org--insert-item 'above)))

(defun my/org-reflow-paragraph ()
  "Join paragraph lines for visual wrapping, preserving Org structure."
  (interactive)
  (let ((fill-column most-positive-fixnum))
    (call-interactively #'org-fill-paragraph)))

(provide 'my-org-editing)
;;; my-org-editing.el ends here
