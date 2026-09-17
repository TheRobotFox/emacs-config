;;; my-org-editing.el --- Personal configuration helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Loaded by config.org; edit this file directly.

;;; Code:

(require 'org)
(require 'org-element)
(require 'org-list)
(require 'org-fold)
(require 'ox)

(defun my/org--block-fold-state (element)
  "Return `hide', `off', or nil for ELEMENT's initial folding."
  (pcase (org-export-read-attribute :attr_org element :fold)
    ("yes" 'hide)
    ("no" 'off)
    (_ (when (and (eq (org-element-type element) 'src-block)
                  (member (org-element-property :language element)
                          '("d2" "dot" "plantuml")))
         'hide))))

(defun my/org-fold-note-blocks ()
  "Fold D2, Dot and PlantUML blocks, honoring per-block ATTR_ORG overrides.
Put #+ATTR_ORG: :fold yes or :fold no immediately above any block to
override its initial visibility.  Other blocks are left alone by default."
  (interactive)
  (org-with-wide-buffer
   (org-block-map
    (lambda ()
      (let* ((element (org-element-at-point))
             (state (my/org--block-fold-state element)))
        (when state (org-fold-hide-block-toggle state nil element)))))))

(defun my/org--fold-note-blocks-on-display (window)
  "Apply block folding once when this buffer is first displayed in WINDOW."
  ;; Window change hooks can also run in the buffer being switched away from.
  (when (eq (window-buffer window) (current-buffer))
    (my/org-fold-note-blocks)
    (remove-hook 'window-buffer-change-functions
                 #'my/org--fold-note-blocks-on-display t)))

(defun my/org-defer-note-folding ()
  "Arrange block folding on first display, including agenda-loaded notes.
Do not scan undisplayed agenda files or temporary export/metadata buffers."
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

(defvar olivetti-mode)

(defvar-local my/olivetti-previous-text-scale nil
  "Text scale before enabling Olivetti, or nil when not saved.")
(defun my/olivetti-text-scale ()
  "Apply focused-writing scale and restore it when leaving Olivetti."
  (if olivetti-mode
      (progn
        (unless my/olivetti-previous-text-scale
          (setq my/olivetti-previous-text-scale
                (if (bound-and-true-p text-scale-mode)
                    text-scale-mode-amount 0)))
        (text-scale-set 1.8))
    (when my/olivetti-previous-text-scale
      (text-scale-set my/olivetti-previous-text-scale)
      (setq my/olivetti-previous-text-scale nil))))

(provide 'my-org-editing)
;;; my-org-editing.el ends here
