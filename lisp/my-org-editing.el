;;; my-org-editing.el --- Structural insertion and paragraph commands -*- lexical-binding: t; -*-

;;; Commentary:
;; Editing commands for Org: insert the next item, row or heading in one key,
;; reflow paragraphs for visual wrapping, and export the Beispiel block.

;;; Code:
(require 'org)
(require 'org-element)
(require 'org-list)
(require 'ox)

(defun my/org--insert-item (direction)
  "Insert a list item, table row or heading in DIRECTION (`above' or `below')."
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
                ;; Edge case: point at the beginning of the first item.
                (beginning-of-list? (and (not ctx-item?)
                                         (= ctx-cb orig-point)))
                (item-context (if beginning-of-list?
                                  (org-element-context)
                                context))
                ;; Edge case: the bullet line is empty.
                (ictx-cb (org-element-property :contents-begin item-context))
                (empty? (and (eq direction 'below)
                             (or (not ictx-cb)
                                 (= ictx-cb
                                    (1+ (point))))))
                (pre-insert-point (point)))
           ;; Insert dummy content so that `org-insert-item' inserts below.
           (when empty?
             (insert " "))
           (org-insert-item (org-element-property :checkbox context))
           (when empty?
             (delete-region pre-insert-point (1+ pre-insert-point))))))
      ;; Add a new table row
      ((or `table `table-row)
       (pcase direction
         ('below (org-table-next-row))
         ('above (org-table-insert-row))))

      ;; Otherwise, add a new heading, carrying over any todo state.
      (_
       (let ((level (or (org-current-level) 1)))
         ;; `org-insert-heading' applies whitespace rules that depend on the
         ;; cursor position; inserting at a lower level is predictable.
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
      (org-fold-show-hidden-entry))))

(defun my/org-insert-item-below (count)
  "Insert COUNT new headings, table rows or items below the current one."
  (interactive "p")
  (dotimes (_ count) (my/org--insert-item 'below)))

(defun my/org-insert-item-above (count)
  "Insert COUNT new headings, table rows or items above the current one."
  (interactive "p")
  (dotimes (_ count) (my/org--insert-item 'above)))

(defun my/org-reflow-paragraph ()
  "Join paragraph lines for visual wrapping, preserving Org structure."
  (interactive)
  (let ((fill-column most-positive-fixnum))
    (call-interactively #'org-fill-paragraph)))

(defun my/org-beispiel-export-options (options backend)
  "Define the Beispiel environment in OPTIONS when exporting with LaTeX BACKEND."
  (if (org-export-derived-backend-p backend 'latex)
      (plist-put options :latex-header-extra
                 (concat (plist-get options :latex-header-extra)
                         "\n\\newenvironment{beispiel}{\\begin{quote}\\textbf{Beispiel.}\\quad}{\\end{quote}}\n"))
    options))

(provide 'my-org-editing)
;;; my-org-editing.el ends here
