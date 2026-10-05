;;; org-note-graph-edit.el --- Small Org writing helpers -*- lexical-binding: t; -*-

;;; Code:
(require 'org-note-graph-ui)
(require 'org-list)

(defun org-note-graph--last-item (tree)
  "Return the last item of a section-level plain list in TREE."
  (let ((lists (org-element-map tree 'plain-list
                 (lambda (list)
                   (when (and (memq (org-element-property :type list) '(unordered descriptive))
                              (eq (org-element-type (org-element-property :parent list)) 'section))
                     list)))))
    (when-let* ((list (car (last lists))))
      (org-element-property :begin (car (last (org-element-contents list)))))))

(defun org-note-graph--append-bullet (text begin end)
  "Append TEXT in BEGIN..END using Org's list editing and existing bullet style."
  (let ((item (save-restriction
                (narrow-to-region begin end)
                (org-note-graph--last-item (org-element-parse-buffer)))))
    (if item
        (progn
          (goto-char item)
          (org-fold-show-context 'lineage)
          ;; Create a sibling after all continuation text and nested items.
          (end-of-line)
          (let ((org-M-RET-may-split-line nil)
                (org-blank-before-new-entry
                 (cons '(plain-list-item . nil) org-blank-before-new-entry)))
            (org-insert-item))
          ;; Org creates an empty description term; the formatter supplies it.
          (delete-region (point) (line-end-position))
          (insert text))
      (goto-char end)
      (unless (bolp) (insert "\n"))
      (unless (or (= (point) (point-min))
                  (save-excursion (forward-line -1) (looking-at-p "[ \t]*$")))
        (insert "\n"))
      (insert " * " text "\n"))))

(defun org-note-graph-append (node text)
  "Append a list item containing TEXT to NODE's section, leaving edits unsaved."
  (with-current-buffer (find-file-noselect (org-note-graph-node-file node))
    (save-excursion
      (save-restriction
        (org-note-graph--goto node)
        (atomic-change-group
          (org-note-graph--append-bullet
           text (point) (save-excursion (outline-next-heading) (point))))))))

(provide 'org-note-graph-edit)
;;; org-note-graph-edit.el ends here
