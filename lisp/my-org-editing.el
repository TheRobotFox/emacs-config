;;; my-org-editing.el --- Structural insertion and paragraph commands -*- lexical-binding: t; -*-

;;; Commentary:
;; Editing commands for Org: insert the next item, row or heading in one key,
;; reflow paragraphs for visual wrapping, and export the Beispiel block.

;;; Code:
(require 'org)
(require 'org-element)
(require 'org-list)
(require 'ox)

(defun my/org--insert-list-item (direction)
  "Insert a sibling item in DIRECTION, carrying over the item's checkbox."
  (org-beginning-of-item)
  (let ((checkbox (org-list-get-checkbox (point) (org-list-struct)))
        (empty (save-excursion (looking-at org-list-full-item-re)
                               (goto-char (match-end 0))
                               (looking-at-p "[ \t]*$"))))
    (if (eq direction 'above)
        (org-insert-item checkbox)
      (end-of-line)
      (if (not empty)
          (org-insert-item checkbox)
        ;; Point in an empty item reads as before its contents, which makes
        ;; Org insert above; a placeholder character puts point after them.
        (insert "x")
        (let ((placeholder (1- (point))))
          (org-insert-item checkbox)
          (save-excursion (goto-char placeholder) (delete-char 1)))))))

(defun my/org--insert-heading (direction)
  "Insert a sibling heading in DIRECTION, carrying over an open TODO keyword.
After a done heading, the new one starts with the first TODO keyword."
  (let ((state (org-get-todo-state))
        (stars (concat (make-string (or (org-current-level) 1) ?*) " ")))
    ;; Inserting the stars directly avoids `org-insert-heading', whose
    ;; whitespace rules depend on where point is.
    (if (eq direction 'above)
        (progn (org-back-to-heading t)
               (insert stars)
               (save-excursion (insert "\n")))
      (end-of-line)
      (org-end-of-subtree)
      (insert "\n" stars))
    (run-hooks 'org-insert-heading-hook)
    (when state
      (org-todo (if (member state org-done-keywords) (car org-not-done-keywords) state)))))

(defun my/org--insert-item (direction)
  "Insert a list item, table row or heading in DIRECTION, `above' or `below'."
  (pcase (org-element-type (org-element-lineage (org-element-context)
                                                '(table-row table item plain-list headline inlinetask)
                                                t))
    ((or 'item 'plain-list) (my/org--insert-list-item direction))
    ((or 'table-row 'table) (org-table-insert-row (eq direction 'below)))
    (_ (my/org--insert-heading direction)))
  (when (org-invisible-p) (org-fold-show-hidden-entry)))

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
