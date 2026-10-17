;;; my-org-editing.el --- Structural editing commands for Org -*- lexical-binding: t; -*-

;;; Commentary:
;; Editing commands for Org: insert the next item, row or heading in one key,
;; export the Beispiel block, and remove the indentation that
;; `org-adapt-indentation' wrote under headings.

;;; Code:
(require 'org)
(require 'org-element)
(require 'org-list)
(declare-function org-export-derived-backend-p "ox" (backend &rest backends))

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

(defun my/org-beispiel-export-options (options backend)
  "Define the Beispiel environment in OPTIONS when exporting with LaTeX BACKEND."
  (if (org-export-derived-backend-p backend 'latex)
      (plist-put options :latex-header-extra
                 (concat (plist-get options :latex-header-extra)
                         "\n\\newenvironment{beispiel}{\\begin{quote}\\textbf{Beispiel.}\\quad}{\\end{quote}}\n"))
    options))

(defun my/org--flatten-lines (begin end shift)
  "Move the lines from BEGIN to END left by one amount, at most SHIFT: the
most every line can lose, a star bullet keeping one column."
  (goto-char begin)
  (while (< (point) end)
    (unless (looking-at-p "[ \t]*$")
      (setq shift (min shift (- (current-indentation) (if (looking-at-p "[ \t]*\\*[ \t]") 1 0)))))
    (forward-line 1))
  (when (> shift 0) (indent-rigidly begin end (- shift))))

(defun my/org-flatten-indentation ()
  "Remove the indentation written under headings, now that `org-indent-mode'
displays it.  Each paragraph, list, drawer or block line under a heading of
level N moves left by one amount, up to N+1 columns, so its structure stays;
a block's content moves apart from its first and last line."
  (interactive)
  (barf-if-buffer-read-only)
  (let ((blocks '(special-block quote-block center-block))
        units)
    (org-element-map (org-element-parse-buffer 'element) org-element-all-elements
      (lambda (element)
        (when (memq (org-element-type (org-element-parent element)) (cons 'section blocks))
          (let ((shift (if-let* ((headline (org-element-lineage element '(headline))))
                           (1+ (org-element-property :level headline))
                         0))
                (begin (org-element-property :begin element))
                (end (org-element-property :end element)))
            (if (not (memq (org-element-type element) blocks))
                (push (list begin end shift) units)
              (save-excursion
                (goto-char begin)
                (push (list (point) (line-beginning-position 2) shift) units)
                (goto-char (org-element-property :contents-end element))
                (push (list (point) (line-beginning-position 2) shift) units)))))))
    (save-excursion
      ;; From the end, so earlier positions stay valid.
      (pcase-dolist (`(,begin ,end ,shift) (sort units (lambda (a b) (> (car a) (car b)))))
        (my/org--flatten-lines begin end shift)))))

(provide 'my-org-editing)
;;; my-org-editing.el ends here
