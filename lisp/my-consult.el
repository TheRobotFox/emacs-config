;;; my-consult.el --- Additional Consult readers -*- lexical-binding: t; -*-

;;; Commentary:
;; Consult-based selection with previews for TODO markers and project files.

;;; Code:
(require 'consult)
(require 'hl-todo)
(require 'project)

(defvar my/consult-todo-history nil)

(defun my/consult--todo-candidates ()
  "Collect highlighted marker lines in the accessible buffer."
  (save-excursion
    (goto-char (point-min))
    (let (candidates)
      (while (hl-todo--search nil (point-max))
        (let ((line (line-number-at-pos (point) consult-line-numbers-widen)))
          (push (consult--location-candidate
                 (buffer-substring-no-properties
                  (line-beginning-position) (line-end-position))
                 (cons (current-buffer) (match-beginning 2)) line line)
                candidates))
        (forward-line 1))
      (nreverse candidates))))

(defun my/consult-todos ()
  "Browse buffer TODO markers with source previews, respecting narrowing."
  (interactive)
  (consult--forbid-minibuffer)
  (let ((candidates (my/consult--todo-candidates)))
    (unless candidates (user-error "No TODO markers in the accessible buffer"))
    (consult--read candidates
                   :prompt "TODO: "
                   :category 'consult-location
                   :sort nil
                   :require-match t
                   :history '(:input my/consult-todo-history)
                   :lookup #'consult--lookup-location
                   :state (consult--location-state candidates))))

(defun my/consult-project-todos ()
  "Search project TODO markers on disk with ripgrep and source previews.
This textual search also matches markers outside comments and strings."
  (interactive)
  (consult-ripgrep (project-root (project-current t))
                   "#\\b(TODO|FIXME|NOTE|HACK|XXX)\\b#"))

(defun my/consult-project-file (prompt files &optional predicate history defaults)
  "Read from project FILES with Consult previews.
PROMPT, PREDICATE, HISTORY and DEFAULTS follow project.el's file reader."
  (expand-file-name
   (consult--read (mapcar #'file-relative-name files)
                  :prompt (concat prompt ": ")
                  :predicate predicate
                  :require-match t
                  :category 'file
                  :history history
                  :add-history (mapcar #'file-relative-name (ensure-list defaults))
                  :state (consult--file-preview))))

(provide 'my-consult)
;;; my-consult.el ends here
