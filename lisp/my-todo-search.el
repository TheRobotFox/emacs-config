;;; my-todo-search.el --- Browse comment markers -*- lexical-binding: t; -*-

(require 'consult)
(require 'hl-todo)
(require 'project)

(defvar my/todo-search-history nil)

(defun my/todo--candidates ()
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
  (let ((candidates (my/todo--candidates)))
    (unless candidates (user-error "No TODO markers in the accessible buffer"))
    (consult--read candidates
                   :prompt "TODO: "
                   :category 'consult-location
                   :sort nil
                   :require-match t
                   :history '(:input my/todo-search-history)
                   :lookup #'consult--lookup-location
                   :state (consult--location-state candidates))))

(defun my/consult-project-todos ()
  "Search project TODO markers on disk with ripgrep and source previews.
This textual search also matches markers outside comments and strings."
  (interactive)
  (consult-ripgrep (project-root (project-current t))
                  "#\\b(TODO|FIXME|NOTE|HACK|XXX)\\b#"))

(provide 'my-todo-search)
;;; my-todo-search.el ends here
