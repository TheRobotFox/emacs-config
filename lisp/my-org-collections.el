;;; my-org-collections.el --- Refreshable Roam collections -*- lexical-binding: t; -*-

;;; Code:
(require 'org)
(require 'org-element)

(defun org-dblock-write:roam-list (params)
  "Insert linked bullets for PARAMS' :query, ordered by :sort."
  (require 'org-roam-ql)
  (dolist (node (org-roam-ql-nodes (plist-get params :query)
                                 (plist-get params :sort)))
    (insert " * " (org-link-make-string
                  (concat "id:" (org-roam-node-id node))
                  (org-roam-node-title node)) "\n")))

(defun my/org-collections-refresh ()
  "Refresh collections and project task counts without modifying unchanged text.
Task summaries include edits in open task buffers.  Generate in a scratch
buffer so failed queries cannot erase existing results."
  (interactive)
  (unless buffer-read-only
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (let ((case-fold-search t)
              (source (current-buffer)))
          (when (re-search-forward "^[ \t]*#\\+begin: \\(org-roam-ql\\|roam-list\\|project-tasks\\)\\_>" nil t)
            (with-temp-buffer
              (insert-buffer-substring source)
              (let ((org-inhibit-startup t)) (delay-mode-hooks (org-mode)))
              (goto-char (point-min))
              (dolist (block (reverse (org-element-map (org-element-parse-buffer)
                                         'dynamic-block #'identity)))
                (goto-char (org-element-property :post-affiliated block))
                (when (and (looking-at org-dblock-start-re)
                           (member (match-string 1) '("org-roam-ql" "roam-list" "project-tasks")))
                  (require (if (equal (match-string 1) "project-tasks")
                               'my-org-workflow 'org-roam-ql))
                  (org-update-dblock)))
              (org-map-dblocks
               (lambda ()
                 (when (looking-at "[ \t]*#\\+begin: project-tasks\\_>")
                   (org-update-checkbox-count))))
              (let ((result (current-buffer)))
                (with-current-buffer source
                  (replace-buffer-contents result))))))))))

(defun my/org-collections-on-display (window)
  "Refresh collections when their buffer is displayed in WINDOW."
  (when (eq (window-buffer window) (current-buffer))
    (condition-case err
        (my/org-collections-refresh)
      (error (message "Collection refresh: %s" (error-message-string err))))))

(defun my/org-collections-setup ()
  "Arrange collection refresh on display and before saving."
  (add-hook 'window-buffer-change-functions #'my/org-collections-on-display nil t)
  (add-hook 'before-save-hook #'my/org-collections-refresh nil t))

(provide 'my-org-collections)
;;; my-org-collections.el ends here
