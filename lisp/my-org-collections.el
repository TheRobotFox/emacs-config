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

(defun my/org-collections--update (block)
  "Regenerate BLOCK, replacing its text only when results changed."
  (let* ((beg (org-element-property :begin block))
         (end (org-element-property :end block))
         (old (buffer-substring-no-properties beg end))
         (new (with-temp-buffer
                (insert old)
                (delay-mode-hooks (org-mode))
                (goto-char (point-min))
                (org-update-dblock)
                (buffer-substring-no-properties (point-min) (point-max)))))
    (unless (equal old new)
      (atomic-change-group
        (goto-char beg)
        (delete-region beg end)
        (insert new)))))

(defun my/org-collections-refresh ()
  "Refresh Roam collection blocks from saved notes.
Uses the saved Roam database.  Unchanged results leave the buffer unmodified;
other dynamic blocks are not executed automatically."
  (interactive)
  (unless buffer-read-only
    (save-excursion
      (save-restriction
        (widen)
        (let ((case-fold-search t))
          (goto-char (point-min))
          (when (re-search-forward "^[ \t]*#\\+begin: \\(org-roam-ql\\|roam-list\\)\\_>" nil t)
            (require 'org-roam-ql)
            (goto-char (point-min))
            ;; Earlier positions stay valid when later blocks change size.
            (dolist (block (reverse (org-element-map (org-element-parse-buffer)
                                       'dynamic-block #'identity)))
              (goto-char (org-element-property :post-affiliated block))
              (when (and (looking-at org-dblock-start-re)
                         (member (match-string 1) '("org-roam-ql" "roam-list")))
                (my/org-collections--update block)))))))))

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
