;;; my-org-note-db-edit.el --- Move between file and heading notes -*- lexical-binding: t; -*-
;;; Code:
(require 'org-note-db-writing)

(defun my/org-note-db--file-text (tree title tags)
  "Convert TREE to a file note with TITLE and local TAGS, preserving IDs."
  (with-temp-buffer
    (let ((org-inhibit-startup t) (org-odd-levels-only nil) org-markers-to-move)
      (delay-mode-hooks (org-mode))
      (org-paste-subtree 1 tree)
      (goto-char (point-min))
      (org-entry-put nil "ID" (or (org-entry-get nil "ID") (org-id-new)))
      (delete-region (point) (line-beginning-position 2))
      (org-map-region #'org-promote (point-min) (point-max))
      (goto-char (point-min))
      (goto-char (cdr (org-get-property-block)))
      (forward-line)
      (insert "#+title: " title "\n")
      (when tags (insert "#+filetags: :" (string-join tags ":") ":\n"))
      (unless (looking-at-p "[ \t]*$") (insert "\n"))
      (buffer-string))))

(defun my/org-note-db--subtree-text (text title)
  "Convert file TEXT to a subtree named TITLE, preserving IDs and local tags."
  (with-temp-buffer
    (insert text)
    (let ((org-inhibit-startup t) (org-odd-levels-only nil))
      (delay-mode-hooks (org-mode))
      (let* ((keywords (org-element-map (org-element-parse-buffer) 'keyword #'identity))
             (tags (org-get-tags (point-min)))
             (category (cadr (assoc "CATEGORY" (org-collect-keywords '("CATEGORY"))))))
        (dolist (keyword keywords)
          (unless (member (org-element-property :key keyword) '("TITLE" "FILETAGS" "CATEGORY"))
            (user-error "Convert #+%s to heading-local settings before moving this file"
                        (org-element-property :key keyword))))
        (dolist (keyword (reverse keywords))
          (delete-region (org-element-property :begin keyword) (org-element-property :end keyword)))
        (org-map-region #'org-demote (point-min) (point-max))
        (goto-char (point-min))
        (insert "* " title "\n")
        (goto-char (point-min))
        (org-set-tags tags)
        (org-entry-put nil "ID" (or (org-entry-get nil "ID") (org-id-new)))
        (when category (org-entry-put nil "CATEGORY" category))
        (buffer-string)))))

(defun my/org-note-db--editable-file ()
  "Return the current Org buffer's local file, checking that it can be edited."
  (unless (derived-mode-p 'org-mode) (user-error "Use an Org note"))
  (barf-if-buffer-read-only)
  (let ((file (buffer-file-name (buffer-base-buffer))))
    (unless (and file (not (file-remote-p file))) (user-error "Use a local Org file"))
    file))

(defun my/org-note-db-extract (&optional file)
  "Move the current subtree into a new FILE note and save both buffers.
Preserve IDs, creating one for an unregistered root.  Leave nothing behind.
File/search links and inherited settings are not rewritten."
  (interactive)
  (my/org-note-db--editable-file)
  (when (use-region-p) (user-error "Extract operates on one subtree; deactivate the region first"))
  (save-restriction
    (widen)
    (save-excursion
      (org-back-to-heading t)
      (let* ((heading (org-element-at-point))
             (title (org-element-property :raw-value heading))
             (begin (point))
             (end (save-excursion (org-end-of-subtree t t)))
             (source (current-buffer)))
        (when (seq-some (lambda (property) (org-element-property property heading))
                        '(:todo-keyword :priority :scheduled :deadline :closed :commentedp :archivedp))
          (user-error "Use org-refile for task, commented or archived headings"))
        (setq file (expand-file-name
                    (or file (read-file-name
                              "Extract to: " org-note-db-capture-directory nil nil
                              (concat (string-trim (replace-regexp-in-string
                                                    "[^[:alnum:]]+" "-" (downcase title)) "-" "-") ".org")))))
        (unless (and (not (file-remote-p file)) (string-suffix-p ".org" file)
                     (file-directory-p (file-name-directory file)))
          (user-error "Choose a local .org file in an existing directory"))
        (when (or (file-exists-p file) (find-buffer-visiting file))
          (user-error "Destination already exists: %s" file))
        (unless (seq-some (lambda (root) (and (file-directory-p root) (file-in-directory-p file root)))
                          org-note-db-roots)
          (user-error "Choose a file inside a configured note directory"))
        (let* ((text (my/org-note-db--file-text
                      (buffer-substring-no-properties begin end) title (org-get-tags nil t)))
               (target (find-file-noselect file)))
          (with-current-buffer target
            (atomic-change-group
              (insert text)
              (org-set-regexps-and-options)
              (save-buffer)))
          (with-current-buffer source
            (atomic-change-group
              (delete-region begin end)
              (save-buffer)))
          (org-note-db-refresh)
          (funcall org-note-db-open-function target)
          (message "Extracted %s; saved both files. Review file/search links and inherited settings" title)
          target)))))

(defun my/org-note-db-refile-file (&optional target)
  "Move this whole file note beneath TARGET, save it, then trash the source file.
TARGET is an existing file or heading node.  IDs follow the moved note;
file/search links and file-wide settings require review."
  (interactive)
  (let* ((file (my/org-note-db--editable-file))
         (source (or (buffer-base-buffer) (current-buffer)))
         (db (org-note-db-refresh))
         (target (or target
                     (org-note-db-read
                      db (org-note-db-select db (lambda (node) (not (file-equal-p file (org-note-db-node-file node)))))
                      "Move this file under: "))))
    (when (equal (file-truename file) (file-truename (org-note-db-node-file target)))
      (user-error "Choose a node in another file"))
    (with-current-buffer source
      (save-restriction
        (widen)
        (let* ((title (or (cadar (org-collect-keywords '("TITLE"))) (file-name-base file)))
               (tree (my/org-note-db--subtree-text (buffer-string) title))
               (destination (find-file-noselect (org-note-db-node-file target))))
          (with-current-buffer destination
            (barf-if-buffer-read-only)
            (save-excursion
              (save-restriction
                (org-note-db-goto target)
                (let ((position (unless (zerop (org-note-db-node-level target)) (point))))
                  (atomic-change-group
                    (with-temp-buffer
                      (insert tree)
                      (let ((org-inhibit-startup t)) (delay-mode-hooks (org-mode)))
                      (goto-char (point-min))
                      (let ((org-refile-keep nil) (mark-active nil))
                        (org-refile nil nil (list (org-note-db-node-title target)
                                                 (org-note-db-node-file target) nil position))))
                    (save-buffer))))))
          (when (file-exists-p file) (move-file-to-trash file))
          (set-buffer-modified-p nil)
          (kill-buffer source)
          (org-note-db-refresh)
          (funcall org-note-db-open-function destination)
          (message "Moved %s; destination saved and original trashed. Review file/search links" title)
          destination)))))

(provide 'my-org-note-db-edit)
;;; my-org-note-db-edit.el ends here
