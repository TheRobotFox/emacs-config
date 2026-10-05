;;; org-note-graph-templates.el --- Small userland collection specifications -*- lexical-binding: t; -*-

;;; Code:
(require 'org-note-graph-edit)
(require 'org-note-graph-labels)
(require 'org-capture)

(defcustom org-note-graph-directory (expand-file-name "notes/" org-directory)
  "Directory for new standalone notes."
  :type 'directory :group 'org-note-graph)

(defun org-note-graph-capture-note (title callback)
  "Capture a new file named TITLE using Org's capture lifecycle.
Call CALLBACK with its indexed node after successful finalization."
  (when (or (string-empty-p (string-trim title)) (string-match-p "[\n\r]" title))
    (user-error "A nonempty, single-line title is required"))
  (unless (seq-some (lambda (root)
                     (and (file-directory-p root)
                          (file-in-directory-p org-note-graph-directory root)))
                   org-note-graph-roots)
    (user-error "The new notes directory must be within a graph source directory"))
  (let* ((id (org-id-new))
         (slug (string-trim (replace-regexp-in-string "[^[:alnum:]]+" "-" (downcase title)) "-" "-"))
         (file (expand-file-name (concat slug "-" (substring id 0 8) ".org") org-note-graph-directory))
         (org-capture-templates
          `(("g" "Graph note" plain (file ,file)
             ,(format ":PROPERTIES:\n:ID: %s\n:END:\n#+title: Note\n\n%%?\n" id)
             :no-save t
             :hook ,(lambda ()
                      (save-excursion
                        (goto-char (point-min))
                        (re-search-forward "^#\\+title: ")
                        (delete-region (point) (line-end-position))
                        (insert title)))
             :after-finalize
             ,(lambda ()
                (unless org-note-abort
                  (with-current-buffer (find-file-noselect file) (save-buffer))
                  (let* ((db (org-note-graph-refresh))
                         (node (org-note-graph-node db (concat "id:" id))))
                    (unless node (user-error "Captured note was not indexed: %s" file))
                    (funcall callback node)))))))
         (org-capture-templates-contexts nil))
    (make-directory org-note-graph-directory t)
    (when (file-exists-p file) (user-error "Capture file already exists: %s" file))
    (org-capture nil "g")))

(defun org-note-graph-templates-add (target context)
  "Append CONTEXT's node as a link in TARGET, reusing its :db when supplied."
  (let* ((node (or (plist-get context :node) (user-error "No current node")))
         (db (or (plist-get context :db) (org-note-graph-refresh)))
         (key (org-note-graph-node-key node)))
    (unless (member (or (org-note-graph-resolve db key) key)
                    (org-note-graph-out db (list (org-note-graph-node-key target))))
      (org-note-graph-append target (org-note-graph--link node (org-note-graph-node-file target))))
    (message "Linked from %s; save its buffer to persist the edit" (org-note-graph-node-title target))))

(defun org-note-graph-templates-capture (target _context)
  "Capture a standalone note and append its link to TARGET."
  (org-note-graph-capture-note
   (read-string "Title: ")
   (lambda (node)
     (org-note-graph-templates-add target (list :node node :db (org-note-graph-database)))
     (org-note-graph-refresh))))

(org-note-graph-register-collection
 "Link list" :type "list" :actions '((add . org-note-graph-templates-add)))
(org-note-graph-register-collection
 "Capture target" :type "capture-target"
 :actions '((add . org-note-graph-templates-add)
            (capture . org-note-graph-templates-capture)))

(provide 'org-note-graph-templates)
;;; org-note-graph-templates.el ends here
