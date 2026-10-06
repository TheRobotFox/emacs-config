;;; my-org-note-db.el --- Personal note policies and bindings -*- lexical-binding: t; -*-
;;; Code:
(require 'org-note-db-search)
(require 'org-note-db-keywords)
(require 'my-org-note-db-pairs)
(require 'my-org-note-db-actions)
(require 'my-org-note-db-edit)
(require 'my-org-note-db-xref)

(dolist (spec '((graph-tags "GRAPH_TAGS" "Tags") (aliases "ALIASES" "Aliases")))
  (pcase-let ((`(,provider ,property ,destination) spec))
    (org-note-db-register-destination destination 'tagged-location)
    (org-note-db-register-provider provider
     :destination destination :scan (org-note-db-property-scanner #'org-note-db-keywords property)
     :formatters '((tagged-location . identity)))))

(defun my/org-note-db-tag-add (&optional remove)
  "Add a graph tag at this file or heading; with a prefix, remove one."
  (interactive "P")
  (org-note-db-keywords-edit "GRAPH_TAGS" "Tags" remove))

(defun my/org-note-db-alias-add (&optional remove)
  "Add an alias at this file or heading; with a prefix, remove one."
  (interactive "P")
  (org-note-db-keywords-edit "ALIASES" "Aliases" remove))

(defcustom my/org-note-db-projects-node nil
  "Node key whose file links make repository notes discoverable."
  :type '(choice (const nil) string) :group 'org-note-db)

(defun my/org-note-db-collect-project ()
  "Link the repository note by file so the graph can discover it after rebuild."
  (when my/org-note-db-projects-node
    (let* ((db (org-note-db-refresh))
           (target (or (org-note-db-node db my/org-note-db-projects-node)
                       (user-error "Projects node is missing")))
           (node (org-note-db-external buffer-file-name))
           (key (org-note-db-node-key node)))
      (unless (member (or (org-note-db-resolve db key) key)
                      (org-note-db-out db (list (org-note-db-node-key target))))
        (org-note-db-append target (org-note-db-location-link node (org-note-db-node-file target)))
        (message "Linked from Projects; save its buffer to persist the edit"))
      (org-note-db-refresh))))

(defvar-keymap my/org-note-db-map
  "f" #'org-note-db-find
  "i" #'org-note-db-insert
  "c" #'org-capture
  "T" #'org-note-db-capture
  "b" #'org-note-db-backlinks
  "l" #'org-note-db-forward-links
  "g" #'org-note-db-search-content
  "s" #'org-note-db-lookup
  "C" #'org-id-get-create
  "d" #'org-note-db-declare
  "t" #'my/org-note-db-tag-add
  "a" #'my/org-note-db-alias-add
  "q" #'org-note-db-search
  "h" #'org-note-db-unresolved
  "e" #'my/org-note-db-extract
  "m" #'my/org-note-db-refile-file
  "r" #'org-note-db-refresh)

(provide 'my-org-note-db)
;;; my-org-note-db.el ends here
