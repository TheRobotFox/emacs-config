;;; org-note-graph-labels.el --- Labels attached to nodes -*- lexical-binding: t; -*-

;;; Commentary:
;; A userland table derived from description lists:
;;   * [[id:node][Title]] :: "Another name" "Short name"

;;; Code:
(require 'org-note-graph-store)
(require 'org-note-graph-query)

(declare-function org-note-graph--link "org-note-graph-ui" (node &optional source-file))
(declare-function org-note-graph-append "org-note-graph-edit" (node text))

(defun org-note-graph-labels--names (text)
  "Read a sequence of quoted names from TEXT."
  (let* ((input (concat "(" text "\n)"))
         (parsed (read-from-string input))
         (names (car parsed)))
    (unless (and (= (cdr parsed) (length input))
                 (listp names) (seq-every-p #'stringp names))
      (user-error "Labels must be quoted strings"))
    (delete-dups names)))

(defun org-note-graph-labels--item (node text item)
  "Extract label rows from NODE's ITEM in source TEXT."
  (let* ((tag (org-element-property :tag item))
         (link (org-element-map tag 'link #'identity nil t))
         (body (car (org-element-contents item)))
         (file (org-note-graph-node-file node))
         (target (and link (org-note-graph-reference link file))))
    (when (and target (eq (org-element-type body) 'paragraph))
      (mapcar
       (lambda (name)
         (vector file (org-note-graph-node-key node) target name
                 (org-element-property :begin item)))
       (org-note-graph-labels--names
        (substring text (1- (org-element-property :contents-begin body))
                   (1- (org-element-property :contents-end body))))))))

(defun org-note-graph-labels--rows (document)
  "Extract label rows from DOCUMENT without modifying its sources."
  (let ((nodes (org-note-graph-document-nodes document))
        (tree (org-note-graph-document-tree document))
        (text (org-note-graph-document-text document)))
    (cl-loop for node in nodes
             when (equal (org-note-graph--property node "GRAPH_TYPE") "labels")
             append
             (apply #'append
                    (org-note-graph-map-owned-elements
                     node tree nodes 'item
                     (lambda (item)
                       (org-note-graph-labels--item node text item)))))))

(defun org-note-graph-labels--initialize (db)
  "Create the label specification's table in DB."
  (sqlite-execute db "CREATE TABLE graph_labels
                     (file TEXT, source TEXT, target TEXT, name TEXT, position INTEGER)"))

(defun org-note-graph-labels--replace (db changes)
  "Replace label rows in DB authored by the files in CHANGES."
  (dolist (change changes)
    (sqlite-execute db "DELETE FROM graph_labels WHERE file = ?" (vector (car change))))
  (dolist (change changes)
    (when (cdr change)
      (dolist (row (org-note-graph-labels--rows (cdr change)))
        (sqlite-execute db "INSERT INTO graph_labels VALUES (?, ?, ?, ?, ?)" row)))))

(cl-defun org-note-graph-labels-candidates (db &optional (keys nil keys-p) (sources nil sources-p))
  "Return (LABEL . NODE-KEY) pairs from DB.
Restrict to KEYS and SOURCES when supplied.
Unresolved targets stay in the table but do not appear as candidates."
  (delete-dups
   (cl-loop for (name target source) in (sqlite-select db "SELECT name, target, source FROM graph_labels")
            for key = (org-note-graph-resolve db target)
            when (and key (or (not keys-p) (member key keys))
                      (or (not sources-p) (member source sources)))
            collect (cons name key))))

(cl-defun org-note-graph-labels-query (db regexp &optional (sources nil sources-p))
  "Return node keys with labels matching REGEXP in DB, optionally from SOURCES."
  (delete-dups
   (cl-loop for (name . key) in (if sources-p
                                  (org-note-graph-labels-candidates
                                   db (mapcar #'org-note-graph-node-key (org-note-graph-nodes db)) sources)
                                (org-note-graph-labels-candidates db))
            when (string-match-p regexp name) collect key)))

(defun org-note-graph-labels-add (target context)
  "Append labels for CONTEXT's node to the declared TARGET."
  (let ((node (or (plist-get context :node) (user-error "No current node")))
        (names (org-note-graph-labels--names (read-string "Labels (quoted): "))))
    (unless names (user-error "Enter at least one label"))
    (org-note-graph-append
     target (format "%s :: %s"
                    (org-note-graph--link node (org-note-graph-node-file target))
                    (mapconcat #'prin1-to-string names " ")))))

(org-note-graph-register-collection
 "Labels" :type "labels"
 :initialize #'org-note-graph-labels--initialize
 :replace #'org-note-graph-labels--replace
 :actions '((add . org-note-graph-labels-add)))

(provide 'org-note-graph-labels)
;;; org-note-graph-labels.el ends here
