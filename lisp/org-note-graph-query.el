;;; org-note-graph-query.el --- Composable graph operations -*- lexical-binding: t; -*-

;;; Code:
(require 'org-note-graph-store)

(defun org-note-graph--row-node (row)
  "Decode a node ROW from SQLite."
  (pcase-let ((`(,key ,id ,file ,position ,end ,title ,level ,parent ,properties ,text) row))
    (make-org-note-graph-node :key key :id id :file file :position position :end end
                              :title title :level level :parent parent
                              :properties (read properties) :text text)))

(defun org-note-graph-node (db key)
  "Return the node with KEY in DB, or nil."
  (when-let* ((row (car (sqlite-select db "SELECT * FROM nodes WHERE key = ?" (list key)))))
    (org-note-graph--row-node row)))

(defun org-note-graph-nodes (db)
  "Return DB's nodes in file and source order."
  (mapcar #'org-note-graph--row-node
          (sqlite-select db "SELECT * FROM nodes ORDER BY file, position, level")))

(defun org-note-graph-select (db predicate)
  "Return keys of nodes in DB satisfying PREDICATE."
  (mapcar #'org-note-graph-node-key (seq-filter predicate (org-note-graph-nodes db))))

(defun org-note-graph-resolve (db key)
  "Resolve an ID or file KEY to an existing node key in DB."
  (caar (if (string-prefix-p "file:" key)
            (sqlite-select db "SELECT key FROM nodes WHERE file = ? AND level = 0" (list (substring key 5)))
          (sqlite-select db "SELECT key FROM nodes WHERE key = ?" (list key)))))

(cl-defun org-note-graph-references (db &optional (sources nil sources-p))
  "Return reference occurrences in DB, optionally limited to SOURCES.
An explicit empty source set returns no references.  Missing targets retain
their original keys."
  (cl-loop for (source target file position type path) in
           (sqlite-select db "SELECT * FROM connections ORDER BY file, position")
           when (or (not sources-p) (member source sources))
           collect (make-org-note-graph-link :source source :target target :file file
                                             :position position :type type :path path)))

(defun org-note-graph--neighbors (db keys sql)
  "Select distinct neighbors of KEYS in DB using parameterized SQL."
  (seq-uniq (seq-mapcat (lambda (key) (mapcar #'car (sqlite-select db sql (list key)))) keys)
            #'equal))

(defun org-note-graph-out (db keys)
  "Return existing nodes referenced by KEYS in DB."
  (org-note-graph--neighbors db keys
    "SELECT c.target FROM connections c JOIN nodes n ON n.key = c.target WHERE c.source = ?"))

(defun org-note-graph-in (db keys)
  "Return nodes referencing KEYS in DB."
  (org-note-graph--neighbors db keys "SELECT source FROM connections WHERE target = ?"))

(defun org-note-graph-parents (db keys)
  "Return the immediate structural parents of KEYS in DB."
  (org-note-graph--neighbors db keys "SELECT parent FROM nodes WHERE key = ? AND parent IS NOT NULL"))

(defun org-note-graph-children (db keys)
  "Return the immediate structural children of KEYS in DB."
  (org-note-graph--neighbors db keys "SELECT key FROM nodes WHERE parent = ?"))

(defun org-note-graph-closure (db keys neighbors)
  "Return KEYS and everything reachable through NEIGHBORS in DB.
NEIGHBORS receives DB and a list of keys.  Cycles are visited only once."
  (let ((seen (make-hash-table :test #'equal))
        (pending (copy-sequence keys)) result)
    (while pending
      (let ((key (pop pending)))
        (unless (gethash key seen)
          (puthash key t seen)
          (push key result)
          (setq pending (append (funcall neighbors db (list key)) pending)))))
    (nreverse result)))

(defun org-note-graph-unresolved-references (db)
  "Return authored references whose targets are absent from DB."
  (seq-filter (lambda (link) (not (org-note-graph-node db (org-note-graph-link-target link))))
              (org-note-graph-references db)))

(provide 'org-note-graph-query)
;;; org-note-graph-query.el ends here
