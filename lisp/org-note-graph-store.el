;;; org-note-graph-store.el --- Rebuildable Org graph index -*- lexical-binding: t; -*-

;;; Code:
(require 'org-note-graph-model)
(require 'org-element)
(require 'org-id)
(require 'sqlite)

(defvar org-note-graph--db nil)
(defvar org-note-graph--stamps (make-hash-table :test #'equal))
(defvar org-note-graph--indexed-revision nil)

(defun org-note-graph--canonical-file (file)
  "Return FILE's expanded, canonical local name."
  (when (file-remote-p file) (user-error "Remote graph files are not supported"))
  (file-truename (expand-file-name file)))

(defun org-note-graph--owned-text (node nodes)
  "Read NODE's text in the current buffer, excluding descendant ID nodes."
  (let ((start (org-note-graph-node-position node))
        (end (min (point-max) (org-note-graph-node-end node))) parts)
    (dolist (child nodes)
      (when (and (> (org-note-graph-node-level child) (org-note-graph-node-level node))
                 (<= start (org-note-graph-node-position child))
                 (< (org-note-graph-node-position child) end))
        (push (buffer-substring-no-properties start (org-note-graph-node-position child)) parts)
        (setq start (min end (org-note-graph-node-end child)))))
    (push (buffer-substring-no-properties start end) parts)
    (apply #'concat (nreverse parts))))

(defun org-note-graph-parse (text file)
  "Parse TEXT from FILE into nodes and raw link occurrences.
Does not visit files, run user mode hooks, or create IDs.  Headings without
IDs belong to the nearest addressable ancestor.  A file without an ID has
a temporary file-addressed vertex."
  (with-temp-buffer
    (insert text)
    (let ((org-inhibit-startup t)) (delay-mode-hooks (org-mode)))
    (goto-char (point-min))
    (skip-chars-forward " \t\n")
    (let* ((tree (org-element-parse-buffer))
           (properties (when (org-before-first-heading-p)
                         (org-entry-properties nil 'standard)))
           (id (cdr (assoc "ID" properties)))
           (title (or (cadar (org-collect-keywords '("TITLE")))
                      (file-name-base file)))
           (nodes (list (make-org-note-graph-node
                         :key (if id (concat "id:" id) (concat "file:" file))
                         :id id :file file :position 1 :end (1+ (point-max))
                         :title title :level 0 :properties properties)))
           links)
      (org-element-map tree 'headline
		       (lambda (heading)
			 (when-let* ((id (org-element-property :ID heading)))
			   (goto-char (org-element-property :begin heading))
			   (push (make-org-note-graph-node
				  :key (concat "id:" id) :id id :file file
				  :position (point) :end (org-element-property :end heading)
				  :title (org-get-heading t t t t)
				  :level (org-element-property :level heading)
				  :properties (org-entry-properties nil 'standard)) nodes))))
      ;; Org visits headings in source order; the initial file node stays first.
      (setq nodes (nreverse nodes))
      (dolist (node (cdr nodes))
        (setf (org-note-graph-node-parent node)
              (org-note-graph-node-key
               (org-note-graph--owner (seq-take-while (lambda (parent) (not (eq parent node))) nodes)
                                     (org-note-graph-node-position node)))))
      (dolist (node nodes)
        (setf (org-note-graph-node-text node) (org-note-graph--owned-text node nodes)))
      (cl-labels ((collect-link (link &optional offset)
                    (when-let* ((target (org-note-graph-reference link file)))
                      (let ((position (+ (or offset 0) (org-element-property :begin link))))
                        (push (make-org-note-graph-link
                               :source (org-note-graph-node-key (org-note-graph--owner nodes position))
                               :target target :file file :position position
                               :type (org-element-property :type link)
                               :path (org-element-property :path link)) links)))))
        (org-element-map tree 'link #'collect-link)
        ;; Property values are secondary text, not link objects in the Org AST.
        (org-element-map tree 'node-property
			 (lambda (property)
			   (let ((value (org-element-property :value property)))
			     (goto-char (org-element-property :begin property))
			     (when (search-forward value (line-end-position) t)
			       (let ((offset (- (point) (length value) 1)))
				 (org-element-map (org-element-parse-secondary-string value '(link)) 'link
						  (lambda (link) (collect-link link offset)))))))))
      (make-org-note-graph-document :nodes nodes :links (nreverse links)
                                    :tree tree :text text))))

(defun org-note-graph-reference (link source-file)
  "Return the graph key of Org LINK relative to SOURCE-FILE, or nil.
References retain their authored ID or canonical file key even if unresolved.
Only local Org files participate in file discovery; search suffixes address
locations within their file vertex, rather than independent graph nodes."
  (pcase (org-element-property :type link)
    ("id" (concat "id:" (org-element-property :path link)))
    ("file"
     (let ((path (org-link-unescape (org-element-property :path link))))
       (unless (file-remote-p path)
         (let ((file (expand-file-name path (file-name-directory source-file))))
           (when (string-suffix-p ".org" file)
             (concat "file:" (org-note-graph--canonical-file file)))))))))

(defun org-note-graph--stamp (file)
  "Return FILE's buffer revision or disk modification stamp."
  (if-let* ((buffer (find-buffer-visiting file)))
      (with-current-buffer buffer (list buffer (buffer-chars-modified-tick)))
    (let ((attributes (file-attributes file)))
      (list (file-attribute-modification-time attributes)
            (file-attribute-size attributes)))))

(defun org-note-graph--read-document (file)
  "Parse FILE, preferring the full contents of its visiting buffer."
  (org-note-graph-parse
   (if-let* ((buffer (find-buffer-visiting file)))
       (with-current-buffer buffer
         (save-restriction (widen) (buffer-substring-no-properties (point-min) (point-max))))
     (with-temp-buffer (insert-file-contents file) (buffer-string))) file))

(defun org-note-graph--seed-files ()
  "Expand seed roots, including new visiting files and deduplicating symlinks."
  (let ((roots (mapcar #'org-note-graph--canonical-file org-note-graph-roots)))
    (delete-dups
     (mapcar #'org-note-graph--canonical-file
             (seq-filter
              (lambda (file) (not (string-prefix-p ".#" (file-name-nondirectory file))))
              (append
               (cl-loop for root in roots append
                        (if (file-directory-p root) (directory-files-recursively root "\\.org\\'")
                          (list root)))
               (cl-loop for buffer in (buffer-list)
                        for file = (buffer-file-name buffer)
                        when (and file (not (file-remote-p file)) (string-suffix-p ".org" file)
                                  (seq-some (lambda (root)
                                              (if (file-directory-p root) (file-in-directory-p file root)
                                                (equal root (org-note-graph--canonical-file file)))) roots))
                        collect file)))))))

(defun org-note-graph--discover (db force)
  "Return reachable (FILE STAMP DOCUMENT) records, parsing changed files only."
  (let ((queue (org-note-graph--seed-files))
        (seen (make-hash-table :test #'equal)) records)
    (while queue
      (let ((file (pop queue)))
        (unless (gethash file seen)
          (puthash file t seen)
          (when (or (file-regular-p file) (find-buffer-visiting file))
            (let* ((stamp (org-note-graph--stamp file))
                   (document (when (or force (not (equal stamp (gethash file org-note-graph--stamps))))
                               (org-note-graph--read-document file)))
                   (targets (if document
                                (mapcar #'org-note-graph-link-target (org-note-graph-document-links document))
                              (mapcar #'car (sqlite-select db "SELECT target FROM links WHERE file = ?" (list file))))))
              (push (list file stamp document) records)
              (dolist (target targets)
                (when (string-prefix-p "file:" target) (push (substring target 5) queue))))))))
    (nreverse records)))

(defun org-note-graph--initialize (db)
  "Initialize core and registered extension tables in DB."
  (sqlite-execute db "CREATE TABLE nodes (key TEXT PRIMARY KEY, id TEXT, file TEXT,
position INTEGER, end INTEGER, title TEXT, level INTEGER, parent TEXT, properties TEXT, text TEXT)")
  (sqlite-execute db "CREATE INDEX nodes_file ON nodes(file)")
  (sqlite-execute db "CREATE TABLE links (source TEXT, target TEXT, file TEXT, position INTEGER, type TEXT, path TEXT)")
  (dolist (column '(source target file))
    (sqlite-execute db (format "CREATE INDEX links_%s ON links(%s)" column column)))
  ;; Resolve file links at query time, including files that later acquire an ID.
  (sqlite-execute db "CREATE VIEW connections AS
SELECT l.source, COALESCE(n.key, l.target) AS target, l.file, l.position, l.type, l.path
FROM links l LEFT JOIN nodes n ON n.level = 0 AND l.target = 'file:' || n.file")
  (dolist (entry org-note-graph-collection-templates)
    (when-let* ((initialize (plist-get (cdr entry) :initialize))) (funcall initialize db))))

(defun org-note-graph--replace (db changes)
  "Replace CHANGES in DB, then update extension-owned data."
  ;; Remove all old locations before inserting nodes moved between sources.
  (dolist (change changes)
    (sqlite-execute db "DELETE FROM links WHERE file = ?" (list (car change)))
    (sqlite-execute db "DELETE FROM nodes WHERE file = ?" (list (car change))))
  (dolist (change changes)
    (when-let* ((document (cdr change)))
      (dolist (node (org-note-graph-document-nodes document))
        (sqlite-execute db "INSERT INTO nodes VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
                        (list (org-note-graph-node-key node) (org-note-graph-node-id node)
                              (org-note-graph-node-file node) (org-note-graph-node-position node)
                              (org-note-graph-node-end node) (org-note-graph-node-title node)
                              (org-note-graph-node-level node) (org-note-graph-node-parent node)
                              (prin1-to-string (org-note-graph-node-properties node))
                              (org-note-graph-node-text node))))
      (dolist (link (org-note-graph-document-links document))
        (sqlite-execute db "INSERT INTO links VALUES (?, ?, ?, ?, ?, ?)"
                        (list (org-note-graph-link-source link) (org-note-graph-link-target link)
                              (org-note-graph-link-file link) (org-note-graph-link-position link)
                              (org-note-graph-link-type link) (org-note-graph-link-path link))))))
  (dolist (entry org-note-graph-collection-templates)
    (when-let* ((replace (plist-get (cdr entry) :replace))) (funcall replace db changes))))

(defun org-note-graph-reset ()
  "Discard the derived database; leave Org sources untouched."
  (interactive)
  (when org-note-graph--db (sqlite-close org-note-graph--db))
  (setq org-note-graph--db nil org-note-graph--indexed-revision nil
        org-note-graph--stamps (make-hash-table :test #'equal)))

(defun org-note-graph-refresh (&optional force)
  "Index changed sources and return the database, rebuilding with FORCE.
Live buffers take precedence over disk.  Failed updates leave the previous
index intact.  Registered schema changes trigger a complete rebuild."
  (interactive "P")
  (let* ((rebuild (or force (null org-note-graph--db)
                      (not (equal org-note-graph--indexed-revision org-note-graph--registry-revision))))
         (db (if rebuild (sqlite-open) org-note-graph--db))
         (stamps (make-hash-table :test #'equal)) accepted changes)
    (unwind-protect
        (progn
          (with-sqlite-transaction db
            (when rebuild (org-note-graph--initialize db))
            (dolist (record (org-note-graph--discover db rebuild))
              (pcase-let ((`(,file ,stamp ,document) record))
                (puthash file stamp stamps)
                (when document (push (cons file document) changes))))
            (unless rebuild
              (maphash (lambda (file _stamp)
                         (unless (gethash file stamps) (push (cons file nil) changes)))
                       org-note-graph--stamps))
            (when changes (org-note-graph--replace db changes)))
          (when (and rebuild org-note-graph--db) (sqlite-close org-note-graph--db))
          (setq org-note-graph--db db org-note-graph--stamps stamps
                org-note-graph--indexed-revision org-note-graph--registry-revision accepted t)
          (dolist (change changes)
            (when (cdr change)
              (dolist (node (org-note-graph-document-nodes (cdr change)))
                (when (org-note-graph-node-id node)
                  (org-id-add-location (org-note-graph-node-id node) (org-note-graph-node-file node))))))
          (when (called-interactively-p 'interactive)
            (message "Indexed %d Org files" (hash-table-count stamps)))
          db)
      (when (and rebuild (not accepted)) (sqlite-close db)))))

(defun org-note-graph-database ()
  "Return the current database, building it on first use."
  (or org-note-graph--db (org-note-graph-refresh)))

(provide 'org-note-graph-store)
;;; org-note-graph-store.el ends here
