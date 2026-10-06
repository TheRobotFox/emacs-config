;;; my-org-note-graph.el --- Personal graph writing commands -*- lexical-binding: t; -*-

;;; Code:
(require 'org-note-graph-view)
(require 'org-note-graph-discovery)
(require 'org-note-graph-capture)
(require 'org-note-graph-keywords)
(require 'org-refile)
(require 'my-org-note-graph-query)

(dolist (spec '((graph-tags "GRAPH_TAGS" "Tags") (aliases "ALIASES" "Aliases")))
  (pcase-let ((`(,provider ,property ,destination) spec))
    (org-note-graph-register-destination destination 'tagged-location)
    (org-note-graph-register-data-provider provider
     :destination destination :scan (org-note-graph-property-scanner #'org-note-graph-keywords property)
     :formatters '((tagged-location . identity)))))

(defun my/org-note-graph-tag-add (&optional remove)
  "Add a graph tag at this file or heading; with a prefix, remove one."
  (interactive "P")
  (org-note-graph-keywords-edit "GRAPH_TAGS" "Tags" remove))

(defun my/org-note-graph-alias-add (&optional remove)
  "Add an alias at this file or heading; with a prefix, remove one."
  (interactive "P")
  (org-note-graph-keywords-edit "ALIASES" "Aliases" remove))

(defcustom my/org-note-graph-projects-node nil
  "Node key whose file links make repository notes discoverable."
  :type '(choice (const nil) string) :group 'org-note-graph)

(defun my/org-note-graph-collect-project ()
  "Link the repository note by file so the graph can discover it after rebuild."
  (when my/org-note-graph-projects-node
    (let* ((db (org-note-graph-refresh))
           (target (or (org-note-graph-node db my/org-note-graph-projects-node)
                       (user-error "Projects node is missing")))
           (node (my/org-note-graph-external buffer-file-name))
           (key (org-note-graph-node-key node)))
      (unless (member (or (org-note-graph-resolve db key) key)
                      (org-note-graph-out db (list (org-note-graph-node-key target))))
        (org-note-graph-append target (org-note-graph-location-link node (org-note-graph-node-file target)))
        (message "Linked from Projects; save its buffer to persist the edit"))
      (org-note-graph-refresh))))

(defun my/org-note-graph-all (db _context)
  "Select all addressable nodes from DB."
  (mapcar #'org-note-graph-node-key (org-note-graph-nodes db)))

(defvar my/org-note-graph-find-query #'my/org-note-graph-all
  "Query supplying candidates for finding and inserting note links.")

(defun my/org-note-graph-backlink-query (db context)
  "Select nodes referring to CONTEXT in DB."
  (org-note-graph-in db (list context)))

(defun my/org-note-graph-forward-query (db context)
  "Select nodes referenced by CONTEXT in DB."
  (org-note-graph-out db (list context)))

(defun my/org-note-graph-read (db results &optional prompt allow-new)
  "Select RESULTS by name, adding aliases for node results."
  (let* ((locations (org-note-graph-result-locations db results))
         (keys (mapcar #'org-note-graph-node-key (seq-filter #'org-note-graph-node-p locations)))
         (scopes my/org-note-graph-alias-sources)
         (labels (org-note-graph-labels-candidates db keys scopes))
         (keywords (cl-loop for scope in (seq-intersection scopes (org-note-graph-data-destinations 'named-location) #'equal)
                            append (cl-loop for (name location) in (org-note-graph-data-query db scope 'named-location)
                                            for node = (org-note-graph-node-at db location)
                                            when (and node (member (org-note-graph-node-key node) keys))
                                            collect (cons name (org-note-graph-node-key node))))))
    (org-note-graph-read-locations locations prompt allow-new
     (mapcar (lambda (candidate) (cons (car candidate) (org-note-graph-node db (cdr candidate))))
             (append labels keywords)))))

(defun my/org-note-graph-selection (db)
  "Return the configured find query's candidates for the current context."
  (funcall my/org-note-graph-find-query db
           (when-let* ((node (org-note-graph-current-node db)))
             (org-note-graph-node-key node))))

(defvar my/notes-directory)

(defun my/org-note-graph-capture-note (title callback)
  "Capture a new file named TITLE using Org's capture lifecycle.
Call CALLBACK with its indexed node after successful finalization."
  (when (or (string-empty-p (string-trim title)) (string-match-p "[\n\r]" title))
    (user-error "A nonempty, single-line title is required"))
  (unless (seq-some (lambda (root)
                     (and (file-directory-p root)
                          (file-in-directory-p my/notes-directory root)))
                   org-note-graph-roots)
    (user-error "The new notes directory must be within a graph source directory"))
  (let* ((id (org-id-new))
         (slug (string-trim (replace-regexp-in-string "[^[:alnum:]]+" "-" (downcase title)) "-" "-"))
         (file (expand-file-name (concat slug "-" (substring id 0 8) ".org") my/notes-directory))
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
    (make-directory my/notes-directory t)
    (when (file-exists-p file) (user-error "Capture file already exists: %s" file))
    (org-capture nil "g")))

(defun my/org-note-graph-use (choice function)
  "Apply FUNCTION to CHOICE, capturing it first when it is a new title."
  (if (stringp choice) (my/org-note-graph-capture-note choice function)
    (funcall function choice)))

(defun my/org-note-graph-find (&optional query)
  "Find a note or capture a new one; with QUERY, select from a graph query."
  (interactive "P")
  (if query (my/org-note-graph-query-find)
    (let ((db (org-note-graph-database t)))
      (my/org-note-graph-use
       (my/org-note-graph-read db (my/org-note-graph-selection db) "Find note: " t)
       #'org-note-graph-open))))

(defun my/org-note-graph-heading (node)
  "Select a heading within NODE using Org, registering only the chosen target."
  (with-current-buffer (find-file-noselect (org-note-graph-node-file node))
    (save-excursion
      (save-restriction
        (org-note-graph-goto node)
        (let* ((org-refile-targets '((nil . t)))
               (org-refile-use-cache nil)
               (org-refile-use-outline-path (if (zerop (org-note-graph-node-level node)) 'file t))
               (org-refile-target-verify-function
                (lambda () (and (<= (org-note-graph-node-position node) (point))
                                (< (point) (org-note-graph-node-end node)))))
               (target (org-refile-get-location "Link to heading")))
          (if (not (nth 3 target)) node
            (goto-char (nth 3 target))
            (let ((id (org-id-get-create)))
              (org-note-graph-node (org-note-graph-refresh) (concat "id:" id)))))))))

(defun my/org-note-graph-insert-at (position node)
  "Insert NODE's link at POSITION and release that marker."
  (unwind-protect
      (if (not (marker-buffer position)) (user-error "The insertion buffer was closed")
        (with-current-buffer (marker-buffer position)
          (save-restriction
            (widen)
            (goto-char position)
            (insert (org-note-graph-location-link node (buffer-file-name (buffer-base-buffer)))))))
    (set-marker position nil)))

(defun my/org-note-graph-insert (&optional heading)
  "Insert a note link, capturing on nonmatch.
With HEADING, select a subheading and assign it an ID if needed."
  (interactive "P")
  (barf-if-buffer-read-only)
  (let* ((db (org-note-graph-refresh))
         (choice (my/org-note-graph-read db (my/org-note-graph-selection db) "Link to: " t)))
    (when (and heading (org-note-graph-node-p choice))
      (setq choice (my/org-note-graph-heading choice)))
    (my/org-note-graph-use choice (apply-partially #'my/org-note-graph-insert-at (point-marker)))))

(defun my/org-note-graph-external (file)
  "Describe FILE by its path so a new link also supports discovery."
  (setq file (org-note-graph--canonical-file file))
  (unless (and (file-readable-p file) (string-suffix-p ".org" file))
    (user-error "Select a readable Org file"))
  (with-current-buffer (find-file-noselect file)
    (make-org-note-graph-node
     :key (concat "file:" file) :file file :level 0 :position 1
     :title (or (cadar (org-collect-keywords '("TITLE"))) (file-name-base file)))))

(defun my/org-note-graph-capture ()
  "Capture at a declared destination.
Prefill item targets with the current node's link when available."
  (interactive)
  (let* ((db (org-note-graph-refresh))
         (node (org-note-graph-current-node db))
         (targets (org-note-graph-capture-targets db))
         (_ (unless targets (user-error "No capture targets; declare one with C-c n d")))
         (target (org-note-graph-read-locations targets "Capture in: "))
         (link (when (and node (equal "item" (org-note-graph-property target "CAPTURE")))
                 (org-note-graph-location-link node (org-note-graph-location-file target)))))
    (org-note-graph-capture-at target)
    (when link (insert link))))

(defun my/org-note-graph-lookup (&optional insert)
  "Choose an indexed name, then its location; with INSERT, insert its Org link."
  (interactive "P")
  (let* ((db (org-note-graph-database t))
         (scopes (org-note-graph-data-destinations 'named-location))
         (_ (unless scopes (user-error "No named location destinations")))
         (scope (completing-read "Lookup in: " scopes nil t))
         (rows (org-note-graph-data-query db scope 'named-location))
         (_ (unless rows (user-error "No names indexed in %s" scope)))
         (name (completing-read "Name: " (seq-uniq (mapcar #'car rows) #'equal) nil t))
         (locations (seq-uniq (mapcar #'cadr (seq-filter (lambda (row) (equal name (car row))) rows)) #'equal))
         (location (if (length= locations 1) (car locations)
                     (org-note-graph-read-locations locations "Find: "))))
    (if insert
        (insert (org-note-graph-location-link location buffer-file-name))
      (org-note-graph-open location))))

(defun my/org-note-graph-show (query title)
  "Display QUERY for the current node under TITLE."
  (let* ((db (org-note-graph-database t))
         (node (org-note-graph-current-node db)))
    (org-note-graph-view query (when node (org-note-graph-node-key node)) title)))

(defun my/org-note-graph-backlinks ()
  "Show the current node's backlinks."
  (interactive)
  (my/org-note-graph-show #'my/org-note-graph-backlink-query "Backlinks"))

(defun my/org-note-graph-forward-links ()
  "Show the current node's outgoing references."
  (interactive)
  (my/org-note-graph-show #'my/org-note-graph-forward-query "Forward links"))

(defvar my/org-note-graph-query-history nil)
(defvar-local my/org-note-graph-query-text nil)
(defvar-local my/org-note-graph-query-origin nil)

(defun my/org-note-graph-query-anchor ()
  "Insert a precise node reference into the query minibuffer."
  (interactive)
  (let* ((db (org-note-graph-database t))
         (node (my/org-note-graph-read db (my/org-note-graph-all db nil) "Query anchor: ")))
    (insert (org-note-graph-location-link node))))

(defvar-keymap my/org-note-graph-query-minibuffer-map
  :parent minibuffer-local-map
  "M-i" #'my/org-note-graph-query-anchor)

(defun my/org-note-graph-query-read (&optional initial)
  "Read a graph expression, starting with INITIAL."
  (read-from-minibuffer "Graph query (M-i: node): " initial
                        my/org-note-graph-query-minibuffer-map nil
                        'my/org-note-graph-query-history))

(defun my/org-note-graph-query (&optional action text)
  "Read and show a graph expression; edit the expression in an existing query view.
ACTION may be `find' or `insert', selecting a result instead of showing a view.
TEXT supplies the expression without prompting."
  (interactive)
  (let* ((db (org-note-graph-database t))
         (node (org-note-graph-current-node db))
         (context (if my/org-note-graph-query-text org-note-graph--view-context
                    (when node (org-note-graph-node-key node))))
         (origin (copy-marker (or my/org-note-graph-query-origin (point)) t))
         (enable-recursive-minibuffers t)
         (text (or text (if (and action my/org-note-graph-query-text) my/org-note-graph-query-text
                         (my/org-note-graph-query-read my/org-note-graph-query-text))))
         (query (my/org-note-graph-query-compile text))
         (results (funcall query db context)))
    (if action
        (progn
          (when (eq action 'insert)
            (unless (marker-buffer origin) (user-error "The insertion buffer was closed"))
            (with-current-buffer (marker-buffer origin) (barf-if-buffer-read-only)))
          (my/org-note-graph-use
           (my/org-note-graph-read db results "Query result: " t)
           (if (eq action 'insert) (apply-partially #'my/org-note-graph-insert-at origin)
             (set-marker origin nil)
             #'org-note-graph-open)))
      (when my/org-note-graph-query-origin
        (set-marker my/org-note-graph-query-origin nil))
      (let ((buffer (org-note-graph-view query context "Query")))
        (with-current-buffer buffer
          (setq my/org-note-graph-query-text text
                my/org-note-graph-query-origin origin)
          (add-hook 'kill-buffer-hook
                    (lambda ()
                      (when my/org-note-graph-query-origin
                        (set-marker my/org-note-graph-query-origin nil))) nil t)
          (use-local-map (copy-keymap (current-local-map)))
          (local-set-key "e" #'my/org-note-graph-query)
          (local-set-key "f" #'my/org-note-graph-query-find)
          (local-set-key "i" #'my/org-note-graph-query-insert))))))

(defun my/org-note-graph-query-find ()
  "Select a query result to visit, capturing a new title on nonmatch."
  (interactive)
  (my/org-note-graph-query 'find))

(defun my/org-note-graph-query-insert ()
  "Select a query result to link at the original writing position."
  (interactive)
  (my/org-note-graph-query 'insert))

(org-link-set-parameters
 "query"
 :follow (lambda (text _arg) (my/org-note-graph-query nil text))
 :complete (lambda (&optional _arg)
             (concat "query:" (my/org-note-graph-query-read)))
 :store (lambda (&optional _interactive)
          (when (and (derived-mode-p 'org-note-graph-view-mode) my/org-note-graph-query-text)
            (org-link-store-props
             :type "query" :link (concat "query:" my/org-note-graph-query-text)
             :description my/org-note-graph-query-text))))

(defun my/org-note-graph-search (regexp)
  "Show nodes whose own text matches REGEXP."
  (interactive "sContent regexp: ")
  (org-note-graph-view
   (lambda (db _context)
     (org-note-graph-select db (lambda (node) (string-match-p regexp (org-note-graph-node-text node)))))
   nil (concat "Search: " regexp)))

(defun my/org-note-graph-unresolved ()
  "List unresolved references using native source navigation."
  (interactive)
  (require 'compile)
  (let ((links (org-note-graph-unresolved-references (org-note-graph-database t)))
        (buffer (get-buffer-create "*Note Graph: Unresolved*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (dolist (link links)
          (let ((line (with-temp-buffer
                        (if-let* ((source (find-buffer-visiting (org-note-graph-link-file link))))
                            (insert (with-current-buffer source
                                      (save-restriction
                                        (widen)
                                        (buffer-substring-no-properties (point-min) (point-max)))))
                          (insert-file-contents (org-note-graph-link-file link)))
                        (line-number-at-pos (org-note-graph-link-position link)))))
            (insert (format "%s:%d: Unresolved target %s\n"
                            (org-note-graph-link-file link) line (org-note-graph-link-target link)))))
        (unless links (insert "No unresolved references.\n")))
      (compilation-mode)
      (set-buffer-modified-p nil)
      (use-local-map (copy-keymap (current-local-map)))
      (local-set-key "g" #'my/org-note-graph-unresolved))
    (funcall org-note-graph-display-function buffer)))

(defun my/org-note-graph-declare ()
  "Configure a scanner at the current heading or file preamble."
  (interactive)
  (unless (derived-mode-p 'org-mode) (user-error "Use an Org buffer"))
  (let* ((commands '(("Scanner" . org-note-graph-text-setup-content)
                     ("Capture Target" . org-note-graph-capture-setup)))
         (choice (completing-read "Set up: " commands nil t)))
    (call-interactively (cdr (assoc choice commands)))))

(defvar-keymap my/org-note-graph-map
  "f" #'my/org-note-graph-find
  "i" #'my/org-note-graph-insert
  "c" #'org-capture
  "T" #'my/org-note-graph-capture
  "b" #'my/org-note-graph-backlinks
  "l" #'my/org-note-graph-forward-links
  "g" #'my/org-note-graph-search
  "s" #'my/org-note-graph-lookup
  "C" #'org-id-get-create
  "d" #'my/org-note-graph-declare
  "t" #'my/org-note-graph-tag-add
  "a" #'my/org-note-graph-alias-add
  "q" #'my/org-note-graph-query
  "h" #'my/org-note-graph-unresolved
  "r" #'org-note-graph-refresh)

(provide 'my-org-note-graph)
;;; my-org-note-graph.el ends here
