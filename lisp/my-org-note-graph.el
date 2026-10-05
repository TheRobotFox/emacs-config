;;; my-org-note-graph.el --- Personal graph writing commands -*- lexical-binding: t; -*-

;;; Code:
(require 'org-note-graph-templates)
(require 'org-refile)
(require 'my-org-note-graph-query)

(defcustom my/org-note-graph-collections-node nil
  "Node key of the list receiving links to newly declared collections."
  :type '(choice (const nil) string) :group 'org-note-graph)

(defcustom my/org-note-graph-project-collection nil
  "Node key of the collection linking repository notes."
  :type '(choice (const nil) string) :group 'org-note-graph)

(defun my/org-note-graph-collect-project ()
  "Link the repository note by file so the graph can discover it after rebuild."
  (when my/org-note-graph-project-collection
    (let* ((db (org-note-graph-refresh))
           (target (or (org-note-graph-node db my/org-note-graph-project-collection)
                       (user-error "Project collection is missing"))))
      (org-note-graph-templates-add
       target (list :node (my/org-note-graph-external buffer-file-name) :db db))
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

(defun my/org-note-graph-read (db keys &optional prompt allow-new)
  "Select KEYS from DB by title or alias, with PROMPT and ALLOW-NEW."
  (org-note-graph-read
   db keys prompt allow-new
   (mapcar (lambda (candidate)
             (cons (car candidate) (org-note-graph-node db (cdr candidate))))
           (org-note-graph-labels-candidates db keys (when my/org-note-graph-alias-collection
                                                      (list my/org-note-graph-alias-collection))))))

(defun my/org-note-graph-register-declaration (key target)
  "Append declared KEY to TARGET, resolving their current locations."
  (let* ((db (org-note-graph-refresh))
         (node (org-note-graph-node db key))
         (collection (or (org-note-graph-node db target)
                         (user-error "Collections node is missing: %s" target))))
    (when (and node (org-note-graph-collection-definition node))
      (org-note-graph-templates-add collection (list :node node :db db)))))

(defun my/org-note-graph-capture-collection (target _context)
  "Capture a link-list heading beneath TARGET and register its links."
  (let* ((key (concat "id:" (org-id-new)))
         (catalog (org-note-graph-node-key target))
         (registry my/org-note-graph-collections-node)
         (org-capture-templates
          `(("g" "Collection" entry
             (function ,(lambda () (org-note-graph--goto target)))
             ,(format "* %%^{Name}\n:PROPERTIES:\n:ID: %s\n:GRAPH_TYPE: list\n:END:\n%%?"
                      (substring key 3))
             :empty-lines 1
             :after-finalize
             ,(lambda ()
                (unless org-note-abort
                  (dolist (destination (delete-dups (delq nil (list catalog registry))))
                    (my/org-note-graph-register-declaration key destination)))))))
         (org-capture-templates-contexts nil))
    (org-capture nil "g")))

(org-note-graph-register-collection
 "Collection catalog" :type "collection-catalog"
 :actions '((add . org-note-graph-templates-add)
            (capture . my/org-note-graph-capture-collection)))

(defun my/org-note-graph-collection (db &optional action)
  "Select a declared node in DB, optionally supporting ACTION."
  (org-note-graph-read
   db (org-note-graph-select
       db (lambda (node)
            (when-let* ((spec (org-note-graph-collection-definition node)))
              (or (not action) (assq action (plist-get spec :actions))))))
   "Collection: "))

(defun my/org-note-graph-selection (db scoped)
  "Return writing candidates from DB, choosing a collection when SCOPED."
  (if (not scoped)
      (funcall my/org-note-graph-find-query db
               (when-let* ((node (org-note-graph-current-node db)))
                 (org-note-graph-node-key node)))
    (let* ((target (my/org-note-graph-collection db))
           (spec (org-note-graph-collection-definition target)))
      (funcall (or (plist-get spec :query) #'my/org-note-graph-forward-query)
               db (org-note-graph-node-key target)))))

(defun my/org-note-graph-use (choice function)
  "Apply FUNCTION to CHOICE, capturing it first when it is a new title."
  (if (stringp choice) (org-note-graph-capture-note choice function)
    (funcall function choice)))

(defun my/org-note-graph-find (&optional scoped)
  "Find a note or capture a new one; with SCOPED, choose a collection first."
  (interactive "P")
  (let ((db (org-note-graph-refresh)))
    (my/org-note-graph-use
     (my/org-note-graph-read db (my/org-note-graph-selection db scoped) "Find note: " t)
     #'org-note-graph-open)))

(defun my/org-note-graph-heading (node)
  "Select a heading within NODE using Org, registering only the chosen target."
  (with-current-buffer (find-file-noselect (org-note-graph-node-file node))
    (save-excursion
      (save-restriction
        (org-note-graph--goto node)
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
            (insert (org-note-graph--link node (buffer-file-name (buffer-base-buffer)))))))
    (set-marker position nil)))

(defun my/org-note-graph-insert (&optional heading)
  "Insert a note link, capturing on nonmatch.
With HEADING, select a subheading and assign it an ID if needed."
  (interactive "P")
  (barf-if-buffer-read-only)
  (let* ((db (org-note-graph-refresh))
         (choice (my/org-note-graph-read db (my/org-note-graph-selection db nil) "Link to: " t)))
    (when (and heading (org-note-graph-node-p choice))
      (setq choice (my/org-note-graph-heading choice)))
    (my/org-note-graph-use choice (apply-partially #'my/org-note-graph-insert-at (point-marker)))))

(defun my/org-note-graph-declare ()
  "Declare the current file or heading using a registered specification."
  (interactive)
  (unless (and (derived-mode-p 'org-mode) (buffer-file-name (buffer-base-buffer)))
    (user-error "Declare collections in an Org file"))
  (let* ((name (completing-read "Specification: " org-note-graph-collection-templates nil t))
         (spec (cdr (assoc name org-note-graph-collection-templates)))
         (target my/org-note-graph-collections-node))
    (when (and target (not (org-note-graph-node (org-note-graph-refresh) target)))
      (user-error "Collections node is missing: %s" target))
    (save-excursion
      (unless (org-before-first-heading-p) (org-back-to-heading t))
      (org-id-get-create)
      (org-entry-put nil "GRAPH_TYPE" (plist-get spec :type)))
    (let* ((db (org-note-graph-refresh))
           (node (org-note-graph-current-node db)))
      (when-let* ((setup (plist-get spec :declare))) (funcall setup node))
      (when target
        (let* ((key (org-note-graph-node-key node))
               (register (lambda ()
                           (unless org-note-abort
                             (my/org-note-graph-register-declaration key target)))))
          (if (bound-and-true-p org-capture-mode)
              (let ((after (org-capture-get :after-finalize t)))
                (setq org-capture-current-plist
                      (plist-put org-capture-current-plist :after-finalize
                                 (append (if (functionp after) (list after) after) (list register)))))
            (my/org-note-graph-register-declaration key target)))))))

(defun my/org-note-graph-external (file)
  "Describe FILE by its path so a new link also supports discovery."
  (setq file (org-note-graph--canonical-file file))
  (unless (and (file-readable-p file) (string-suffix-p ".org" file))
    (user-error "Select a readable Org file"))
  (with-current-buffer (find-file-noselect file)
    (make-org-note-graph-node
     :key (concat "file:" file) :file file :level 0 :position 1
     :title (or (cadar (org-collect-keywords '("TITLE"))) (file-name-base file)))))

(defun my/org-note-graph-act (action &optional source db)
  "Invoke ACTION on a selected declaration with SOURCE or the current node.
DB may supply an already refreshed index.
Interactively choose a target and one of its named actions."
  (interactive (list nil))
  (let* ((db (or db (org-note-graph-refresh)))
         (node (or source (org-note-graph-current-node db)))
         (target (my/org-note-graph-collection db action))
         (actions (plist-get (org-note-graph-collection-definition target) :actions))
         (action (or action (intern (completing-read "Action: " actions nil t))))
         (function (or (alist-get action actions) (user-error "No supported action"))))
    (funcall function target (list :node node :db db))
    (org-note-graph-refresh)))

(defun my/org-note-graph-collect (&optional select)
  "Add the current node to a collection.
With SELECT, choose an indexed note or registered heading instead."
  (interactive "P")
  (let* ((db (org-note-graph-refresh))
         (node (if select
                   (my/org-note-graph-read db (my/org-note-graph-selection db nil) "Add node: ")
                 (or (org-note-graph-current-node db)
                     (when (and (derived-mode-p 'org-mode) (buffer-file-name (buffer-base-buffer)))
                       (my/org-note-graph-external (buffer-file-name (buffer-base-buffer))))))))
    (unless node (user-error "No current node; use C-u to select one"))
    (my/org-note-graph-act 'add node db)))

(defun my/org-note-graph-capture ()
  "Select a collection offering a capture action."
  (interactive)
  (my/org-note-graph-act 'capture))

(defun my/org-note-graph-show (query title)
  "Display QUERY for the current node under TITLE."
  (let* ((db (org-note-graph-refresh))
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
  (let* ((db (org-note-graph-refresh))
         (node (my/org-note-graph-read db (my/org-note-graph-all db nil) "Query anchor: ")))
    (insert (org-note-graph--link node))))

(defvar-keymap my/org-note-graph-query-minibuffer-map
  :parent minibuffer-local-map
  "M-i" #'my/org-note-graph-query-anchor)

(defun my/org-note-graph-query (&optional action)
  "Read and show a graph expression; edit the expression in an existing query view.
ACTION may be `find' or `insert', selecting a result instead of showing a view."
  (interactive)
  (let* ((db (org-note-graph-refresh))
         (node (org-note-graph-current-node db))
         (context (if my/org-note-graph-query-text org-note-graph--view-context
                    (when node (org-note-graph-node-key node))))
         (origin (copy-marker (or my/org-note-graph-query-origin (point)) t))
         (enable-recursive-minibuffers t)
         (text (if (and action my/org-note-graph-query-text) my/org-note-graph-query-text
                 (read-from-minibuffer "Graph query (M-i: node): " my/org-note-graph-query-text
                                       my/org-note-graph-query-minibuffer-map nil
                                       'my/org-note-graph-query-history)))
         (query (my/org-note-graph-query-compile text))
         (keys (funcall query db context)))
    (if action
        (progn
          (when (eq action 'insert)
            (unless (marker-buffer origin) (user-error "The insertion buffer was closed"))
            (with-current-buffer (marker-buffer origin) (barf-if-buffer-read-only)))
          (my/org-note-graph-use
           (my/org-note-graph-read db keys "Query result: " t)
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

(defun my/org-note-graph-search (regexp)
  "Show nodes whose own text matches REGEXP."
  (interactive "sContent regexp: ")
  (org-note-graph-view
   (lambda (db _context)
     (org-note-graph-select db (lambda (node) (string-match-p regexp (org-note-graph-node-text node)))))
   nil (concat "Search: " regexp)))

(defun my/org-note-graph-view-collection ()
  "Display a collection's query, defaulting to its outgoing references."
  (interactive)
  (let* ((db (org-note-graph-refresh))
         (node (my/org-note-graph-collection db))
         (spec (org-note-graph-collection-definition node)))
    (org-note-graph-view (or (plist-get spec :query) #'my/org-note-graph-forward-query)
                         (org-note-graph-node-key node) (org-note-graph-node-title node))))

(defun my/org-note-graph-unresolved ()
  "List unresolved references using native source navigation."
  (interactive)
  (require 'compile)
  (let ((links (org-note-graph-unresolved-references (org-note-graph-refresh)))
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

(defvar-keymap my/org-note-graph-map
  "f" #'my/org-note-graph-find
  "i" #'my/org-note-graph-insert
  "c" #'org-capture
  "T" #'my/org-note-graph-capture
  "t" #'my/org-note-graph-collect
  "a" #'my/org-note-graph-act
  "b" #'my/org-note-graph-backlinks
  "l" #'my/org-note-graph-forward-links
  "g" #'my/org-note-graph-search
  "C" #'org-id-get-create
  "d" #'my/org-note-graph-declare
  "v" #'my/org-note-graph-view-collection
  "q" #'my/org-note-graph-query
  "h" #'my/org-note-graph-unresolved
  "r" #'org-note-graph-refresh)

(provide 'my-org-note-graph)
;;; my-org-note-graph.el ends here
