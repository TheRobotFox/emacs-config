;;; org-note-graph-ui.el --- Query selection and Org result views -*- lexical-binding: t; -*-

;;; Code:
(require 'org-note-graph-query)

(defcustom org-note-graph-display-function #'pop-to-buffer
  "Function displaying a query result buffer."
  :type 'function :group 'org-note-graph)
(defcustom org-note-graph-open-function #'pop-to-buffer-same-window
  "Function displaying a selected note buffer."
  :type 'function :group 'org-note-graph)
(defcustom org-note-graph-read-function #'org-note-graph--completing-read
  "Selector accepting CANDIDATES, PROMPT and ALLOW-NEW.
Candidates are (LABEL . NODE) pairs; return a node or a new title."
  :type 'function :group 'org-note-graph)

(defvar-local org-note-graph--view-query nil)
(defvar-local org-note-graph--view-context nil)
(defvar-local org-note-graph--view-title nil)

(defun org-note-graph--goto (node)
  "Select NODE's buffer and locate it by identity."
  (set-buffer (find-file-noselect (org-note-graph-node-file node)))
  (widen)
  (goto-char (if-let* ((id (org-note-graph-node-id node)))
                 (or (org-find-property "ID" id)
                     (user-error "Node disappeared: %s" id))
               (point-min))))

(defun org-note-graph--link (node &optional source-file)
  "Format an Org link to NODE, relative to SOURCE-FILE when possible."
  (org-link-make-string
   (if-let* ((id (org-note-graph-node-id node))) (concat "id:" id)
     (concat "file:" (org-link-escape
                      (if source-file
                          (file-relative-name (org-note-graph-node-file node)
                                              (file-name-directory source-file))
                        (org-note-graph-node-file node)))))
   (org-note-graph-node-title node)))

(defun org-note-graph-current-node (&optional db)
  "Return the enclosing indexed node or result context, without registering it."
  (setq db (or db (org-note-graph-database)))
  (if org-note-graph--view-context
      (org-note-graph-node db org-note-graph--view-context)
    (when (and (derived-mode-p 'org-mode) (buffer-file-name (buffer-base-buffer)))
      (let ((file (org-note-graph--canonical-file (buffer-file-name (buffer-base-buffer)))))
        (org-note-graph--owner
         (seq-filter (lambda (node) (equal file (org-note-graph-node-file node)))
                     (org-note-graph-nodes db))
         (point))))))

(defun org-note-graph--unique-candidates (candidates)
  "Give CANDIDATES distinct labels, preserving their associated nodes."
  (let ((names (mapcar #'car candidates)) used)
    (mapcar (lambda (candidate)
              (let ((label (car candidate)) (count 1))
                (while (or (member label used)
                           (and (> count 1) (member label names)))
                  (setq label (format "%s <%d>" (car candidate) (cl-incf count))))
                (push label used)
                (cons label (cdr candidate))))
            candidates)))

(defun org-note-graph--annotation (node)
  "Return NODE's file annotation."
  (concat "  " (propertize (file-relative-name (org-note-graph-node-file node)) 'face 'shadow)))

(defun org-note-graph--completing-read (candidates prompt allow-new)
  "Select from CANDIDATES with PROMPT; ALLOW-NEW permits a title string."
  (let* ((completion-extra-properties
          (list :annotation-function
                (lambda (label)
                  (when-let* ((node (cdr (assoc label candidates))))
                    (org-note-graph--annotation node)))))
         (choice (completing-read prompt candidates nil (not allow-new))))
    (or (cdr (assoc choice candidates)) choice)))

(defun org-note-graph-read (db keys &optional prompt allow-new extra-candidates)
  "Select from KEYS in DB, using PROMPT and optionally ALLOW-NEW.
EXTRA-CANDIDATES supplies additional (LABEL . NODE) pairs."
  (let* ((candidates
          (org-note-graph--unique-candidates
           (delete-dups
            (append (mapcar (lambda (key)
                              (let ((node (or (org-note-graph-node db key)
                                              (user-error "Missing node: %s" key))))
                                (cons (org-note-graph-node-title node) node)))
                            keys)
                    extra-candidates))))
         (_ (unless (or candidates allow-new) (user-error "No matching nodes")))
         (choice (funcall org-note-graph-read-function candidates (or prompt "Node: ") allow-new)))
    (when (and (stringp choice) (string-empty-p (string-trim choice)))
      (user-error "A title is required"))
    choice))

(defun org-note-graph-open (node)
  "Visit NODE and reveal its Org context."
  (org-note-graph--goto node)
  (funcall org-note-graph-open-function (current-buffer))
  (org-fold-show-context 'link-search))

(defvar-keymap org-note-graph-view-mode-map
  :parent org-mode-map
  "g" #'org-note-graph-view-refresh
  "q" #'quit-window
  "RET" #'org-note-graph-view-open)

(define-derived-mode org-note-graph-view-mode org-mode "Note Graph"
  "Read-only query results; g refreshes and q closes the view."
  (setq buffer-read-only t))

(defun org-note-graph-view-open ()
  "Open the Org link at point through the configured note opener."
  (interactive)
  (let* ((link (org-element-context))
         (key (when (eq (org-element-type link) 'link)
                (concat (org-element-property :type link) ":"
                        (org-element-property :path link))))
         (db (org-note-graph-database))
         (resolved (and key (org-note-graph-resolve db key)))
         (node (and resolved (org-note-graph-node db resolved))))
    (if node (org-note-graph-open node) (org-open-at-point))))

(defun org-note-graph-view-refresh ()
  "Refresh this view using its query function and context."
  (interactive)
  (unless (derived-mode-p 'org-note-graph-view-mode) (user-error "Not a graph view"))
  (let* ((db (org-note-graph-refresh))
         (keys (funcall org-note-graph--view-query db org-note-graph--view-context))
         (text (concat "#+title: " org-note-graph--view-title "\n\n"
                       (mapconcat (lambda (key)
                                    (concat " * " (org-note-graph--link
                                                   (org-note-graph-node db key))))
                                  keys "\n") "\n"))
         (position (point))
         (inhibit-read-only t))
    (erase-buffer)
    (insert text)
    (goto-char (min position (point-max)))
    (set-buffer-modified-p nil)))

(defun org-note-graph-view (query &optional context title)
  "Display QUERY's node keys in a refreshable Org buffer.
QUERY receives a database and CONTEXT node key.  TITLE names the view."
  (let ((buffer (get-buffer-create (format "*Note Graph: %s*" (or title "Results")))))
    (with-current-buffer buffer
      (org-note-graph-view-mode)
      (setq org-note-graph--view-query query
            org-note-graph--view-context context
            org-note-graph--view-title (or title "Results"))
      (org-note-graph-view-refresh))
    (funcall org-note-graph-display-function buffer)
    buffer))

(provide 'org-note-graph-ui)
;;; org-note-graph-ui.el ends here
