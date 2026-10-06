;;; my-org-note-db-actions.el --- Embark actions on notes -*- lexical-binding: t; -*-
;;; Code:
(require 'org-note-db-writing)
(require 'org-note-db-consult)
(require 'embark)
(require 'embark-org)

(defun my/org-note-db-action-nodes (keys)
  "Resolve KEYS to distinct owning notes, failing if a note disappeared."
  (let ((db (org-note-db-database t))
        (owners (mapcar (lambda (key) (car (split-string key "::"))) keys)))
    (mapcar (lambda (key)
              (or (org-note-db-node db key) (user-error "Missing note: %s" key)))
            (seq-uniq owners #'equal))))

(defun my/org-note-db-map-notes (function keys)
  "Call FUNCTION with each distinct note in KEYS, at its start in its buffer.
Heading notes are narrowed to their subtree.  Restore point and narrowing;
leave edits unsaved and undoable.  Stop on error, retaining earlier edits."
  (mapcar (lambda (node)
            (with-current-buffer (find-file-noselect (org-note-db-node-file node))
              (save-excursion
                (save-restriction
                  (org-note-db-goto node)
                  (when (> (org-note-db-node-level node) 0) (org-narrow-to-subtree))
                  (let ((mark-active nil)) (funcall function node))))))
          (my/org-note-db-action-nodes keys)))

(defun my/org-note-db-tag-notes (keys)
  "Add one graph tag to KEYS, prompting once and leaving edits unsaved."
  (let ((tag (completing-read
              "Add tag: " (seq-uniq (mapcar #'car (org-note-db-data-query
                                                  (org-note-db-database t) "Tags")) #'equal))))
    (unless (org-string-nw-p tag) (user-error "Enter a tag"))
    (my/org-note-db-map-notes
     (lambda (_node)
       (barf-if-buffer-read-only)
       (let ((tags (split-string-and-unquote (or (org-entry-get nil "GRAPH_TAGS") ""))))
         (unless (member tag tags)
           (org-note-db-set-properties
            (list (cons "GRAPH_TAGS" (combine-and-quote-strings (append tags (list tag)))))))))
     keys)
    (message "Added %s; save the changed note buffers when ready" tag)))

(defun my/org-note-db-command-on-notes (keys)
  "Run an Emacs command or named keyboard macro in each note in KEYS.
Choose the command once; its own prompts occur for each note."
  (let ((command (read-command "Command on notes: "))
        (prefix current-prefix-arg))
    (save-window-excursion
      (my/org-note-db-map-notes
       (lambda (_node)
         (switch-to-buffer (current-buffer))
         (let ((this-command command) (current-prefix-arg prefix) (prefix-arg prefix))
           (command-execute command)))
       keys))))

(defun my/org-note-db-action-open (key)
  "Follow KEY, including its search suffix, in the configured note window."
  (let ((node (car (my/org-note-db-action-nodes (list key)))))
    (funcall org-note-db-open-function (find-file-noselect (org-note-db-node-file node)))
    (org-link-open-from-string (org-link-make-string key))))

(defun my/org-note-db-action-key (location)
  "Return LOCATION's note key and search suffix, displaying its title."
  (let ((key (cond ((org-note-db-node-p location) (org-note-db-node-key location))
                   ((org-note-db-location-id location) (concat "id:" (org-note-db-location-id location)))
                   (t (when-let* ((node (org-note-db-node-at (org-note-db-database) location)))
                        (org-note-db-node-key node))))))
    (unless key (user-error "Location has no owning note"))
    (propertize (concat key (when-let* ((search (org-note-db-location-search location)))
                             (concat "::" search)))
                'display (org-note-db-location-title location))))

(defun my/org-note-db-read (candidates prompt allow-new)
  "Read CANDIDATES with Consult previews and persistent Embark note identities."
  (org-note-db-consult-read
   (mapcar (pcase-lambda (`(,label . ,location))
             (cons (propertize label 'multi-category
                               (cons 'my-org-note-db-note (my/org-note-db-action-key location)))
                   location)) candidates)
   prompt allow-new))

(defun my/org-note-db-action-target ()
  "Expose the query result at point as a note location for Embark."
  (when-let* ((location (get-text-property (point) 'org-note-db-location)))
    `(my-org-note-db-note ,(my/org-note-db-action-key location)
                         ,(line-beginning-position) . ,(line-end-position))))

(defun my/org-note-db-action-link (type target)
  "Refine Org ID links to note targets, retaining native handling for other links."
  (pcase-let ((`(,kind . ,link) (embark-org--refine-link-type type target)))
    (if (and (eq kind 'org-link) (string-prefix-p "id:" link))
        (cons 'my-org-note-db-note link)
      (cons kind link))))

(defun my/org-note-db-action-candidates ()
  "Collect query results in the active region, or the whole view."
  (when (derived-mode-p 'org-note-db-view-mode)
    (cons 'my-org-note-db-note
          (save-restriction
            (when (use-region-p) (narrow-to-region (region-beginning) (region-end)))
            (save-excursion
              (goto-char (point-min))
              (let (targets)
                (while (not (eobp))
                  (when-let* ((target (my/org-note-db-action-target)))
                    (push (cdr target) targets))
                  (forward-line 1))
                (nreverse targets)))))))

(defun my/org-note-db-link-candidates ()
  "Collect ID links in the active region or nearest Org list, including sublists.
Explicit Embark selections take precedence.  Only complete links participate."
  (when (derived-mode-p 'org-mode)
    (or (embark-selected-candidates)
        (when-let* ((bounds (if (use-region-p) (cons (region-beginning) (region-end))
                             (when-let* ((list (org-element-lineage (org-element-context) '(plain-list) t)))
                               (cons (org-element-property :begin list) (org-element-property :end list))))))
          (cons 'org-link
                (org-element-map (org-element-parse-buffer) 'link
                  (lambda (link)
                    (let ((begin (org-element-property :begin link))
                          (end (- (org-element-property :end link) (org-element-property :post-blank link))))
                      (when (and (equal (org-element-property :type link) "id")
                                 (<= (car bounds) begin) (<= end (cdr bounds)))
                        `(,(buffer-substring-no-properties begin end) ,begin . ,end))))))))))

(defun my/org-note-db-actions-clear-selection (&rest _)
  "Clear Embark marks when a query view is rewritten."
  (dolist (item embark--selection)
    (when (overlayp (cdr item)) (delete-overlay (cdr item))))
  (setq embark--selection nil))

(defun my/org-note-db-actions-setup ()
  "Connect this query view to Embark's selection and action interface."
  (add-hook 'embark-target-finders #'my/org-note-db-action-target nil t)
  (setq-local embark-candidate-collectors
              '(embark-selected-candidates my/org-note-db-action-candidates))
  (add-hook 'before-change-functions #'my/org-note-db-actions-clear-selection nil t)
  (keymap-local-set "m" #'embark-select)
  (keymap-local-set "a" #'embark-act)
  (keymap-local-set "A" #'embark-act-all))

(defvar-keymap my/org-note-db-action-map
  :parent embark-general-map
  "RET" #'my/org-note-db-action-open
  "t" #'my/org-note-db-tag-notes
  "x" #'my/org-note-db-command-on-notes)

(add-to-list 'embark-keymap-alist '(my-org-note-db-note . my/org-note-db-action-map))
(add-to-list 'embark-transformer-alist '(org-note-db-node . embark--refine-multi-category))
(add-to-list 'embark-transformer-alist '(org-link . my/org-note-db-action-link))
(add-to-list 'embark-multitarget-actions #'my/org-note-db-tag-notes)
(add-to-list 'embark-multitarget-actions #'my/org-note-db-command-on-notes)
(add-hook 'embark-candidate-collectors #'my/org-note-db-link-candidates)
(add-hook 'org-note-db-view-mode-hook #'my/org-note-db-actions-setup)

(provide 'my-org-note-db-actions)
;;; my-org-note-db-actions.el ends here
