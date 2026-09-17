;;; my-treesit.el --- Personal Tree-sitter tools -*- lexical-binding: t; -*-

;;; Commentary:
;; Shared home for tools using Emacs's built-in treesit API.
;; Use M-x treesit-explore-mode in a source buffer to inspect its syntax tree.
;; Evaluate a defun with C-M-x, then try it in that buffer; C-h f treesit-node-at
;; and treesit-query-capture are useful starting points for new tools.
;; Global entry bindings live in config.org; selection keys are local to this file.

;;; Code:

(require 'treesit)

(require 'cl-lib)
(autoload 'er/expand-region "expand-region" nil t)
(autoload 'er/contract-region "expand-region" nil t)
(declare-function mc/create-fake-cursor-at-point "multiple-cursors-core" (&optional id))
(declare-function mc/remove-fake-cursors "multiple-cursors-core" ())
(declare-function multiple-cursors-mode "multiple-cursors-core" (&optional arg))
(declare-function mc/all-fake-cursors "multiple-cursors-core" (&optional start end))
(declare-function mc/remove-fake-cursor "multiple-cursors-core" (cursor))
(defvar mc/max-cursors)
(defvar mc--default-cmds-to-run-once)

(defconst my/treesit--commands
  '(my/treesit-resume-selection my/treesit-expand my/treesit-contract
    my/treesit-previous my/treesit-next my/treesit-child
    my/treesit-transpose-backward my/treesit-transpose-forward
    my/treesit-edit-siblings my/treesit-filter-children)
  "Commands operating once on the complete selection set.")

(defvar-local my/treesit--history nil
  "Previous selections, valid only for the recorded buffer state.")
(defvar-local my/treesit--selection-state nil)

(defvar-keymap my/treesit-filter-map
  "t" #'my/treesit-filter-children
  "s" #'my/treesit-edit-siblings)

(defvar-keymap my/treesit-action-map
  "s" #'my/treesit-resume-selection
  "f" (cons "Filter" my/treesit-filter-map))

(defvar-keymap my/treesit-selection-mode-map
  "C-=" #'my/treesit-expand
  "C-M-=" #'my/treesit-contract
  "M-p" #'my/treesit-previous
  "M-n" #'my/treesit-next
  "M-u" #'my/treesit-expand
  "M-d" #'my/treesit-child
  "M-P" #'my/treesit-transpose-backward
  "M-N" #'my/treesit-transpose-forward)

(defun my/treesit--selection-pre-command ()
  "Leave structural selection before ordinary commands execute."
  (unless (memq this-command my/treesit--commands)
    (my/treesit-selection-mode -1)))

(defun my/treesit--selection-check ()
  "Leave structural selection when its region is no longer active."
  (unless (region-active-p) (my/treesit-selection-mode -1)))

(define-minor-mode my/treesit-selection-mode
  "Temporary navigation keys for one or more active syntax regions."
  :lighter " TS-select"
  (if my/treesit-selection-mode
      (progn
        (add-hook 'pre-command-hook #'my/treesit--selection-pre-command nil t)
        (add-hook 'post-command-hook #'my/treesit--selection-check nil t)
        (add-hook 'deactivate-mark-hook #'my/treesit--selection-check nil t))
    (remove-hook 'pre-command-hook #'my/treesit--selection-pre-command t)
    (remove-hook 'post-command-hook #'my/treesit--selection-check t)
    (remove-hook 'deactivate-mark-hook #'my/treesit--selection-check t)
    (setq my/treesit--history nil my/treesit--selection-state nil)))

(defun my/treesit-resume-selection ()
  "Resume structural navigation across the current regions or cursor nodes."
  (interactive)
  (unless (treesit-parser-list)
    (user-error "This buffer has no Tree-sitter parser"))
  (if (bound-and-true-p multiple-cursors-mode)
      (my/treesit--edit-ranges
       (my/treesit--map-selections
        (lambda () (if (use-region-p)
                       (cons (region-beginning) (region-end))
                     (my/treesit--bounds (my/treesit--node))))))
    (unless (use-region-p) (my/treesit--select (my/treesit--node))))
  (setq deactivate-mark nil)
  (my/treesit-selection-mode 1)
  (message "Structural selection: C-M-= back · M-p/n siblings · M-u/d parent/child · C-c s filters"))

(defun my/enable-treesit-extras ()
  "Enable buffer-local structural indentation."
  (setq-local tab-width 4 tab-always-indent nil
              indent-region-function #'treesit-indent-region))

(defun my/treesit--selections ()
  "Snapshot point, mark and activation for the real cursor, then fake cursors."
  (cons (list (point) (mark t) (and (region-active-p) t))
        (when (bound-and-true-p multiple-cursors-mode)
          (mapcar (lambda (cursor)
                    (list (marker-position (overlay-get cursor 'point))
                          (marker-position (overlay-get cursor 'mark))
                          (and (overlay-get cursor 'mark-active) t)))
                  (sort (save-restriction (widen) (mc/all-fake-cursors))
                        (lambda (a b) (< (overlay-start a) (overlay-start b))))))))

(defun my/treesit--state ()
  "Return the buffer revision and complete selection set."
  (list (buffer-chars-modified-tick) (my/treesit--selections)))

(defun my/treesit--restore-point (selection)
  "Restore one SELECTION without changing the other cursors."
  (goto-char (nth 0 selection))
  (set-marker (mark-marker) (nth 1 selection))
  (setq mark-active (nth 2 selection) deactivate-mark nil))

(defun my/treesit--map-selections (function)
  "Call FUNCTION in each selection, collecting results without changing cursors."
  (let ((selections (my/treesit--selections)) (deactivate-mark-hook nil))
    (save-mark-and-excursion
      (mapcar (lambda (selection)
                (unless (and (<= (point-min) (car selection) (point-max))
                             (or (not (nth 2 selection))
                                 (and (nth 1 selection)
                                      (<= (point-min) (nth 1 selection) (point-max)))))
                  (user-error "A cursor selection is outside the accessible buffer"))
                (my/treesit--restore-point selection)
                (funcall function))
              selections))))

(defun my/treesit--bounds (node)
  "Return accessible NODE bounds, or fail before changing the selection set."
  (unless (and node (<= (point-min) (treesit-node-start node))
               (<= (treesit-node-end node) (point-max)))
    (user-error "No accessible node in that direction"))
  (cons (treesit-node-start node) (treesit-node-end node)))

(defun my/treesit--node ()
  "Find the named node containing the region or point in the primary parser."
  (let* ((parser (or (and (boundp 'treesit-primary-parser) treesit-primary-parser)
                     (car (treesit-parser-list))))
         (language (and parser (treesit-parser-language parser))))
    (unless parser (user-error "This buffer has no Tree-sitter parser"))
    (let ((node (if (use-region-p)
                    (treesit-node-on (region-beginning) (region-end) language t)
                  (treesit-node-at (point) language))))
      (while (and node (not (treesit-node-check node 'named)))
        (setq node (treesit-node-parent node)))
      (my/treesit--bounds node)
      node)))

(defun my/treesit--select (node &optional remember)
  "Select NODE, optionally remembering the previous complete selection set."
  (my/treesit--edit-ranges (list (my/treesit--bounds node)) (not remember)))

(defun my/treesit-expand ()
  "Expand each selection through enclosing nodes, merging shared parents."
  (interactive)
  (if (not (treesit-parser-list))
      (call-interactively #'er/expand-region)
    (my/treesit--edit-ranges
     (my/treesit--map-selections
      (lambda ()
        (let ((node (my/treesit--node)))
          (while (and node (use-region-p)
                      (= (treesit-node-start node) (region-beginning))
                      (= (treesit-node-end node) (region-end)))
            (setq node (treesit-node-parent node)))
          (my/treesit--bounds node)))))
    (when (called-interactively-p 'any)
      (my/treesit-selection-mode 1)
      (message "Syntax: C-M-= back · M-p/n siblings · M-u/d parent/child · C-c s filters · C-g exit"))))

(defun my/treesit-contract ()
  "Restore the previous selection set, or select each node's first child."
  (interactive)
  (if (not (treesit-parser-list))
      (call-interactively #'er/contract-region)
    (if (and my/treesit--history
             (equal my/treesit--selection-state (my/treesit--state)))
        (let ((previous (car my/treesit--history)))
          (my/treesit--install-selections previous)
          (setq my/treesit--history (cdr my/treesit--history)
                my/treesit--selection-state (my/treesit--state)))
      (my/treesit-child))))

(defun my/treesit-child ()
  "Select each node's first named child with different bounds."
  (interactive)
  (my/treesit--edit-ranges
   (my/treesit--map-selections
    (lambda ()
      (let* ((node (my/treesit--node))
             (child (treesit-node-child node 0 t)))
        (while (and child
                    (= (treesit-node-start child) (treesit-node-start node))
                    (= (treesit-node-end child) (treesit-node-end node)))
          (setq child (treesit-node-child child 0 t)))
        (my/treesit--bounds child))))))

(defun my/treesit-next ()
  "Select the next named sibling at every cursor."
  (interactive)
  (my/treesit--edit-ranges
   (my/treesit--map-selections
    (lambda () (my/treesit--bounds (treesit-node-next-sibling (my/treesit--node) t))))))

(defun my/treesit-previous ()
  "Select the previous named sibling at every cursor."
  (interactive)
  (my/treesit--edit-ranges
   (my/treesit--map-selections
    (lambda () (my/treesit--bounds (treesit-node-prev-sibling (my/treesit--node) t))))))

(defun my/treesit-transpose (direction)
  "Run native transposition in DIRECTION across independent selected nodes."
  (let* ((original (my/treesit--selections))
         (cursors (when (bound-and-true-p multiple-cursors-mode)
                    (sort (mc/all-fake-cursors)
                          (lambda (a b) (< (overlay-start a) (overlay-start b))))))
         (plans
          (my/treesit--map-selections
           (lambda ()
             (let* ((bounds (my/treesit--bounds (my/treesit--node)))
                    (end (cdr bounds)))
               (goto-char end)
               ;; Use the same native range calculations as `transpose-sexps'.
               (let ((first (treesit-transpose-sexps -1)))
                 (unless (equal first bounds)
                   (user-error "Native transpose does not match the selected node"))
                 (when (< direction 0) (goto-char (car first)))
                 (let* ((other (treesit-transpose-sexps direction))
                        (beg2 (min (car other) (cdr other)))
                        (end2 (max (car other) (cdr other))))
                   (when (< (max (car bounds) beg2) (min end end2))
                     (user-error "Native transpose ranges overlap"))
                   (list end (- end (car bounds))
                         (cons (min (car bounds) beg2) (max end end2)))))))))
         (spans (sort (mapcar (lambda (plan) (nth 2 plan)) plans)
                      (lambda (a b) (< (car a) (car b)))))
         last-end ranges)
    (dolist (span spans)
      (when (and last-end (< (car span) last-end))
        (user-error "Structural edits overlap; select fewer nodes"))
      (setq last-end (cdr span)))
    (condition-case err
        (atomic-change-group
          (dolist (plan plans)
            (goto-char (car plan))
            (let ((transpose-sexps-function #'treesit-transpose-sexps))
              (transpose-sexps direction))
            (push (cons (- (point) (cadr plan)) (point)) ranges))
          (my/treesit--edit-ranges (nreverse ranges) t))
      ((error quit)
       ;; Text rollback can collapse markers at deleted boundaries.  Restore
       ;; surviving cursor overlays without allocating another set of cursors.
       (cl-mapc
        (lambda (cursor selection)
          (set-marker (overlay-get cursor 'point) (nth 0 selection))
          (set-marker (overlay-get cursor 'mark) (nth 1 selection))
          (overlay-put cursor 'mark-active (nth 2 selection))
          (move-overlay cursor (car selection) (min (point-max) (1+ (car selection))))
          (when-let* ((region (overlay-get cursor 'region-overlay)))
            (move-overlay region (min (car selection) (cadr selection))
                          (max (car selection) (cadr selection)))))
        cursors (cdr original))
       (my/treesit--restore-point (car original))
       (signal (car err) (cdr err))))))

(defun my/treesit-transpose-forward ()
  "Run native forward transposition, keeping each moved node selected."
  (interactive)
  (condition-case err (my/treesit-transpose 1)
    (error (user-error "%s" (error-message-string err)))))

(defun my/treesit-transpose-backward ()
  "Run native backward transposition, keeping each moved node selected."
  (interactive)
  (condition-case err (my/treesit-transpose -1)
    (error (user-error "%s" (error-message-string err)))))

(defun my/treesit-edit-siblings ()
  "Select same-type siblings at every cursor, merging duplicate results."
  (interactive)
  (my/treesit--edit-ranges
   (apply #'append
          (my/treesit--map-selections
           (lambda ()
             (let* ((node (my/treesit--node))
                    (parent (treesit-node-parent node))
                    (type (treesit-node-type node))
                    (sibling (and parent (treesit-node-child parent 0 t))) ranges)
               (while sibling
                 (when (and (equal type (treesit-node-type sibling))
                            (<= (point-min) (treesit-node-start sibling))
                            (<= (treesit-node-end sibling) (point-max)))
                   (push (my/treesit--bounds sibling) ranges))
                 (setq sibling (treesit-node-next-sibling sibling t)))
               (nreverse ranges))))))
  (when (called-interactively-p 'any) (my/treesit-selection-mode 1)))

(defun my/treesit--normalize-ranges (ranges)
  "Sort RANGES, merge duplicates and keep outermost nested selections.
Reject partial overlaps instead of selecting text that was not requested."
  (let ((sorted (sort (copy-sequence ranges)
                      (lambda (a b) (if (= (car a) (car b)) (> (cdr a) (cdr b))
                                     (< (car a) (car b)))))) result)
    (dolist (range sorted)
      (unless (<= (point-min) (car range) (1- (cdr range)) (1- (point-max)))
        (user-error "Selection lies outside the accessible buffer"))
      (cond ((or (null result) (>= (car range) (cdar result))) (push range result))
            ((> (cdr range) (cdar result)) (user-error "Selections partially overlap"))))
    (nreverse result)))

(defun my/treesit--install-selections (selections)
  "Install SELECTIONS, staging new fake cursors before removing existing ones."
  (require 'multiple-cursors-core)
  (when (and mc/max-cursors (> (length selections) mc/max-cursors))
    (user-error "Selection exceeds mc/max-cursors (%d)" mc/max-cursors))
  (let ((original (car (my/treesit--selections)))
        (old (mc/all-fake-cursors))
        (deactivate-mark-hook nil)
        (mc/max-cursors nil)
        created)
    (condition-case err
        (progn
          (dolist (selection (cdr selections))
            (my/treesit--restore-point selection)
            (push (mc/create-fake-cursor-at-point) created))
          (my/treesit--restore-point (car selections)))
      ((error quit)
       (mapc #'mc/remove-fake-cursor created)
       (my/treesit--restore-point original)
       (signal (car err) (cdr err))))
    (mapc #'mc/remove-fake-cursor old)
    (if (cdr selections)
        (unless (bound-and-true-p multiple-cursors-mode) (multiple-cursors-mode 1))
      (when (bound-and-true-p multiple-cursors-mode) (multiple-cursors-mode -1)))
    (setq deactivate-mark nil)))

(defun my/treesit--edit-ranges (ranges &optional forget)
  "Install normalized RANGES and remember the previous selection set.
FORGET starts a new history, for example after a text transformation."
  (let* ((normalized (my/treesit--normalize-ranges ranges))
         (original (my/treesit--selections))
         (history (and (not forget)
                       (equal my/treesit--selection-state (my/treesit--state))
                       my/treesit--history))
         ;; Keep the real cursor near its previous scope when selections merge.
         (anchor (if (use-region-p) (region-beginning) (point)))
         (primary (or (cl-find-if (lambda (range)
                                   (and (<= (car range) anchor) (< anchor (cdr range)))) normalized)
                      (cl-find-if (lambda (range)
                                    (and (<= (car range) (caar ranges))
                                         (< (caar ranges) (cdr range)))) normalized)
                      (car normalized))))
    (unless normalized (user-error "No matching nodes in the selection set"))
    (setq normalized (cons primary (remove primary normalized)))
    (my/treesit--install-selections
     (mapcar (lambda (range) (list (cdr range) (car range) t)) normalized))
    (setq my/treesit--history (unless forget (cons original history))
          my/treesit--selection-state (my/treesit--state))))

(defun my/treesit--descendants (root beg end)
  "Return named descendants of ROOT fully inside BEG and END, in source order."
  (let ((pending (list root)) nodes)
    (while pending
      (let ((node (pop pending)))
        (when (and (< (treesit-node-start node) end)
                   (> (treesit-node-end node) beg))
          (when (and (not (eq node root))
                     (<= beg (treesit-node-start node))
                     (<= (treesit-node-end node) end))
            (push node nodes))
          (let ((index (treesit-node-child-count node t)))
            (while (> index 0)
              (push (treesit-node-child node (cl-decf index) t) pending))))))
    (nreverse nodes)))

(defun my/treesit-filter-children (&optional type)
  "Filter all current selections to descendants of TYPE.
Prompt once for a type present in any scope; scopes with no matches disappear.
Nested matches retain the outermost node.
No matches leave selections unchanged."
  (interactive)
  (let* ((state (my/treesit--state))
         (nodes (apply #'append
                       (my/treesit--map-selections
                        (lambda ()
                          (unless (use-region-p)
                            (user-error "Every cursor needs an active region to filter"))
                          (my/treesit--descendants
                           (my/treesit--node) (region-beginning) (region-end))))))
         (types (sort (delete-dups (mapcar #'treesit-node-type nodes)) #'string-lessp)))
    (unless types (user-error "No named child nodes inside the selections"))
    (setq type (or type (completing-read "Select descendants of type: " types nil t)))
    (unless (equal state (my/treesit--state))
      (user-error "Buffer or selections changed while choosing a filter; try again"))
    (let ((ranges (cl-loop for node in nodes
                           when (equal type (treesit-node-type node))
                           collect (my/treesit--bounds node))))
      (my/treesit--edit-ranges ranges)
      (when (called-interactively-p 'any) (my/treesit-selection-mode 1))
      (message "Selected %d %s node(s)" (length (my/treesit--selections)) type))))

(with-eval-after-load 'multiple-cursors-core
  (dolist (command my/treesit--commands)
    (add-to-list 'mc--default-cmds-to-run-once command)))

(defun my/create-src-file ()
  "Create a new implementation buffer for the current C or C++ header."
  (interactive)
  (unless buffer-file-name
    (user-error "This buffer does not visit a file"))
  (let* ((file buffer-file-name)
         (ext (file-name-extension file))
         (new-ext (pcase ext ("h" "c") ("hpp" "cpp")
                         (_ (user-error "Expected a .h or .hpp header"))))
         (target (file-name-with-extension file new-ext))
         namespaces)
    (when (or (file-exists-p target) (get-file-buffer target))
      (user-error "Implementation file or buffer already exists: %s" target))
    ;; Inspect the header before switching buffers.  Plain C needs no parser.
    (when (equal ext "hpp")
      (unless (treesit-parser-list)
        (user-error "C++ namespace detection requires a Tree-sitter parser"))
      (let ((node (treesit-node-at (point))))
        (while node
          (when (equal (treesit-node-type node) "namespace_definition")
            (when-let* ((name (treesit-node-child-by-field-name node "name")))
              (push (treesit-node-text name t) namespaces)))
          (setq node (treesit-node-parent node)))))
    (let* ((namespace (string-join namespaces "::"))
           (contents (concat "#include \"" (file-name-nondirectory file) "\"\n\n"
                             (unless (string-empty-p namespace)
                               (format "namespace %s {\n} // %s" namespace namespace)))))
      (find-file-other-window target)
      (atomic-change-group (insert contents)))))

(provide 'my-treesit)
;;; my-treesit.el ends here
