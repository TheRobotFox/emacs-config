;;; my-window-workflow.el --- Predictable navigation and supporting panes -*- lexical-binding: t; -*-

;;; Commentary:
;; Popper controls visibility; one display action gives supporting panes slots.
;; Documentation and processes coexist.  Direct commands retain source context.

;;; Code:
(require 'cl-lib)
(require 'project)
(require 'eldoc)

(defgroup my/window-workflow nil "Supporting windows." :group 'windows)
(defcustom my/popup-right-min-width 140
  "Minimum frame width in columns for a supporting pane on the right."
  :type 'integer)
(defcustom my/popup-width 0.35
  "Fraction of frame width used by the right supporting panes."
  :type 'float)
(defcustom my/popup-height 0.30
  "Fraction of frame height used by bottom supporting panes."
  :type 'float)
(defvar-local my/popup-project-root nil)
;; Eldoc refreshes its buffer with `special-mode', which resets local variables.
(put 'my/popup-project-root 'permanent-local t)
(defvar eshell-buffer-name)
(defvar my/popup--side-override nil
  "Side forced temporarily while moving supporting panes.")

(defun my/popup-documentation-p (buffer)
  "Whether BUFFER contains documentation rather than interactive output."
  (with-current-buffer buffer
    (or (derived-mode-p 'help-mode)
        (string-match-p "\\` *\\*eldoc\\*" (buffer-name)))))

(defun my/popup-process-p (buffer)
  "Whether BUFFER is a shell or language interaction buffer."
  (with-current-buffer buffer
    (derived-mode-p 'comint-mode 'eshell-mode 'vterm-mode 'term-mode
                    'sly-mrepl-mode 'haskell-interactive-mode)))

(defun my/popup-reference-p (buffer)
  "Classify supporting buffers, including modes derived from comint or help."
  (or (my/popup-documentation-p buffer) (my/popup-process-p buffer)))

(defun my/popup-source-window ()
  "Find the editing window associated with the selected supporting pane."
  (let* ((window (selected-window))
         (origin (window-parameter window 'my/popup-origin)))
    (cond ((not (window-parameter window 'window-side)) window)
          ((and (window-live-p origin)
                (eq (window-frame origin) (selected-frame))
                (not (window-parameter origin 'window-side))) origin)
          (t (or (cl-find-if
                  (lambda (win) (not (window-parameter win 'window-side)))
                  (window-list nil 'no-mini))
                 (user-error "No editing window available"))))))

(defun my/popup-side ()
  "Choose a stable side based on the existing supporting area or editing layout."
  (let* ((windows (window-list nil 'no-mini))
         (support (cl-find-if (lambda (win) (window-parameter win 'my/popup-pane)) windows))
         (editors (cl-remove-if (lambda (win) (window-parameter win 'window-side)) windows))
         (columns (delete-dups (mapcar (lambda (win) (car (window-edges win))) editors))))
    (or my/popup--side-override
        (and support (window-parameter support 'window-side))
        (if (and (= (length columns) 1)
                 (>= (frame-width) my/popup-right-min-width)) 'right 'bottom))))

(defun my/popup-display (buffer &optional alist)
  "Display BUFFER in its supporting slot without changing keyboard focus.
Documentation uses slot -1; processes and other output share slot 0.
Reuse a visible buffer and preserve placements while supporting panes are open."
  (let ((root (with-current-buffer (window-buffer (my/popup-source-window))
                (my/popup-root))))
    (with-current-buffer buffer
      (cond ((my/popup-documentation-p buffer)
             (setq-local my/popup-project-root root))
            ((not my/popup-project-root)
             (setq-local my/popup-project-root
                         (if (my/popup-process-p buffer) (my/popup-root) root))))))
  (or (get-buffer-window buffer (selected-frame))
      (let* ((origin (my/popup-source-window))
             (side (my/popup-side))
             (window
              (display-buffer-in-side-window
               buffer
               (append `((side . ,side)
                         (slot . ,(if (my/popup-documentation-p buffer) -1 0))
                         (window-width . ,my/popup-width)
                         (window-height . ,my/popup-height))
                       (cl-remove-if
                        (lambda (entry) (memq (car entry) '(side slot window-width window-height)))
                        alist)))))
        (when window
          (set-window-parameter window 'my/popup-pane t)
          (set-window-parameter window 'my/popup-origin origin))
        window)))

(defun my/popup-select (buffer)
  "Display and select BUFFER's supporting pane."
  (select-window (or (my/popup-display buffer)
                     (user-error "No room for a supporting pane"))))

(defun my/popup-toggle-side ()
  "Move visible supporting panes between right and bottom.
Keep their buffers, view positions, source context and keyboard focus.
Restore the previous layout if the requested side cannot accommodate them."
  (interactive)
  (let* ((panes (cl-remove-if-not
                 (lambda (win) (window-parameter win 'my/popup-pane))
                 (window-list nil 'no-mini)))
         (side (if (eq (my/popup-side) 'right) 'bottom 'right)))
    (unless panes (user-error "No supporting panes are open"))
    (when (cl-some (lambda (win)
                     (and (eq (window-parameter win 'window-side) side)
                          (not (memq win panes))))
                   (window-list nil 'no-mini))
      (user-error "The %s side is occupied by another side window" side))
    (let* ((configuration (current-window-configuration))
           (focus (selected-window))
           (source (my/popup-source-window))
           (states (mapcar
                    (lambda (win)
                      (list (window-buffer win) (window-start win)
                            (window-point win) (window-hscroll win)
                            (window-parameter win 'my/popup-origin)
                            (eq win focus)))
                    panes))
           (my/popup--side-override side)
           completed)
      (unwind-protect
          (progn
            (select-window source)
            (mapc #'delete-window panes)
            (dolist (state states)
              (pcase-let ((`(,buffer ,start ,point ,hscroll ,origin ,focused) state))
                (let* ((root (buffer-local-value 'my/popup-project-root buffer))
                       (window (my/popup-display buffer)))
                  (with-current-buffer buffer (setq my/popup-project-root root))
                  (unless (and window (eq (window-parameter window 'window-side) side))
                    (user-error "No room for supporting panes on the %s" side))
                  (set-window-parameter window 'my/popup-origin origin)
                  (set-window-start window start t)
                  (set-window-point window point)
                  (set-window-hscroll window hscroll)
                  (when focused (setq focus window)))))
            (select-window focus)
            (setq completed t))
        (unless completed (set-window-configuration configuration))))))

(defun my/popup-root ()
  "Return the source project's root, falling back to its current directory."
  (let ((root (expand-file-name
               (or my/popup-project-root
                   (if (fboundp 'my/task-project-root)
                       (my/task-project-root)
                     (when-let* ((project (project-current nil))) (project-root project)))
                   default-directory))))
    (file-name-as-directory (if (file-remote-p root) root (file-truename root)))))

(defun my/popup-group ()
  "Use the same project/directory identity for source buffers and their popups."
  (my/popup-root))

(defun my/popup-eshell ()
  "Select this project's Eshell, creating it on first use.
Outside a project, use a shell associated with the source directory."
  (interactive)
  (let* ((source (my/popup-source-window))
         (root (with-current-buffer (window-buffer source) (my/popup-root)))
         (existing (cl-find-if
                    (lambda (buf)
                      (with-current-buffer buf
                        (and (derived-mode-p 'eshell-mode)
                             (equal my/popup-project-root root))))
                    (buffer-list))))
    (if existing (my/popup-select existing)
      (require 'eshell)
      (let* ((default-directory root)
             (eshell-buffer-name
              (format "*eshell:%s:%s*"
                      (file-name-nondirectory (directory-file-name root))
                      (substring (secure-hash 'sha1 root) 0 8)))
             (display-buffer-overriding-action '(my/popup-display))
             (buffer (eshell)))
        (with-current-buffer buffer (setq-local my/popup-project-root root))))))

(defun my/popup--associated-repl ()
  "Return the source mode's associated REPL when its package is available."
  (cond
   ((my/popup-process-p (current-buffer)) (current-buffer))
   ((and (derived-mode-p 'python-mode 'python-ts-mode)
         (fboundp 'python-shell-get-process))
    (when-let* ((process (python-shell-get-process))) (process-buffer process)))
   ((and (derived-mode-p 'lisp-mode)
         (fboundp 'sly-current-connection) (sly-current-connection))
    (require 'sly-mrepl)
    (sly-mrepl))
   ((and (derived-mode-p 'haskell-mode 'haskell-ts-mode)
         (fboundp 'haskell-session-maybe))
    ;; `haskell-session-interactive-buffer' can create and select a new REPL
    ;; even when the session has no process.  Navigation must only reuse one.
    (let* ((session (haskell-session-maybe))
           (state (and session (haskell-session-process session)))
           (process (and state (haskell-process-process state)))
           (buffer (and session (haskell-session-get session 'interactive-buffer))))
      (unless (and (processp process) (process-live-p process)
                   (buffer-live-p buffer))
        (user-error "No running GHCi session; start it with M-x haskell-process-load-file"))
      buffer))))

(defun my/popup-repl ()
  "Select the source's REPL, or choose an existing process in its project.
Start language runtimes with their normal package commands first."
  (interactive)
  (let* ((source (my/popup-source-window))
         (buffer
          (with-current-buffer (window-buffer source)
            (or (my/popup--associated-repl)
                (let* ((root (my/popup-root))
                       (candidates
                        (cl-remove-if-not
                         (lambda (buf)
                           (and (my/popup-process-p buf)
                                (with-current-buffer buf
                                  (and (not (derived-mode-p 'eshell-mode 'shell-mode 'term-mode 'vterm-mode))
                                       (equal root (my/popup-root))))))
                         (buffer-list))))
                  (cond ((null candidates)
                         (user-error "No project REPL; start one with your language's normal command"))
                        ((null (cdr candidates)) (car candidates))
                        (t (get-buffer (completing-read "Project REPL: "
                                                       (mapcar #'buffer-name candidates) nil t))))))))
         (root (with-current-buffer (window-buffer source) (my/popup-root))))
    (with-current-buffer buffer (setq-local my/popup-project-root root))
    (my/popup-select buffer)))

(defun my/popup-eldoc ()
  "Toggle the full Eldoc pane for the source window, without selecting it.
New documentation requests use Eldoc's normal asynchronous display pipeline."
  (interactive)
  (let* ((source (my/popup-source-window))
         (buffer (condition-case nil (eldoc-doc-buffer) (user-error nil)))
         (window (and buffer (get-buffer-window buffer (selected-frame)))))
    (if window (quit-window nil window)
      (with-selected-window source
        (if buffer
            (progn (my/popup-display buffer) (eldoc t))
          (eldoc t))))))

(provide 'my-window-workflow)
;;; my-window-workflow.el ends here
