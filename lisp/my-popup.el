;;; my-popup.el --- Supporting panes beside an editing window -*- lexical-binding: t; -*-

;;; Commentary:
;; Popper controls visibility; one display action gives supporting panes slots.
;; Documentation and processes coexist.  Direct commands retain source context.
;; Which buffers count as documentation or processes, how a REPL is found and
;; what a buffer's root directory is are configured through the variables below;
;; this file knows no particular language or package.

;;; Code:
(require 'cl-lib)
(require 'project)
(require 'eldoc)

(defgroup my/popup nil "Supporting windows." :group 'windows)
(defcustom my/popup-right-min-width 140
  "Minimum frame width in columns for a supporting pane on the right."
  :type 'integer)
(defcustom my/popup-width 0.35
  "Fraction of frame width used by the right supporting panes."
  :type 'float)
(defcustom my/popup-height 0.30
  "Fraction of frame height used by bottom supporting panes."
  :type 'float)
(defcustom my/popup-documentation-modes '(help-mode)
  "Major modes whose buffers are documentation panes."
  :type '(repeat symbol))
(defcustom my/popup-documentation-names '("\\` *\\*eldoc\\*")
  "Buffer name regexps of documentation panes, for buffers shown before their mode."
  :type '(repeat regexp))
(defcustom my/popup-process-modes '(comint-mode eshell-mode vterm-mode term-mode)
  "Major modes of shell and language interaction buffers."
  :type '(repeat symbol))
(defcustom my/popup-shell-modes '(eshell-mode shell-mode term-mode vterm-mode)
  "Process modes that are plain shells rather than language REPLs."
  :type '(repeat symbol))
(defcustom my/popup-repl-functions nil
  "Alist of (MODE . FUNCTION) locating a source buffer's REPL.
FUNCTION is called with no arguments in the source buffer and returns an
existing REPL buffer, or nil to fall back to process buffers sharing the
project.  It may signal a `user-error' explaining how to start the REPL."
  :type '(alist :key-type symbol :value-type function))
(defcustom my/popup-root-function #'my/popup-project-root
  "Function returning the current buffer's root directory, or nil."
  :type 'function)

(defvar-local my/popup-project-root nil)
;; Eldoc refreshes its buffer with `special-mode', which resets local variables.
(put 'my/popup-project-root 'permanent-local t)
(defvar eshell-buffer-name)

(defun my/popup-project-root ()
  "Return the current project's root directory, or nil."
  (when-let* ((project (project-current nil))) (project-root project)))

(defun my/popup-documentation-p (buffer)
  "Whether BUFFER contains documentation rather than interactive output."
  (with-current-buffer buffer
    (or (apply #'derived-mode-p my/popup-documentation-modes)
        (seq-some (lambda (regexp) (string-match-p regexp (buffer-name)))
                  my/popup-documentation-names))))

(defun my/popup-process-p (buffer)
  "Whether BUFFER is a shell or language interaction buffer."
  (with-current-buffer buffer (apply #'derived-mode-p my/popup-process-modes)))

(defun my/popup-reference-p (buffer)
  "Classify supporting buffers, including modes derived from the configured ones."
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

(defun my/popup--context ()
  "Return the editing window and its project root as (WINDOW . ROOT)."
  (let ((source (my/popup-source-window)))
    (cons source (with-current-buffer (window-buffer source) (my/popup-root)))))

(defun my/popup--choose-side (wide-p columns existing-side)
  "Choose a side from WIDE-P, editing COLUMNS, and EXISTING-SIDE."
  (or existing-side (if (and wide-p (= columns 1)) 'right 'bottom)))

(defun my/popup-side ()
  "Choose a stable side based on the existing supporting area or editing layout."
  (let* ((windows (window-list nil 'no-mini))
         (support (cl-find-if (lambda (win) (window-parameter win 'my/popup-pane)) windows))
         (editors (cl-remove-if (lambda (win) (window-parameter win 'window-side)) windows))
         (columns (delete-dups (mapcar (lambda (win) (car (window-edges win))) editors))))
    (my/popup--choose-side
     (>= (frame-width) my/popup-right-min-width)
     (length columns)
     (and support (window-parameter support 'window-side)))))

(defun my/popup--display (buffer origin side &optional alist)
  "Place BUFFER on SIDE with ORIGIN, without changing project association."
  (or (get-buffer-window buffer (selected-frame))
      (let ((window
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

(defun my/popup-display (buffer &optional alist)
  "Display BUFFER without selecting it, preserving its project context.
Documentation uses slot -1; processes and other output share slot 0."
  (pcase-let ((`(,source . ,root) (my/popup--context)))
    (with-current-buffer buffer
      (cond ((my/popup-documentation-p buffer)
             (setq-local my/popup-project-root root))
            ((not my/popup-project-root)
             (setq-local my/popup-project-root
                         (if (my/popup-process-p buffer) (my/popup-root) root)))))
    (my/popup--display buffer source (my/popup-side) alist)))

(defun my/popup-select (buffer)
  "Display and select BUFFER's supporting pane."
  (select-window (or (my/popup-display buffer)
                     (user-error "No room for a supporting pane"))))

(defun my/popup-open-source (buffer)
  "Open BUFFER in the editing window belonging to the selected pane."
  (select-window (my/popup-source-window))
  (switch-to-buffer buffer))

(defun my/popup-toggle (predicate show)
  "Hide the visible pane whose buffer satisfies PREDICATE, or call SHOW.
SHOW runs in the editing window; PREDICATE receives a buffer."
  (let ((window (cl-find-if (lambda (win) (funcall predicate (window-buffer win)))
                            (window-list nil 'no-mini))))
    (if window (quit-window nil window)
      (select-window (my/popup-source-window))
      (funcall show))))

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
           completed)
      (unwind-protect
          (progn
            (select-window source)
            (mapc #'delete-window panes)
            (dolist (state states)
              (pcase-let ((`(,buffer ,start ,point ,hscroll ,origin ,focused) state))
                (let ((window (my/popup--display buffer origin side)))
                  (unless (and window (eq (window-parameter window 'window-side) side))
                    (user-error "No room for supporting panes on the %s" side))
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
                   (funcall my/popup-root-function)
                   default-directory))))
    (file-name-as-directory (if (file-remote-p root) root (file-truename root)))))

(defun my/popup-eshell ()
  "Select this project's Eshell, creating it on first use.
Outside a project, use a shell associated with the source directory."
  (interactive)
  (let* ((root (cdr (my/popup--context)))
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
  "Return the source buffer's REPL through `my/popup-repl-functions'."
  (if (my/popup-process-p (current-buffer)) (current-buffer)
    (seq-some (lambda (entry)
                (and (derived-mode-p (car entry)) (funcall (cdr entry))))
              my/popup-repl-functions)))

(defun my/popup-repl ()
  "Select the source buffer's REPL, or a project REPL when its mode has none.
Start language runtimes with their normal package commands first."
  (interactive)
  (pcase-let* ((`(,source . ,root) (my/popup--context))
               (buffer
                (with-current-buffer (window-buffer source)
                  (or (my/popup--associated-repl)
                      (let ((candidates
                             (cl-remove-if-not
                              (lambda (buf)
                                (and (my/popup-process-p buf)
                                     (with-current-buffer buf
                                       (and (not (apply #'derived-mode-p my/popup-shell-modes))
                                            (equal root (my/popup-root))))))
                              (buffer-list))))
                        (cond ((null candidates)
                               (user-error "No project REPL; start one with your language's normal command"))
                              ((null (cdr candidates)) (car candidates))
                              (t (get-buffer (completing-read "Project REPL: "
                                                              (mapcar #'buffer-name candidates) nil t)))))))))
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

(provide 'my-popup)
;;; my-popup.el ends here
