;;; my-org-workflow.el --- Project notes and task capture -*- lexical-binding: t; -*-

;;; Commentary:
;; Project notes live in the repository; tasks are ordinary README headings.
;; project.el supplies repository context and remembers agenda sources.

;;; Code:
(require 'cl-lib)
(require 'project)
(require 'org)
(require 'org-id)
(require 'org-capture)
(require 'org-agenda)
(require 'seq)
(require 'subr-x)

(defgroup my/org-workflow nil "Personal project and task workflow." :group 'org)
(defcustom my/project-note-file "README.org"
  "Org note relative to each project root, also containing its Tasks heading."
  :type 'string)
(defcustom my/task-legacy-files
  '("~/org/todos.org" "~/org/questions.org" "~/org/ideas.org" "~/org/notes.org")
  "Existing files included in the overview without moving their contents."
  :type '(repeat file))
(defcustom my/task-extra-files nil
  "Additional Org files to include in task views."
  :type '(repeat file))
(defvar org-roam-directory)
(defvar my/task-project-agenda-blocks nil
  "Org block-agenda specification for the project view, set in config.org.")
(defvar my/project-note-hook nil
  "Hook run in the repository note opened by the project commands.")

(defun my/task--canonical-root (directory)
  "Normalize DIRECTORY, preserving a trailing slash."
  (let ((path (expand-file-name directory)))
    (file-name-as-directory (if (file-remote-p path) path (file-truename path)))))

(defun my/task--available-file-p (file)
  "Whether FILE exists or has an unsaved visiting buffer."
  (or (file-regular-p file) (buffer-live-p (get-file-buffer file))))

(defun my/task--roam-file-p (&optional file)
  "Whether FILE belongs to the configured Roam tree."
  (let ((file (or file buffer-file-name)))
    (and file (boundp 'org-roam-directory)
         (file-in-directory-p file org-roam-directory))))

(defun my/task--roam-files ()
  "List Roam Org files, including new files in unsaved buffers."
  (when (boundp 'org-roam-directory)
    (delete-dups
     (append
      (when (file-directory-p org-roam-directory)
        (seq-filter (lambda (file)
                      (and (file-regular-p file)
                           (not (string-prefix-p ".#" (file-name-nondirectory file)))))
                    (directory-files-recursively org-roam-directory "\\.org\\'")))
      (delq nil
            (mapcar (lambda (buffer)
                      (let ((file (buffer-file-name buffer)))
                        (when (and file (string-suffix-p ".org" file)
                                   (my/task--roam-file-p file)) file)))
                    (buffer-list)))))))

(defun my/task-project-root (&optional prompt)
  "Resolve current project context, optionally PROMPT for another project.
Personal notes do not inherit the notes repository as their project."
  (let ((root (when (or project-current-directory-override (not (my/task--roam-file-p)))
                (when-let* ((project (project-current nil))) (project-root project)))))
    (when (and (not root) prompt)
      (setq root (funcall project-prompter)))
    (when root
      (unless (file-directory-p root)
        (user-error "Project directory is unavailable: %s" root))
      (my/task--canonical-root root))))

(defun my/project-note-path (root)
  "Return ROOT's Org note, preserving the case of an existing filename."
  (expand-file-name
   (or (seq-find (lambda (name) (string-equal-ignore-case name my/project-note-file))
                 (directory-files root nil nil t))
       my/project-note-file)
   root))

(defun my/task--prepare-file (file title &optional node)
  "Visit FILE without displaying it; initialize a new empty file with TITLE.
When NODE is non-nil, assign a file ID for stable links."
  (make-directory (file-name-directory file) t)
  (let ((buffer (find-file-noselect file)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'org-mode) (org-mode))
      (org-with-wide-buffer
       (when (= (point-min) (point-max))
         (insert "#+title: " title "\n#+category: " title "\n\n")
         (when node (goto-char (point-min)) (org-id-get-create)))))
    buffer))

(defun my/task--tasks-heading ()
  "Position at a top-level Tasks heading, creating it when absent."
  (widen)
  (goto-char (point-min))
  (unless (re-search-forward "^\\* Tasks[ \t]*$" nil t)
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert "* Tasks\n"))
  (beginning-of-line)
  (unless (org-at-heading-p) (forward-line -1)))

(defun my/task--remember-root (root)
  "Remember ROOT so its tasks remain discoverable after restarting Emacs."
  (project-remember-project (cons 'transient root)))

(defun my/task--project-target (root)
  "Position capture under Tasks in ROOT's repository note."
  (set-buffer (my/project-note-buffer root))
  (my/task--tasks-heading))

(defun my/task-inbox-target ()
  "Position capture in the Roam inbox."
  (unless (boundp 'org-roam-directory) (user-error "Roam directory is not configured"))
  (set-buffer (my/task--prepare-file
               (expand-file-name "inbox.org" org-roam-directory) "Inbox" t))
  (my/task--tasks-heading))

(defun my/task-project-context-p ()
  "Whether the current buffer has an available project for capture."
  (condition-case nil
      (and (my/task-project-root) t)
    (user-error nil)))

(defun my/task-project-target ()
  "Position capture under Tasks in the originating project's note."
  (let* ((origin (org-capture-get :original-buffer))
         (root (when (buffer-live-p origin)
                 (with-current-buffer origin (my/task-project-root)))))
    (unless root (user-error "No project in the capture context"))
    (my/task--project-target root)))

(defun my/task--node-target (node)
  "Position capture beneath the existing Roam NODE."
  (set-buffer (find-file-noselect (org-roam-node-file node)))
  (widen)
  (if (zerop (org-roam-node-level node)) (my/task--tasks-heading)
    (goto-char (or (org-find-property "ID" (org-roam-node-id node))
                   (user-error "Roam heading no longer exists; refresh the Roam database")))
    (org-back-to-heading t)))

(defun my/task-roam-target ()
  "Capture in the current Roam node, or select an existing node elsewhere."
  (let ((origin (org-capture-get :original-buffer)))
    (if (and (buffer-live-p origin)
             (with-current-buffer origin (my/task--roam-file-p)))
        (progn
          (set-buffer origin)
          (widen)
          ;; Read the live outline, including new nodes not indexed yet.
          (unless (org-before-first-heading-p)
            (org-back-to-heading t)
            (while (and (not (org-entry-get nil "ID")) (org-up-heading-safe))))
          (unless (and (org-at-heading-p) (org-entry-get nil "ID"))
            (my/task--tasks-heading)))
      (require 'org-roam)
      (my/task--node-target (org-roam-node-read nil nil nil t "Task in node: ")))))

(defun my/task-context-target ()
  "Capture in the project, current Roam node, or inbox, in that order."
  (let* ((origin (org-capture-get :original-buffer))
         (root (when (buffer-live-p origin)
                 (with-current-buffer origin (my/task-project-root)))))
    (cond (root (my/task--project-target root))
          ((and (buffer-live-p origin)
                (with-current-buffer origin (my/task--roam-file-p)))
           (my/task-roam-target))
          (t (my/task-inbox-target)))))

(defun my/task-capture ()
  "Capture a task with source context, without requiring a deadline."
  (interactive)
  (org-capture nil "t"))

(defun my/roam-task-capture ()
  "Capture a task in a Roam node even when it is associated with a project."
  (interactive)
  (org-capture nil "r"))

(defun my/project-task-capture ()
  "Capture a task under Tasks in the current or selected project's note."
  (interactive)
  (let* ((root (my/task-project-root t))
         ;; This explicit command also supports selecting a project from outside one.
         (org-capture-templates-contexts nil)
         (org-capture-templates
          `(("p" "Project task" entry
             (function ,(lambda () (my/task--project-target root)))
             "* TODO %?\n%a\n%i\n" :empty-lines 1))))
    (org-capture nil "p")))

(defun my/project-tasks ()
  "Open the Tasks heading in the current or selected project's note."
  (interactive)
  (my/task--project-target (my/task-project-root t))
  (pop-to-buffer-same-window (current-buffer))
  (org-fold-show-context 'agenda))

(defun my/project-note-buffer (root)
  "Visit ROOT's repository note and run `my/project-note-hook' at file level."
  (let ((buffer (my/task--prepare-file
                 (my/project-note-path root)
                 (file-name-nondirectory (directory-file-name root)) t)))
    (my/task--remember-root root)
    (with-current-buffer buffer
      (unless (file-exists-p buffer-file-name) (save-buffer))
      (org-with-wide-buffer
       (goto-char (point-min))
       (run-hooks 'my/project-note-hook)))
    buffer))

(defun my/project-note ()
  "Open or create the current project's repository note."
  (interactive)
  (pop-to-buffer-same-window (my/project-note-buffer (my/task-project-root t))))

(defun my/task-agenda-files ()
  "Discover personal notes and known projects' Org notes for the agenda."
  (let* ((notes (my/task--roam-files))
         (roots (project-known-project-roots))
         (files (append notes my/task-legacy-files my/task-extra-files
                        (cl-loop for root in roots
                                 when (and (not (file-remote-p root)) (file-directory-p root))
                                 collect (my/project-note-path root)))))
    (delete-dups
     (mapcar #'file-truename
             (seq-filter #'my/task--available-file-p (mapcar #'expand-file-name files))))))

(defun my/task-refresh-agenda (&rest _)
  "Refresh task discovery before opening the agenda."
  (setq org-agenda-files (my/task-agenda-files)))

(defun my/task--refresh-vc-on-display (window)
  "Initialize deferred version-control state when displayed in WINDOW."
  (when (eq (window-buffer window) (current-buffer))
    (vc-refresh-state)
    (remove-hook 'window-buffer-change-functions #'my/task--refresh-vc-on-display t)))

(defun my/task-prepare-agenda-buffers (prepare &rest args)
  "Run PREPARE with ARGS, deferring new buffers' VC checks until display."
  (let ((find-file-hook
         (cl-substitute
          (lambda ()
            (add-hook 'window-buffer-change-functions #'my/task--refresh-vc-on-display nil t))
          #'vc-refresh-state find-file-hook)))
    (apply prepare args)))

(defun my/task--inbox-p ()
  "Whether the current entry is in the general Roam inbox."
  (and buffer-file-name (boundp 'org-roam-directory)
       (equal (file-truename buffer-file-name)
              (file-truename (expand-file-name "inbox.org" org-roam-directory)))))

(defun my/task-skip-outside-inbox ()
  "Skip non-inbox files in the review's inbox block."
  (unless (my/task--inbox-p) (point-max)))

(defun my/task-skip-inbox ()
  "Skip the inbox, whose tasks already appear in their own review block."
  (when (my/task--inbox-p) (point-max)))

(defun my/task-agenda-appearance ()
  "Give agenda rows readable spacing and a consistently aligned font."
  (setq-local line-spacing 0.15)
  (setq-local truncate-lines t)
  (setq-local buffer-face-mode-face 'fixed-pitch)
  (buffer-face-mode 1))

(defun my/task--skip-other-project (root)
  "Skip this heading unless it belongs to ROOT, without skipping its children."
  (unless (and buffer-file-name (file-in-directory-p buffer-file-name root))
    (save-excursion (outline-next-heading) (point))))

(defun my/project-task-agenda (&optional _match)
  "Render the configured task blocks for the current or selected project.
Use ordinary Org block-agenda settings so its standard refresh retains ROOT."
  (let* ((root (my/task-project-root t))
         (title (concat "Project: " (file-name-nondirectory (directory-file-name root))))
         (skip (lambda () (my/task--skip-other-project root))))
    (unless my/task-project-agenda-blocks
      (user-error "The project agenda blocks are not configured"))
    (org-agenda-run-series
     title
     (list my/task-project-agenda-blocks
           `((org-agenda-files (my/task-agenda-files))
             (org-agenda-buffer-name "*Project Tasks*")
             (org-agenda-skip-function ',skip))))))

(defun my/project-task-overview ()
  "Open the standard agenda menu's project overview command."
  (interactive)
  (org-agenda nil "p"))

(provide 'my-org-workflow)
;;; my-org-workflow.el ends here
