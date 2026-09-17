;;; my-org-workflow.el --- Project tasks and Roam context -*- lexical-binding: t; -*-

;;; Commentary:
;; Project work lives in task.org; personal work can stay in any Roam node.
;; Agenda views discover both without copying tasks.  No new package is needed.
;; Project association is a file-level PROJECT_ROOT property in a Roam note.

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
(defcustom my/task-file-name "task.org"
  "Task file relative to each project root."
  :type 'string)
(defcustom my/task-legacy-files
  '("~/org/todos.org" "~/org/questions.org" "~/org/ideas.org" "~/org/notes.org")
  "Existing files included in the overview without moving their contents."
  :type '(repeat file))
(defcustom my/task-extra-files nil
  "Additional Org files to include in task views."
  :type '(repeat file))
(defvar org-roam-directory)
(defvar my/task--root-cache (make-hash-table :test #'equal))
(defvar my/task-project-agenda-blocks nil
  "Org block-agenda specification for the project view, set in config.org.")

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

(defun my/task--buffer-root (&optional file)
  "Read the file-level PROJECT_ROOT property in the current Org buffer.
Use FILE, or the visited file, to resolve relative project paths."
  (when (and (or file buffer-file-name) (derived-mode-p 'org-mode))
    (org-with-wide-buffer
     (goto-char (point-min))
     (when-let* ((root (org-entry-get nil "PROJECT_ROOT")))
       (my/task--canonical-root
        (expand-file-name root (file-name-directory (or file buffer-file-name))))))))

(defun my/task--file-root (file)
  "Return FILE's project association, caching unchanged files."
  (let* ((buffer (get-file-buffer file))
         (stamp (if buffer (with-current-buffer buffer (buffer-chars-modified-tick))
                  (file-attribute-modification-time (file-attributes file))))
         (key (list buffer stamp))
         (cached (gethash file my/task--root-cache)))
    (if (equal key (car cached)) (cdr cached)
      (let ((root (if buffer
                      (with-current-buffer buffer (my/task--buffer-root))
                    (with-temp-buffer
                      (insert-file-contents file)
                      ;; Reading metadata must not render LaTeX, align tables,
                      ;; or initialize document visibility in every Roam file.
                      (let ((org-inhibit-startup t))
                        (delay-mode-hooks (org-mode)))
                      ;; Keep the scratch buffer non-visiting: marking it as
                      ;; FILE would make killing it prompt to save its contents.
                      (my/task--buffer-root file)))))
        (puthash file (cons key root) my/task--root-cache)
        root))))

(defun my/project-from-roam (directory)
  "Supply project.el context for a project-associated Roam buffer."
  (when (and (my/task--roam-file-p)
             (equal (expand-file-name directory) (expand-file-name default-directory)))
    (when-let* ((root (my/task--buffer-root))
                (_ (file-directory-p root)))
      (let ((project-find-functions (remq #'my/project-from-roam project-find-functions)))
        (or (project-current nil root) (cons 'transient root))))))

(defun my/task-project-root (&optional prompt)
  "Resolve current project context, optionally PROMPT for another project.
Unassociated Roam notes do not inherit the notes repository as their project."
  (let ((root (if (and (my/task--roam-file-p)
                       (not project-current-directory-override))
                  (my/task--buffer-root)
                (when-let* ((project (project-current nil))) (project-root project)))))
    (when (and (not root) prompt)
      (setq root (funcall project-prompter)))
    (when root
      (unless (file-directory-p root)
        (user-error "Project directory is unavailable: %s" root))
      (my/task--canonical-root root))))

(defun my/task--project-file (root)
  "Select ROOT's task file, honoring an existing tasks.org convention."
  (let ((preferred (expand-file-name my/task-file-name root))
        (alternate (expand-file-name "tasks.org" root)))
    (if (and (not (my/task--available-file-p preferred))
             (my/task--available-file-p alternate)) alternate preferred)))

(defun my/task--prepare-file (file title &optional roam)
  "Visit FILE without displaying it; initialize a new empty file with TITLE.
When ROAM is non-nil, assign a file ID for Roam indexing."
  (make-directory (file-name-directory file) t)
  (let ((buffer (find-file-noselect file)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'org-mode) (org-mode))
      (org-with-wide-buffer
       (when (= (point-min) (point-max))
         (insert "#+title: " title "\n#+category: " title "\n\n")
         (when roam (goto-char (point-min)) (org-id-get-create)))))
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
  "Position capture in ROOT's task file."
  (my/task--remember-root root)
  (set-buffer (my/task--prepare-file
               (my/task--project-file root)
               (file-name-nondirectory (directory-file-name root))))
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
  "Position capture in the originating buffer's project task file."
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
  "Capture a task in the current or selected project's task file."
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
  "Open the current or selected project's task file."
  (interactive)
  (let* ((root (my/task-project-root t))
         (buffer (my/task--prepare-file
                  (my/task--project-file root)
                  (file-name-nondirectory (directory-file-name root)))))
    (my/task--remember-root root)
    (pop-to-buffer-same-window buffer)))

(defun my/project-note ()
  "Open or create the current project's file-level Roam node."
  (interactive)
  (require 'org-roam)
  (let* ((root (my/task-project-root t))
         (matches (seq-filter (lambda (file) (equal (my/task--file-root file) root))
                              (my/task--roam-files))))
    (my/task--remember-root root)
    (if matches
        (find-file (if (cdr matches) (completing-read "Project note: " matches nil t)
                     (car matches)))
      (let* ((title (read-string "Project title: "
                                 (file-name-nondirectory (directory-file-name root))))
             (id (org-id-new))
             (file (expand-file-name (concat "project-" id ".org") org-roam-directory)))
        (make-directory org-roam-directory t)
        (find-file file)
        (insert (format ":PROPERTIES:\n:ID: %s\n:PROJECT_ROOT: %s\n:END:\n#+title: %s\n#+filetags: :project:\n\n"
                        id root title))
        (insert (org-link-make-string (concat "file:" root) "Repository") "\n"
                (org-link-make-string (concat "file:" (my/task--project-file root)) "Project tasks")
                "\n\n* Context\n\n* Decisions\n")
        (save-buffer)
        (org-roam-db-update-file file)))))

(defun my/project-link-note ()
  "Associate this Roam file with a selected project, preserving its contents."
  (interactive)
  (unless (my/task--roam-file-p) (user-error "Open a Roam note first"))
  (let ((root (my/task--canonical-root (funcall project-prompter))))
    (unless (file-directory-p root) (user-error "Project directory does not exist"))
    (my/task--remember-root root)
    (org-with-wide-buffer
     (goto-char (point-min))
     (org-id-get-create)
     (org-entry-put nil "PROJECT_ROOT" root))
    (save-buffer)
    (when (featurep 'org-roam) (org-roam-db-update-file buffer-file-name))
    (message "Note associated with %s" root)))

(defun my/task-agenda-files ()
  "Discover existing task sources without depending on Roam's TODO index."
  (let* ((notes (my/task--roam-files))
         (roots (delete-dups
                 (append (project-known-project-roots)
                         (delq nil (mapcar #'my/task--file-root notes)))))
         (files (append notes my/task-legacy-files my/task-extra-files
                        (cl-loop for root in roots
                                 unless (file-remote-p root)
                                 append (list (expand-file-name my/task-file-name root)
                                              (expand-file-name "tasks.org" root))))))
    (delete-dups
     (mapcar #'file-truename
             (seq-filter #'my/task--available-file-p (mapcar #'expand-file-name files))))))

(defun my/task-refresh-agenda (&rest _)
  "Refresh task discovery before opening the agenda."
  (setq org-agenda-files (my/task-agenda-files)))

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
  (unless (or (and buffer-file-name (file-in-directory-p buffer-file-name root))
              (equal (when-let* ((value (org-entry-get nil "PROJECT_ROOT" t)))
                       (my/task--canonical-root
                        (expand-file-name value (file-name-directory buffer-file-name))))
                     root))
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
