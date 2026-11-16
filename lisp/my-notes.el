;;; my-notes.el --- Personal graph construction and refresh -*- lexical-binding: t; -*-
(require 'org-note-graph-org)
(require 'org-note-graph-notes)
(require 'org-note-graph-context)
(require 'my-tasks)

(defvar my/note-files nil "Files in the last document snapshot.")
(defvar my/note-source
  (org-note-graph-source #'org-note-graph-org-build))
(defvar my/org-cache
  (org-note-graph-cached-builder
   (lambda ()
     (setq my/note-files (delete-dups (append (directory-files-recursively my/notes-directory "\\.org$")
                                              (my/project-notes)))
           org-id-extra-files my/note-files)
     (org-note-graph-org-connect (funcall (car my/note-source) my/note-files)))))
(defvar my/note-cache
  (org-note-graph-cached-builder
   (lambda () (org-note-graph-prose-view (org-note-graph-notes-build (my/org-graph))))))

(defun my/org-graph ()
  "Return the current document snapshot, including unsaved edits."
  (funcall (car my/org-cache)))

(defun my/note-graph ()
  "Return the note-taking graph built over the document snapshot."
  (funcall (car my/note-cache)))

(defun my/notes-invalidate-all (&rest _)
  "Invalidate both views without reading files or rebuilding."
  (funcall (cadr my/org-cache)) (funcall (cadr my/note-cache)))

(defun my/notes-invalidate (&rest _)
  "Invalidate after a change to an indexed Org document."
  (when (and buffer-file-name (derived-mode-p 'org-mode)
             (or (file-in-directory-p buffer-file-name my/notes-directory)
                 (member buffer-file-name my/note-files)))
    (my/notes-invalidate-all)))

(defun my/notes-refresh ()
  "Refresh the file list and document cache explicitly."
  (interactive)
  (funcall (cadr my/note-source))
  (my/notes-invalidate-all)
  (my/note-graph)
  (message "Refreshed %d note files" (length my/note-files)))

(defun my/notes-follow-context ()
  "Follow the selected Org buffer on idle while the context pane is visible."
  (when (and (not (active-minibuffer-window))
             (get-buffer-window org-note-graph-context-buffer)
             (derived-mode-p 'org-mode) buffer-file-name
             (or (file-in-directory-p buffer-file-name my/notes-directory)
                 (member buffer-file-name my/note-files)))
    (while-no-input
      (let ((graph (my/note-graph)))
        (org-note-graph-context-show graph (org-note-graph-here graph) (current-buffer))))))

(provide 'my-notes)
