;;; my-org-capture.el --- Task and homework capture -*- lexical-binding: t; -*-

;;; Commentary:
;; Select unused homework files without changing existing files or buffers.
;; Template activation and context rules live in config.org.

;;; Code:
(require 'seq)
(require 'subr-x)

(defvar my/homework-directory (expand-file-name "~/org/tu/")
  "Directory containing one subdirectory per university module.")

(defun my/ha-file-for-class ()
  "Prepare an unused homework buffer for an Org capture template."
  (interactive)
  (unless (file-directory-p my/homework-directory)
    (user-error "Homework directory does not exist: %s" my/homework-directory))
  (let* ((classes (seq-filter
                   (lambda (name)
                     (file-directory-p (expand-file-name name my/homework-directory)))
                   (directory-files my/homework-directory nil "\\`[^.]")))
         (class (if classes (completing-read "Modul: " classes nil t)
                  (user-error "No module directories found")))
         (directory (expand-file-name class my/homework-directory))
         (number 1))
    (dolist (name (directory-files directory nil "\\`HA[0-9]+\\.org\\'"))
      (when (string-match "\\`HA\\([0-9]+\\)\\.org\\'" name)
        (setq number (max number (1+ (string-to-number (match-string 1 name)))))))
    (let ((file (expand-file-name (format "HA%d.org" number) directory)))
      ;; Unsaved homework buffers reserve their numbers too.
      (while (or (file-exists-p file) (get-file-buffer file))
        (setq number (1+ number)
              file (expand-file-name (format "HA%d.org" number) directory)))
      (set-buffer (find-file-noselect file))
      (goto-char (point-max))
      (insert (format "#+TITLE: %s Hausaufgabe %d\n" class number)))))

(defconst my/org-capture-templates
  '(("t" "Task: project / current node / inbox" entry
     (function my/task-context-target)
     "* TODO %?\n%a\n%i\n" :empty-lines 1)
    ("p" "Project TODO" entry
     (function my/task-project-target)
     "* TODO %?\n%a\n%i\n" :empty-lines 1)
    ("r" "Task in current or selected Roam node" entry
     (function my/task-roam-target)
     "* TODO %?\n%a\n%i\n" :empty-lines 1)
    ("i" "Task in Roam inbox" entry
     (function my/task-inbox-target)
     "* TODO %?\n%a\n%i\n" :empty-lines 1)
    ("h" "Homework" plain (function my/ha-file-for-class) ""))
  "Capture templates for context-aware tasks and homework.")

(provide 'my-org-capture)
;;; my-org-capture.el ends here
