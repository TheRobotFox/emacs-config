;;; my-project-search.el --- Project file previews -*- lexical-binding: t; -*-

(require 'consult)

(defun my/project-read-file-name (prompt files &optional predicate history defaults)
  "Read from project FILES with Consult previews.
PROMPT, PREDICATE, HISTORY and DEFAULTS follow project.el's file reader."
  (expand-file-name
   (consult--read (mapcar #'file-relative-name files)
                  :prompt (concat prompt ": ")
                  :predicate predicate
                  :require-match t
                  :category 'file
                  :history history
                  :add-history (mapcar #'file-relative-name (ensure-list defaults))
                  :state (consult--file-preview))))

(provide 'my-project-search)
;;; my-project-search.el ends here
