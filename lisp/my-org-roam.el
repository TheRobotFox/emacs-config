;;; my-org-roam.el --- Roam display helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Implementation helpers; settings and hooks live in config.org.

;;; Code:
(require 'org-roam)

;; Completion hierarchy adapted from Doom Emacs.
(cl-defmethod org-roam-node-doom-filetitle ((node org-roam-node))
  "Return the value of \"#+title:\" (if any) from file that NODE resides in.
    If there's no file-level title in the file, return empty string."
  (or (if (= (org-roam-node-level node) 0)
          (org-roam-node-title node)
        (org-roam-node-file-title node))
      ""))
(cl-defmethod org-roam-node-doom-hierarchy ((node org-roam-node))
  "Return hierarchy for NODE, constructed of its file title, OLP and direct title.
      If some elements are missing, they will be stripped out."
  (let ((title     (org-roam-node-title node))
        (olp       (org-roam-node-olp   node))
        (level     (org-roam-node-level node))
        (filetitle (org-roam-node-doom-filetitle node))
        (separator (propertize org-eldoc-breadcrumb-separator 'face 'shadow)))
    (cl-case level
      ;; node is a top-level file
      (0 filetitle)
      ;; node is a level 1 heading
      (1 (concat (propertize filetitle 'face '(shadow italic))
                 separator title))
      ;; node is a heading with an arbitrary outline path
      (t (concat (propertize filetitle 'face '(shadow italic))
                 separator (propertize (string-join olp separator) 'face '(shadow italic))
                 separator title)))))

(defun my/org-roam-preview-latex ()
  "Render LaTeX fragments in the current Roam backlink buffer."
  (org-latex-preview--preview-region
   org-latex-preview-process-default (point-min) (point-max)))

(provide 'my-org-roam)
;;; my-org-roam.el ends here
