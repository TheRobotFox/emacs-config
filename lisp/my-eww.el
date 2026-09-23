;;; my-eww.el --- EWW reading style -*- lexical-binding: t; -*-

(require 'eww)
(require 'face-remap)

;;;###autoload
(defun my/eww-reading-setup ()
  "Apply a compact, theme-aware reading style to an EWW buffer."
  (setq-local shr-use-fonts t
              shr-use-colors nil
              shr-width nil
              shr-max-width nil
              shr-fill-text nil
              shr-bullet "• "
              shr-hr-line ?─
              shr-max-image-proportion 0.7)
  (display-line-numbers-mode -1)
  (dolist (heading '((shr-h1 1.5) (shr-h2 1.3) (shr-h3 1.15)))
    (face-remap-add-relative (car heading)
                             :height (cadr heading) :weight 'bold :slant 'normal)))

(provide 'my-eww)
;;; my-eww.el ends here
