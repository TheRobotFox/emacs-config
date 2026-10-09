;;; my-reading.el --- Layout for reading longer text -*- lexical-binding: t; -*-

;;; Commentary:
;; Centered margins come from Olivetti; this file adds spacing, a text scale
;; fitted to the window, and a matching style for EWW pages.  Enable through
;; `olivetti-mode-hook' and `eww-mode-hook'.

;;; Code:
(require 'face-remap)

(defvar olivetti-mode)
(defvar olivetti-body-width)
(defvar-local my/reading-scale 1.8
  "Minimum text scale for reading buffers.")
(defvar-local my/reading-max-scale 4.0
  "Maximum text scale for a single reading window.")
(defvar-local my/reading-auto-scale nil
  "Last fitted scale, excluding manual zoom.")

(defun my/reading-scale-for-window (window)
  "Choose a capped reading scale for WINDOW, allowing for its full width."
  (if (and (one-window-p t (window-frame window))
           (integerp olivetti-body-width))
      (max my/reading-scale
           (min my/reading-max-scale
                (/ (round (* 10 (/ (log (/ (* 0.65 (window-total-width window))
                                           olivetti-body-width))
                                   (log text-scale-mode-step))))
                   10.0)))
    my/reading-scale))

(defun my/update-reading-scale (&optional _window)
  "Fit reading text to the layout, preserving manual zoom relative to that fit."
  (when olivetti-mode
    (let* ((windows (get-buffer-window-list (current-buffer) nil t))
           (scale (if windows
                      (apply #'min (mapcar #'my/reading-scale-for-window windows))
                    my/reading-scale)))
      (unless (equal my/reading-auto-scale scale)
        (let ((zoom (if my/reading-auto-scale
                        (- text-scale-mode-amount my/reading-auto-scale)
                      0)))
          (setq my/reading-auto-scale scale)
          (text-scale-set (+ scale zoom)))))))

(defvar-local my/reading-previous-text-scale nil
  "Text scale before enabling Olivetti, or nil when not saved.")
(defvar-local my/reading-previous-line-spacing nil
  "Saved local binding and value of `line-spacing'.")

;;;###autoload
(defun my/reading-layout ()
  "Apply reading spacing and adaptive scale; restore both on exit."
  (if olivetti-mode
      (progn
        (unless my/reading-previous-text-scale
          (setq my/reading-previous-text-scale
                (if (bound-and-true-p text-scale-mode)
                    text-scale-mode-amount 0))
          (setq my/reading-previous-line-spacing
                (cons (local-variable-p 'line-spacing) line-spacing)))
        (setq-local line-spacing 0.15)
        (add-hook 'window-size-change-functions #'my/update-reading-scale nil t)
        (my/update-reading-scale))
    (remove-hook 'window-size-change-functions #'my/update-reading-scale t)
    (when my/reading-previous-text-scale
      (text-scale-set my/reading-previous-text-scale)
      (if (car my/reading-previous-line-spacing)
          (setq-local line-spacing (cdr my/reading-previous-line-spacing))
        (kill-local-variable 'line-spacing))
      (setq my/reading-previous-text-scale nil
            my/reading-previous-line-spacing nil
            my/reading-auto-scale nil))))

;;;###autoload
(defun my/reading-eww-setup ()
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

(provide 'my-reading)
;;; my-reading.el ends here
