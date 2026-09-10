;;; my-treesit.el --- Personal structural editing helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Buffer-local indentation and transposition of same-type syntax siblings.
;; Package activation and keybindings live in config.org.

;;; Code:

(require 'treesit)
(require 'ts-movement)

(defun my/enable-treesit-extras ()
  "Enable structural editing without changing other buffers' defaults."
  (setq-local tab-width 4
              tab-always-indent nil
              indent-region-function #'treesit-indent-region)
  (ts-movement-mode 1))

(defun my/tsm/-transpose (node sibling-function)
  "Swap NODE with the next same-type sibling found by SIBLING-FUNCTION.
Return the new start position of NODE's text, or signal a user error."
  (unless node
    (user-error "No syntax node at point"))
  (let* ((type (treesit-node-type node))
         (other (funcall sibling-function node t)))
    (while (and other (not (equal (treesit-node-type other) type)))
      (setq other (funcall sibling-function other t)))
    (unless other
      (user-error "No %s sibling in that direction" type))
    (let* ((start (treesit-node-start node))
           (end (treesit-node-end node))
           (other-start (treesit-node-start other))
           (other-end (treesit-node-end other))
           (destination (if (< other-start start) other-start
                          (- other-end (- end start)))))
      (atomic-change-group
        (if (< start other-start)
            (transpose-regions start end other-start other-end)
          (transpose-regions other-start other-end start end)))
      destination)))

(defun my/tsm/transpose (sibling-function)
  "Transpose the selected syntax node using SIBLING-FUNCTION."
  (unless (treesit-parser-list)
    (user-error "This buffer has no Tree-sitter parser"))
  (let* ((overlay (tsm/-find-overlay-at-point (point)))
         (node (if overlay (tsm/-get-node overlay)
                 (if (use-region-p)
                     (treesit-node-on (region-beginning) (region-end))
                   (treesit-node-at (point)))))
         (type (and node (treesit-node-type node)))
         (destination (my/tsm/-transpose node sibling-function)))
    (goto-char destination)
    (when overlay
      ;; Reparse at the destination instead of reusing the changed syntax node.
      (remhash overlay tsm/-overlays)
      (delete-overlay overlay)
      (when-let* ((moved (treesit-parent-until
                          (treesit-node-at destination)
                          (lambda (candidate)
                            (equal (treesit-node-type candidate) type)) t)))
        (tsm/-overlay-at-node moved)))))

(defun my/tsm/transpose-forward ()
  "Transpose with the next sibling of the same syntax type."
  (interactive)
  (my/tsm/transpose #'treesit-node-next-sibling))

(defun my/tsm/transpose-backward ()
  "Transpose with the previous sibling of the same syntax type."
  (interactive)
  (my/tsm/transpose #'treesit-node-prev-sibling))

(provide 'my-treesit)
;;; my-treesit.el ends here
