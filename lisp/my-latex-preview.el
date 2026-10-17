;;; my-latex-preview.el --- LaTeX previews in code comments -*- lexical-binding: t; -*-

;;; Commentary:
;; preview-auto in code comments: each buffer previews against a copy of the
;; shared preamble, latex/preview-master.tex, in the temporary directory, with
;; settings that keep previews to comments and out of the way of point.

;;; Code:
(declare-function preview-auto-mode "preview-auto" (&optional arg))
(defvar TeX-master)

(defun my/latex-comment-preview-p ()
  "Check comment syntax without disturbing preview-auto's delimiter match."
  (save-excursion
    (save-match-data (nth 4 (syntax-ppss)))))

(defun my/latex-comment-preview-setup ()
  "Enable comment previews with a cached copy of the shared TeX preamble."
  (interactive)
  (require 'preview-auto)
  (unless (and (local-variable-p 'TeX-master) (stringp TeX-master))
    (let* ((directory
            (expand-file-name
             (concat "latex-comments/" (secure-hash 'sha1 (or buffer-file-name (buffer-name))) "/")
             temporary-file-directory))
           (master (expand-file-name "master.tex" directory)))
      (make-directory directory t)
      (copy-file (expand-file-name "latex/preview-master.tex" user-emacs-directory)
                 master t)
      (setq-local TeX-master master)))
  (setq-local preview-protect-point t
              preview-auto-cache-preamble nil
              preview-auto-check-function #'my/latex-comment-preview-p
              preview-locating-previews-message nil
              preview-visibility-style 'off-point
              preview-keep-stale-images nil
              preview-LaTeX-command-replacements '(preview-LaTeX-disable-pdfoutput))
  (preview-auto-mode 1))

(defun my/latex-comment-preview-auto-enable ()
  "Start comment previews in file buffers, for mode hooks.
Temporary buffers, such as those fontifying Org source blocks, run mode
hooks too and must not start previews."
  (when buffer-file-name (my/latex-comment-preview-setup)))

(provide 'my-latex-preview)
;;; my-latex-preview.el ends here
