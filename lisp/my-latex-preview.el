;;; my-latex-preview.el --- LaTeX previews in code comments -*- lexical-binding: t; -*-

;;; Commentary:
;; Implementation helpers; settings and hooks live in config.org.

;;; Code:

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
             (concat ".cache/latex-comments/"
                     (secure-hash 'sha1 (or buffer-file-name (buffer-name))) "/")
             user-emacs-directory))
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
  "Start comment previews in programming modes other than TeX."
  (unless (derived-mode-p 'tex-mode)
    (my/latex-comment-preview-setup)))

(provide 'my-latex-preview)
;;; my-latex-preview.el ends here
