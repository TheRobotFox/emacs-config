;;; -*- lexical-binding: t -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-view-program-selection
   '(((output-dvi has-no-display-manager) "dvi2tty")
     ((output-dvi style-pstricks) "dvips and gv") (output-dvi "xdvi")
     (output-pdf "PDF Tools") (output-html "xdg-open")))
 '(custom-safe-themes
   '("0dd2666921bd4c651c7f8a724b3416e95228a13fca1aa27dc0022f4e023bf197" "b11edd2e0f97a0a7d5e66a9b82091b44431401ac394478beb44389cf54e6db28" "04aa1c3ccaee1cc2b93b246c6fbcd597f7e6832a97aaeac7e5891e6863236f9f" "6bdc4e5f585bb4a500ea38f563ecf126570b9ab3be0598bdf607034bb07a8875" default))
 '(eglot-confirm-server-edits nil nil nil "Customized with use-package eglot")
 '(fill-column 1100 nil nil "Customized with use-package org")
 '(org-confirm-babel-evaluate nil)
 '(org-display-remote-inline-images 'cache)
 '(org-image-actual-width 500)
 '(org-image-align 'center)
 '(org-latex-packages-alist nil)
 '(org-export-backends '(ascii beamer html icalendar latex man odt))
 '(org-latex-preview-cache 'temp)
 '(org-safe-remote-resources '("\\`https://fniessen\\.github\\.io\\(?:/\\|\\'\\)"))
 '(org-startup-with-link-previews t)
 '(package-vc-selected-packages
   '((doxymacs :url "https://github.com/pniedzielski/doxymacs.git" :lisp-dir "lisp/"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
