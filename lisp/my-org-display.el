;;; my-org-display.el --- Note typography, image zoom and block folding -*- lexical-binding: t; -*-

;;; Commentary:
;; Buffer-local display behaviour for Org notes: spacing, inline images that
;; follow text zoom, and default folding of diagram and proof blocks.
;; Enable through `org-mode-hook'; faces and fonts are set in config.org.

;;; Code:
(require 'cl-lib)
(require 'org)
(require 'org-element)
(require 'org-fold)
(require 'face-remap)

;;;; Typography and images

(defun my/org-typography ()
  "Apply note typography without changing fonts in other buffers."
  (setq-local line-spacing 0.12)
  ;; Air below the first and last line of a block.
  (setq-local font-lock-extra-managed-props (cons 'line-height font-lock-extra-managed-props))
  (font-lock-add-keywords nil '(("^[ \t]*#\\+\\(?:begin\\|end\\)_.*\\(\n\\)" 1 '(face nil line-height 1.5))) t)
  (add-hook 'text-scale-mode-hook #'my/org-scale-inline-images nil t))

(defun my/org-scale-image (image)
  "Return IMAGE with the buffer's text zoom, without changing its base dimensions."
  (if (eq (car-safe image) 'image)
      (cons 'image
            (plist-put (copy-sequence (cdr image)) :scale
                       (expt text-scale-mode-step
                             (if text-scale-mode text-scale-mode-amount 0))))
    image))

(defun my/org-scale-inline-images ()
  "Update displayed images and their alignment to match text zoom."
  (dolist (overlay org-link-preview-overlays)
    (let ((image (overlay-get overlay 'display)))
      (when (eq (car-safe image) 'image)
        (let* ((scaled (my/org-scale-image image))
               (before (overlay-get overlay 'before-string))
               (alignment (and (stringp before) (> (length before) 0)
                               (get-text-property 0 'display before))))
          (overlay-put overlay 'display scaled)
          (when (eq (car-safe alignment) 'space)
            (let ((spacer (copy-sequence before)))
              (put-text-property
               0 1 'display
               (cl-subst-if scaled
                            (lambda (form) (eq (car-safe form) 'image))
                            alignment)
               spacer)
              (overlay-put overlay 'before-string spacer))))))))

(defun my/org-inline-image (image)
  "Apply text zoom to IMAGE and keep D2 math independent of the buffer font.
Use as a return filter on Org's inline image creation."
  (let* ((image (my/org-scale-image image))
         (properties (cdr image))
         (file (plist-get properties :file)))
    (if (and (eq (plist-get properties :type) 'svg)
             file (not (file-remote-p file))
             (with-temp-buffer
               (insert-file-contents file nil 0 512)
               (search-forward "data-d2-version=" nil t)))
        (cons 'image
              (plist-put (copy-sequence properties) :css
                         (concat (plist-get properties :css)
                                 "\nsvg { font-size: 16px; }")))
      image)))

(defun my/org-fit-image-spacer (overlay &rest _)
  "Make the spacer aligning OVERLAY's image as tall as the image.
Point at the image shows on the spacer, which otherwise sits on the baseline
in the middle of the image: below the window while its top shows, so
scrolling past a tall image pulls the window back.  Use after
`org-link-preview-file'."
  (let* ((before (overlay-get overlay 'before-string))
         (image (overlay-get overlay 'display))
         (spacer (and (stringp before) (> (length before) 0)
                      (get-text-property 0 'display before))))
    (when (and (eq (car-safe spacer) 'space) (eq (car-safe image) 'image))
      (setq before (copy-sequence before))
      ;; Org's images keep the default :ascent, half above the baseline.
      (put-text-property 0 1 'display
                         (append spacer `(:height (1.0 . ,image) :ascent 50))
                         before)
      (overlay-put overlay 'before-string before))))

;;;; Folding

(defcustom my/org-folded-languages '("d2" "dot" "plantuml")
  "Source block languages folded by default."
  :type '(repeat string) :group 'org)
(defcustom my/org-folded-blocks '("beispiel" "proof")
  "Special block types folded by default under headings.  At a note's top
level they stay open: there a proof is the note's content, as in a theorem
note."
  :type '(repeat string) :group 'org)

(defun my/org--block-fold-state (element)
  "Return `hide', `off', or nil for ELEMENT's initial folding."
  ;; Read as Org's image previews read :width: `org-export-read-attribute'
  ;; would load the export framework and its backends with the first note.
  (pcase (let ((attributes (string-join (org-element-property :attr_org element) " ")))
           (and (string-match ":fold[ \t]+\\([^ \t]+\\)" attributes)
                (match-string 1 attributes)))
    ("yes" 'hide)
    ("no" 'off)
    (_ (when (pcase (org-element-type element)
               ('src-block
                (member (org-element-property :language element) my/org-folded-languages))
               ('special-block
                (and (member (downcase (org-element-property :type element)) my/org-folded-blocks)
                     (org-element-lineage element '(headline)))))
         'hide))))

(defun my/org-fold-note-blocks ()
  "Fold diagram sources and the configured special blocks by default.
Put #+ATTR_ORG: :fold yes or :fold no immediately above any block to
override its initial visibility.  Other blocks are left alone by default."
  (interactive)
  (org-with-wide-buffer
   (org-block-map
    (lambda ()
      (let* ((element (org-element-at-point))
             (state (my/org--block-fold-state element)))
        (when state (org-fold-hide-block-toggle state nil element)))))))

(defvar-local my/org--startup-visibility-pending nil
  "Whether background loading skipped Org's startup in this buffer.")

(defun my/org--fold-note-blocks-on-display (window)
  "Apply pending startup visibility and block folding on first display in WINDOW."
  ;; Window change hooks can also run in the buffer being switched away from.
  (when (eq (window-buffer window) (current-buffer))
    (when my/org--startup-visibility-pending
      ;; Agenda loading skipped all of Org's startup: indentation, visibility
      ;; and previews.  The restart reruns it and clears the flag.
      (let ((inhibit-message t)) (org-mode-restart))
      (unless (org-before-first-heading-p)
        (org-fold-show-context 'agenda)))
    (my/org-fold-note-blocks)
    (remove-hook 'window-buffer-change-functions
                 #'my/org--fold-note-blocks-on-display t)))

(defun my/org-defer-note-folding ()
  "Arrange initial folding on first display, including agenda-loaded notes.
Do not scan undisplayed agenda files or temporary export/metadata buffers."
  (setq my/org--startup-visibility-pending org-inhibit-startup)
  (add-hook 'window-buffer-change-functions
            #'my/org--fold-note-blocks-on-display nil t))

(provide 'my-org-display)
;;; my-org-display.el ends here
