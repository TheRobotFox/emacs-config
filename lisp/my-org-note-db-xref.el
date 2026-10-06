;;; my-org-note-db-xref.el --- Tagged definitions through Xref -*- lexical-binding: t; -*-
;;; Code:
(require 'org-note-db-locations)
(require 'xref)

(defcustom my/org-note-db-definition-tag "Definition"
  "Tag in Tags whose locations supply Org definition lookup."
  :type 'string :group 'org-note-db)

(defun my/org-note-db-definitions ()
  "Return locations tagged with `my/org-note-db-definition-tag'."
  (seq-uniq (org-note-db-locations-lookup
             (org-note-db-database t)
             (concat "\\`" (regexp-quote my/org-note-db-definition-tag) "\\'") '("Tags"))
            #'equal))

(defun my/org-note-db-xref-backend ()
  "Use note definitions for Xref in Org buffers."
  (when (derived-mode-p 'org-mode) 'my-org-note-db))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql my-org-note-db)))
  (cond ((use-region-p) (string-trim (buffer-substring-no-properties (region-beginning) (region-end))))
        ((when-let* ((element (org-element-lineage (org-element-context) '(bold italic underline link) t))
                     (begin (org-element-property :contents-begin element))
                     (end (org-element-property :contents-end element)))
           (org-string-nw-p (buffer-substring-no-properties begin end))))
        (t (thing-at-point 'symbol t))))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql my-org-note-db)))
  (seq-uniq (mapcar #'org-note-db-location-title (my/org-note-db-definitions)) #'equal))

(cl-defmethod xref-backend-definitions ((_backend (eql my-org-note-db)) name)
  (mapcar
   (lambda (location)
     (with-current-buffer (find-file-noselect (org-note-db-location-file location))
       (save-excursion
         (save-restriction
           (org-note-db-goto location)
           (xref-make (org-note-db-location-title location)
                      (xref-make-buffer-location (current-buffer) (point)))))))
   (seq-filter (lambda (location) (string-equal-ignore-case name (org-note-db-location-title location)))
               (my/org-note-db-definitions))))

(defun my/org-note-db-xref-reveal ()
  "Reveal the destination's entry and ancestors after Xref navigation in Org."
  (when (derived-mode-p 'org-mode)
    (org-fold-show-context 'link-search)
    (org-fold-show-entry)))

(add-hook 'xref-backend-functions #'my/org-note-db-xref-backend)
(add-hook 'xref-after-jump-hook #'my/org-note-db-xref-reveal)
(add-hook 'xref-after-return-hook #'my/org-note-db-xref-reveal)

(provide 'my-org-note-db-xref)
;;; my-org-note-db-xref.el ends here
