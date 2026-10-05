;;; org-note-graph-consult.el --- Consult previews for note graphs -*- lexical-binding: t; -*-

;;; Commentary:
;; Optional completion adapter; Consult owns preview restoration.

;;; Code:
(require 'org-note-graph-ui)
(require 'consult)

(defun org-note-graph-consult-read (candidates prompt allow-new)
  "Select from CANDIDATES with PROMPT, previewing nodes through Consult.
ALLOW-NEW permits returning a new title.  Preview does not commit a visit,
so inserting a link retains the original buffer and point."
  (let ((preview (consult--jump-preview))
        (locations (make-hash-table :test #'equal)))
    (unwind-protect
        (consult--read candidates
                       :prompt prompt :require-match (not allow-new)
                       :category 'org-note-graph-node
                       :annotate (lambda (candidate)
                                   (when-let* ((node (cdr (assoc candidate candidates))))
                                     (org-note-graph--annotation node)))
                       :lookup (lambda (candidate &rest _)
                                 (or (cdr (assoc candidate candidates)) candidate))
                       :state (lambda (action node)
                                (funcall preview action
                                         (when (and (eq action 'preview) (org-note-graph-node-p node))
                                           (or (gethash (org-note-graph-node-key node) locations)
                                               (puthash (org-note-graph-node-key node)
                                                        (with-current-buffer (find-file-noselect (org-note-graph-node-file node))
                                                          (save-excursion
                                                            (save-restriction
                                                              (org-note-graph--goto node)
                                                              (point-marker)))) locations))))))
      (maphash (lambda (_ marker) (set-marker marker nil)) locations))))

(provide 'org-note-graph-consult)
;;; org-note-graph-consult.el ends here
