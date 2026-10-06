;;; my-org-note-db-pairs.el --- Trial pair lists and dualisms -*- lexical-binding: t; -*-
;;; Code:
(require 'org-note-db-writing)

(defun my/org-note-db-pairs--item (item file)
  "Read exactly two node links in ITEM's own text, relative to FILE."
  (let* ((links (org-element-map item 'link #'identity nil nil 'plain-list))
         (keys (mapcar (lambda (link) (org-note-db-reference link file)) links)))
    (if (and (length= keys 2) (seq-every-p #'identity keys)) keys
      (display-warning 'org-note-db
                       (format "%s:%d: Pair item needs exactly two node links"
                               file (org-element-property :begin item)))
      nil)))

(defun my/org-note-db-pairs--scan (document)
  "Extract direct list items from sections declaring PAIR_TARGET."
  (let ((regions (org-note-db-document-regions document))
        (file (org-note-db-document-file document)))
    (cl-loop for region in regions
             when (org-note-db-property region "PAIR_TARGET") collect
             (cons region
                   (org-note-db-map-region
                    document region 'item
                    (lambda (item)
                      (let ((list (org-element-property :parent item)))
                        (when (and (eq (org-element-type (org-element-property :parent list)) 'section)
                                   (eq region (org-note-db-region-at
                                               regions (org-element-property :begin item))))
                          (my/org-note-db-pairs--item item file)))))))))

(org-note-db-register-destination "Dualismen" 'undirected-edge)
(org-note-db-register-provider 'link-pairs
 :scan #'my/org-note-db-pairs--scan :output "PAIR_TARGET"
 :formatters '((undirected-edge . identity) (directed-edge . identity)))

(defun my/org-note-db-pairs-capture-template ()
  "Choose pair participants before opening native item capture."
  (let* ((db (org-note-db-database t))
         (origin (org-capture-get :note-db-origin))
         (target (org-capture-get :note-db-target))
         (keys (org-note-db-selection db))
         (left (or origin (org-note-db-read db keys "First note: ")))
         (right (org-note-db-read db keys "Second note: "))
         (text (mapconcat (lambda (node)
                            (org-note-db-location-link node (org-note-db-location-file target)))
                          (list left right) " ↔ "))
         (hook (org-capture-get :hook)))
    (org-capture-put :hook (cons (lambda ()
                                  (when (zerop (current-indentation)) (indent-line-to 1))
                                  (org-cycle-list-bullet "*")
                                  (end-of-line)
                                  (insert text))
                                (if (functionp hook) (list hook) hook)))
    "- %?"))

(add-to-list 'org-note-db-capture-templates
             '("link-pair" item (function my/org-note-db-pairs-capture-template)))

(defun my/org-note-db-pairs-setup (destination)
  "Mark this section as a pair list, defaulting to link-pair capture."
  (interactive
   (list (completing-read "Pair destination: "
                          (cl-loop for (name . schema) in org-note-db-destinations
                                   when (memq schema '(directed-edge undirected-edge)) collect name)
                          nil t nil nil "Dualismen")))
  (org-note-db-set-properties
   `(("PAIR_TARGET" . ,destination)
     ("CAPTURE" . ,(or (org-entry-get nil "CAPTURE") "link-pair")))))

(add-to-list 'org-note-db-setup-commands '("Link pairs" . my/org-note-db-pairs-setup))

(defun my/org-note-db-related (destination)
  "Show neighbors of the current note through an edge DESTINATION."
  (interactive (list (completing-read "Relations: " (org-note-db-data-destinations 'directed-edge)
                                     nil t nil nil "Dualismen")))
  (unless (org-note-db-current-node (org-note-db-database t)) (user-error "No current note"))
  (org-note-db-show (lambda (db context) (org-note-db-out db (list context) destination))
                    destination))

(provide 'my-org-note-db-pairs)
;;; my-org-note-db-pairs.el ends here
