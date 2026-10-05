;;; org-note-graph-model.el --- Nodes and extension specifications -*- lexical-binding: t; -*-

;;; Code:
(require 'cl-lib)
(require 'seq)
(require 'org)
(require 'org-element)
(require 'subr-x)

(defgroup org-note-graph nil "Directed graphs of Org notes." :group 'org)
(defcustom org-note-graph-roots (list org-directory)
  "Seed Org files or directories; linked local Org files are also indexed."
  :type '(repeat file))
(defvar org-note-graph-collection-templates nil
  "Registered specifications, as (NAME . PLIST) pairs.")
(defvar org-note-graph--registry-revision 0)

(cl-defstruct org-note-graph-node key id file position end title level parent properties text)
(cl-defstruct org-note-graph-document stamp nodes links tree text)
(cl-defstruct org-note-graph-link source target file position type path)

(defun org-note-graph-register-collection (name &rest specification)
  "Register NAME with SPECIFICATION, replacing its previous definition.
:type identifies declarations through the local GRAPH_TYPE property.
:actions maps names to functions receiving a target node and context plist.
:declare optionally configures a newly declared node.
:query optionally receives a database and context key and returns node keys.
:initialize creates owned database tables.  :replace receives the database
and (FILE . DOCUMENT) changes; nil documents denote removed sources.
Index callbacks run within the core transaction and must not edit sources."
  (let ((type (plist-get specification :type)))
    (unless (and (stringp type) (not (string-empty-p type)))
      (error "Collection %s requires a :type string" name))
    (when (seq-some (lambda (entry)
                     (and (not (equal name (car entry)))
                          (equal type (plist-get (cdr entry) :type))))
                   org-note-graph-collection-templates)
      (error "Collection type already registered: %s" type))
    (setf (alist-get name org-note-graph-collection-templates nil nil #'equal)
          specification)
    (cl-incf org-note-graph--registry-revision)
    specification))

(defun org-note-graph-definition (type)
  "Return the specification for TYPE."
  (cdr (seq-find (lambda (entry) (equal type (plist-get (cdr entry) :type)))
                org-note-graph-collection-templates)))

(defun org-note-graph--property (node name)
  "Return NODE's nonempty local property NAME."
  (when-let* ((value (cdr (assoc name (org-note-graph-node-properties node)))))
    (unless (string-empty-p (string-trim value)) value)))

(defun org-note-graph-collection-definition (node)
  "Return NODE's specification, if registered."
  (org-note-graph-definition (org-note-graph--property node "GRAPH_TYPE")))

(defun org-note-graph--owner (nodes position)
  "Return the innermost node in source-ordered NODES at POSITION."
  (car (last (seq-filter
              (lambda (node)
                (and (<= (org-note-graph-node-position node) position)
                     (< position (org-note-graph-node-end node)))) nodes))))

(defun org-note-graph-map-owned-elements (node tree nodes element-type function)
  "Map FUNCTION over NODE's owned ELEMENT-TYPE objects in TREE.
Independent ID descendants in NODES own their elements separately."
  (org-element-map tree element-type
    (lambda (element)
      (when (eq node (org-note-graph--owner nodes (org-element-property :begin element)))
        (funcall function element)))))

(provide 'org-note-graph-model)
;;; org-note-graph-model.el ends here
