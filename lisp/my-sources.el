;;; my-sources.el --- Reading queue, source notes and bibliography -*- lexical-binding: t; -*-

;;; Commentary:
;; A source is a web page, a PDF or a bibliography entry.  The reading queue
;; (`my/reading-file') holds TODO items pointing at sources; a source note in
;; the note graph holds thoughts about one.  Notes are created on demand: when
;; reading starts with a prefix argument, or at the first captured thought.
;; Papers keep their BibTeX metadata in Citar; their notes carry CITEKEY and
;; NOTER_DOCUMENT, other sources carry SOURCE.  Citar reads the same notes.

;;; Code:
(require 'citar)
(require 'org)
(require 'org-id)
(require 'org-capture)
(require 'org-agenda)
(require 'subr-x)
(require 'org-note-graph-ui)

(declare-function eww-current-url "eww")
(declare-function eww "eww" (url &optional new-buffer buffer))
(declare-function pdf-view-active-region-p "pdf-view")
(declare-function pdf-view-active-region-text "pdf-view")
(declare-function bibtex-beginning-of-entry "bibtex")
(declare-function bibtex-clean-entry "bibtex" (&optional new-key called-by-reformat))
(declare-function bibtex-key-in-head "bibtex" (&optional empty))
(defvar eww-data)
(defvar bibtex-entry-format)
(defvar bibtex-autokey-edit-before-use)
(defvar url-mime-accept-string)
(defvar url-http-response-status)
(defvar url-http-end-of-headers)

(defgroup my/sources nil "Reading queue and source notes." :group 'org)
(defcustom my/source-bibliography (expand-file-name "references.bib" org-directory)
  "Shared bibliography used by source notes and Org citations."
  :type 'file)
(defcustom my/source-directory (expand-file-name "sources/" org-note-graph-capture-directory)
  "Directory for source notes."
  :type 'directory)
(defcustom my/reading-file (expand-file-name "reading.org" org-directory)
  "Reading queue: sources to read, as TODO items tagged reading."
  :type 'file)

;;;; Resources

;; A resource is (TYPE . VALUE): (citekey . KEY), (url . URL) or (file . PATH).

(defun my/source-url-p (text)
  "Whether TEXT is a web URL."
  (and text (string-match-p "\\`https?://[^[:space:]]+\\'" text)))

(defun my/source--arxiv-id (text)
  "Return the arXiv identifier in TEXT, or nil."
  (let ((id "\\([a-z-]+\\(?:\\.[A-Z]\\{2\\}\\)?/[0-9]\\{5,7\\}\\|[0-9]\\{4\\}\\.[0-9]\\{4,5\\}\\(?:v[0-9]+\\)?\\)"))
    (cond ((string-match (concat "\\`https?://arxiv\\.org/\\(?:abs\\|pdf\\|format\\)/" id) text) (match-string 1 text))
          ((string-match (concat "\\`arxiv:" id "\\'") (downcase text)) (match-string 1 text))
          ((string-match (concat "\\`" id "\\'") text) (match-string 1 text)))))

(defun my/source--doi (text)
  "Return the DOI in TEXT, or nil."
  (let ((doi "\\(10\\.[0-9]\\{4,9\\}/[-._;()/:A-Za-z0-9]+\\)"))
    (cond ((string-match (concat "\\`https?://\\(?:dx\\.\\)?doi\\.org/" doi "\\'") text) (match-string 1 text))
          ((string-match (concat "\\`doi:" doi "\\'") text) (match-string 1 text))
          ((string-match (concat "\\`" doi "\\'") text) (match-string 1 text)))))

;;;; Import

(defun my/source--fetch (url accept read)
  "Call READ at the body of URL's response, requesting media type ACCEPT.
Return its value, or nil when the request fails."
  (let ((url-mime-accept-string accept))
    (when-let* ((buffer (ignore-errors (url-retrieve-synchronously url t t 30))))
      (with-current-buffer buffer
        (unwind-protect
            (when (eql url-http-response-status 200)
              (goto-char url-http-end-of-headers)
              (forward-line)
              (funcall read))
          (kill-buffer buffer))))))

(defun my/source--text ()
  "Return the rest of the buffer, decoded as UTF-8."
  (decode-coding-string (buffer-substring-no-properties (point) (point-max)) 'utf-8))

(defun my/source--open-access-pdf (doi)
  "Return Unpaywall's URL for an open-access PDF of DOI, or nil."
  (my/source--fetch (format "https://api.unpaywall.org/v2/%s?email=%s" doi user-mail-address)
                    "application/json"
                    (lambda () (let-alist (json-parse-buffer :object-type 'alist :null-object nil)
                                 .best_oa_location.url_for_pdf))))

(defun my/source--save-pdf (url key)
  "Save the PDF at URL as KEY's library file; return the file, or nil."
  (let ((file (expand-file-name (concat key ".pdf") (car citar-library-paths))))
    (make-directory (file-name-directory file) t)
    (my/source--fetch url "application/pdf"
                      (lambda () (when (looking-at-p "%PDF")
                                   (let ((coding-system-for-write 'binary))
                                     (write-region (point) (point-max) file nil 'silent))
                                   file)))))

(defun my/source--key-where (field value)
  "Return the key of the bibliography entry whose FIELD is VALUE, or nil."
  (seq-find (lambda (key) (string-equal-ignore-case value (or (citar-get-value field key) "")))
            (hash-table-keys (citar-get-entries))))

(defun my/source--add-entry (bibtex)
  "Append the BibTeX entry BIBTEX to the bibliography and return its new key.
Publisher metadata is often incomplete, so missing mandatory fields pass."
  (with-current-buffer (find-file-noselect my/source-bibliography)
    (save-excursion
      (goto-char (point-max))
      (insert "\n" (string-trim bibtex) "\n")
      (bibtex-beginning-of-entry)
      (let ((bibtex-entry-format '(opts-or-alts numerical-fields realign))
            (bibtex-autokey-edit-before-use nil))
        (bibtex-clean-entry t))
      (bibtex-beginning-of-entry)
      (prog1 (bibtex-key-in-head) (save-buffer)))))

(defun my/source--import (field id bibtex-url pdf-url)
  "Return the key of the entry whose FIELD is ID, importing it when missing.
BIBTEX-URL serves the entry; PDF-URL is a function returning a PDF URL or nil."
  (if-let* ((key (my/source--key-where field id)))
      (progn (message "%s is already in the bibliography" key) key)
    (let* ((key (my/source--add-entry
                 (or (my/source--fetch bibtex-url "application/x-bibtex" #'my/source--text)
                     (user-error "No BibTeX entry for %s" id))))
           (pdf (when-let* ((url (funcall pdf-url))) (my/source--save-pdf url key))))
      (message (if pdf "Imported %s with its PDF"
                 "Imported %s; attach a PDF with `citar-add-file-to-library'")
               key)
      key)))

(defun my/source-import (text)
  "Resolve TEXT to a resource, importing a paper into the bibliography.
An arXiv reference brings its entry and PDF, a DOI its entry and an
open-access PDF when Unpaywall knows one.  A URL stays a URL."
  (let ((arxiv (my/source--arxiv-id text))
        (doi (my/source--doi text)))
    (cond (arxiv
           (let ((id (replace-regexp-in-string "v[0-9]+\\'" "" arxiv)))
             (cons 'citekey (my/source--import "eprint" id (concat "https://arxiv.org/bibtex/" id)
                                               (lambda () (concat "https://arxiv.org/pdf/" id))))))
          (doi
           (cons 'citekey (my/source--import "doi" doi (concat "https://doi.org/" doi)
                                             (lambda () (my/source--open-access-pdf doi)))))
          ((my/source-url-p text) (cons 'url text)))))

(defun my/source-add (text)
  "Import the paper TEXT names, a DOI or an arXiv reference."
  (interactive
   (let ((copied (string-trim (or (ignore-errors (current-kill 0 t)) ""))))
     (list (read-string "DOI or arXiv reference: "
                        (and (or (my/source--doi copied) (my/source--arxiv-id copied)) copied)))))
  (unless (eq (car-safe (my/source-import (string-trim text))) 'citekey)
    (user-error "Not a DOI or arXiv reference: %s" text)))

(defun my/source-new-entry ()
  "Add a bibliography entry by hand: insert a template at the end of the file.
Fill the fields, then C-c C-c cleans the entry and generates its key."
  (interactive)
  (find-file my/source-bibliography)
  (goto-char (point-max))
  (unless (bolp) (insert "\n"))
  (call-interactively #'bibtex-entry))

(defun my/source--file-citekey (file)
  "Return the citekey whose library files include FILE, or nil."
  (let ((library (citar-get-files)))
    (seq-find (lambda (key) (seq-some (lambda (candidate) (file-equal-p candidate file))
                                      (gethash key library)))
              (hash-table-keys library))))

(defun my/source-entry-resource ()
  "Return the resource described by the Org entry at point, or nil."
  (let ((key (org-entry-get nil "CITEKEY"))
        (source (org-entry-get nil "SOURCE")))
    (cond (key (cons 'citekey key))
          (source (cons (if (my/source-url-p source) 'url 'file) source))
          ((save-excursion
             (org-back-to-heading t)
             (let ((end (save-excursion (org-end-of-subtree t t))))
               (when (re-search-forward org-link-any-re end t)
                 (let ((url (or (match-string-no-properties 2) (match-string-no-properties 0))))
                   (and (my/source-url-p url) (cons 'url url))))))))))

(defun my/source-current-resource ()
  "Return the resource of the current buffer.
A page in EWW, a PDF, a queue item or agenda line, or a source note."
  (cond ((derived-mode-p 'eww-mode) (cons 'url (eww-current-url)))
        ((derived-mode-p 'pdf-view-mode)
         (if-let* ((key (my/source--file-citekey buffer-file-name))) (cons 'citekey key)
           (cons 'file buffer-file-name)))
        ((derived-mode-p 'org-agenda-mode)
         (org-agenda-with-point-at-orig-entry nil (my/source-entry-resource)))
        ((derived-mode-p 'org-mode)
         (or (unless (org-before-first-heading-p) (my/source-entry-resource))
             (org-with-wide-buffer (goto-char (point-min)) (my/source-entry-resource))))))

(defun my/source-resource-title (resource)
  "Return a title for RESOURCE from the bibliography, the page, or by asking."
  (pcase resource
    (`(citekey . ,key) (my/source-title (citar-get-entry key)))
    (`(url . ,url) (or (and (derived-mode-p 'eww-mode) (org-string-nw-p (plist-get eww-data :title)))
                       (read-string "Source title: " url)))
    (`(file . ,file) (file-name-base file))))

(defun my/source-open-resource (resource)
  "Show RESOURCE: a PDF through Citar, a page in EWW, a file in Emacs."
  (pcase resource
    (`(citekey . ,key)
     (if (gethash key (citar-get-files)) (citar-open-files key) (citar-open (list key))))
    (`(url . ,url) (eww url))
    (`(file . ,file) (find-file file))))

;;;; Source notes

(defun my/source-title (entry)
  "Return a single-line title for the Citar ENTRY."
  (string-clean-whitespace (or (citar-get-value "title" entry) (citar-get-value "=key=" entry))))

(defun my/source-tag (entry)
  "Describe the Citar ENTRY's publication type with a note tag."
  (pcase (downcase (citar-get-value "=type=" entry))
    ((or "book" "inbook" "incollection" "proceedings") "book")
    ((or "phdthesis" "mastersthesis") "thesis")
    ("techreport" "report")
    (_ "paper")))

(defun my/source-note-p (node)
  "Whether NODE is a source note rather than a queue item."
  (and (not (org-mem-entry-subtree-p node))
       (not (file-equal-p (org-mem-entry-file node) my/reading-file))))

(defun my/source-notes (&optional keys)
  "Return Citar's KEY-to-files map, restricted to KEYS when supplied."
  (org-note-graph-ensure)
  (let ((notes (make-hash-table :test #'equal)))
    (dolist (node (org-note-graph-nodes) notes)
      (when-let* (((my/source-note-p node))
                  (key (org-mem-entry-property "CITEKEY" node))
                  ((or (null keys) (member key keys))))
        (push (org-mem-entry-file node) (gethash key notes))))))

(defun my/source-note (resource)
  "Return the note describing RESOURCE, or nil."
  (org-note-graph-ensure)
  (pcase-let ((`(,type . ,value) resource))
    (seq-find (lambda (node)
                (and (my/source-note-p node)
                     (equal value (org-mem-entry-property (if (eq type 'citekey) "CITEKEY" "SOURCE") node))))
              (org-note-graph-nodes))))

(defun my/source-write-note (resource &optional title)
  "Create the note for RESOURCE with TITLE and return its node."
  (pcase-let* ((`(,type . ,value) resource)
               (entry (and (eq type 'citekey) (or (citar-get-entry value) (user-error "Unknown citekey %s" value))))
               (title (or title (my/source-resource-title resource)))
               (pdf (pcase type
                      ('citekey (let ((files (gethash value (citar-get-files)))) (and (length= files 1) (car files))))
                      ('file value)))
               (org-note-graph-capture-directory my/source-directory)
               (properties (append (if entry `(("CITEKEY" . ,value)) `(("SOURCE" . ,value)))
                                   (when pdf `(("NOTER_DOCUMENT" . ,(file-relative-name pdf my/source-directory))))))
               (node (org-note-graph-write-note title properties (list (if entry (my/source-tag entry) "web")))))
    (when entry
      (with-current-buffer (find-file-noselect (org-mem-entry-file node))
        (org-with-wide-buffer (goto-char (point-max)) (insert (format "[cite:@%s]\n" value)))
        (save-buffer)))
    node))

(defun my/source-ensure-note (resource &optional title)
  "Return the note for RESOURCE, creating it with TITLE when missing."
  (or (my/source-note resource) (my/source-write-note resource title)))

(defun my/source-associate (entry)
  "Associate this Org document with bibliography ENTRY, saving nothing."
  (interactive (list (citar-get-entry (citar-select-ref))))
  (unless (and (derived-mode-p 'org-mode) (buffer-file-name (buffer-base-buffer)))
    (user-error "Use a visiting Org document"))
  (barf-if-buffer-read-only)
  (unless entry (user-error "No bibliography entry selected"))
  (org-with-wide-buffer
   (goto-char (point-min))
   (let* ((key (citar-get-value "=key=" entry))
          (current (unless (org-at-heading-p) (org-entry-get nil "CITEKEY")))
          (other (seq-find (lambda (file) (not (file-equal-p file (buffer-file-name (buffer-base-buffer)))))
                           (gethash key (my/source-notes (list key))))))
     (when (and current (not (equal current key)))
       (user-error "This document already describes %s" current))
     (when other (user-error "Publication already has a note: %s" other))
     (atomic-change-group
       (when (org-at-heading-p) (insert "\n") (goto-char (point-min)))
       (org-entry-put nil "CITEKEY" key)
       (org-id-get-create)
       (goto-char (point-min))
       (org-note-graph-toggle-tag (my/source-tag entry))))))

;;;; Reading queue

(defun my/reading--map (function)
  "Call FUNCTION at each open queue item and return its non-nil results."
  (with-current-buffer (find-file-noselect my/reading-file)
    (org-with-wide-buffer
     (delq nil (org-map-entries function "/!TODO|NEXT" 'file)))))

(defun my/reading--items ()
  "Return (STATE . MARKER) for open queue items, in-progress ones first."
  (sort (my/reading--map (lambda () (cons (org-get-todo-state) (point-marker))))
        (lambda (a b) (and (equal (car a) "NEXT") (not (equal (car b) "NEXT"))))))

(defun my/reading--item-for (resource)
  "Return a marker at the queue item describing RESOURCE, or nil."
  (car (my/reading--map (lambda () (and (equal (my/source-entry-resource) resource) (point-marker))))))

(defun my/reading--select-item ()
  "Choose an open queue item and return a marker at it."
  (let* ((items (my/reading--items))
         (candidates (mapcar (lambda (item)
                               (cons (org-with-point-at (cdr item)
                                       (format "%s %s" (car item) (org-get-heading t t t t)))
                                     (cdr item)))
                             items)))
    (unless candidates (user-error "The reading queue is empty"))
    (cdr (assoc (completing-read "Read: " candidates nil t) candidates))))

(defun my/reading--item-at-point ()
  "Return a marker at the queue item at point, selecting one elsewhere."
  (cond ((derived-mode-p 'org-agenda-mode)
         (or (org-get-at-bol 'org-marker) (user-error "No item here")))
        ((and (derived-mode-p 'org-mode) (file-equal-p (or buffer-file-name "") my/reading-file)
              (not (org-before-first-heading-p)))
         (save-excursion (org-back-to-heading t) (point-marker)))
        (t (my/reading--select-item))))

(defun my/reading-capture-resource ()
  "Resolve the queue capture's source once.
The clipboard supplies a URL, DOI or arXiv reference; otherwise pick an entry."
  (or (org-capture-get :note-source)
      (let* ((text (string-trim (or (ignore-errors (current-kill 0)) "")))
             (resource (or (condition-case err (my/source-import text)
                             (error (user-error "Import failed: %s. Add the entry with C-c r b, then capture again"
                                                (error-message-string err))))
                           (cons 'citekey (citar-select-ref)))))
        (org-capture-put :note-source resource)
        resource)))

(defun my/reading-capture-title ()
  "Return the queue capture's title, for the template."
  (my/source-resource-title (my/reading-capture-resource)))

(defun my/reading-capture-properties ()
  "Return the queue capture's property line, for the template."
  (pcase (my/reading-capture-resource)
    (`(citekey . ,key) (format ":CITEKEY: %s" key))
    (`(,_ . ,value) (format ":SOURCE: %s" value))))

(defun my/reading-start (&optional create-note)
  "Start reading the queue item at point: mark it NEXT and open its source.
With CREATE-NOTE, open or create its note as well."
  (interactive "P")
  (let* ((marker (my/reading--item-at-point))
         (resource (org-with-point-at marker
                     (or (my/source-entry-resource) (user-error "This item names no source"))))
         (title (org-with-point-at marker (org-get-heading t t t t))))
    (org-with-point-at marker (org-todo "NEXT"))
    (my/source-open-resource resource)
    (when-let* ((node (if create-note (my/source-ensure-note resource title) (my/source-note resource))))
      (display-buffer (find-file-noselect (org-mem-entry-file node))))))

(defun my/reading-finish ()
  "Finish the source being read: mark its queue item DONE and record a verdict.
The verdict goes into the source note, or onto the queue item without one."
  (interactive)
  (let* ((resource (or (my/source-current-resource)
                       (org-with-point-at (my/reading--select-item) (my/source-entry-resource))))
         (marker (or (my/reading--item-for resource) (user-error "No open queue item for this source")))
         (verdict (string-trim (read-string "Verdict (empty to skip): ")))
         (node (my/source-note resource)))
    (org-with-point-at marker (org-todo "DONE"))
    (unless (string-empty-p verdict)
      (if node
          (with-current-buffer (find-file-noselect (org-mem-entry-file node))
            (org-with-wide-buffer
             (goto-char (point-max))
             (unless (bolp) (insert "\n"))
             (insert "\nVerdict: " verdict "\n"))
            (save-buffer))
        (org-with-point-at marker
          (org-end-of-meta-data t)
          (insert "Verdict: " verdict "\n"))))
    (with-current-buffer (marker-buffer marker) (save-buffer))
    (message "Finished %s" (org-with-point-at marker (org-get-heading t t t t)))))

;;;; Noter

(defun my/source-noter-notes (document)
  "Return the source note annotating DOCUMENT, for org-noter's notes lookup.
Noter otherwise only considers notes named after the document."
  (when-let* ((resource (if-let* ((key (my/source--file-citekey document))) (cons 'citekey key)
                          (cons 'file (expand-file-name document))))
              (node (my/source-note resource)))
    (org-mem-entry-file node)))

;;;; Capture into the source note

(defun my/reading-note-target ()
  "Position capture at the end of the current source's note, creating it if needed."
  (let* ((origin (org-capture-get :original-buffer))
         (resource (with-current-buffer origin
                     (or (my/source-current-resource) (user-error "No source in this buffer"))))
         (title (with-current-buffer origin
                  (or (when-let* ((marker (my/reading--item-for resource)))
                        (org-with-point-at marker (org-get-heading t t t t)))
                      (my/source-resource-title resource))))
         (node (my/source-ensure-note resource title)))
    (set-buffer (find-file-noselect (org-mem-entry-file node)))
    (widen)
    (goto-char (point-max))))

(defun my/reading-selection ()
  "Return the text selected in the capture's origin, for quote templates."
  (let ((origin (org-capture-get :original-buffer)))
    (string-trim
     (or (with-current-buffer origin
           (when (and (derived-mode-p 'pdf-view-mode) (pdf-view-active-region-p))
             (string-join (pdf-view-active-region-text) " ")))
         (org-capture-get :initial)
         ""))))

;;;; Citar notes source

(defun my/source-has-notes ()
  "Return a Citar predicate using one snapshot of the note graph."
  (let ((notes (my/source-notes)))
    (lambda (key) (gethash key notes))))

(defun my/source-open (file)
  "Open the source note FILE through the note graph."
  (org-note-graph-open (or (org-note-graph-file-node file) (user-error "Note is not indexed: %s" file))))

(defun my/source-create-note (key entry)
  "Create and open the note for KEY, described by the Citar ENTRY."
  (unless entry (user-error "No bibliography entry for %s" key))
  (org-note-graph-open (my/source-ensure-note (cons 'citekey key))))

(citar-register-notes-source
 'org-note-graph
 (list :name "Source notes"
       :category 'file
       :items #'my/source-notes
       :hasitems #'my/source-has-notes
       :open #'my/source-open
       :create #'my/source-create-note
       :transform #'file-name-nondirectory))

(provide 'my-sources)
;;; my-sources.el ends here
