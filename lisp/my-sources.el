;;; my-sources.el --- Bibliography, source notes and reading -*- lexical-binding: t; -*-
(require 'citar)
(require 'org-capture)
(require 'org-id)
(require 'bibtex)
(require 'url)
(require 'url-handlers)
(require 'dom)
(require 'org-note-graph-core)

(declare-function my/org-graph "my-notes" ())
(declare-function org-note-graph-query "org-note-graph-commands" (graph &optional text context))
(declare-function eww "eww" (url &optional new-buffer buffer))
(defvar url-http-response-status)
(defvar url-http-end-of-headers)
(defvar my/source-bibliography (expand-file-name "references.bib" org-directory))
(defvar my/source-directory (expand-file-name "sources/" org-directory))
(defvar my/source-capture-key nil "Bibliography key supplied by Citar for this capture.")

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

(defun my/source--key-where (field value)
  "Return the key of the bibliography entry whose FIELD is VALUE, or nil."
  (seq-find (lambda (key) (string-equal-ignore-case value (or (citar-get-value field key) "")))
            (hash-table-keys (citar-get-entries))))

(defun my/source--add-entry (bibtex)
  "Append the BibTeX entry BIBTEX to the bibliography and return its key.
Publisher metadata is often incomplete, so missing mandatory fields pass."
  (with-current-buffer (find-file-noselect my/source-bibliography)
    (bibtex-set-dialect 'biblatex t)
    (save-excursion
      (goto-char (point-max))
      (insert "\n" (string-trim bibtex) "\n")
      (bibtex-beginning-of-entry)
      (let ((bibtex-entry-format '(opts-or-alts numerical-fields realign))
            (bibtex-autokey-edit-before-use nil))
        (bibtex-clean-entry (not (org-string-nw-p (bibtex-key-in-head)))))
      (bibtex-beginning-of-entry)
      (prog1 (bibtex-key-in-head) (save-buffer)))))

(defun my/source-new-entry ()
  "Add a bibliography entry by hand: insert a template at the end of the file.
Fill the fields, then C-c C-c cleans the entry and generates its key."
  (interactive)
  (find-file my/source-bibliography)
  (goto-char (point-max))
  (unless (bolp) (insert "\n"))
  (call-interactively #'bibtex-entry))

(defun my/source-title (entry)
  "Return a single-line title for the Citar ENTRY."
  (string-clean-whitespace (or (citar-get-value "title" entry) (citar-get-value "=key=" entry))))

(defun my/source--online-entry (url &optional fields)
  "Draft an online bibliography entry for URL, fetching available metadata."
  (let* ((tree (my/source--fetch url "text/html" (lambda () (libxml-parse-html-region (point) (point-max)))))
         (meta (dom-by-tag tree 'meta)))
    (cl-labels ((value (&rest names)
                  (seq-some (lambda (name)
                              (when-let* ((node (seq-find (lambda (node)
                                                            (equal name (or (dom-attr node 'name) (dom-attr node 'property)))) meta)))
                                (dom-attr node 'content))) names)))
      (let ((fields (append fields
			    `(("title" . ,(or (value "citation_title" "og:title")
					      (org-string-nw-p (dom-inner-text (car (dom-by-tag tree 'title)))) url))
			      ("author" . ,(value "citation_author" "author"))
			      ("date" . ,(value "citation_publication_date" "article:published_time"))
			      ("url" . ,url) ("urldate" . ,(format-time-string "%Y-%m-%d"))))))
	(concat "@online{web-" (substring (secure-hash 'sha1 url) 0 12) ",\n"
		(mapconcat (pcase-lambda (`(,name . ,value))
					 (when (org-string-nw-p value)
					   (format "  %s = {%s}," name (string-clean-whitespace value))))
			   (seq-filter #'cdr fields) "\n") "\n}")))))

(defun my/source-import (text)
  "Return TEXT's bibliography key, importing metadata without downloading files."
  (let* ((arxiv (when-let* ((id (my/source--arxiv-id text))) (replace-regexp-in-string "v[0-9]+\\'" "" id)))
         (doi (my/source--doi text))
         (field (cond (arxiv "eprint") (doi "doi") (t "url")))
         (value (or arxiv doi text))
         (url (cond (arxiv (concat "https://arxiv.org/abs/" arxiv))
                    (doi (concat "https://doi.org/" doi)) (t text))))
    (unless (my/source-url-p url) (user-error "Enter a URL, DOI, or arXiv reference"))
    (or (my/source--key-where field value)
        (my/source--add-entry
         (or (when (or arxiv doi)
               (my/source--fetch (if arxiv (concat "https://arxiv.org/bibtex/" arxiv) url)
                                 "application/x-bibtex" #'my/source--text))
             (my/source--online-entry url (when (or arxiv doi) (list (cons field value)))))))))

(defun my/source-add (text)
  "Import bibliography metadata from TEXT without creating a note."
  (interactive "sURL, DOI, or arXiv reference: ")
  (message "Bibliography: %s" (my/source-import (string-trim text))))

(defun my/source-notes (&optional keys)
  "Return Citar's key-to-files map from document metadata, optionally for KEYS."
  (let ((notes (make-hash-table :test #'equal)))
    (dolist (location (org-note-graph-objects (my/org-graph)) notes)
      (when (eq (plist-get location :type) 'note)
        (when-let* ((key (cdr (assoc "CITEKEY" (plist-get location :properties))))
                    ((or (not keys) (member key keys))))
          (push (plist-get location :file) (gethash key notes)))))))

(defun my/source-capture-target ()
  "Select or import a reference, then position capture in its source note."
  (let* ((text (unless my/source-capture-key
                 (string-trim (read-from-minibuffer "URL, DOI, arXiv (empty selects bibliography): "
                                                    nil nil nil nil (ignore-errors (current-kill 0 t))))))
         (key (or my/source-capture-key (if (string-empty-p text) (citar-select-ref) (my/source-import text))))
         (notes (gethash key (my/source-notes (list key))))
         (file (if notes (if (cdr notes) (completing-read "Source note: " notes nil t) (car notes))
                 (expand-file-name (concat (string-replace "/" "_" key) ".org") my/source-directory))))
    (unless (or notes (file-in-directory-p file my/source-directory)) (user-error "Invalid bibliography key"))
    (make-directory my/source-directory t)
    (set-buffer (find-file-noselect file))
    (widen) (goto-char (point-max))
    (when (and (> (buffer-size) 0)
               (not (equal key (save-excursion (goto-char (point-min)) (org-entry-get nil "CITEKEY")))))
      (user-error "Existing source file has no matching CITEKEY: %s" file))
    (org-capture-put :source-key key :source-new (= (point-min) (point-max)))))

(defun my/source-capture-header ()
  "Initialize a new source note with metadata and an ordinary reading tag."
  (when (org-capture-get :source-new)
    (let ((key (org-capture-get :source-key)))
      (insert (format ":PROPERTIES:\n:ID: %s\n:CITEKEY: %s\n:END:\n#+title: %s\n#+filetags: :reading:\n\n[cite:@%s]\n\n"
                      (org-id-new) key (my/source-title (citar-get-entry key)) key)))))

(defun my/source-capture (&optional key)
  "Capture a source bookmark; KEY is supplied when called by Citar."
  (interactive)
  (let ((my/source-capture-key key)) (org-capture nil "s")))

(defun my/source-capture-save ()
  "Save the source note after successful capture, leaving aborts unsaved."
  (unless org-note-abort
    (with-current-buffer (marker-buffer org-capture-last-stored-marker)
      (save-buffer)
      (org-with-wide-buffer
       (save-excursion
         (goto-char (point-min))
         (when-let* ((id (org-entry-get nil "ID"))) (org-id-add-location id buffer-file-name)))))))

(defun my/source-associate (entry)
  "Associate this file with bibliography ENTRY, leaving the buffer modified."
  (interactive (list (citar-get-entry (citar-select-ref))))
  (unless (and entry (derived-mode-p 'org-mode) buffer-file-name) (user-error "Use a visiting Org file"))
  (org-with-wide-buffer
   (save-excursion
     (goto-char (point-min))
     (when (org-at-heading-p) (insert "\n") (goto-char (point-min)))
     (org-entry-put nil "CITEKEY" (citar-get-value "=key=" entry)))))

(defun my/source-acquire (key)
  "Fetch KEY's supported PDF lazily, using Citar's library placement."
  (let* ((arxiv (citar-get-value "eprint" key))
         (doi (citar-get-value "doi" key))
         (url (cond ((org-string-nw-p arxiv) (concat "https://arxiv.org/pdf/" arxiv))
                    ((org-string-nw-p doi) (my/source--open-access-pdf doi)))) file)
    (when url
      (funcall citar-add-file-function key
               (list :extension "pdf" :write-file
                     (lambda (destination overwrite)
                       (url-copy-file url destination overwrite)
                       (setq file destination)))))
    file))

(defun my/source-show-document ()
  "Open this source note's document alongside the note in the current frame."
  (interactive)
  (unless (and (derived-mode-p 'org-mode) buffer-file-name) (user-error "Use a source note"))
  (let* ((note (current-buffer))
         (key (org-with-wide-buffer (save-excursion (goto-char (point-min)) (org-entry-get nil "CITEKEY"))))
         (_ (unless key (user-error "This note has no CITEKEY")))
         (files (gethash key (citar-get-files (list key))))
         (file (if files (if (cdr files) (completing-read "Document: " files nil t) (car files))
                 (condition-case err (my/source-acquire key)
                   (error (message "Document retrieval: %s" (error-message-string err)) nil))))
         (url (or (org-string-nw-p (citar-get-value "url" key))
                  (when-let* ((doi (org-string-nw-p (citar-get-value "doi" key)))) (concat "https://doi.org/" doi))
                  (when-let* ((id (org-string-nw-p (citar-get-value "eprint" key)))) (concat "https://arxiv.org/abs/" id)))))
    (unless (or file url) (user-error "No document or URL; attach one with citar-add-file-to-library"))
    (display-buffer note '((display-buffer-reuse-window display-buffer-pop-up-window) (inhibit-same-window . t)))
    (if file (find-file file) (eww url))))

(defun my/source-queue ()
  "Find a reading-tagged note, independently of bibliography and acquisition."
  (interactive)
  (org-note-graph-query (my/org-graph) "location[type=note] & #reading"))

(defun my/source-has-notes ()
  "Return Citar's predicate using one document snapshot."
  (let ((notes (my/source-notes))) (lambda (key) (gethash key notes))))

(defun my/source-create-note (key entry)
  "Capture a source note for KEY, described by ENTRY."
  (unless entry (user-error "Unknown bibliography entry %s" key))
  (my/source-capture key))

(citar-register-notes-source
 'source-notes
 (list :name "Source notes" :category 'file :items #'my/source-notes :hasitems #'my/source-has-notes
       :open (lambda (file) (pop-to-buffer (find-file-noselect file))) :create #'my/source-create-note
       :transform #'file-name-nondirectory))

(provide 'my-sources)
