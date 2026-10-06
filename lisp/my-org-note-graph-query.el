;;; my-org-note-graph-query.el --- Symbolic location and graph queries -*- lexical-binding: t; -*-

;;; Code:
(require 'org-note-graph-labels)
(require 'org-note-graph-text)

(defcustom my/org-note-graph-alias-sources '("Aliases")
  "Destinations and existing label nodes supplying alternate names."
  :type '(repeat string) :group 'org-note-graph)

(defun my/org-note-graph-query--tokens (text)
  "Tokenize TEXT without evaluating Lisp."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let ((via "-\\(\\$?[[:alnum:]_]+\\)>")
          (operators (regexp-opt '("-*>" "<*-" "->" "<-" "(" ")" "&" "|" "\\" "!" ";" "." "*" "@")))
          tokens)
      (while (progn (skip-chars-forward " \t\n") (not (eobp)))
        (cond
         ((looking-at via)
          (let ((name (match-string-no-properties 1)))
            (push (list 'via (if (string-prefix-p "$" name)
                                (list 'variable (substring name 1))
                              (list 'name name nil))) tokens))
          (goto-char (match-end 0)))
         ((looking-at org-link-bracket-re)
          (let* ((text (match-string-no-properties 0))
                 (end (match-end 0))
                 (link (org-element-map (org-element-parse-secondary-string text '(link)) 'link #'identity nil t))
                 (key (org-note-graph-reference link (expand-file-name "query.org"))))
            (unless key (user-error "Expected an ID or local Org file link"))
            (push (list 'key key) tokens)
            (goto-char end)))
         ((looking-at "\\[\\[") (user-error "Unclosed Org link"))
         ((eq (char-after) ?\")
          (condition-case nil
              (push (list 'name (read (current-buffer)) t) tokens)
            (error (user-error "Unclosed or invalid quoted name"))))
         ((looking-at "[?$][[:alnum:]_]*")
          (let ((name (substring (match-string 0) 1)))
            (when (and (eq (char-after) ?$) (string-empty-p name))
              (user-error "Expected a name after $"))
            (push (list (if (eq (char-after) ??) 'hole 'variable)
                        (unless (string-empty-p name) name)) tokens)
            (goto-char (match-end 0))))
         ((looking-at operators)
          (push (match-string-no-properties 0) tokens)
          (goto-char (match-end 0)))
         (t
          (let ((start (point)))
            (while (and (not (eobp))
                        (not (looking-at via))
                        (not (looking-at "[ \t\n()&|\\\\!;?$*<>\"@]\\|->\\|-\\*>")))
              (forward-char))
            (when (= start (point)) (user-error "Unexpected character at %d" start))
            (push (list 'name (buffer-substring-no-properties start (point)) nil) tokens)))))
      (nreverse tokens))))

(defun my/org-note-graph-query--parse (text)
  "Parse TEXT into set expressions, one per semicolon-separated clause."
  (let ((tokens (my/org-note-graph-query--tokens text)))
    (cl-labels
        ((atom-expression ()
           (let ((token (pop tokens)))
             (cond
              ((equal token "(")
               (prog1 (expression 0)
                 (unless (equal (pop tokens) ")") (user-error "Expected closing parenthesis"))))
              ((equal token ".") '(here))
              ((equal token "*") '(all))
              ((and (consp token) (not (eq (car token) 'via))) token)
              (t (user-error "Expected a node set, got %s" (or token "end of query"))))))
         (term-expression ()
           (if (equal (car tokens) "!")
               (progn (pop tokens) (list 'not (term-expression)))
             (let ((term (atom-expression)))
               (if (equal (car tokens) "@")
                   (progn (pop tokens) (list 'scope term (atom-expression)))
                 term))))
         (path-expression ()
           (let ((terms (list (term-expression))) arrows)
             (while (or (member (car tokens) '("->" "<-" "-*>" "<*-"))
                        (eq (car-safe (car tokens)) 'via))
               (push (pop tokens) arrows)
               (push (term-expression) terms))
             (if (null arrows) (car terms)
               (unless (= 1 (seq-count (lambda (term) (eq (car term) 'hole)) terms))
                 (user-error "Each path needs exactly one ? or ?name"))
               (list 'path (nreverse terms) (nreverse arrows)))))
         (expression (precedence)
           (let ((left (path-expression)))
             (while (let ((level (cdr (assoc (car tokens) '(("|" . 1) ("&" . 2) ("\\" . 2))))))
                      (when (and level (> level precedence))
                        (let ((operator (pop tokens)))
                          (setq left (list operator left (expression level))))
                        t)))
             left)))
      (let (clauses)
        (push (expression 0) clauses)
        (while tokens
          (unless (equal (pop tokens) ";") (user-error "Expected an operator or semicolon"))
          (when tokens (push (expression 0) clauses)))
        (nreverse clauses)))))

(defun my/org-note-graph-query--holes (expression)
  "Collect hole names in EXPRESSION, including nil for anonymous holes."
  (when (consp expression)
    (if (eq (car expression) 'hole) (list (cadr expression))
      (seq-mapcat #'my/org-note-graph-query--holes expression))))

(defun my/org-note-graph-query--pattern (name exact)
  "Match literal NAME, anchored when EXACT is non-nil."
  (if exact (concat "\\`" (regexp-quote name) "\\'") (regexp-quote name)))

(defun my/org-note-graph-query--same-location-p (left right)
  "Compare positions, preserving distinct whole-node IDs at a shared position."
  (and (equal (org-note-graph-location-file left) (org-note-graph-location-file right))
       (= (org-note-graph-location-position left) (org-note-graph-location-position right))
       (equal (unless (org-note-graph-location-search left) (org-note-graph-location-id left))
              (unless (org-note-graph-location-search right) (org-note-graph-location-id right)))))

(defun my/org-note-graph-query-lookup (db regexp scopes)
  "Find precise locations named by labels or scanner contributions in SCOPES."
  (seq-uniq
   (append (mapcar (lambda (key) (org-note-graph-node db key))
                   (org-note-graph-labels-query db regexp scopes))
           (org-note-graph-locations-lookup db regexp scopes))
   #'my/org-note-graph-query--same-location-p))

(defun my/org-note-graph-query--names (db name exact)
  "Find NAME in titles and aliases in DB; EXACT requires a complete name."
  (let ((case-fold-search t)
        (pattern (my/org-note-graph-query--pattern name exact)))
    (seq-union
     (org-note-graph-select db (lambda (node) (string-match-p pattern (org-note-graph-node-title node))))
     (org-note-graph-location-nodes
      db (my/org-note-graph-query-lookup db pattern my/org-note-graph-alias-sources)) #'equal)))

(defun my/org-note-graph-query--scopes (expression db context resolve)
  "Resolve a destination or node-set EXPRESSION for scoped lookup."
  (pcase expression
    (`(name ,name ,exact)
     (let ((case-fold-search t) (pattern (my/org-note-graph-query--pattern name exact)))
       (seq-union (my/org-note-graph-query--names db name exact)
                  (seq-filter (lambda (destination) (string-match-p pattern destination))
                              (org-note-graph-data-destinations 'named-location)) #'equal)))
    (`(all) (append (mapcar #'org-note-graph-node-key (org-note-graph-nodes db))
                    (org-note-graph-data-destinations 'named-location)))
    (`(not ,term)
     (seq-difference (my/org-note-graph-query--scopes '(all) db context resolve)
                     (my/org-note-graph-query--scopes term db context resolve) #'equal))
    (`(,(and operator (or "&" "|" "\\")) ,left ,right)
     (funcall (pcase operator ("&" #'seq-intersection) ("|" #'seq-union) ("\\" #'seq-difference))
              (my/org-note-graph-query--scopes left db context resolve)
              (my/org-note-graph-query--scopes right db context resolve) #'equal))
    (_ (org-note-graph-location-nodes db (my/org-note-graph-query--eval expression db context resolve)))))

(defun my/org-note-graph-query--walk (terms arrows db context resolve reverse names)
  "Follow TERMS and ARROWS toward a hole, reversing edges when REVERSE is set."
  (if (null terms) (mapcar #'org-note-graph-node-key (org-note-graph-nodes db))
    (let ((keys (org-note-graph-location-nodes
                 db (my/org-note-graph-query--eval (pop terms) db context resolve names))))
      (dolist (arrow arrows keys)
        (let ((step (if (xor reverse (and (stringp arrow) (string-prefix-p "<" arrow)))
                        #'org-note-graph-in #'org-note-graph-out)))
          (setq keys
                (if (consp arrow)
                    (let ((expandable
                           (seq-union keys (org-note-graph-location-nodes
                                            db (my/org-note-graph-query--eval (cadr arrow) db context resolve names))
                                      #'equal)))
                      (org-note-graph-closure
                       db keys (lambda (db frontier)
                                 (funcall step db (seq-intersection frontier expandable #'equal)))))
                  (if (= (length arrow) 3) (org-note-graph-closure db keys step)
                    (funcall step db keys)))))
        (when terms
          (setq keys (seq-intersection keys
                                      (org-note-graph-location-nodes
                                       db (my/org-note-graph-query--eval (pop terms) db context resolve names))
                                      #'equal)))))))

(defun my/org-note-graph-query--eval (expression db context resolve &optional names)
  "Evaluate EXPRESSION in DB, using CONTEXT and named-set RESOLVE.
Return locations.  NAMES optionally supplies scoped location lookup."
  (pcase expression
    (`(name ,name ,exact)
     (if names (funcall names db name exact)
       (mapcar (lambda (key) (org-note-graph-node db key)) (my/org-note-graph-query--names db name exact))))
    (`(scope ,term ,tables)
     (let ((sources (my/org-note-graph-query--scopes tables db context resolve)))
       (my/org-note-graph-query--eval
        term db context resolve
        (lambda (db name exact)
          (let ((case-fold-search t))
            (my/org-note-graph-query-lookup db (my/org-note-graph-query--pattern name exact) sources))))))
    (`(key ,key) (when-let* ((key (org-note-graph-resolve db key))) (list (org-note-graph-node db key))))
    (`(variable ,name) (funcall resolve name))
    (`(here) (unless (and context (org-note-graph-node db context))
               (user-error "This query needs a current node for ."))
     (list (org-note-graph-node db context)))
    ((or `(all) `(hole ,_)) (if names (funcall names db "" nil) (org-note-graph-nodes db)))
    (`(not ,term)
     (seq-difference (my/org-note-graph-query--eval '(all) db context resolve names)
                     (my/org-note-graph-query--eval term db context resolve names) #'my/org-note-graph-query--same-location-p))
    (`(path ,terms ,arrows)
     (let ((hole (cl-position-if (lambda (term) (eq (car term) 'hole)) terms)))
       (mapcar
        (lambda (key) (org-note-graph-node db key))
        (seq-intersection
         (my/org-note-graph-query--walk (seq-take terms hole) (seq-take arrows hole) db context resolve nil names)
         (my/org-note-graph-query--walk (reverse (seq-drop terms (1+ hole)))
                                      (reverse (seq-drop arrows hole)) db context resolve t names) #'equal))))
    ((or `("&" (hole ,_) ,term) `("&" ,term (hole ,_)))
     (my/org-note-graph-query--eval term db context resolve names))
    (`(,operator ,left ,right)
     (funcall (pcase operator ("&" #'seq-intersection) ("|" #'seq-union) ("\\" #'seq-difference))
              (my/org-note-graph-query--eval left db context resolve names)
              (my/org-note-graph-query--eval right db context resolve names) #'my/org-note-graph-query--same-location-p))))

(defun my/org-note-graph-query-compile (text)
  "Translate TEXT to a query function accepting a database and context key.
Named holes define intersected sets; $name references their completed value.
The final clause returns locations; arrows traverse their owning nodes.
Named dependencies must be acyclic."
  (let* ((clauses (my/org-note-graph-query--parse text))
         (result (car (last clauses))) definitions)
    (dolist (clause (butlast clauses))
      (let ((holes (my/org-note-graph-query--holes clause)))
        (unless (and (= (length holes) 1) (car holes))
          (user-error "Each definition needs one named hole, such as ?x"))
        (push clause (alist-get (car holes) definitions nil nil #'equal))))
    (when (seq-some #'identity (my/org-note-graph-query--holes result))
      (user-error "Use $name to read a named set in the final clause"))
    (lambda (db context)
      (let ((cache (make-hash-table :test #'equal)))
        (cl-labels
            ((value (name &optional trail)
               (when (member name trail) (user-error "Circular definition of $%s" name))
               (let ((cached (gethash name cache 'absent)))
                 (if (not (eq cached 'absent)) cached
                   (let* ((bodies (or (cdr (assoc name definitions))
                                      (user-error "Undefined set $%s" name)))
                          (sets (mapcar
                                 (lambda (body)
                                   (my/org-note-graph-query--eval
                                    body db context (lambda (other) (value other (cons name trail))))) bodies)))
                     (puthash name (seq-reduce (lambda (a b) (seq-intersection a b #'my/org-note-graph-query--same-location-p))
                                              (cdr sets) (car sets)) cache))))))
          (dolist (definition definitions) (value (car definition)))
          (my/org-note-graph-query--eval result db context #'value))))))

(provide 'my-org-note-graph-query)
;;; my-org-note-graph-query.el ends here
