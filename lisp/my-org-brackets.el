;;; my-org-brackets.el --- Block brackets under org-indent -*- lexical-binding: t; -*-

;;; Commentary:
;; org-modern draws block brackets in `line-prefix', which org-indent uses for
;; indentation, so org-modern turns them off under org-indent.  This mode adds
;; the bracket to org-indent's own prefixes, as org-indent computes them, in
;; one of two styles:
;;
;;   `indent'  org-modern's shape in the indentation, touching the block: a
;;             thin bar between ticks level with the block names.  A block
;;             before the first heading has no indentation and moves half a
;;             column in.
;;   `fringe'  org-modern's bitmaps at the text's left edge.
;;
;; Emacs places a tick inside a line only in an image, and an image lines up
;; with a line's top only when it sets the line's height.  So the begin and end
;; corners are images exactly as tall as the glyphs on those lines, measured
;; once the buffer is shown and again when its text scale changes; lines keep
;; their height.  The `indent' style needs a graphical frame.  Compare the
;; styles with `my/org-brackets-toggle-style'.

;;; Code:
(require 'org)
(require 'org-indent)

(declare-function org-modern--update-bitmaps "org-modern" ())
(defvar org-modern-block-fringe)
(defvar my/org-brackets-mode)

(defcustom my/org-brackets-style 'indent
  "How `my/org-brackets-mode' draws block brackets: `indent' or `fringe'."
  :type '(choice (const indent) (const fringe))
  :group 'org-indent)

(defcustom my/org-brackets-corner '(0.85 . 0.3)
  "Space a corner takes (ABOVE . BELOW) the baseline, in lines of text, until
the buffer is shown and the glyphs of its begin and end lines are measured.
Those lines take the corner's height, so it must hold their tallest glyphs."
  :type '(cons number number)
  :group 'org-indent)

(defcustom my/org-brackets-corner-floor 1.5
  "The largest `line-height' on begin and end lines, in frame lines.  The
corner takes the space it adds above the text, so the lines keep their height
and the corner its place at the top."
  :type 'number
  :group 'org-indent)

(defconst my/org-brackets--marker-re "^[ \t]*#\\+\\(begin\\|end\\)_"
  "A block's begin or end line.")

(defvar-local my/org-brackets--pieces nil
  "Bracket pieces by line kind (`begin', `inner', `end') and the `fill' before
them, for this buffer's style and font size.  Begin and end pieces stand in
until lines can be measured.")

(defvar-local my/org-brackets--sizes nil
  "This buffer's sizes, see `my/org-brackets--metrics'.")

(defvar-local my/org-brackets--corners nil
  "Corners built for this buffer, by line kind and glyph extent.")

(defvar-local my/org-brackets--measured nil
  "Whether the pieces were made while the buffer was shown in a window.")

(defvar my/org-brackets--inside nil
  "During an org-indent pass, whether the next line lies inside a block.")

(defvar my/org-brackets--window nil
  "During an org-indent pass, the window begin and end lines are measured in.")

(defvar-local my/org-brackets--marker-touched nil
  "Whether the coming change touches a block's begin or end line.")

;;;; Pieces

(defun my/org-brackets--line-extent (window)
  "Return (RISE FALL NEWLINE-RISE NEWLINE-FALL LABEL) for the current line's
glyphs in WINDOW: the most they rise above and fall below the baseline, how far
the newline's font rises and falls, and the font size of the first glyph, the
block name.  A display string counts once, in the font of the text it
replaces; text org-modern hides does not count, text a fold hides does, so
unfolding keeps the size."
  (let ((pos (pos-bol)) (eol (pos-eol)) fonts)
    (while (< pos eol)
      (let ((display (get-char-property pos 'display))
            (next (next-single-char-property-change pos 'display nil eol)))
        (cond ((eq (get-char-property pos 'invisible) 'org-modern)
               (setq pos (next-single-char-property-change pos 'invisible nil eol)))
              ;; A display string takes the face of the text it replaces, and
              ;; only a buffer position gets the buffer's text scale.
              ((stringp display)
               (when-let* ((font (font-at pos window))) (push font fonts))
               (setq pos next))
              (display (setq pos next))
              (t (when-let* ((font (font-at pos window))) (push font fonts))
                 (setq pos (1+ pos))))))
    (let* ((glyphs (mapcar #'font-info (nreverse fonts)))
           (newline (and (< eol (point-max)) (font-at eol window)))
           (newline (and newline (font-info newline))))
      (and (or glyphs newline)
           (list (seq-max (cons 0 (mapcar (lambda (font) (aref font 8)) glyphs)))
                 (seq-max (cons 0 (mapcar (lambda (font) (aref font 9)) glyphs)))
                 (and newline (aref newline 8))
                 (and newline (aref newline 9))
                 (aref (or (car glyphs) newline) 2))))))

(defun my/org-brackets--metrics ()
  "Return this buffer's sizes in a graphical frame, or nil without one: a
plist of its font :height and :width, the block name face's font size
\(:label), the :floor `my/org-brackets-corner-floor' asks for, the frame
font's :frame-descent, the bracket :color and the :background."
  (when-let* ((frame (if (display-graphic-p) (selected-frame) (seq-find #'display-graphic-p (frame-list)))))
    (with-selected-frame frame
      (list :height (default-font-height) :width (default-font-width)
            :label (aref (font-info (or (and (facep 'org-modern-block-name) (face-font 'org-modern-block-name))
                                        (face-font 'default)))
                         2)
            ;; `line-height' counts in the frame's line height and font.
            :floor (truncate (* my/org-brackets-corner-floor (frame-char-height)))
            :frame-descent (aref (font-info (frame-parameter nil 'font)) 9)
            :color (or (face-foreground 'org-block-begin-line nil t) "gray50")
            :background (or (face-background 'default nil t) "white")))))

(defun my/org-brackets--corner-size (extent sizes)
  "Return (ABOVE . BELOW) the baseline for a corner of a line with EXTENT, see
`my/org-brackets--line-extent': as much as its glyphs take, so the line keeps
its height.  The `line-height' the floor in SIZES stands for makes the newline
as tall as the floor and as deep as the frame font, whatever its own font.
Without an extent, `my/org-brackets-corner' stands in."
  (pcase-let* ((`(,rise ,fall ,newline-rise ,newline-fall) extent)
               (floor (plist-get sizes :floor))
               (frame-fall (plist-get sizes :frame-descent))
               (height (plist-get sizes :height))
               (`(,newline-above . ,newline-below)
                (if (> floor 0) (cons (- floor frame-fall) frame-fall)
                  (cons (or newline-rise 0) (or newline-fall 0)))))
    (cons (max (if extent rise (ceiling (* (car my/org-brackets-corner) height))) newline-above)
          (max (if extent fall (ceiling (* (cdr my/org-brackets-corner) height))) newline-below))))

(defun my/org-brackets--image (width height ascent rects)
  "Return an SVG image WIDTH by HEIGHT pixels, ASCENT percent above the
baseline, filled with RECTS, each (X Y WIDTH HEIGHT COLOR)."
  `(image :type svg :scale 1.0 :ascent ,ascent
          :data ,(concat (format "<svg xmlns='http://www.w3.org/2000/svg' width='%d' height='%d'>" width height)
                         (mapconcat (lambda (rect) (apply #'format "<rect x='%d' y='%d' width='%d' height='%d' fill='%s'/>" rect))
                                    rects "")
                         "</svg>")))

(defun my/org-brackets--corner (kind sizes extent)
  "Return the corner for a KIND line, `begin' or `end', of glyph EXTENT (see
`my/org-brackets--line-extent'), built from SIZES: a one-pixel column with the
bar in its lower or upper part, and a tick level with the top of the block
name's lowercase letters.  The begin column's background paints the line
spacing below it, joining the next line's bar."
  (pcase-let* ((`(,above . ,below) (my/org-brackets--corner-size extent sizes))
               (tall (+ above below))
               ;; Emacs truncates the image's ascent, so round the percentage up.
               (ascent (ceiling (* 100.0 above) tall))
               (tick (max 1 (- above (round (* 0.53 (or (nth 3 extent) (plist-get sizes :label)))))))
               (wide (my/org-brackets--wide sizes))
               (color (plist-get sizes :color))
               (background (plist-get sizes :background)))
    (concat (propertize " " 'face `(:background ,(if (eq kind 'begin) color background))
                        'display (my/org-brackets--image
                                  1 tall ascent (if (eq kind 'begin)
                                                    `((0 0 1 ,tick ,background) (0 ,tick 1 ,(- tall tick) ,color))
                                                  `((0 0 1 ,(1+ tick) ,color) (0 ,(1+ tick) 1 ,(- tall tick 1) ,background)))))
            (propertize " " 'display (my/org-brackets--image (1- wide) tall ascent `((0 ,tick ,(1- wide) 1 ,color)))))))

(defun my/org-brackets--wide (sizes)
  "Return a bracket's width in pixels for SIZES: half a column."
  (max 3 (round (* 0.5 (plist-get sizes :width)))))

(defun my/org-brackets--indent-pieces (sizes)
  "Return the `indent' pieces for SIZES, see `my/org-brackets--metrics'.
A bracket touches the text.  Inner lines get a one-pixel space painted over
its whole row, line spacing included, and low so it leaves the line's height
alone; begin and end lines get corners, these sized without measuring."
  (let ((wide (my/org-brackets--wide sizes))
        (color (plist-get sizes :color)))
    `((begin . ,(my/org-brackets--corner 'begin sizes nil))
      (inner . ,(concat (propertize " " 'display '(space :width (1) :height 0.1) 'face `(:background ,color))
                        (propertize " " 'display `(space :width (,(1- wide)) :height 0.1) 'face 'org-indent)))
      (end . ,(my/org-brackets--corner 'end sizes nil))
      (fill . ,(propertize " " 'display `(space :width (- width (,wide)) :height 0.1) 'face 'org-indent)))))

(defun my/org-brackets--prepare ()
  "Make this buffer's sizes and pieces for `my/org-brackets-style'; without a
graphical frame to size them, the `indent' style has none."
  (setq my/org-brackets--corners (make-hash-table :test #'equal)
        my/org-brackets--measured (and (get-buffer-window nil t) t))
  (if (eq my/org-brackets-style 'fringe)
      (progn
        (require 'org-modern)
        (let ((org-modern-block-fringe 2)) (org-modern--update-bitmaps))
        (setq my/org-brackets--sizes nil
              my/org-brackets--pieces
              (mapcar (lambda (kind)
                        (cons kind (propertize " " 'display `(left-fringe ,(intern (format "org-modern--block-%s" kind))
                                                                          org-block-begin-line))))
                      '(begin inner end))))
    (setq my/org-brackets--sizes (my/org-brackets--metrics)
          my/org-brackets--pieces (and my/org-brackets--sizes (my/org-brackets--indent-pieces my/org-brackets--sizes)))))

(defun my/org-brackets--piece (kind)
  "Return the piece for the current line, of KIND.  A begin or end line gets a
corner fitting its glyphs when `my/org-brackets--window' can measure them."
  (if-let* (((memq kind '(begin end)))
            (window my/org-brackets--window)
            (extent (my/org-brackets--line-extent window)))
      (let ((key (cons kind extent)))
        (or (gethash key my/org-brackets--corners)
            (puthash key (my/org-brackets--corner kind my/org-brackets--sizes extent) my/org-brackets--corners)))
    (alist-get kind my/org-brackets--pieces)))

(defun my/org-brackets--prefix (prefix piece)
  "Return org-indent's PREFIX with the bracket PIECE.
The `indent' piece takes the place of the prefix's last column; without
indentation it stands before the text."
  (cond ((eq my/org-brackets-style 'fringe) (concat prefix piece))
        ((string-empty-p prefix) piece)
        (t (concat (substring prefix 0 -1) (alist-get 'fill my/org-brackets--pieces) piece))))

;;;; Inside org-indent's pass

(defun my/org-brackets--inside-p (pos)
  "Whether the line at POS lies inside a block, judged by the marker above it."
  (save-excursion
    (goto-char pos)
    (forward-line 0)
    (let ((case-fold-search t))
      (and (re-search-backward my/org-brackets--marker-re nil t)
           (string-equal-ignore-case (match-string 1) "begin")))))

(defun my/org-brackets--add-properties (add beg end &optional delay)
  "Run ADD, org-indent's pass from BEG to END with DELAY, knowing whether BEG
lies inside a block."
  (when (and my/org-brackets-mode (not my/org-brackets--pieces))
    (my/org-brackets--prepare))
  (let ((my/org-brackets--inside (and my/org-brackets-mode my/org-brackets--pieces
                                      (my/org-brackets--inside-p beg)))
        (my/org-brackets--window (and my/org-brackets-mode (eq my/org-brackets-style 'indent)
                                      (get-buffer-window nil t))))
    (funcall add beg end delay)))

(defun my/org-brackets--set-line (set level indentation &optional heading)
  "Run SET, org-indent's prefix for one line at LEVEL with INDENTATION and
HEADING, then add the bracket to a block's lines."
  (if (not (and my/org-brackets-mode my/org-brackets--pieces))
      (funcall set level indentation heading)
    (let* ((start (point))
           (kind (cond (heading nil)
                       ((looking-at my/org-brackets--marker-re)
                        (if (string-equal-ignore-case (match-string 1) "begin") 'begin 'end))
                       (my/org-brackets--inside 'inner)))
           ;; Measured on this line, before SET moves to the next.
           (piece (and kind (my/org-brackets--piece kind))))
      (funcall set level indentation heading)
      (when kind
        (let ((line (get-text-property start 'line-prefix))
              (wrap (get-text-property start 'wrap-prefix)))
          (add-text-properties
           start (point)
           `(line-prefix ,(my/org-brackets--prefix line piece)
             wrap-prefix ,(concat (my/org-brackets--prefix line (alist-get 'inner my/org-brackets--pieces))
                                  (substring wrap (length line)))))))
      ;; Blocks end at headings.
      (setq my/org-brackets--inside (pcase kind ('begin t) ('end nil) (_ (and (not heading) my/org-brackets--inside)))))))

(advice-add 'org-indent-add-properties :around #'my/org-brackets--add-properties)
(advice-add 'org-indent-set-line-properties :around #'my/org-brackets--set-line)

;;;; Edits to begin and end lines

(defun my/org-brackets--marker-in-p (beg end)
  "Whether a block's begin or end line lies on the lines from BEG to END."
  (save-excursion
    (goto-char beg)
    (forward-line 0)
    (let ((case-fold-search t))
      (re-search-forward my/org-brackets--marker-re (save-excursion (goto-char end) (line-end-position)) t))))

(defun my/org-brackets--before-change (beg end)
  "Note whether the change from BEG to END touches a begin or end line."
  (setq my/org-brackets--marker-touched (my/org-brackets--marker-in-p beg end)))

(defun my/org-brackets--after-change (beg end _length)
  "After a change from BEG to END to a begin or end line, redo the lines up to
the next heading: whether they lie inside a block may have changed."
  (when (and org-indent-mode (or my/org-brackets--marker-touched (my/org-brackets--marker-in-p beg end)))
    (setq my/org-brackets--marker-touched nil)
    (org-with-wide-buffer
     (org-indent-add-properties beg (save-excursion (goto-char end) (outline-next-heading) (point))))))

;;;; Mode

(defun my/org-brackets--redo (&rest _)
  "Rebuild this buffer's pieces and org-indent's prefixes, unless its agent is
yet to reach the buffer."
  (if my/org-brackets-mode (my/org-brackets--prepare) (setq my/org-brackets--pieces nil))
  (when (and org-indent-mode (not (memq (current-buffer) org-indent-agentized-buffers)))
    (org-with-wide-buffer (org-indent-add-properties (point-min) (point-max)))))

(defun my/org-brackets--shown (window)
  "Size the pieces from the glyphs once WINDOW first shows this buffer."
  (unless my/org-brackets--measured
    (with-current-buffer (window-buffer window) (my/org-brackets--redo))))

(define-minor-mode my/org-brackets-mode
  "Draw block brackets in org-indent's prefixes; see `my/org-brackets-style'."
  :group 'org-indent
  (if my/org-brackets-mode
      (progn
        (add-hook 'before-change-functions #'my/org-brackets--before-change nil t)
        (add-hook 'after-change-functions #'my/org-brackets--after-change 90 t)
        ;; Corners are sized from the fonts on screen.
        (add-hook 'text-scale-mode-hook #'my/org-brackets--redo nil t)
        (add-hook 'window-buffer-change-functions #'my/org-brackets--shown nil t))
    (remove-hook 'before-change-functions #'my/org-brackets--before-change t)
    (remove-hook 'after-change-functions #'my/org-brackets--after-change t)
    (remove-hook 'text-scale-mode-hook #'my/org-brackets--redo t)
    (remove-hook 'window-buffer-change-functions #'my/org-brackets--shown t))
  (my/org-brackets--redo))

(defun my/org-brackets-toggle-style ()
  "Switch between the `indent' and `fringe' styles in all Org buffers."
  (interactive)
  (setq my/org-brackets-style (if (eq my/org-brackets-style 'fringe) 'indent 'fringe))
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when my/org-brackets-mode (my/org-brackets--redo))))
  (message "Block brackets: %s" my/org-brackets-style))

(provide 'my-org-brackets)
;;; my-org-brackets.el ends here
