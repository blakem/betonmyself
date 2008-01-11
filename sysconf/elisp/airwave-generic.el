(provide 'airwave-generic)

(defun airwave-save-and-make-executable ()
  (interactive)
  (save-buffer)
  (if (= (shell-command (concat "chmod a+x " (buffer-file-name))) 0)
      (message (concat "Wrote and made executable " (buffer-file-name)))))

(defun airwave-zap-to-char (arg char)
  "Kill up to *but not including* ARG'th occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found."
  (interactive "p\ncZap to char: ")
  (kill-region (point) (progn
			 (search-forward (char-to-string char) nil nil arg)
                         (backward-char 1)
			 (point))))

(defun airwave-delete-whole-line ()
  (delete-region (line-beginning-position) (1+ (line-end-position))))

(defun airwave-file-exists-p (file)
  "True for files, false for directories"
  (and (file-exists-p file) (not (car (file-attributes file)))))

(defun airwave-toggle-quotes ()
  "Converts double quotes to singles and vice versa"
  (interactive)
  (save-excursion
    (let ((start (point)) 
          (face_at_point (last-element (face-at-point)))
          beg end)
      (while (eq face_at_point (last-element (face-at-point)))
        (forward-char -1))
      (forward-char 1)
      (while (looking-at "[ \t\n]") (forward-char 1))
      (setq beg (point))
      (if (not (looking-at "[\"\']")) (search-forward-regexp "[\"\']" 'nil 't))
      (if (<= (point) start) (setq beg (point)))

      (goto-char start)

      (while (eq face_at_point (last-element (face-at-point)))
        (forward-char 1))
      (forward-char -1)
      (while (looking-at "[ \t\n]") (forward-char -1))
      (setq end (point))
      (if (not (looking-at "[\"\']")) (search-backward-regexp "[\"\']" 'nil 't))
      (if (>= (point) start) (setq end (point)))

      (goto-char beg)
      (if (looking-at "\"") 
          (progn (delete-char 1) (insert-char ?\047 1))
        (if (looking-at "\'") 
            (progn (delete-char 1) (insert-char ?\042 1))
          (insert-char ?\042 1)))
      
      (goto-char end)
      (if (looking-at "\"") 
          (progn (delete-char 1) (insert-char ?\047 1))
        (if (looking-at "\'") 
            (progn (delete-char 1) (insert-char ?\042 1))
          (progn (forward-char 2) (insert-char ?\042 1))))
      )))

;;;;;;;;;;;;;;;;;; customized word killing
(defun backward-kill-line ()
  "Like `kill-line' but backward."
  (interactive)
  (when (not (bobp))
    (if (bolp)                          ;Kill previous newline (shift line up)
        (backward-delete-char 1)
      (delete-region (point) (line-beginning-position)))))

(defun airwave-kill-word ()
  "Kill characters forward until the end of a word or line"
  (interactive)
  (let (endofword (endofline (line-end-position)))
    (save-excursion (forward-word 1) (setq endofword (point)))
    (kill-region (point) 
                 (if (= endofline (point))
                     (1+ endofline)
                   (if (< endofline endofword) endofline endofword)))))
(defun airwave-backward-kill-word ()
  "Kill characters backward until the beg of a word or line"
  (interactive)
  (let (begofword (begofline (line-beginning-position)))
    (save-excursion (forward-word -1) (setq begofword (point)))
    (kill-region (point) 
                 (if (= begofline (point))
                     (1- begofline)
                   (if (> begofline begofword) begofline begofword)))))
(defun airwave-forward-word ()
   ;; Move one word forward. Leave the pointer at start of word
   ;; instead of emacs default end of word. Treat _ as part of word
   (interactive)
   (let ((start (point)) boundary at-boundary jump)
     (setq at-boundary (eolp))
     (setq boundary (line-end-position))
     (forward-char 1)
     (backward-word 1)
     (forward-word 2)
     (backward-word 1)
     (backward-char 1)
     (cond ((or (looking-at "_") (looking-at "\\.[0-9]\\.")) (forward-char 1) 
            (airwave-forward-word))
           (t (forward-char 1)))
     (if (and (not at-boundary) (> (point) boundary)) (goto-char boundary))
     (setq jump (airwave-count-lines start (point)))
     (if (> jump 0) (progn (forward-line (- 1 jump)) (back-to-indentation)))
     ))
(defun airwave-backward-word ()
   ;; Move one word backward. Leave the pointer at start of word
   ;; Treat _ as part of word
   (interactive)
   (let ((start (point)) boundary at-boundary jump)
     (setq at-boundary (empty-line-prefix))
     (setq boundary (line-beginning-position))
     (backward-word 1)
     (backward-char 1)
     (cond ((or (looking-at "_") (looking-at "\\.[0-9]\\.")) 
            (airwave-backward-word))
           (t (forward-char 1)))
     (if (and (not at-boundary) (< (point) boundary)) 
         (progn (goto-char boundary) (back-to-indentation)))
     (setq jump (airwave-count-lines start (point)))
     (if (> jump 0) (progn (forward-line (- jump 1)) (end-of-line)))
     ))
(defun airwave-count-lines (beg end)
  (let (tmp)
    (if (< end beg) (progn (setq tmp beg) (setq beg end) (setq end tmp)))
    (save-excursion 
      (goto-char beg) (setq beg (line-beginning-position))
      (goto-char end) (setq end (line-beginning-position))
      )
    (count-lines beg end)))

(defun airwave-mark-whole-line ()
  (interactive)
  (push-mark (line-beginning-position))
  (forward-line 1)
  (beginning-of-line)
  (exchange-point-and-mark)
  (exchange-point-and-mark)
)
(defun airwave-kill-whole-line ()
  (interactive)
  (airwave-mark-whole-line)
  (kill-region (mark) (point))
)
(defun airwave-kill-whole-word ()
  "Kill the whole word"
  (interactive)
  (if mark-active
      (kill-region (mark) (point))
    (let* ((charset "A-Za-z") beg end)
      (setq non-charset (concat  "^" charset))
      (setq re-charset (concat  "[" charset "]"))
      (setq re-non-charset (concat  "[^" charset "]"))
      (backward-char 1)
      (if (looking-at "[^ \t\n][ \t\n][^ \t\n]")
          (forward-char 2)
        (progn 
          (forward-char 1)
          (if (looking-at "[ \t\n]")
              (fixup-whitespace)
            
            (if (looking-at re-non-charset)
                (kill-region 
                 (point) 
                 (progn (skip-chars-forward non-charset (line-end-position))
                        (point)))
              (progn
                
                (kill-region
                 (progn
                   (skip-chars-backward charset (line-beginning-position))
                   (point))
                 (progn
                   (skip-chars-forward  charset (line-end-position))
                   (point))
                 )))))))))
(defun airwave-kill-line-or-region ()
  "Kill region if active, otherwise kill line"
  (interactive)
  (if mark-active (kill-region (mark) (point)) (kill-line)))

(defun airwave-upcase-word ()
  "Upcase region if active, otherwise upcase word"
  (interactive)
  (if mark-active (upcase-region (mark) (point)) (upcase-word 1)))
(defun airwave-downcase-word ()
  "Downcase region if active, otherwise downcase word"
  (interactive)
  (if mark-active (downcase-region (mark) (point)) (downcase-word 1)))
(defun airwave-upcase-char ()
  "Upcase character under point, scoot ahead"
  (interactive)
  (airwave-manip-char 'upcase-region))
(defun airwave-downcase-char ()
  "Downcase character under point, scoot ahead"
  (interactive)
  (airwave-manip-char 'downcase-region))
(defun airwave-manip-char (manipulator)
  (when (not (eobp))
    (funcall manipulator (point) (1+ (point)))
    (forward-char)))

(defun quick-copy-line ()
    "Copy the whole line that point is on and move to the beginning 
     of the next line. Consecutive calls to this command append each 
     line to the kill-ring."
    (interactive)
    (let ((beg (line-beginning-position 1))
          (end (line-beginning-position 2)))
      (if (eq last-command 'quick-copy-line)
          (kill-append (buffer-substring beg end) (< end beg))
        (kill-new (buffer-substring beg end))))
    (beginning-of-line 2))

(defun airwave-update-buffers ()
  "Refreshs all open buffers from their respective files"
  (interactive)
  (let* ((list (buffer-list)) (buffer (car list)) errmesg)
    (loop for buffer in (buffer-list) do
          (if (and (not (string-match "\\*" (buffer-name buffer)))
                   (buffer-file-name buffer)
                   (file-exists-p (buffer-file-name buffer)))
              (if (and (not (verify-visited-file-modtime buffer)) ; been touched
                       (buffer-modified-p buffer)) ; and modified 
                  (setq errmesg (concat errmesg 
                    (format "Buffer '%s' has file and buffer changes!\n" buffer)))
                (airwave-update-buffer buffer))))
    (when (file-exists-p "/tmp/mercury_utility_svn_cachefile")
      (delete-file "/tmp/mercury_utility_svn_cachefile"))
    (if errmesg (setq errmesg (chomp errmesg)))
    (message "%s" (or errmesg "Done refreshing all open non-modified files..."))))
(defun airwave-update-buffer (buffer)
  (set-buffer buffer) 
  (message "Refreshing %s" (buffer-file-name buffer))
  (if (not (verify-visited-file-modtime buffer))
      (if (buffer-modified-p buffer) (error "Buffer has file and buffer changes")
        (revert-buffer t t t))) ; revert if touched and not modified
  (vc-file-clearprops (buffer-file-name))
  (if hide-copyleft-mode (airwave-hide-copyleft 't))
  )

(defun airwave-jump-to-top ()
  (interactive)
  (goto-line (- (current-line-number) (window-line))))
(defun airwave-jump-to-bottom ()
  (interactive)
  (goto-line (- (+ (current-line-number) (window-line-from-bottom)) 1)))

(defun airwave-scootch-up ()
  (interactive)
  (airwave-scootch -1))
(defun airwave-scootch-down ()
  (interactive)
  (airwave-scootch 1))
(defun airwave-scootch-left ()
  (interactive)
  (airwave-scootch -1 't))
(defun airwave-scootch-right ()
  (interactive)
  (airwave-scootch 1 't))
(defun airwave-scootch (linecount &optional horizontal)
  (let ((col (current-column)) mark-was-active beg end region)
    (if mark-active
        (progn
          (setq mark-was-active t)
          (if (< (point) (mark)) (exchange-point-and-mark))
          (setq beg (mark))
          (setq end (point))
          )
      (progn
        (setq beg (line-beginning-position))
        (setq end (1+ (line-end-position)))
        ))
    (setq region (buffer-substring beg end))
    (delete-region beg end)
    (if horizontal (forward-char linecount) (forward-line linecount))
    (setq beg (point))
    (insert region)
    (if mark-was-active 
        (progn
          (goto-char beg)
          (setq deactivate-mark 'nil)
          (set-mark (point))
          (goto-char (+ (point) (length region)))
          )
      (progn 
        (if horizontal (forward-char linecount) (forward-line linecount))
        (move-to-column col)
        ))))
