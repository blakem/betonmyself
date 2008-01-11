(provide 'airwave-func)
(require 'cperl-mode)

;;;;;;;;;;;;;;;;;;
(defun mercmerc (&rest path)
  (concat airwave-cvs-root "/mercury" (apply 'concat path)))
(defun root (&rest path)
  (concat (getenv "root") (apply 'concat path)))
(defun home (&rest path)
  (concat (getenv "HOME") (apply 'concat path)))

;;;;;;;;;;;;;;;;;;
(defun airwave-deparse-perl ()
   "Deparse and add parenthesis to perl code in region"
   (interactive)
   (shell-command-on-region (mark) (point) "perl -MO=Deparse,-p,-sCi2" 
     "*deparse*"))

;;;;;;;;;;;;;;;;;;
(defun parse-method (mod-string)
  (let (pathlist function arrow super)
    (setq arrow (string-match "->" mod-string))
    (setq pathlist (reverse (string-split "::\\|->" mod-string)))
    (setq function (pop pathlist))
    (setq super (pop pathlist))
    (if (string-equal super "SUPER")
        (setq function (format "%s::%s" super function))
      (push super pathlist))
    (if (and (not arrow) 
             (or (not (or (downcasep function) (starts-with function "_"))) 
                 (null pathlist)))
        (progn (push function pathlist) (setq function 'nil)))
    (list (list-join (reverse pathlist) "::") function)))
(defun filename-to-module (filename)
  (if (string-match "\\.pm$" filename)
      (progn
        (setq filename (string-replace-match "\\.pm$" filename ""))
        (setq filename (string-replace-match ".*\\/lib\\/perl\\/" filename ""))
        (if filename (list-join (string-split "\\/" filename) "::")
          'nil)
        )
    nil))
(defun buffer-file-module ()
  (if (buffer-file-name) (filename-to-module (buffer-file-name))))
(defun current-method-call ()
  (parse-method (current-method)))
(defun current-method-method ()
  (car (cdr (parse-method (current-method)))))
(defun current-method-class ()
  (car (parse-method (current-method))))
(defun current-oid ()
  (save-excursion
    (skip-chars-backward "A-Za-z0-9_\\-\\.")
    (let ((beg (point)))
      (skip-chars-forward "A-Za-z0-9_\\-\\.")
      (buffer-substring beg (point)))))
(defun current-function ()
  (save-excursion
    (skip-chars-backward "A-Za-z0-9_\\-")
    (let ((beg (point)))
      (skip-chars-forward "A-Za-z0-9_\\-")
      (buffer-substring beg (point)))))
(defun prev-module ()
  (save-excursion
    (search-backward-regexp "[A-Za-z]::[A-Z]")
    (current-module)))
(defun current-module ()
  (save-excursion
    (skip-chars-backward "A-Za-z0-9:_")
    (let ((beg (point)) module curclass)
      (skip-chars-forward "A-Za-z0-9:_")
      (setq module (buffer-substring beg (point)))
      (when (string-match ":$" module)
        (setq module (string-replace-match ":+$" module "")))
      (when (string-match "^::" module) 
        (setq curclass (airwave-perl-currentclass))
        (message "Assuming class is %s" curclass)
        (sit-for 0.2)
        (setq module (format "%s%s" curclass module)))
      module
      )))
(defun current-module-with-optional-method ()
  (let ((module (current-module)))
    (save-excursion
      (skip-chars-forward "A-Za-z0-9:_")
      (when (looking-at " -+> ")
        (skip-chars-forward " ->")
        (setq module (concat module "->" (current-word)))))
    module))

(defun current-method ()
  (save-excursion
    (skip-chars-forward "A-Za-z0-9:_\\-")
    (if (char-equal (previous-char) "-") (backward-char 1))
    (let ((end (point)) beg)
      (skip-chars-backward "A-Za-z0-9:_\\")
      (skip-chars-backward "->")
      (skip-chars-backward "A-Za-z0-9:_\\")
      (setq beg (point))
      (if (string= (buffer-substring (- (point) 7) (point)) "->new()")
          (let (end2 beg2)
            (backward-char 7)
            (setq end2 (point))
            (skip-chars-backward "A-Za-z0-9:_\\")
            (setq beg2 (point))
            (concat (buffer-substring beg2 end2) (buffer-substring beg end))
            )
        (if (string= (buffer-substring (point) (+ (point) 5)) "new->")
            (let (end2 beg2)
              (setq beg (+ (point) 3))
              (backward-char 2)
              (setq end2 (point))
              (skip-chars-backward "A-Za-z0-9:_\\")
              (setq beg2 (point))
              (concat (buffer-substring beg2 end2) (buffer-substring beg end))
              )
          (buffer-substring beg end))))))
(defun current-constant ()
  (save-excursion
    (let (beg)
      (skip-chars-backward "A-Z0-9_")
      (setq beg (point))
      (skip-chars-forward "A-Z0-9_")
      (buffer-substring beg (point)))))
(defun current-variable ()
  (save-excursion
    (let (beg variable)
      (if (looking-at "[@$%]") (forward-char 1))
      (skip-chars-backward "{A-Za-z0-9_")
      (setq beg (1- (point)))
      (if (looking-at "{") (skip-chars-forward "{A-Za-z0-9_")
        (skip-chars-forward "A-Za-z0-9_"))
      (setq variable (buffer-substring beg (point)))
      (or (string-replace-match "{" variable "" 'nil t) variable)
      )))
(defun current-line ()
  (buffer-substring (line-beginning-position) (line-end-position)))
(defun current-line-full ()
  (buffer-substring (line-beginning-position) (+ 1 (line-end-position))))
(defun current-line-prefix ()
 (buffer-substring (line-beginning-position) (point)))
(defun current-line-suffix () (buffer-substring (point) (line-end-position)))
(defun current-line-number () 
  (let ((linenum (string-to-int (substring (what-line) 5))))
    (message "")
    linenum))
(defun current-number ()
  (save-excursion
    (let (beg)
      (skip-chars-backward "0-9")
      (setq beg (point))
      (skip-chars-forward "0-9")
      (buffer-substring beg (point)))))
    
(defun empty-line-suffix () (only-whitespace (current-line-suffix)))
(defun empty-line-prefix () (only-whitespace (current-line-prefix)))
(defun only-whitespace (str) (and (string-match "^[ \r\t]*\$" str) 't))
(defun next-char ()
  (if (>= (point) (1- (point-max)))
      (current-char)
    (char-to-string (char-after (1+ (point))))))
(defun current-char ()
  (char-to-string (following-char)))
(defun previous-char ()
  (char-to-string (preceding-char)))
(defun previous-string (&rest strlist)
  (let (found length)
    (loop for str in (flatten strlist) do
          (setq length (length str))
          (and (not found) (> length 0) (< length (point))
               (save-excursion
                 (backward-char length)
                 (when (looking-at str) (setq found str)))))
    found
    ))
(defun previous-key (&optional arg)
  (if (not arg) (setq arg 1))
  (let (recent-keys index)
    (setq recent-keys (recent-keys))
    (setq index (- (length recent-keys) (1+ arg)))
    (if (>= index 0)
        (aref recent-keys index)
      'nil)))
(defun previous-key-string (&optional arg)
  (airwave-chord-for-key (vector (previous-key arg))))
(defun airwave-eval-last-sexp ()
  (interactive)
  (if (char-equal (previous-char) ")") 
      (eval-last-sexp 'nil)
    (if (looking-at ")")
        (save-excursion
          (forward-char 1)
          (eval-last-sexp 'nil)
          )
      (if (looking-at "(")
          (progn
            (airwave-forward-to-enclosing-sexp)
            (forward-char 1)
            (eval-last-sexp 'nil)
            (search-forward "(")
            (backward-char 1)
            )
        (eval-defun 'nil)))))

;;;;;;;;;;;;;;;;;;
(defadvice tooltip-identifier-from-point (after parse-perl activate compile)
  (save-excursion
    (goto-char point)
    (let ((my-perl-var (airwave-find-perl-variable)))
      (setq ad-return-value 
            (if my-perl-var 
                (progn (message "%s" my-perl-var)
                       (concat "x \\" my-perl-var)
                       )
              nil)))))

(defun airwave-find-perl-variable ()
  (save-excursion
    (let ((origpoint (point))
          (start (progn 
                   (skip-chars-backward "^$@% ")
                   (backward-char 1)
                   (point))))
      (if (looking-at " ") nil
        (progn
          (skip-chars-forward "$@%A-Za-z0-9_{}>[]\\-")
          (if (or (looking-at "(") (> origpoint (point))) nil
            (buffer-substring start (point))))))))
;;;;;;;;;;;;;;;;;; timers
(require 'time-date)
(defun hires-time () (time-to-seconds (current-time)))
(defvar last-char-typed-time (hires-time))
(defun airwave-self-insert-command (arg)
  (interactive "p")
  (self-insert-command arg)
  (setq last-char-typed-time (hires-time)))
(defun time-since-last-keypress ()
  (- (hires-time) last-char-typed-time))
(substitute-key-definition 'self-insert-command
                           'airwave-self-insert-command (current-global-map))
;;;;;;;;;;;;;;;;;; debugger customizations
(defadvice gud-find-file (after show-line-numbers activate compile)
  (with-current-buffer (get-file-buffer file) 
    (if (string-match "^perl5db.pl\$" (buffer-name)) 
        'nil 
      (setnu-mode 1))))

(defadvice gud-sentinel (after remove-line-numbers activate compile)
  (airwave-remove-line-numbers))

(defun airwave-remove-line-numbers ()
  (interactive)
  (loop for buf in (buffer-list) do
        (with-current-buffer buf (if setnu-mode (setnu-mode nil)))))

(defun airwave-perldb ()
  "Run the debugger"
  (interactive)
  (airwave-save-and-save-some-buffers)
  (make-local-variable 'airwave-debugger-command)
  (when (string= (buffer-name) "*compilation*")
    (delete-other-windows)
    (airwave-jump-to-last-test))
  (if (boundp 'airwave-debugger-command) 
      (airwave-run-perldb) 
    (airwave-perldb-ask)))

(defun airwave-perldb-ask ()
  "Run the debugger after asking for how to run it"
  (interactive)
  (airwave-save-and-save-some-buffers)
  (make-local-variable 'airwave-debugger-command)
  (setq airwave-debugger-command (read-string ""
     (format "perl -d %s" (airwave-find-test-file (buffer-file-name)))))
  (airwave-run-perldb)
  )

(defun airwave-run-perldb ()
  (message airwave-debugger-command)
  (perldb airwave-debugger-command))

(defun airwave-make-dbap-file (apid)
  (save-excursion 
    (find-file "/tmp/dbap.pl")
    (kill-region (point-min) (point-max))
    (insert "BEGIN {\n  use Mercury::AP;")
    (insert (format "\n  $ap = Mercury::AP::Factory->get_by_id(%s);\n" apid))
    (insert "  $ap->{_snmp_warn} = 1;\n}\nwhile (1) { 1 }\n")
    (save-buffer)
    ))

;;;;;;;;;;;;;;;;;;

(defun airwave-toggle-hide-subs ()
  (interactive)
  (let ((funcstr "sub "))
    (if (string= comment-start ";") 
        (setq funcstr 
          "(def\\(un\\|var\\|group\\|alias\\|custom\\|const\\|subst\\|macro\\|face\\) "))
    (if line-move-ignore-invisible
        (progn (show-all-invisible) (setq line-move-ignore-invisible nil))
      (hide-non-matching-lines (format "^[\t ]*%s" funcstr))
      )))

;;;;;;;;;;;;;;;;;;
;; uimake and make libperl
(defun starts-with (haystack needle)
  "Returns true if the second argument is a prefix of the first."
  (and (>= (length haystack) (length needle))
       (string= needle (substring haystack 0 (length needle)))))
(defun in-template-dir () 
  "Returns true if the current buffer is visiting a file in the templates/ dir."
  (starts-with (buffer-file-name) 
               (mercmerc "/lib/templates/")))
(defun in-html-dir ()
  "Returns true if the current buffer is visiting a file in the html/ dir."
  (starts-with (buffer-file-name)
               (mercmerc "/lib/html/")))
(defun airwave-check-modified-files ()
  (interactive)
  (airwave-save-and-save-some-buffers)
  (airwave-shell-function-basic "check_modified_files"))

(defun airwave-method-signature (method-sig-command)
  "Find a method signature"
   (interactive
    (list (read-string "Run method_signature on: " (current-keyword))))
  (airwave-save-and-save-some-buffers)
   (setq method-sig-command (format "method_signature '%s'" method-sig-command))
   (grep method-sig-command))
(defun flatten (l)
  (cond
   ((null l) nil)
   ((atom (car l)) (cons (car l) (flatten (cdr l))))
   (t (append (flatten (car l)) (flatten (cdr l))))))

(defun svnbase ()
  (chomp (airwave-shell-function-eval "svnbase")))
(defun svnurl (&rest proj)
  (concat (string-replace-match "\\/[^/\n]+$" (svnbase) "/") (apply 'concat proj)))
(defun svnproj ()
  (chomp (airwave-shell-function-eval "svnproj")))
(defun towho ()
  (chomp (airwave-shell-function-eval "towho")))
(defun devamps ()
  (string-split "\n" (airwave-shell-function-eval "devamps")))

(defun airwave-shell-function (cmd &optional buffername quiet)
  "Run a function defined in our bash configuration"
  (interactive (list (read-string "% ")))
  (if (not buffername) (setq buffername "*shell function output*"))
  (if (string= buffername "discard output") (setq buffername 'nil))
  (if (string= buffername "stdout") 
      (setq buffername 't)
    (if buffername (airwave-select-empty-output-buffer buffername)))
  (when (not quiet) (message "Running: %s" cmd))
  (call-process "bash" nil buffername nil 
                "--noprofile" "-O" "expand_aliases" "-l" "-c" 
                (format "cd %s; %s" (root) cmd))
  (when (not quiet) (message "Done."))
  )
(defun airwave-shell-function-basic (cmd &optional args)
  (let ((bufname (format "*%s*" cmd)))
    (when args (setq cmd (format "%s %s" cmd args)))
    (airwave-shell-function cmd bufname)))
(defun airwave-shell-function-no-output (cmd &optional args)
  (when args (setq cmd (format "%s %s" cmd args)))
  (airwave-shell-function cmd "discard output" 't))
(defun airwave-shell-function-eval (cmd)
  "Evaluate a function and return its output"
  (with-output-to-string 
    (with-current-buffer
        standard-output
      (airwave-shell-function cmd "stdout" 't))))
(defun airwave-shell-function-insert (cmd)
  (insert (airwave-shell-function-eval cmd)))

(defun airwave-select-empty-output-buffer (buffername)
  (switch-to-buffer (get-buffer-create buffername))
  (airwave-erase-buffer))
(defun airwave-erase-buffer ()
  (setq buffer-read-only 'nil)
  (erase-buffer))

(defun airwave-generate-empty-output-buffer (buffername)
  (let (current-buffer (current-buffer))
    (airwave-select-empty-output-buffer buffername)
    (switch-to-buffer current-buffer)))
;;;;;;;;;;;;;;;;;;
(defun airwave-indent-cperl-region-or-line ()
  (interactive)
  (if mark-active 
      (progn  ; set beg end to marked region
        (if (< (point) (mark)) (exchange-point-and-mark))
        (cperl-indent-region (mark) (1- (point)))
        )
    (let ((previous-key (previous-key)))
      (if (and (or (looking-at "\\>")
                   (empty-line-suffix)
                   (char-equal (previous-char) ":")
                   (eq previous-key 'left)
                   (eq previous-key 'right))
               (not (eq previous-key 'down))
               (not (eq previous-key 'up))
               (not (and (eolp) (string-match "[\]\)\}\,]" (previous-char))))
               (not (empty-line-prefix)))
          (hippie-expand 'nil)
        (progn
          (setq he-num -1)
          (cperl-indent-command))))))

(defun airwave-indent-lisp-region-or-line ()
  (interactive)
  (if mark-active 
      (progn  ; set beg end to marked region
        (if (< (point) (mark)) (exchange-point-and-mark))
        (indent-region (mark) (1- (point)) 'nil)
        )
    (let ((previous-key (previous-key)))
      (if (and (or (looking-at "\\>")
                   (empty-line-suffix)
                   (char-equal (previous-char) ":")
                   (eq previous-key 'left)
                   (eq previous-key 'right))
               (not (eq previous-key 'down))
               (not (eq previous-key 'up))
               (not (and (eolp) (string-match "[\]\)\}\,]" (previous-char))))
               (not (empty-line-prefix)))
          (hippie-expand 'nil)
        (progn
          (setq he-num -1)
          (lisp-indent-line))))))

(defun airwave-add-vertical-space ()
  (interactive)
  (if (not (empty-line-prefix)) (forward-line 1))
  (beginning-of-line)
  (open-line 1))


;;;;;;;;;;;;;;;;;;
(defun airwave-add-braces-to-word ()
  (interactive)
  (save-excursion
    (if mark-active 
        (progn  ; set beg end to marked region
          (if (< (point) (mark)) (exchange-point-and-mark))
          (setq beg (mark))
          (setq end (point))
          )
      (progn    ; set beg end to word boundaries
        (forward-word 1)
        (setq end (point))
        (backward-word 1)
        (setq beg (point))
        )
      )
    
    ; insert braces
    (goto-char end)
    (insert "}")
    (goto-char beg)
    (insert "{")
    (goto-char end)
    )
  )

(defun airwave-remove-braces ()
  "Remove innermost braces, i.e. convert a hash lookup into a method call"
  (interactive)
  (save-excursion
    (setq start (point))
    (if (search-forward "}" (line-end-position) t) 
        (progn 
          (goto-char start)
          (if (search-backward "{" (line-beginning-position) t)
              (progn
                (delete-char 1)
                (search-forward "}" (line-end-position))
                (backward-delete-char 1)
                )
            )
          )
      )
    )
  )

;;;;;;;;;;;;;;;;;;
(defun airwave-run-tests ()
  (interactive)
  (require 'test-harness)
  (test-emacs-test-file "~/.elisp/airwave-func-tests.el")
)

(defun airwave-end-and-format-list ()
  (interactive)
  (insert-char ?\) 1)
  (backward-char 1)
  (airwave-toggle-vertical-horizontal-list)
  (search-forward-regexp "[\]\}\)]")
  )

(defvar aw-list-mode)
(defvar aw-list-beg (make-marker))
(defvar aw-list-end (make-marker))
(defun airwave-toggle-vertical-horizontal-list ()
  "Toggle perl lists from single-line horizontal to multiline vertical"
  (interactive)
  (if (or (not (interactive-p)) (not (equal this-command last-command)))
      (progn (airwave-set-list-boundaries)
             (setq aw-list-mode (pick-aw-list-mode)))
    (if (equal aw-list-mode "one") 
        (setq aw-list-mode "two")
      (if (equal aw-list-mode "two") 
          (setq aw-list-mode "three")
        (if (equal aw-list-mode "three") 
            (setq aw-list-mode "four")
          (if (equal aw-list-mode "four") 
              (setq aw-list-mode "one")
          )))))
  (save-excursion
    (airwave-backward-to-current-pblock)
    (if (equal aw-list-mode "one")
        (progn (message "Horizontal List") (airwave-make-list-horizontal t))
      (if (equal aw-list-mode "two")
          (progn (message "Wrapped List") (airwave-make-list-wrapped t))
        (if (equal aw-list-mode "three")
            (progn (message "Bare Wrapped List") 
                   (airwave-make-list-wrapped-bare t))
          (if (equal aw-list-mode "four")
            (progn (message "Vertical List") (airwave-make-list-vertical t))
            ))))))

(defun pick-aw-list-mode ()
  (save-excursion
    (let (qw_p mode str)
      (goto-char (- aw-list-beg 2))
      (setq qw_p (looking-at "qw"))
      (if qw_p "three" ; bare
        (setq str (buffer-substring aw-list-beg aw-list-end))
        (if (string-match "[-=]>" str) "four" ; vertical
          "two"); wrapped
        ))))
  
(defun airwave-set-list-boundaries ()
  (let ((info-list (current-pblock-boundaries)))
    (set-marker aw-list-beg (pop info-list))
    (set-marker aw-list-end (1+ (pop info-list)))))

(defun airwave-make-list-horizontal (&optional preset-boundaries)
  "Condense a multiline perl list into a single line"
  (interactive)
  (save-excursion
   (if (not preset-boundaries) (airwave-set-list-boundaries))
   (save-restriction
     (narrow-to-region aw-list-beg (1+ aw-list-end))

     ; fold it until there is only one line
     (goto-char aw-list-end)
     (while (> (count-lines (point-min) (point-max)) 1) (delete-indentation))

     ; remove optional trailing commas and spaces
     (goto-char (- aw-list-end 2))
     (while (looking-at "\\(,\\|\\ \\)") (delete-char 1) (backward-char 1))
     
     ; ensure proper spacing
     (goto-char aw-list-beg)
     (while (< (point) (1- aw-list-end))
       (forward-char 1)
       (if (looking-at " ") 
           (just-one-space)
         (if (looking-at ",") 
             (progn (forward-char 1) (just-one-space))
           (if (looking-at "=>")
               (progn (just-one-space)
                      (forward-char 2)
                      (just-one-space))))))
     ; remove leading and trailing space
     (goto-char (1+ aw-list-beg)) (delete-if-space)
     (goto-char (1- aw-list-end)) (delete-if-space)
   )))

(defun airwave-make-list-wrapped (&optional preset-boundaries)
  "Wrap long list at 80 chars"
  (interactive)
  (if (not preset-boundaries) (airwave-set-list-boundaries))
  (airwave-make-list-vertical t "Wrapped"))
  
(defun airwave-make-list-wrapped-bare (&optional preset-boundaries)
  "Wrap long list at 80 chars with a bare paren in front"
  (interactive)
  (if (not preset-boundaries) (airwave-set-list-boundaries))
  (airwave-make-list-vertical t "Wrapped" "Bare"))
  
(defun airwave-make-list-vertical (&optional preset-boundaries wrapped bare)
  "Expand a single line perl list into a multiline one"
  (interactive)
  (if (not preset-boundaries) (airwave-set-list-boundaries))
  (airwave-make-list-horizontal t)
  (let* (qw_p indent prevpoint current-wrap-column (wrap-column 79) 
             (default-indent-step 2))
    (save-excursion
      (save-restriction
        ; find current indentation
        (setq current-wrap-column (- wrap-column (current-column)))

        ; are we in a qw?
        (goto-char (- aw-list-beg 2))
        (setq qw_p (looking-at "qw"))

        ; determine indentation
        (back-to-indentation)
        (setq indent (+ (current-column) default-indent-step))

        ; narrow region
        (narrow-to-region aw-list-beg aw-list-end)

        ; remove optional trailing commas and spaces
        (goto-char (- aw-list-end 1))
        (skip-chars-backward " ,\n\t") 
        (while (looking-at "\\(,\\|\\ \\)") (delete-char 1))

        ; indent first element
        (goto-char aw-list-beg)
        (skip-chars-forward "(\\|{")
        (while (looking-at "\\ \\|\n") (delete-char 1))
        (if (or bare (not wrapped))  
            (progn
              (setq current-wrap-column wrap-column)
              (newline) 
              (insert-char ?\  indent)))
        
        ; indent the rest of the elements
        (while (< (point) (- (point-max) 1))
          ; set marker for wrapping
         (if (and (not (looking-at "[ \t\n]*[\-\=]\>"))
                   (< (point) (- (point-max) 4)))
              (setq prevpoint (point)))

          ; move forward
          (forward-sexp 1)

          ; gobble white space
          (if (not wrapped) 
              (while (looking-at "\\ \\|\n") (delete-char 1)))

          ; qw wrapping
          (if (and qw_p wrapped (> (current-column) current-wrap-column))
              (save-excursion
                (setq current-wrap-column wrap-column)
                (goto-char prevpoint)
                (while (looking-at "[ \t\n]") (delete-char 1))
                (newline)
                (insert-char ?\  indent)))
          ; skip over ,;
          (while (looking-at ",\\|;") (forward-char 1))

          ; skip over fat arrows
          (if (looking-at "[ \t\n]*\-\>") 'nil
            (if (looking-at "[ \t\n]*\=\>")
                (progn 
                  (while (looking-at "[ \t\n]") (delete-char 1))
                  (insert-char ?\  1)
                  (forward-char 2)
                  (while (looking-at "[ \t\n]") (delete-char 1))
                  (insert-char ?\  1)
                  (backward-char 3)
                  )
              ; indent next element
              (if (not wrapped)
                  (progn
                    (while (looking-at "\\ \\|\n") (delete-char 1))
                    (newline) 
                    (insert-char ?\  indent))
                (if (> (current-column) current-wrap-column)
                    ; indent prev element if wrapped and we're over limit
                    (save-excursion
                      (setq current-wrap-column wrap-column)
                      (goto-char prevpoint)
                      (while (looking-at "\\ \\|\n") (delete-char 1))
                      (newline)
                      (insert-char ?\  indent))))))
           ); end of while

          ; wrap last element of list if its over limit
          (if (> (current-column) current-wrap-column)
              (save-excursion
                (setq current-wrap-column wrap-column)
                (goto-char prevpoint)
                (newline)
                (insert-char ?\  indent)))
          ; indent closing paren on bare wrapped list
          (if (or bare (> (current-column) (- current-wrap-column 1)))
              (progn (if (not qw_p) (insert-char ?\, 1))
                     (newline)
                     (insert-char ?\  (- indent default-indent-step))))

          ; line up closing paren
          (if (not wrapped) (backward-delete-if-space default-indent-step))

          ; add in trailing comma
          (if (and (not qw_p) (not wrapped))
              (progn
                (forward-line -1)
                (end-of-line)
                (while (string-equal (previous-char) ",") 
                  (backward-delete-char 1))
                (insert-char ?\, 1)))))))

(defun delete-if-space (&optional arg)
  (if (not arg) (setq arg 1))
  (dotimes (i arg) (if (looking-at "[ \t\n\r]") (delete-char 1))))
(defun backward-delete-if-space (&optional arg)
  (if (not arg) (setq arg 1))
  (dotimes (i arg)
    (if (string-equal (previous-char) " ") (backward-delete-char 1))))

;;;;;;;;;;;;;;;;;;
(defun airwave-apache-tail-up ()
  (beginning-of-line)
  (search-backward-regexp "^\\[" 'nil t)
  (beginning-of-line))
(defun airwave-apache-tail-down ()
  (end-of-line)
  (search-forward-regexp "^\\[" 'nil t)
  (beginning-of-line))
(defun current-pblock-boundaries ()
  (save-excursion
    (let (pblock-beg)
      (airwave-backward-to-current-pblock)
      (setq pblock-beg (point))
      (airwave-forward-to-current-pblock)
      (list pblock-beg (point)))))

(defun airwave-forward-to-enclosing-pblock ()
  "Go forward to enclosing paren or brace block"
  (interactive)
  (let ((brace-count 0) (paren-count 0) (start (point)))
    (forward-char 1)
    (while (and (not (= brace-count 1)) 
                (not (= paren-count 1))
                (not (eobp))
                (search-forward-regexp "[\(\)\{\}]" nil t))
      (progn
        (backward-char 1)
        (if (looking-at "{") (setq brace-count (1- brace-count)))
        (if (looking-at "}") (setq brace-count (1+ brace-count)))
        (if (looking-at "(") (setq paren-count (1- paren-count)))
        (if (looking-at ")") (setq paren-count (1+ paren-count)))
        (forward-char 1)
        )
      )
    (backward-char 1)
    (if (and (not (= brace-count 1)) (not (= paren-count 1)))
        (progn (goto-char start) 
               (error "airwave-forward-to-enclosing-block: At top level")))
    (if (and (= brace-count 1) (not (= paren-count 0)))
        (error "airwave-forward-to-enclosing-block: Mismatched parens"))
    (if (and (= paren-count 1) (not (= brace-count 0)))
        (error "airwave-forward-to-enclosing-block: Mismatched braces"))
    )
  )

(defun airwave-backward-to-enclosing-pblock ()
  "Go backward to enclosing paren or brace block"
  (interactive)
  (let ((brace-count 0) (paren-count 0) (start (point)))
    (while (and (not (= brace-count 1)) 
                (not (= paren-count 1))
                (not (bobp))
                (search-backward-regexp "[\(\)\{\}]" nil t)
                )
      (progn
        (if (looking-at "{") (setq brace-count (1+ brace-count)))
        (if (looking-at "}") (setq brace-count (1- brace-count)))
        (if (looking-at "(") (setq paren-count (1+ paren-count)))
        (if (looking-at ")") (setq paren-count (1- paren-count)))
        )
      )
    (if (and (not (= brace-count 1)) (not (= paren-count 1)))
        (progn (goto-char start) 
               (error "airwave-backward-to-enclosing-block: At top level")))
    (if (and (= brace-count 1) (not (= paren-count 0)))
        (error "airwave-backward-to-enclosing-block: Mismatched parens"))
    (if (and (= paren-count 1) (not (= brace-count 0)))
        (error "airwave-backward-to-enclosing-block: Mismatched braces"))
    )
  )
(defun airwave-backward-to-enclosing-sexp ()
  "Go backward to enclosing expression"
  (interactive)
  (if (looking-at "(") (airwave-backward-to-enclosing-pblock)
    (progn
      (if (not (looking-at ")"))
          (progn (search-forward-regexp ")") (backward-char 1)))
      (forward-char 1)
      (backward-sexp 1))))
(defun airwave-forward-to-enclosing-sexp ()
  "Go backward to enclosing expression"
  (interactive)
  (if (looking-at ")") (airwave-forward-to-enclosing-pblock)
    (progn
      (if (not (looking-at "(")) (search-backward-regexp "("))
      (backward-char 1)
      (forward-sexp 1)
      (backward-char 1))))

(defun airwave-backward-to-current-pblock ()
  (interactive)
  (if (not (looking-at "[\(\{]")) (airwave-backward-to-enclosing-pblock)))
(defun airwave-forward-to-current-pblock ()
  (interactive)
  (if (not (looking-at "[\)\}]")) (airwave-forward-to-enclosing-pblock)))

;;;;;;;;;;;;;;;;;;
(defun dap ()
  "insert destroyer(ap) line"
  (interactive)
  (let ((beg (point)))
    (insert "my ($ap, $d) = destroyer(Mercury::AP::Factory->get_test_object(\n")
    (insert "type => AP_TYPE_INTEL2011B,\n));\n")
    (cperl-indent-region beg (1- (point)))
    )
  )

(defun start-test-file ()
  "insert self line"
  (interactive)
  (insert "#!/usr/bin/perl -w\n")
  (insert "# Copyright (c) 2001-2008, AirWave Wireless, Inc.\n")
  (insert "# This material contains trade secrets and confidential ")
  (insert "information of AirWave\n")
  (insert "# Wireless, Inc.\n")
  (insert "# Any use, reproduction, disclosure or dissemination is strictly prohibited\n")
  (insert "# without the explicit written permission of AirWave Wireless, Inc.\n")
  (insert "# All rights reserved.\n")
  (insert "use strict;\n")
  (insert "use Mercury::Test;\n")
  (insert "BEGIN { plan tests => 1 }\n\n")
  (insert "ok(1);\n")
  (airwave-save-and-make-executable)
)

(defun dbs ()
  "insert DB::single line"
  (interactive)
  (airwave-insert-as-new-line
   "$MY::var = 1; # XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX");
  (airwave-insert-as-new-line 
   "$DB::single = 1 if $MY::var; # XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX");
  (save-buffer)
)
(defun airwave-insert-as-new-line (string)
  (beginning-of-line)
  (insert (concat "  " string "\n"))
  (forward-line -1)
  (beginning-of-line)
  (airwave-kill-whole-word)
  (forward-line 1)
)
(defun airwave-stack-trace ()
  "insert cluck line"
  (interactive)
  (airwave-insert-as-new-line
   "$MY::var = 1; # XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX");
  (airwave-insert-as-new-line
   "use Carp qw(cluck carp); if ($MY::var) { local $Carp::MaxArgLen = local $Carp::MaxArgNums = 0; cluck; } # XXXXXXXXXXXXXXXXXXXXXXXXXXX");
  (save-buffer)
)
(defun airwave-end-here ()
  "insert __END__"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (insert (buffer-substring (line-beginning-position) (line-end-position)))
    (insert "__END__")
  ))
(defun airwave-clone-line ()
  "duplicate line under point"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (insert (buffer-substring (line-beginning-position) (line-end-position)))
    (insert "\n")
    )
)
(defun airwave-insult ()
  "insult your pair"
  (interactive)
  (insert "your mother"))
(defun airwave-rat ()
  "A big rat"
  (interactive)
  (insert "Check out that super rat!"))
(defun airwave-init-for-package ()
  "Add a useful line of code"
  (interactive)
  (insert "\n__PACKAGE__->init_for_package;\n"))

;;;;;;;;;;;;;;;;;;
(defun airwave-toggle-spaces-in-sexp ()
  "Add or remove spaces in the expression"
  (interactive)
  (save-excursion
    (let ((start (point)) (info (current-pblock-boundaries)) beg end mode)
      (setq beg (pop info))
      (setq end (pop info))
      (if (> (- start beg) (- end start))
          (progn
            (goto-char beg)
            (setq mode (if (looking-at "[\(\{] ") "remove" "add")))
        (progn
          (goto-char end)
          (setq mode (if (string-equal (previous-char) " ") "remove" "add"))))
      (if (string-equal mode "remove")
          (progn
            (goto-char end)
            (while (string-equal (previous-char) " ") (backward-delete-char 1))
            (goto-char (1+ beg))
            (while (looking-at "\\ ") (delete-char 1)))
        (progn
          (goto-char end) (just-one-space)
          (goto-char (1+ beg)) (just-one-space))))))

;;;;;;;;;;;;;;;;;;
(defun current-keyword ()
  (save-restriction
    (narrow-to-region (line-beginning-position) (line-end-position))
    (save-excursion
      (forward-word 1)
      (let ((beg (point)))
        (backward-word 1)
        (buffer-substring beg (point))))))
(defun active-region ()
  (buffer-substring (point) (mark)))
(defun current-keyword-or-quoted-active-region ()
  (if mark-active (concat "'" (active-region) "'")
    (current-keyword)))

(defun airwave-rename-other-buffer (name)
  (interactive)
  (airwave-kill-buffer-if-exists name)
  (other-window 1)
  (rename-buffer name)
  (other-window 1))

(defun airwave-kill-buffer-if-exists (thisbuffer)
  (interactive)
  (let* ((list (buffer-list)) (buffer (car list)))
    (loop for buffer in (buffer-list) do
      (if (string-equal (buffer-name buffer) thisbuffer)
        (kill-buffer buffer)))))

(defun airwave-findcode  (findcode-command)
   "Run a findcode in separate buffer"
   (interactive
    (list (read-string "Run findcode as: "
                       (format "findcode %s" (current-keyword-or-quoted-active-region)))))
   (grep findcode-command)
   (airwave-rename-other-buffer (format "*%s*" findcode-command)))

(defun airwave-findcallers (method)
   "Run a findcallers in separate buffer"
   (interactive
    (list (read-string "Run findcallers on: " (current-keyword-or-quoted-active-region))))
   (grep (format "findcode -C -X %s" method))
   (airwave-rename-other-buffer (format "*findcallers %s*" method)))

(defun airwave-comparesub (comparesub-command)
   "Run a comparesub in separate buffer"
   (interactive
    (list (read-string "Run comparesub as: "
                       (format "comparesub -b %s" (current-keyword-or-quoted-active-region)))))
   (grep (format "%s/bin/%s" (mercmerc) comparesub-command))
   (airwave-rename-other-buffer (format "*%s*" comparesub-command)))

(defun airwave-find-current-file ()
  (interactive)
  (airwave-findcode (format "findcode %s" (file-name-nondirectory (buffer-file-name)))))

;;;;;;;;;;;;;;;;;;
(defun airwave-toggle-if-unless ()
  "Toggle modifiers between postfix and prefix"
  (interactive)
  (if (looking-at "(")
    (progn 
      (cperl-invert-if-unless) 
      (backward-char 1)
      (skip-chars-backward "A-Za-z\ ")
      )
    (airwave-expand-if-unless)
  )
)

(defun airwave-expand-if-unless ()
  "Convert postfix modifier to prefix form"
  (interactive)
  (let* (start beg indent end (default-indent-step 2) (postfix_p nil)
               chunk1 chunk2)
    (save-excursion
      (skip-chars-backward "A-Za-z")
      (setq start (point))
      (if (looking-at "\\<\\(if\\|unless\\|while\\|until\\|for\\|foreach\\)\\>")
          (setq postfix_p t)
        (error "Not on postfix while, if, for, foreach, until, unless" nil)
        )
      )
    (if postfix_p
        (progn
          (goto-char start)
          ; find beginning of block
          (if mark-active
              (progn
                (goto-char (mark))
                (skip-chars-forward " \t") 
                (back-to-indentation)
                (setq beg (point)))
            (if (eq (current-column) ; two line $this if $that
                    (progn (back-to-indentation) (current-column)))
                (progn 
                  (previous-line 1) 
                  (back-to-indentation)
                  (setq beg (point))
                  )
              (progn
                (back-to-indentation)
                (setq beg (point))
                )
              )
            )
          ; find end of conditional
          (goto-char start) (end-of-line) (setq end (point))

          ; determine indentation
          (goto-char beg)
          (setq indent (+ (current-column) default-indent-step))

          ; kill both sections
          (goto-char start)
          (setq chunk1 (buffer-substring beg start))
          (setq chunk2 (buffer-substring start end))
          (delete-region start end)
          (delete-region beg start)

          ; bring back the 'if'
          (insert chunk2)

          ; remove trailing spaces and semis
          (end-of-line)
          (backward-char 1)
          (while (looking-at ";\\|\\ ") (delete-char 1) (backward-char 1))
          
          ; insert parens
          (back-to-indentation)
          (skip-chars-forward "A-Za-z_")
          (skip-chars-forward " ")
          (insert-char ?\050 1)
          (end-of-line)
          (insert-char ?\051 1)
          (insert-char ?\ 1)
          (insert-char ?\{ 1)

          ; add indentation
          (newline)
          (insert-char ?\  indent)
            
          ; pull back in our block
          (insert chunk1)
          (backward-char 1)
          (while (looking-at ";\\|\\ \\|\n") (delete-char 1) (backward-char 1))
          (forward-char 1)
          (insert-char ?\; 1)

          ; close the block
          (newline)
          (insert-char ?\  (- indent default-indent-step))
          (insert-char ?\} 1)
          (setq end (point))

          ; go to the first paren
          (goto-char beg)
          (search-forward "(")
          (backward-char 1)
          (cperl-indent-region beg end)
          )
      )
    )
  )
;;;;;;;;;;;;;;;;;; Set test count
(defun airwave-set-test-count (count)
  "Set testcount to a specified number"
  (interactive "sSet Testcount to: ")
  (if (not (string-match "\\.t\$" buffer-file-name)) (error "Not a test file"))
  (condition-case nil
      (progn
        (save-excursion 
          (goto-char (point-min))
          (while (not (looking-at "BEGIN *{ *plan *tests? *=> *"))
            (forward-line 1))
          (airwave-delete-whole-line)
          (insert (format "BEGIN { plan tests => %s }\n" count))
          (save-buffer)
          (message "testcount set to %s" count) 
          )
        )
    (error (message "ERR: Testcount not found!"))
    )
  )

(defun airwave-insert-isa-ok ()
  "Inserts an isa-ok test based on what the pm file currently isa"
  (interactive)
  (if (not (string-match "\\.t\$" buffer-file-name)) (error "Not a test file"))
  (find-file (airwave-code-test-map buffer-file-name))
  (let ((beg) (base-classes (list)))
    (save-excursion
      (beginning-of-buffer)
      (while (not (eobp))
        (if (looking-at "use *base *qw(")
            (progn
              (beginning-of-line)
              (search-forward-regexp "use *base *qw(")
              (setq beg (point))
              (skip-chars-forward "A-Za-z0-9:")
              (setq base-classes (cons (buffer-substring beg (point)) base-classes))))
        (forward-line 1)))
    (find-file (airwave-code-test-map buffer-file-name))
    (insert "isa_ok( $class, qw(")
    (while (> (length base-classes) 0)
      (insert (format "%s" (car (last base-classes))))
      (setq base-classes (butlast base-classes))
      (if (> (length base-classes) 0) (insert " ")))
    (insert ") );\n")))

(defun airwave-test-count-increment (&optional n)
  "Increase current test count by ARG (defaults to 1)"
  (interactive "P")
  (if (not (string-match "\\.t\$" buffer-file-name)) (error "Not a test file"))
  (let ((testcount (airwave-find-current-test-count)))
    (airwave-set-test-count (+ (prefix-numeric-value n) testcount))))
(defun airwave-test-count-decrement (&optional n)
  (interactive "P")
  (airwave-test-count-increment (- 0 (prefix-numeric-value n))))

(defun airwave-find-current-test-count ()
  "Return the testcount as stated in the file, or nil if it cant be found"
  (condition-case nil
      (progn
        (save-excursion
          (goto-char (point-min))
          (search-forward-regexp "BEGIN *{ *plan *tests? *=> *\\([0-9]+\\)")
          (string-to-number 
           (buffer-substring (match-beginning 1) (match-end 1)))
          )
        )
    (error (message "ERR: Testcount not found!"))
    )
  )

;;;;;;;;;;;;;;;;;; Turn on and off Mercury::Test::Filter
(defun airwave-test-filter-on ()
  "Turn on the Test Filter";
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (if (and 
           (looking-at "TEST_FILTER")
           (not (looking-at "TEST_FILTER hide")))
          (kill-line 1))
      (forward-line 1)))
  (save-excursion
    (beginning-of-line)
    (insert "TEST_FILTER;\n");
    (goto-char (point-min))
    (search-forward (regexp-quote "ok("))
    (let (goterror)
      (while (not goterror)
        (condition-case nil
            (airwave-backward-to-enclosing-pblock)
          (error (setq goterror t)))))
    (forward-line -1)
    (while (looking-at "#") (forward-line -1))
    (if (not (looking-at "use Mercury::Test::Filter;"))
        (progn (forward-line 1)
               (insert "use Mercury::Test::Filter; # XXXXXXXXXXXXXXXXXXXX\n")))
    )
  (save-buffer)
  )

(defun airwave-test-filter-off ()
  "Turn off the Test Filter";
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (when (or (looking-at "[ \r\t]*use Mercury::Test::Filter;")
                (looking-at "TEST_FILTER"))
        (kill-line 1))
      (when (looking-at "__END__")
        (kill-line 1)
        (insert "\n"))
      (forward-line 1)
      )
    )
  (save-buffer)
  )

;;;;;;;;;;;;;;;;;; Add use line
(defun airwave-add-use-line ()
  "Add a use Package; line"
  (interactive)
  (save-excursion
    (let (module expanded function-module needsqw function)
      (setq function (current-function))
      (if (point-on-constantp) 
          (setq module "Mercury::Constants")
        (if (point-on-class-methodp)
            (setq module (car (parse-method (current-method))))
          (setq module (car (parse-method (current-module))))))
      (if (string= module "")
          (progn
            (setq function-module (airwave-module-for-function function))
            (if function-module 
                (progn
                  (setq module (car function-module))
                  (setq needsqw (car (cdr function-module)))))))
      (setq expanded (expand-module module))
      (if expanded (setq module expanded))
      (if (string-equal module "") (error "Couldn't determine module"))
      (if needsqw
          (airwave-add-exportok-use-line module function)
        (airwave-add-export-use-line module function)))))

(defun last-nonwhitespace-char-in-line ()
  (let ((end (line-end-position-ignore-whitespace)))
    (buffer-substring (- end 1) end)))
(defun line-ends-with-semi ()
  (string= (last-nonwhitespace-char-in-line) ";"))

(defun airwave-add-use-line-location ()
  (save-excursion   
    (while (and 
            (not (bobp)) 
            (or (looking-at "use \\(vars \\|constant \\|base \\|lib \\|warnings \\|strict \\|Mercury::Test::Filter;\\)")
                (not (line-ends-with-semi))
                (not (looking-at "use "))))
      (forward-line -1))
    (if (search-forward-regexp "BEGIN *{ *plan *tests? *=> *" 'nil 't)
        (forward-line 1))
    (while (or
            (empty-line-suffix)
            (looking-at "#\\|package\\|use strict\\|use base"))
      (forward-line 1))
    (backward-line 1)
    (if (looking-at "use base") (progn (forward-line 1) (insert "\n"))
      (if (not (looking-at "#")) (forward-line 1)))
    (point)))

(defun airwave-add-export-use-line (module &optional function)
  (save-excursion
    (goto-char (airwave-add-use-line-location))
    (if (or (not (looking-at "use")) (not (line-ends-with-semi)))
        (progn (insert "\n") (forward-line -1)))
    (if (not (and 
              (string= module "Mercury::Test")
              (save-excursion
                (search-backward "use Mercury::Test;" 'nil 't))))
        (insert (format "use %s;\n" module)))
    (forward-line -1)
    (airwave-sort-use-lines)
    (message "Now using %s goodness" module)))

(defun airwave-add-exportok-use-line (module function)
  (let (beg use-point qw-point end-use-point moduleregexp)
    (save-excursion
      (setq beg (point))
      (setq use-point (airwave-add-use-line-location))
      (goto-char use-point)
      (forward-line 1)
      (setq end-use-point (point))

      (goto-char beg)
      (setq moduleregexp (concat " " (regexp-quote module) "\\(;\\| \\)"))
      (if (not (search-backward "package " 'nil t))
          (goto-char (point-min)))
      (if (string-match moduleregexp (buffer-substring (point) end-use-point))
          (progn
            (search-forward-regexp moduleregexp)
            (if (string= (previous-char) ";")
                (progn
                  (backward-char 1)
                  (insert (format " qw(%s)" function)))
              (progn
                (search-forward "qw(")
                (setq qw-point (point))
                (search-forward ")")
                (if (not (string-match 
                          (concat "[^[:word:]]" (regexp-quote function) "[^[:word:]]")
                          (buffer-substring (1- qw-point) (point))))
                    (progn
                      (goto-char qw-point)
                      (insert (format "%s " function))
                      ))))
            (beginning-of-line)
            )
        (progn
          (goto-char use-point)
          (if (or (not (looking-at "use")) (not (line-ends-with-semi)))
              (progn (insert "\n") (forward-line -1)))
          (insert (format "use %s qw(%s);\n" module function))
          (forward-line -1)
          ))
      (airwave-sort-use-lines)
      (message "Now using %s goodness with qw(%s)" module function))))
  
(defun whichpm (str) (shell-command-to-string (concat "whichpm -i " str)))
(defun expand-module (str) (filename-to-module (whichpm str)))

(defun airwave-module-for-function (function)
  (let (module exported needsqw module-exported)
    (setq module-exported (airwave-exported-function function))
    (setq module (car module-exported))
    (setq exported (car (cdr module-exported)))
    (if (string= exported "EXPORT_OK")
        (setq needsqw t)
      (if (not (string= exported "EXPORT"))
          (error "Couldnt find %s in %s: %s"
                 function module exported)))
    (list module needsqw)))

(defun airwave-insert-current-class ()
  (interactive)
  (insert (airwave-perl-currentclass)))

(defun airwave-insert-current-task ()
  (interactive)
  (airwave-shell-function-insert(format "run_bash_func current_task | tr -d '\n '")))

(defun airwave-perl-currentclass ()
  (let ((filename (buffer-file-name)) new-filename)
    (if (string-match "\\.t$" filename)
        (setq new-filename (airwave-code-test-map filename)))
    (if new-filename (setq filename new-filename))
    (airwave-filename-to-modulename filename)))

(defun airwave-filename-to-modulename (file)
  (interactive)
  (let ((base (mercmerc)))
    (setq file 
          (or 
           (string-replace-match 
            (format "^%s%s" base "/lib/perl/") file "")
           (string-replace-match ".*perl/" file "")
           file
           ))
    (setq file (string-replace-match "\\.pm\$" file ""))
    (string-replace-match "/" file "::" 't 't)
    ))

(defun airwave-random-string () 
  (interactive)
  (insert (shell-command-to-string "random-string -i")))
(defun airwave-random-mac () 
  (interactive)
  (insert (shell-command-to-string "random-mac -i -s 8888")))
(defun airwave-random-ip ()
  (interactive)
  (insert (shell-command-to-string "random-ip -i")))
(defun airwave-random-number (max)
  (interactive "p")
  (if (= max 1) (setq max 10000))
  (insert (shell-command-to-string
           (concat "perl -e 'print int rand(" (int-to-string (+ max 1)) ")'"))))

(defun airwave-insert-unix-time ()
  (interactive)
  (insert (substring  ; 12345.0 ==> 12345
           (int-to-string (time-to-seconds (seconds-only (current-time))))
           0
           -2)))

(defun seconds-only (timelist) ; doesn't actually remove milliseconds, but sets them to zero
  (cons (car timelist)
        (cons (cadr timelist) '(0))))


;;;;;;;;;;;;;;;;;; Sort use lines
(defun sort-use-lines ()
  "Sort lines in region by length, remove dupes"
  (interactive)
  (airwave-sort-use-lines)
)
(defun airwave-sort-use-lines ()
  "Sort lines in region by length, remove dupes"
  (interactive)
  ; set beg to beginning of line
  (let (beg end enduse-marker)
    (if mark-active
        (progn
          (if (< (point) (mark)) (exchange-point-and-mark))
          (setq beg (point) end (mark))

          (goto-char beg)
          (beginning-of-line)
          (setq beg (point))
          
          ; set end to end of line
          (goto-char end)
          (if (not (bolp)) (forward-line 1))
          (setq end (point))
          )
      (progn  ; determine region based on text
        ; find beg
        (beginning-of-line)
        (while (and (not (bobp)) (looking-at "use ")) (forward-line -1))
        (if (not (looking-at "use ")) (forward-line 1))
        (setq beg (point))

        ; find end
        (while (looking-at "use ") (forward-line 1))
        (setq end (point))
        )
      )
    (setq enduse-marker (make-marker))
    (set-marker enduse-marker (+ end 1))
    (shell-command-on-region beg end "perl -ne 's/\\s+\$/\\n/; push @a, \$_ unless \$s{\$_}++;}{ print sort {\$aa = \$a; \$bb = \$b; s/( .*?) .*/\$1;/ for \$aa, \$bb; length \$aa <=> length \$bb} @a'" nil t)

    (goto-char (- enduse-marker 1))
    (set-marker enduse-marker nil)
    )
  )

(defun airwave-shell-replace (shell-replace-command)
  "Replace region with output of shell command on region"
  (interactive
   (list (read-string "Replace region with: ")))
  (let ((beg (if mark-active (mark) (point))))
    (shell-command-on-region beg (point) shell-replace-command nil t)))

;;;;;;;;;;;;;;;;;;
(defvar airwave-last-test-executed nil)
(defvar airwave-next-to-last-test-executed nil)
(defun airwave-run-perl-file ()
   "Run a perl file"
   (interactive)
   (if (not (string-match "^\\*.*\\*$" (buffer-name))) (save-buffer))
   (when (string= (buffer-name) "*compilation*")
     (delete-other-windows)
     (airwave-jump-to-last-test))
   (let ((filename (filename-under-point)))
     (if (and filename (string-match "\\.t\$" filename))
         (progn (airwave-find-file 'nil filename) (airwave-run-perl-file))
       (progn
         (if (not (boundp 'compile-rpf-command))
             (setq compile-rpf-command (buffer-file-name)))
         (if (not (file-exists-p compile-rpf-command))
             (setq compile-rpf-command 
                   (read-string "File to run: "
                                (format "%s" compile-rpf-command))))
         (message "compile-rpf-command: %s" compile-rpf-command)
         (when (not (string= airwave-last-test-executed compile-rpf-command))
           (setq airwave-next-to-last-test-executed airwave-last-test-executed))
         (setq airwave-last-test-executed compile-rpf-command)
         (compile compile-rpf-command)))))

(defun airwave-set-test-file ()
   "Set the testfile to be run"
   (interactive)
   (if (not (boundp 'compile-rpf-command))
       (setq compile-rpf-command (buffer-file-name)))
   (setq compile-rpf-command 
         (read-string "TestFile for Buffer: "
                      (format "%s" compile-rpf-command))))

(defun airwave-perl-check ()
   "Run a perl check on the current buffer."
   (interactive)
   (save-buffer)
   (airwave-push-mark)
   (setq compile-apc-command
      (format "perl -Mwarnings -M-warnings=redefine -c \'%s\'" (buffer-file-name))
   )
   (message "compile-apc-command: %s" compile-apc-command)
   (compile compile-apc-command))

(defun airwave-finish-compilation (compilation-buffer mesg)
  (let ((code-window (selected-window)) (tojump (> (time-since-last-keypress) 1.2)))
    (if (string-match "finished" mesg)
        (progn
          (if (string-match "compilation" (format "%s" compilation-buffer))
              (condition-case nil
                  (progn
                    (when tojump 
                      (progn
                        (next-error)
                        (pop-to-buffer compilation-buffer)
                        (recenter)
                        (select-window code-window)
                        (recenter)))
                    (airwave-run-parse-test-output compilation-buffer tojump)
                    )
                (error (airwave-run-parse-test-output compilation-buffer tojump))
                )
            (if (string-match "grep" (format "%s" compilation-buffer))
                (progn
                  (pop-to-buffer compilation-buffer)
                  (goto-char 0)
                  (select-window code-window)
                  )
              )
            )
          (select-window code-window)
          )
      )
    )
  )

(defun airwave-run-parse-test-output (buffer tojump)
  (let ((code-window (selected-window)) outputbuf cmd)
    (pop-to-buffer buffer)
    (setq outputbuf (get-buffer-create "*parse_test_output*"))
    (setq cmd "parse_test_output -e")
    (when (not tojump) (setq cmd (concat cmd " -f")))
    (shell-command-on-region (point-min) (point-max) cmd outputbuf)
    (airwave-auto-update-test-count buffer outputbuf)
    (select-window code-window)
    )
  )

(defun airwave-auto-update-test-count (comp-buffer parse-buffer)
  (let (parse-output testcount testfile)
    (save-excursion
      (setq parse-output (with-current-buffer parse-buffer 
                           (buffer-substring (point-min) (point-max))))
      (when (string-match "Bad Testcount.*ran \\([0-9]+\\) expected \\([0-9]+\\)" parse-output) 
        (setq testcount (string-to-int 
                         (substring parse-output (match-beginning 1) (match-end 1))))
        (setq expected (string-to-int
                        (substring parse-output (match-beginning 2) (match-end 2))))
        (when (and (> testcount expected) (< testcount 1000))
          (message "Autoupdating test count, please wait...")
          (setq testfile (airwave-test-from-comp-buffer comp-buffer))
          (with-current-buffer (find-file-noselect testfile)
            (airwave-set-test-count testcount))
          (message "%sAuto-Updated testcount to %s" parse-output testcount)
          )))))

(defun airwave-test-from-comp-buffer (buffer)
  (with-current-buffer buffer
    (goto-char (point-min))
    (while (and (not (looking-at "/")) (not (eobp))) (forward-line 1))
    (if (eobp) (error "Couldnt parse last test from buffer"))
    (buffer-substring (point) (line-end-position))))

(defun next-error-recenter ()
   (interactive)
   (let ((code-window (selected-window)))
     (next-error)
     (pop-to-buffer (compilation-find-buffer))
     (recenter)
     (select-window code-window) 
     (recenter)
     )
   )

(defun previous-error-recenter ()
   (interactive)
   (let ((code-window (selected-window)))
     (previous-error)
     (pop-to-buffer (compilation-find-buffer))
     (recenter)
     (select-window code-window) 
     (recenter)
     )
   )

(defun airwave-find-test-file (filename)
  (if (and filename (string-match "\\.pm\$" filename))
      (airwave-code-test-map filename)
    (if (and filename
             (string-match "\\.rb\$" filename)
             (not (file-executable-p filename))
             )
        (airwave-code-test-map filename))
    filename))

(defvar airwave-code-test-hash-table (make-hash-table :test 'equal))
(defun airwave-add-code-test-hash-item (key value)
  (let ((key-expand (substitute-in-file-name key))
        (value-expand (substitute-in-file-name value)))
    (puthash key-expand value-expand airwave-code-test-hash-table)
    (puthash value-expand key-expand airwave-code-test-hash-table)
  ))
(defun airwave-paired-files (file-pairs)
  (loop for keyvalue in file-pairs do
        (let ((key (car keyvalue)) (value (car (cdr keyvalue))))
          (airwave-add-code-test-hash-item key value))))

(defun airwave-expand-cvs-filename (filename)
  (expand-file-name filename (root)))
(defun airwave-contract-cvs-filename (filename)
  (let ((newfile (substitute-in-file-name filename)))
    (if (starts-with newfile (root))
        (setq newfile (file-relative-name newfile (root)))
      )
    newfile
    )
  )

(defun airwave-code-test-map (filename)
  (or (and (gethash (airwave-contract-cvs-filename filename) 
                     airwave-code-test-hash-table)
           (airwave-expand-cvs-filename
            (gethash (airwave-contract-cvs-filename filename) 
                     airwave-code-test-hash-table)))
      (string-replace-match "\\.pm$" filename ".t")
      (string-replace-match "\\.rb$" filename ".t")
      (string-replace-match "\\.t$"  filename ".pm")
      filename
      )
  )

(defun airwave-toggle-code-test-buffer ()
  "Switch buffer from Blah.t to Blah.pm and vice versa" 
  (interactive)
  (let ((new-filename (airwave-code-test-map (buffer-file-name))))
    (if (file-exists-p new-filename) 
        (find-file new-filename)
      (error "File doesn't exist %s" new-filename))))

;;;;;;;;;;;;;;;;;;
(defun airwave-try-expand-perlclass (old)
  (if (not old)
      (progn 
	(he-init-string (he-perlclass-beg) (he-perlclass-end))
	(if (not (he-string-member he-search-string he-tried-table))
	    (setq he-tried-table (cons he-search-string he-tried-table)))
	(setq he-expand-list (airwave-generate-perlclass-list))
        ))
  (while (and he-expand-list
	      (he-string-member (car he-expand-list) he-tried-table))
    (setq he-expand-list (cdr he-expand-list)))
  (if (null he-expand-list)
      (progn (if old (he-reset-string)) 'nil)
    (progn
      (he-substitute-string (car he-expand-list))
      (setq he-expand-list (cdr he-expand-list))
      t)))

(defun airwave-try-expand-hashitems (old)
  (let (wordsize)
    (if (not old)
        (progn
          (if (gethash (buffer-substring (- (point) 4) (point)) airwave-tab-completions)
              (setq wordsize 4)
            (if (gethash (buffer-substring (- (point) 3) (point)) airwave-tab-completions)
                (setq wordsize 3)
              (if (gethash (buffer-substring (- (point) 2) (point)) airwave-tab-completions)
                  (setq wordsize 2)
                (setq wordsize 1))))
          (he-init-string (- (point) wordsize) (point))
          (if (not (he-string-member he-search-string he-tried-table))
              (setq he-tried-table (cons he-search-string he-tried-table)))
          (setq he-expand-list 
                (gethash he-search-string airwave-tab-completions))))
    (while (and he-expand-list
                (he-string-member (car he-expand-list) he-tried-table))
      (setq he-expand-list (cdr he-expand-list)))
    (if (null he-expand-list)
        (progn (if old (he-reset-string)) 'nil)
      (progn
        (he-substitute-string (car he-expand-list))
        (setq he-expand-list (cdr he-expand-list))
        t))))

(defvar airwave-tab-completions (make-hash-table :test 'equal))
(defun airwave-populate-hash (hash pairs)
  (loop for keyvalue in pairs do
        (let ((key (car keyvalue)) (value (cadr keyvalue)))
          (puthash key value hash))))

(defun airwave-generate-perlclass-list ()
  (let (str name-part dir-part dir-list class-list class-prefix class)
   (setq str (buffer-substring-no-properties (he-perlclass-beg) (point)))
   (and (not (string-equal str "")) 
        (progn
          (if (string-match "[^:]:\$" str) (setq str (format "%s:" str)))
          (setq dir-part (reverse (string-split "::" str)))
          (setq name-part (pop dir-part))
          (setq dir-part (reverse dir-part))
          (setq class-prefix (list-join dir-part "::"))
          (setq dir-part (list-join dir-part "/"))
          (if (not dir-part) (setq dir-part ""))
          (setq dir-part (mercmerc (format "/lib/perl/%s" dir-part)))
          (setq dir-list 
                (sort (file-name-all-completions name-part dir-part)
                      'string-lessp))
          ;; (message "'%s'" (list-join dir-list "\n"))
          (loop for class in dir-list do
                (if (and (not (string-match "^\\." class))
                         (or (< (length dir-list) 3)
                             (not 
                              (string-match "^\\(CVS/\\|AMPRoot\\.pm\\)\$" 
                                            class))))
                    (progn
                      (setq class (or 
                                   (string-replace-match "\\.pm$" class "")
                                   (string-replace-match "\\/$" class "::"))
                            )
                      (if class (push 
                                 (if class-prefix 
                                     (format "%s::%s" class-prefix class) 
                                   class)  
                                 class-list)))))
          (setq class-list (reverse class-list))
          ;; (message "%s" (list-join class-list "\n"))
          class-list))))

(defun he-perlclass-beg () 
  (save-excursion (skip-chars-backward "A-Za-z0-9:") (point)))
(defun he-perlclass-end () 
  (save-excursion (skip-chars-forward "A-Za-z0-9:") (point)))
(fset 'airwave-perlclass-complete 
      (make-hippie-expand-function '(airwave-try-expand-perlclass)))
(defvar perlclass-history nil
  "History list for commands asking for a perl class.")
(defvar perlclass-minibuffer-map (copy-keymap minibuffer-local-map))
(define-key perlclass-minibuffer-map [tab] 'airwave-perlclass-complete)

;;;;;;;;;;;;;;;;;; airwave-follow (jump based on text near point)
(defun airwave-follow ()
  "Jump somewhere else based on what is under the point, module or file"
  (interactive)
  (airwave-push-mark)
  (let (diff-buffer (buffer (current-buffer)))
    (if (is-svn-buffer (buffer-name buffer)) (setq diff-buffer buffer))
    (if (point-on-subdefp) (airwave-find-supersub)
      (if (point-on-object-methodp) (airwave-find-object-method)
        (if (point-on-methodp) (airwave-find-method)
          (if (point-on-modulep) (airwave-find-module)
            (if (point-on-qwsubp) (airwave-find-qwsub)
              (if (point-on-constantp) (airwave-find-constant)
                (if (point-on-variablep) (airwave-find-variable)
                  (if (point-on-urlp) (airwave-find-url)
                    (if (point-on-functionp) (airwave-find-function)
                      (if (point-on-svn-revision) (airwave-diff-for-revision-at-point)
                        (if (point-on-devamp) (airwave-diff-for-devamp)
                          (if (function-at-point) (find-function (function-at-point))
                            (if (airwave-find-file 1) 'nil
                              (airwave-find-function))))))))))))))
    (airwave-set-diff-buffer diff-buffer)
    ))

; XXX this only seems to work if your ctrl-left-click lands in your already-active window
;   (if you have more than one window in your active frame).
(defun airwave-follow-mouse (event)
  "Jump somewhere else based on where the mouse event started"
  (interactive "e")
  (set-buffer (window-buffer (posn-window (event-start event))))
  (goto-char (car (cdr (event-start event))))
  (airwave-follow))

(defun airwave-super-follow ()
  "Jump somewhere else based on what is under the point, module or file"
  (interactive)
  (airwave-push-mark)
  (let (diff-buffer (buffer (current-buffer)))
    (if (is-svn-buffer (buffer-name buffer)) (setq diff-buffer buffer))
    (if (point-on-subdefp) (airwave-find-prevmodule-sub) ; only in super
      (if (point-on-object-methodp) (airwave-find-prevmodule-sub) ; only in super
        (if (point-on-methodp) (airwave-find-prevmodule-method) ; only in super
          (if (point-on-modulep) (airwave-find-module)
            (if (point-on-qwsubp) (airwave-find-qwsub)
              (if (point-on-constantp) (airwave-find-constant)
                (if (point-on-variablep) (airwave-find-variable)
                  (if (point-on-urlp) (airwave-find-url)
                    (if (point-on-functionp) (airwave-find-function)
                      (if (point-on-svn-revision) (airwave-diff-for-revision-at-point t) ; only in super
                        (if (point-on-devamp) (airwave-diff-for-devamp)
                          (if (function-at-point) (find-function (function-at-point))
                            (if (airwave-find-sandbox-file 1) 'nil ; only in super
                              (airwave-find-function))))))))))))))
    (airwave-set-diff-buffer diff-buffer)
    ))

(defun airwave-find-prevmodule-sub ()
  (save-excursion
    (let (beg sub)
      (skip-chars-forward "A-Za-z0-9_")
      (setq beg (point))
      (skip-chars-backward "A-Za-z0-9_")
      (setq sub (buffer-substring beg (point)))
      (airwave-find-module-with-sub (format "%s->%s" (prev-module) sub)))))

(defun airwave-push-mark ()
  (interactive)
  (while (and global-mark-ring
	   (eq (marker-buffer (car global-mark-ring)) (current-buffer)))
      (pop global-mark-ring))
  (push-mark nil t nil))

(defun airwave-eval-buffer ()
  (interactive)
  (eval-buffer)
  (message "eval-buffer"))

(defun airwave-find-constant ()
  (let ((str (current-constant)))
    (airwave-find-module "Mercury::Constants") 
    (goto-char (point-min))
    (search-forward str 'nil 't)
    (back-to-indentation)))
(defun point-on-constantp ()
  (save-excursion
    (and (> (length (current-constant)) 6)
         (progn (skip-chars-backward "A-Za-z0-9_")
                (and (not (string= (previous-char) ":"))
                     (not (string= (previous-char) "/")))
                ))))
                     
(defun point-on-subdefp ()
  (save-excursion
    (move-to-sub-definition)
    (skip-chars-backward " ")
    (safe-backward-char 3)
    (looking-at "sub ")))
(defun move-to-sub-definition ()
  (when (not (string-match "{" (current-line-prefix)))
    (beginning-of-line)
    (skip-chars-forward " ")
    (forward-char 5)
    (skip-chars-backward "A-Za-z0-9_")))

(defun safe-backward-char (distance)
  (if (<= (point) distance) (goto-char 0) (backward-char distance)))

(defun subdef-name-im-on ()
  (save-excursion
    (let (beg)
      (if (not (point-on-subdefp))
          ""
        (progn
          (move-to-sub-definition)
          (skip-chars-forward "A-Za-z0-9_")
          (setq beg (point))
          (skip-chars-backward "A-Za-z0-9_")
          (buffer-substring beg (point)))))))
(defun airwave-find-supersub ()
  (save-excursion
    (let (beg sub (module (buffer-file-module)))
      (move-to-sub-definition)
      (setq sub (subdef-name-im-on))
      (if (not (airwave-find-module-with-sub 
                (format "%s->%s" (airwave-perl-superclass) sub)))
          (progn
            (airwave-find-module module)
            (message "Could not find %s" 
                     (format "%s->SUPER::%s" module sub)))))))
            
(defun point-on-modulep ()
  (let ((module (current-module)))
    (and (string-match "::" module)
         (not (string-match "^SUPER::" module)))))
(defun airwave-find-function-super ()
  (let (files function exported)
    (setq function (current-function))
    (setq files (airwave-files-defining-function function))
    (if (= (length files) 1)
        (progn
          (find-file (last-element files))
          (message "Found in: %s" (airwave-short-filename))
          (airwave-goto-method-in-module 'nil function))
      (progn
        (setq exported (airwave-exported-function function))
        (if (and (stringp exported) (string= "NONE" exported))
            (message "%s" (list-join (sort (mapcar 'airwave-shorten-filename files) 
                                           'string-lessp) "\n"))
          (airwave-goto-method-in-module (car exported) function))))))

(defun airwave-files-defining-function (function)
  (setq str (shell-command-to-string 
             (format (concat "find %s -type f -not -name '.*' -not -name '*~' "
                             "! -path '*/.svn/*' "
                             "| xargs grep -l 'sub %s '")
                     (mercmerc "/lib/perl")
                     function)))
  (if (string= str "") (error "Couldn't find any subs called %s" function))
  (string-split "\n" (string-replace-match "\n\$" str "")))

(defun airwave-exported-function (function &optional noerr)
  (let (str)
    (setq str (shell-command-to-string
               (format "exported_function %s" function)))
    (setq str (or (string-replace-match "\n\$" str "") str))
    (if (and (string= str "NONE") (not noerr)) 
        (error (concat "Couldn't find any subs called %s: "
                       "Try M-x rebuild-export-cache") function))
    (string-split " " str)))
(defun rebuild-export-cache ()
  (interactive)
  (call-process "sh" nil "*rebuild-export-cache*" nil "-c" 
                "exported_function -r")
  (message "expire output is in *rebuild-export-cache*"))
  

(defun point-on-functionp ()
  (save-excursion
    (skip-chars-forward "A-Za-z0-9_\\-")
    (looking-at "\(")))
(defun airwave-find-function ()
  (if (not (airwave-goto-method-in-module 'nil (current-function)))
      (airwave-find-function-super)))

(defun point-on-variablep ()
  (save-excursion
    (if (looking-at "[@$%]") (forward-char 1))
    (skip-chars-backward "{A-Za-z0-9_")
    (safe-backward-char 1)
    (looking-at "[@$%]")))

(defun point-on-urlp ()
  (save-excursion
    (if (not (search-backward " " (line-beginning-position) t))
        (beginning-of-line))
    (skip-chars-forward " ")
    (looking-at "https?://")))

(defun airwave-find-url ()
  (let (end handler class)
    (save-excursion
      (skip-chars-forward "A-Za-z:/0-9_.")
      (setq end (point))
      (skip-chars-backward "A-Za-z_")
      (setq handler (buffer-substring (point) end)))
    (setq class (shell-command-to-string 
                 (concat
                  "perl -MMercury::Handler::DispatcherMap -e '"
                  "print Mercury::Handler::DispatcherMap->fetch_map->{\"/"
                  handler
                  "\"}{class}'"
                  )))
    (if (string= "" class) (error "Couldn't determine class for %s" handler))
    (airwave-find-module class)))
(defun point-on-hash-keyp ()
  (save-excursion
    (search-forward " " (line-end-position) 't)
    (looking-at "=>")))
(defun point-on-qwsubp ()
  (save-excursion
    (let ((beg (point)))
      (and (progn
             (search-backward-regexp "\(" 'nil 't)
             (safe-backward-char 2)
             (looking-at "qw"))
           (progn 
             (beginning-of-line) 
             (looking-at "use "))
           (progn (search-forward-regexp "\)")
                  (> (point) beg))))))
(defun airwave-find-qwsub ()
  (let (module function)
    (save-excursion
      (setq function (buffer-substring
                      (progn (skip-chars-backward "A-Za-z0-9_\\-") (point))
                      (progn (skip-chars-forward "A-Za-z0-9_\\-") (point))))
      (search-backward-regexp "\(" 'nil 't)
      (beginning-of-line)
      (forward-char 4)
      (setq module (current-module)))
    (airwave-goto-method-in-module module function)))
(defun airwave-find-variable ()
  (let ((variable (current-variable)) quotevar (skip t) (beg (point)))
    (setq quotevar (regexp-quote variable))
    (if (string-match "^\\\\\\$" quotevar)
        (setq quotevar (string-replace-match "^\\\\\\$" quotevar "[@$%]")))
    (end-of-line)
    (while skip
      (search-backward-regexp (concat "[^[:word:]]"
                                      "\\(my\\|our\\|local\\) [^=;\n]*" 
                                      quotevar 
                                      "[^[:word:]]"))
      (setq skip 'nil)
      (forward-char 1)
      (if (looking-at "\\(my\\|our\\|local\\) *\\([@$%][A-Za-z0-9_]+\\)")
          (if (not (string-match 
                    (format "^%s$" quotevar) 
                    (buffer-substring (match-beginning 2) (match-end 2))))
              (setq skip t)))
      (if (looking-at "\\(my\\|our\\|local\\) *[^(@$% ]") (setq skip t))
      )
    (search-forward-regexp quotevar)
    (skip-chars-backward "A-Za-z0-9_")
    (backward-char 1)
    
    (if (and (eq beg (point)) (string= (previous-key-string) "<f4>"))
        (progn (set-mark-command t) (set-mark-command t)))
    ))

(defun airwave-perl-superclass ()
  (save-excursion
    (goto-char (point-min))
    (let (superclass (found 'nil))
      (while (and (not found) (not (eobp)))
        (if (looking-at "use base qw(")
          (progn
            (while (looking-at "use base qw(") (forward-line 1))
            (forward-line -1)
            (end-of-line-ignore-whitespace)
            (search-backward-regexp " ")
            (if (looking-at " qw(") (forward-char 4) (forward-char 1))
            (looking-at " *\\(.*?\\)[ \\)]")
            (setq superclass 
                  (buffer-substring (match-beginning 1) (match-end 1)))
            (setq found t))
          (forward-line 1)))
      superclass)))

(defun airwave-find-object-method ()
  (move-over-arrow)
  (let ((info (current-method-call)) obj method class default-class)
    (setq obj (pop info))
    (setq method (pop info))
    (setq default-class (buffer-file-module))
    (if (string-match "^SUPER::" method)
        (progn
          (setq default-class (airwave-perl-superclass))
          (setq method (string-replace-match "^SUPER::" method ""))))
    (setq class (read-from-minibuffer 
                 (if (string-equal "" obj)
                     (format "Class for ->%s: " method)
                   (format "Class for $%s->%s: " obj method))
                 (if default-class
                     (cons default-class 
                           (+ (string-match "::" default-class) 3))
                   'nil)
                 perlclass-minibuffer-map
                 'nil
                 'perlclass-history))
    (airwave-find-module-with-sub (format "%s->%s" class method))))

(defun move-over-arrow ()
  (if (looking-at "->")
      (forward-char 2)
    (if (and (looking-at ">") (string-equal (previous-char) "-"))
        (forward-char 1))))
(defun point-on-object-methodp ()
  (and (point-on-methodp) 
       (save-excursion
         (move-over-arrow)
         (skip-chars-backward "A-Za-z0-9:_\\->")
         (safe-backward-char 1)
         (looking-at "\\$"))))
                           
(defun point-on-methodp ()
  (save-excursion
    (move-over-arrow)
    (skip-chars-backward "A-Za-z0-9:_\\-")
    (safe-backward-char 2)
    (looking-at "->")))
(defun point-on-class-methodp ()
  (and (not (point-on-object-methodp)) (point-on-methodp)))

(defun airwave-find-method ()
  (move-over-arrow)
  (let ((info (current-method-call)) class method)
    (setq class (pop info))
    (setq method (pop info))
    (if (or (string= "" class) (downcasep class))
        (airwave-find-object-method)
      (airwave-find-module-with-sub (current-method)))))

(defun airwave-find-prevmodule-method ()
  (move-over-arrow)
  (let ((info (current-method-call)) class method)
    (setq class (pop info))
    (setq method (pop info))
    (airwave-find-module-with-sub (format "%s->%s" (prev-module) method))))

(defun airwave-find-module (&optional mod-string)
  "Switch buffer from Module::Foo::Bar::baz Module/Foo/Bar.pm" 
  (interactive)
  (if (not mod-string) (setq mod-string (current-module-with-optional-method)))
  (if (string-match "^M::H::" mod-string)
      (setq mod-string (string-replace-match "^M::H::" mod-string "Mercury::Handler::")))
  (if (string-match "^M::" mod-string)
      (setq mod-string (string-replace-match "^M::" mod-string "Mercury::")))
  (if (not (airwave-find-inlined-module mod-string))
      (if (string-match "->" mod-string)
          (airwave-find-module-with-sub mod-string)
        (progn
          (let ((new-filename (whichpm mod-string)))
            (if (file-exists-p new-filename)
                (progn (find-file new-filename) (message mod-string))
              (airwave-find-module-with-sub mod-string)))))))

(defun airwave-find-inlined-module (mod-string)
  (let (line-char)
    (if (string= mod-string "") 'nil
      (if (setq line-char 
                (first-line-matching (format "package.*%s;" mod-string)))
          (if (< (airwave-count-lines (point-min) line-char) 10) 'nil
            (progn 
              (goto-char line-char) 
              (back-to-indentation)
              't))
        'nil
        ))))

(defun first-line-matching (regexp)
  (let ((line-char 'nil))
    (save-excursion
      (goto-char 0)
      (if (search-forward-regexp regexp 'nil 't)
          (setq line-char (line-beginning-position))
        )
      line-char
      )
    ))

(defun airwave-find-module-with-sub (mod-string)
  (let ((parsed (parse-method mod-string)) module function)
    (setq module (pop parsed))
    (setq function (pop parsed))
    (if (not function) (error "Can't find Module: %s" module))
    (if (or (not module) (string= module ""))
            (error "Can't find Module: %s" function))
    (if (airwave-goto-method-in-module module function)
        (message "%s->%s" module function)
      (airwave-find-method-definition module function))))

(defun airwave-goto-method-in-module (module method)
  (if module (airwave-find-module module))
  (let (delegator (beg (point)) (found t))
    (save-excursion
      (if (not (looking-at "package ")) (goto-char (point-min)))
      (if (search-forward-regexp 
           (format "sub[ \t\n\r]+%s[ \t\n\r\(]+.*{" method) 'nil 't)
          (setq beg (line-beginning-position))
        (setq found 'nil)
        ))
    (when (not found)
      (save-excursion
        (goto-char (point-min))
        (when (search-forward-regexp "Class::Delegator" 'nil 't)
          (when (airwave-search-forward-but-not-past (format "\\b%s\\b" method) ";")
            (setq beg (line-beginning-position))
            (setq found 't)
            (setq delegator 't)
            ))))
    (goto-char beg)
    (if delegator (recenter 'nil) (recenter 1))
    found))

(defun airwave-search-forward-but-not-past (findregex dontpassregex)
  (let ((beg (point)) found)
    (when (search-forward-regexp findregex 'nil 't)
      (if (string-match dontpassregex (buffer-substring beg (point)))
          (goto-char beg)
        (setq found 't)))
    found))

(defun airwave-find-method-definition (module method)
  (let ((isa (shell-command-to-string (format "isa -li %s %s" module method)))
        definer found)
    (string-match "Method Lookup: \\(.*\\)" isa)
    (setq definer (substring isa (match-beginning 1) (match-end 1)))
    (setq isa (substring isa (1+ (match-end 1))))
    (setq found (airwave-goto-method-in-module definer method))
    (message isa)
    found
    ))

(defun airwave-find-file (&optional quiet new-filename)
  "Switch buffer from Module/Foo/Bar.pm:491: Module/Foo/Bar.pm line 491" 
  (interactive)
  (let ((linenum 7) beg)
    (if (not new-filename) (setq new-filename (filename-under-point)))
    (setq filedata (append (split-string new-filename ":") '("-1")))
    (setq new-filename (pop filedata))
    (setq linenum (string-to-int (pop filedata)))
    (when (eq linenum -1)
      (save-excursion
        (search-forward " " nil t)
        (when (or (looking-at "line ")
                  (looking-at "at line "))
          (when (looking-at "at ") (forward-char 3))
          (forward-char 5)
          (setq beg (point))
          (skip-chars-forward "0-9")
          (when (not (eq (point) beg))
            (setq linenum (string-to-int (buffer-substring beg (point))))))))
    (setq dirs (cons default-directory (list
      (mercmerc "/lib/perl/Mercury")
      (mercmerc "/lib/perl")
      (mercmerc "/lib/templates/html")
      (mercmerc "/lib/templates")
      (mercmerc "/lib/html")
      (mercmerc "")
      (home)
      ""
    )))
    (let (found) 
      (if (not (dolist (dir dirs found)
            (setq full-filename (format "%s/%s" dir new-filename))
            (if (string-match "^//" full-filename)
                (setq full-filename (string-replace-match "^//" full-filename "/")))
            (if (and (airwave-file-exists-p full-filename) (not found)) 
                (progn 
                  (setq found t)
                  (find-file full-filename)
                  (if (not (equal linenum -1)) (goto-line linenum)))
              )
            ))
          (if (airwave-find-possible-module new-filename) 't
            (progn 
              (or quiet (error "File doesn't exist: %s" new-filename))
              'nil))
        found))))

(defun airwave-find-sandbox-file (quiet)
  (airwave-find-file quiet 
   (airwave-sandbox-filename (filename-under-point))))

(defun airwave-find-possible-module (module)
  (condition-case nil
      (airwave-find-module module)
    (error nil)))

(defun filename-under-point ()  ;; parse out AP/R2.pm:20:
  (save-excursion
    (skip-chars-backward "A-Za-z0-9:/_.\\-")
    (let ((beg (point)))
      (skip-chars-forward "A-Za-z0-9:/_.\\-")
      (buffer-substring beg (point)))))

;;;;;;;;;;;;;;;;;; indent whole cperl buffer 
(defun airwave-indent-buffer ()
  "Indent the entire buffer"
  (interactive)
  (save-excursion
    (load-library "cperl-mode")
    (mark-whole-buffer)
    (cperl-indent-region (point) (mark))
  )
)

(defun airwave-revert-buffer ()
  (interactive)
  (if (buffer-modified-p) 
      (progn
        (undo-start)
        (while (buffer-modified-p) (undo-more 1)))))
;;;;;;;;;;;;;;;;;;
(defun airwave-method-extract (beg end method-name argstring)
  (interactive "r\nsMethod name:\nsMethod args:")
  (setq end-marker (make-marker))
  (set-marker end-marker end)

  ; add call to method
  (goto-char beg)
  (beginning-of-line)
  (setq beg (point))
  (insert "\n$self->" method-name "(" argstring ");")
  (cperl-indent-line)
  (insert "\n\n")

  ; add subroutine wrapper
  (if (not (string= argstring "")) (setq argstring (concat ", " argstring)))
  (insert "sub " method-name " {\n")
  (insert "  my ($self" argstring ") = @_;\n")
  (goto-char end-marker);
  (insert "}\n\n")
  (push-mark (point))

  ; reindent
  (goto-char beg)
  (forward-line 4)
  (cperl-indent-region (point) end-marker)
  (backward-line)
  (set-marker end-marker nil)
)

(defun condense-request-params ()
  "Replace $r->param(this, that); with   this => that, for cleanliness."
  (interactive)
  (query-replace-regexp "\\$r->param(\\(.*\\) => \\(.*\\));" "  \\1 => \\2,"))

(defun face-at-point ()
  "shows face under point"
  (interactive)
  (if (interactive-p)
      (princ (plist-get (text-properties-at (point)) 'face))
    (plist-get (text-properties-at (point)) 'face)))

(defun last-element (list)
  (if (listp list) (car (last list)) list))
(defun downcasep (string)
  (let ((char (car (string-to-list string))))
    (and (string-match "^[A-Za-z]" string) (eq char (downcase char)))
    ))
(defun upcasep (string)
  (let ((char (car (string-to-list string))))
    (and (string-match "^[A-Za-z]" string) (eq char (upcase char)))
    ))
(defun list-join (list &optional join-str)
  "Joins string LIST with JOIN-STR, whic defaults to space."
  (let* (ret
	 (ch  (or join-str " ")))
    (while list
      (setq ret (concat (or ret "") (car list)))
      (setq list (cdr list))
      (if list				;only if still elements
	  (setq ret (concat ret ch))))
    ret))

(defun copy-from-above-or-below (&optional arg below)
  "Copy characters from previous nonblank line, starting just above point.
Copy ARG characters, but not past the end of that line.
If no argument given, copy the entire rest of the line.
The characters copied are inserted in the buffer before point."
  (interactive "P")
  (let ((cc (current-column)) n (string "") wordlen)
    (save-excursion
      (if below (progn (end-of-line) (skip-chars-forward "\ \t\n"))
        (progn (beginning-of-line) (skip-chars-backward "\ \t\n")))
      (move-to-column cc)
      ;; Default is enough to copy the whole rest of the line.
      (save-excursion
        (let ((start (point)))
          (forward-word 1)
          (setq wordlen (- (point) start))))
      (setq n (if arg wordlen (point-max)))
      ;; If current column winds up in middle of a tab,
      ;; copy appropriate number of "virtual" space chars.
      (if (< cc (current-column))
	  (if (= (preceding-char) ?\t)
	      (progn
		(setq string (make-string (min n (- (current-column) cc)) ?\ ))
		(setq n (- n (min n (- (current-column) cc)))))
	    ;; In middle of ctl char => copy that whole char.
	    (backward-char 1)))
      (setq string (concat string
			   (buffer-substring
			    (point)
			    (min (save-excursion (end-of-line) (point))
				 (+ n (point)))))))
    (insert string)))

;;;;;;;;;;;;;;;;;;
(defun airwave-emacs-todo (todo)
  "Add something to emacs todo list"
  (interactive "sEmacs Todo: ")
  (let ((mybuff (current-buffer)))
    (save-excursion
      (airwave-dotemacs)
      (goto-char (point-min))
      (while (not (looking-at "; To do:")) (forward-line 1))
      (forward-line 2)
      (insert (format "; %s\n" todo))
      (save-buffer)
      )
    (switch-to-buffer mybuff)
    )
  )
(defun airwave-devquote (quote)
  "Add something to emacs todo list"
  (interactive "sDevQuote: ")
  (let ((mybuff (current-buffer)))
    (save-excursion
      (find-file "/root/devquotes.txt")
      (setq buffer-read-only 'nil)
      (goto-char (point-max))
      (insert (format "\n%s\n" quote))
      (save-buffer)
      )
    (switch-to-buffer mybuff)
    )
  )

(defun airwave-dotemacs ()
  "Jump to dotemacs"
  (interactive)
  (find-file 
   (if (athome) "~/.emacs" (mercmerc "/lib/conf/emacs"))))

(defun airwave-airwavefunc ()
  "Jump to airwave-func.el"
  (interactive)
  (find-file 
   (if (athome) 
       "~/.elisp/airwave-func.el" 
     "~/svn/mercury/lib/conf/elisp/airwave-func.el")))
(defun airwave-airwavegeneric ()
  "Jump to airwave-func.el"
  (interactive)
  (find-file 
   (if (athome) 
       "~/.elisp/airwave-generic.el" 
     "~/svn/mercury/lib/conf/elisp/airwave-generic.el")))
(defun airwave-airwavemerc ()
  "Jump to airwave-func.el"
  (interactive)
  (find-file 
   (if (athome) 
       "~/.elisp/airwave-for-mercury.el" 
     "~/svn/mercury/lib/conf/elisp/airwave-for-mercury.el")))
(defun athome ()
  (string-match "blakem.com\$" (system-name)))
(defun onfedora () (string= (getenv "OS") "FC3"))
(defun airwave-cvs-root ()
  (if (athome) (home "/airwave/svn") (home "/svn")))
(setq airwave-cvs-root (airwave-cvs-root))

(defun machine-name ()
  (let ((sysname (system-name)))
    (or (string-replace-match "\\..*" sysname "")
        sysname)))
(defun set-mark-and-goto-line (line)
   "Set mark and prompt for a line to go to."
   (interactive "NGoto Line: ")
   (push-mark nil t nil)
   (goto-line line))
(defun airwave-short-filename ()
  (if (buffer-file-name)
      (airwave-shorten-filename (buffer-file-name))))
(defun airwave-shorten-filename (filename)
  (let ((base (mercmerc)))
  (or 
   (string-replace-match 
    (format "^%s%s" base "/lib/perl/Mercury/Daemon/") filename "M:D/")
   (string-replace-match 
    (format "^%s%s" base "/lib/perl/Mercury/Handler/") filename "M:H/")
   (string-replace-match 
    (format "^%s%s" base "/lib/perl/Mercury/Reports/") filename "M:R/")
   (string-replace-match 
    (format "^%s%s" base "/lib/perl/Mercury/Test/") filename "M:T/")
   (string-replace-match 
    (format "^%s%s" base "/lib/perl/Mercury/Utility/") filename "M:U/")
   (string-replace-match 
    (format "^%s%s" base "/lib/perl/Mercury/") filename "")
   (string-replace-match 
    (format "^%s%s" base "/lib/perl/") filename "M/")
   (string-replace-match 
    (format "^%s%s" base "/lib/") filename "")
   (string-replace-match 
    (format "^%s%s" base "/") filename "")
   (string-replace-match 
    "^/root/" filename "~/")
   (string-replace-match 
    "^/home/blakem/" filename "~/")
   )))

;;;;;;;;;;;;;;;;;; shell mode utilities
(defun airwave-shell-directory-tracker (cmd)
  "Wrapper around shell-directory-tracker to handle AirWave aliases."
  (let ((newdir (assoc cmd airwave-shell-dir-aliases)))
    (if newdir 
        (shell-directory-tracker (concat "cd " (mercmerc (cdr newdir)) "\n"))
      (shell-directory-tracker cmd))))
(defun airwave-add-shell-directory-tracker ()
  (remove-hook 'comint-input-filter-functions 'shell-directory-tracker)
  (add-hook 'comint-input-filter-functions 'airwave-shell-directory-tracker))
(add-hook 'shell-mode-hook 'airwave-add-shell-directory-tracker)

(defun tail-f (file)
  (interactive "Ftail -f ")
  (find-file-read-only file)
  (view-mode)
  (process-kill-without-query 
    (start-process "tail-f" (current-buffer) "tail" "-0f"
;    "/dev/null" ; necessary for some versions of GNU tail
    (expand-file-name file)))
  (setq buffer-file-name nil)
  (auto-save-mode nil)
  (goto-char (point-max)))
;;;;;;;;;;;;;;;;;;

(defun airwave-find-file-hook ()
  (when (string= (buffer-name) "svn-commit.tmp")
    (make-local-variable 'make-backup-files)
    (setq make-backup-files 'nil))
  )
(add-hook 'find-file-hooks 'airwave-find-file-hook)

(defun airwave-which-func-cleanup (str)
  (or (string-replace-match ".*::" str "") 
      (string-replace-match "^\\+Hierarchy\\+\\.\\.\\.\$" str "")
      str))
(defun airwave-set-frame-title ()
  (modify-frame-parameters nil 
                           (list (cons 'title 
                                       (airwave-build-frame-title)))))
(defun airwave-build-frame-title ()
  (let* ((w1 (frame-first-window)) (curr-win w1) (win-list nil) buffstr)
    (while (progn
             (setq curr-win (previous-window curr-win 'no nil))
             (setq buffstr (buffer-file-name (window-buffer curr-win)))
             (if (not buffstr) (setq buffstr 
                                     (buffer-name (window-buffer curr-win)))
               (setq buffstr (airwave-shorten-filename buffstr)))
             (setq win-list (cons buffstr win-list))
             (not (eq curr-win w1))))
    (format "emacs@%s : %s" (machine-name) (list-join win-list " | "))))
;;;;;;;;;;;;;;;;;;

(defun airwave-x-select-text (text &optional push)
  (if (or (not this-command) ; for mouse-drag-region
          (string= this-command "kill-ring-save"))
      (x-select-text text push)))
(defun airwave-x-paste-text ()
  (let ((current-key (previous-key 0)))
    (if (or (not (integer-or-marker-p current-key))
            (listp current-key)
            (not (or (= current-key ?\s-p) (= current-key ?\s-v) (= current-key ?\C-y))))
        (x-cut-buffer-or-selection-value))))
(defun airwave-paste-it ()
  (interactive)
  (insert (format "%s" (x-get-selection 'PRIMARY 'COMPOUND_TEXT))))

(defun airwave-set-background () 
  (interactive)
  (set-face-background 'default "black")
  (set-face-foreground 'default "white"))
(defun airwave-set-frame-size (&optional width height)
  (interactive)
  (if (not width) (setq width 0.85))
  (if (not height) (setq height 0.95))
  (set-frame-size 
   (selected-frame) 
   (round (/ (* (x-display-pixel-width) width) (frame-char-width)))
   (round (/ (* (x-display-pixel-height) height) (frame-char-height)))))
(defun airwave-set-frame-properties () 
  (interactive)
  (airwave-set-frame-size)
  (airwave-set-background))
(defun airwave-maximize-frame ()
  (interactive)
  (airwave-set-frame-size 0.95 0.95)
  (set-frame-position (selected-frame) 0 0)
  )
(defun airwave-frame-current-width ()
  (/ (float (round (* (/ (float (* (frame-char-width) (frame-parameter nil 'width)))
     (x-display-pixel-width)) 100))) 100))
(defun airwave-frame-current-height ()
  (/ (float (round (* (/ (float (frame-pixel-height)) 
     (x-display-pixel-height)) 100))) 100))

(defun airwave-readable-testoutput ()
  (interactive)
  (remove-text-properties (point-min) (point-max)
                          '(mouse-face highlight help-echo nil))
  (replace-string "\\n" "\012" 'nil (point-min) (point-max))
  (replace-string "\\t" "    " 'nil (point-min) (point-max))
  (replace-string "\\\\" "" 'nil (point-min) (point-max))
  (replace-string "\\cJ" "\012" 'nil (point-min) (point-max))
  (replace-string "\\\"" "\"" 'nil (point-min) (point-max)))
(defun airwave-tailapache (&optional tailn)
  (interactive "P")
  (if (null tailn) (setq tailn 50))
  (airwave-format-apache-log "/var/log/httpd/ssl_error_log" 
                             "*apache ssl_error tail*" 
                             tailn)
  (airwave-format-apache-log "/var/log/httpd/error_log" 
                             "*apache error tail*" 
                             tailn)
  (delete-other-windows)
  (switch-to-buffer "*apache error tail*")
  (split-window-vertically)
  (other-window 1)
  (switch-to-buffer "*apache ssl_error tail*")
  (recenter 0)
  )

(defun airwave-format-apache-log (file buffername tailn)
  (airwave-select-empty-output-buffer buffername)
;;  (picture-mode)   # want picture-mode like movement for long lines....
  (shell-command (format "tail -%s %s > /tmp/apache_tail" tailn file))
  (insert-file "/tmp/apache_tail")
  (airwave-reformat-debugging-output)
  (local-set-key [s-left]  '(lambda () (interactive) (airwave-horizontal-scroll -15)))
  (local-set-key [s-right] '(lambda () (interactive) (airwave-horizontal-scroll 15)))
  (local-set-key [s-up]    '(lambda () (interactive) (next-line -5)))
  (local-set-key [s-down]  '(lambda () (interactive) (next-line 5)))
  (local-set-key [M-up]    '(lambda () (interactive) (airwave-apache-tail-up)))
  (local-set-key [M-down]  '(lambda () (interactive) (airwave-apache-tail-down)))
  )
(defun airwave-horizontal-scroll (&optional n)
  (interactive "P")
  (if (null n) (setq n 10))
  (forward-char n)
  (set-window-hscroll (get-buffer-window (current-buffer)) (+ (window-hscroll) n)))
(defun airwave-reformat-debugging-output ()
  (interactive)
  (airwave-readable-testoutput)
  (replace-regexp "\\( called\\)? at " "\012      called at " 'nil (point-min) (point-max))
  (airwave-color-regexp "\\[client .*\\] \\(.*\\)" 'nil "firebrick1")
  (replace-regexp "\\[client .*\\] " "\\&\012  " 'nil (point-min) (point-max))
  (replace-string ".pm line " ".pm:" 'nil (point-min) (point-max))
  (replace-regexp ", referer: .*" "        \\&\012" 'nil (point-min) (point-max))
  (replace-regexp ", referer: " "referer: " 'nil (point-min) (point-max))
  (airwave-color-regexp "^\\[.*$" "sienna1")
  (airwave-color-regexp "\\[\\(error\\)\\]" 'nil "violetred1")
  (airwave-color-regexp "\\[\\(warn\\)\\]" 'nil "orange2")
  (airwave-color-regexp "\\[\\(notice\\)\\]" 'nil "sienna3")
  (airwave-color-regexp "\\([::A-Za-z_]+::\\)\\([A-Za-z_]+(\\)" 'nil 
                        "light sky blue" "dark orange")
  (goto-char (point-max))
  (search-backward-regexp "^\\[" 'nil t)
  (setq truncate-lines t)
)

;;;;;;;;;;;;;;;;;;
(defun airwave-translate-and-replace ()
  (interactive)
  (let ((translation (airwave-get-oid-translation (current-oid))))
    (skip-chars-backward "A-Za-z0-9_\\-\\.")
    (zap-to-char 1 32)
    (insert (cdr translation))
    (insert " ")))

(defun airwave-translate-oid ()
  (interactive)
  (let ((translation (airwave-get-oid-translation (current-oid))))
    (kill-new (cdr translation))
    (message "%s" (car translation))))

(defun airwave-get-oid-translation (oid)
  (let (output trimmed)
    (if (string-match "[A-Za-z]" oid)
        (setq output (chomp (shell-command-to-string 
                             (format "snmptranslate -On -IR %s" oid))))
      (setq output (chomp (shell-command-to-string 
                           (format "snmptranslate %s" oid)))))
    (setq trimmed output)
    (if (string-match "::" output)
        (setq trimmed (string-replace-match ".*::" output "")))
    (cons output trimmed)))

(defun airwave-oid-info ()
  (interactive)
  (let ((output (airwave-get-oid-info (current-oid))))
    (kill-new output)
    (message "%s" output)))

(defun airwave-get-oid-info (oid)
  (chomp (shell-command-to-string
          (if (string-match "[A-Za-z]" oid)
              (format "snmptranslate -Td -IR %s" oid)
            (format "snmptranslate -Td %s" oid)))))

(defun airwave-type-from-oid-info (oidinfo)
  (if (integerp (string-match "SYNTAX\\W+\\(\\w[A-Z ]+\\w+\\)" oidinfo))
      (substring oidinfo (match-beginning 1) (match-end 1))
    nil))

(defun snmp-config-map-type (snmptools-type)
  (let ((type-map '(("INTEGER" . "i")
                    ("Integer32" . "i")
                    ("Unsigned32" . "u")
                    ("OCTET STRING" . "s")
                    ("IpAddress" . "a"))))
    (if (assoc snmptools-type type-map)
        (cdr (assoc snmptools-type type-map))
      (concat "UNKNOWN TYPE " snmptools-type ": please add it to snmp-config-map-type"))))

(defun current-oid ()
  (interactive)
  (save-excursion
    (let (beg end oid)
      (skip-chars-forward "$A-Za-z0-9_\.")
      (skip-chars-backward "\.")
      (setq end (point))
      (skip-chars-backward "$A-Za-z0-9_\.")
      (setq beg (point))
      (setq oid (interpolate-perl-variables (buffer-substring beg end)))
      (if (not (string-match "^\\.?1\\.\\|[A-Za-z]" oid))
          (interpolate-perl-variables (format "%s%s" "$prefix" oid))
        oid))))
(defvar interpolate-perl-vars-history nil
  "History list for commands asking for a mib base.")
(defun interpolate-perl-variables (str)
  (while (string-match "\\$" str)
    (setq str (interpolate-perl-variable str)))
  str
  )
(defun interpolate-perl-variable (str)
  (let (perlvar)
    (string-match "\\$[A-Za-z_0-9]+" str)
    (setq perlvar (substring str (match-beginning 0) (match-end 0)))
    (string-replace-match "\\$[A-Za-z_0-9]+" str
                          (read-from-minibuffer 
                           (format "%s: " perlvar)
                           'nil
                           'nil
                           'nil
                           interpolate-perl-vars-history))))
(defun chomp (str) (or (string-replace-match "[\n\r]+\$" str "") str))

;;;;;;;;;;;;;;;;;;
(defun airwave-isearch-yank-char ()
  (interactive)
  (isearch-yank-string (buffer-substring (point) (1+ (point)))))
(defun airwave-isearch-star-triggers-regex-mode ()
  (interactive)
  (if (not isearch-regexp) (isearch-toggle-regexp))
  (isearch-process-search-char (string-to-char "*")))

(defun airwave-cdiff (&optional n)
  (interactive "P")
  (airwave-cdiff-viewer nil "*svn diff*" n))
(defun airwave-sorted-cdiff (&optional n)
  (interactive "P")
  (airwave-cdiff-viewer t "*sorted svn diff*" n))
(defun airwave-cdiff-viewer (sort buffername n)
  (let ((diff-args "") linenum)
    (airwave-save-and-save-some-buffers)
    (switch-to-buffer (get-buffer-create buffername))
    (setq linenum (current-line-number))
    (erase-buffer)
    (diff-mode)
    (if n (setq diff-args " -w"))
    (shell-command (format "cd %s; %s" (root)
                           (airwave-build-svn-diff-cmd diff-args))
                   buffername)
    (goto-char (point-min))
    (airwave-shell-function-insert (format "cd %s; svn status | perl -ne '/^\\?/ && print'" (root)))
    (write-region (point-min) (point-max) "/tmp/current_cdiff.diff")
    (if (nonempty-file-p "/tmp/current_cdiff.diff")
        (progn
          (when sort
            (erase-buffer)
            (insert-file (airwave-sort-diff-file "/tmp/current_cdiff.diff"))
            )
          (switch-to-buffer buffername)
          (airwave-diffstat-on-buffer)
          (goto-line linenum)
          )
      (insert (format "Empty%s diff.\n" 
                      (if (string= (svnproj) "mercury") "" 
                        (concat " " (svnproj))))))
      ))
(defun airwave-sort-diff-file (file)
  (let ((sort_file file))
    (setq sort_file (or (string-replace-match "\\.diff$" file "-sorted.diff")
                        (concat file ".sorted")))
    (shell-command (format "sort_diff.pl -o %s %s" sort_file file))
    sort_file))
  
(defun nonempty-file-p (filename)
  (let (size)
    (setq size (nth 7 (file-attributes filename)))
    (and size (not (= size 0)))
    ))
(defun airwave-build-svn-diff-cmd (diffargs &optional svnargs)
  (if (not diffargs) (setq diffargs ""))
  (if (not svnargs) (setq svnargs ""))
  (format "svn diff --diff-cmd diff --extensions '-u --show-function-line=^[[:space:]]*sub[[:space:]] %s' %s" diffargs svnargs))

(defun airwave-diffstat-on-buffer ()
  (interactive)
  (shell-command-on-region (point-min) (point-max) "sorted_diffstat" "*diffstat*"))

(defun airwave-save-and-save-some-buffers ()
  (if (buffer-file-name) (save-buffer))
  (save-some-buffers))

(defun airwave-apply-hunk (&optional revert)
  (interactive)
  (save-excursion 
    (let ((filename (diff-find-file-name)) errormsg)
      (condition-case err (diff-apply-hunk revert)
        (error (setq errormsg (error-message-string err))))
      (save-other-buffer (get-file-buffer filename))
      (when (and errormsg (not (string= errormsg "No next hunk")))
        (message errormsg)))))

(defun airwave-revert-hunk ()
  (interactive) 
  (airwave-apply-hunk 't))

(defun airwave-revert-file ()
  (interactive)
  (save-excursion
    (airwave-walk-diff-hunks 'airwave-revert-hunk)))
(defun airwave-apply-file ()
  (interactive)
  (save-excursion
    (airwave-walk-diff-hunks 'airwave-apply-hunk)))
(defun airwave-walk-diff-hunks (func)
  (let (beg)
    (beginning-of-line 1)
    (if (looking-at "Index: ") (forward-line))
    (search-backward-regexp "^Index: ")
    (forward-line)
    (setq beg (point))
    (condition-case nil (search-forward-regexp "^Index: ")
      (error (goto-char (point-max))))
    (while (> (point) beg) 
      (progn
        (condition-case nil (search-backward-regexp "^@@ ")
          (error (goto-char (point-min))))
        (if (> (point) beg) (funcall func))))))

(defun save-other-buffer (buffer)
  (with-current-buffer buffer
    (progn
      (set-buffer-modified-p 't)
      (save-buffer))))
  
(defun airwave-whatisa (class)
   "Run whatisa"
   (interactive (list (read-string "whatisa: " 
                                   (module-on-point-or-current-module-with-sub))))
   (airwave-shell-function (format "whatisa %s" class) "*whatisa*")
   (airwave-rename-other-buffer (format "*whatisa %s*" class)))

(defun airwave-isa (class)
   "Run isa"
   (interactive (list (read-string "isa: " 
                                   (module-on-point-or-current-module-with-sub))))
   (airwave-shell-function (format "isa %s" class) "*isa*")
   (airwave-rename-other-buffer (format "*isa %s*" class)))

(defun module-on-point-or-current-module ()
  (if (point-on-modulep) (current-module) (buffer-file-module)))
(defun module-on-point-or-current-module-with-sub ()
  (if (point-on-subdefp)
      (format "%s %s" (module-on-point-or-current-module) (subdef-name-im-on))
    (if (point-on-class-methodp)
        (format "%s %s" (current-method-class) (current-method-method))
      (if (point-on-methodp)
          (format "%s %s" (module-on-point-or-current-module) (current-method-method))
        (module-on-point-or-current-module)))))

(defun airwave-make-testfile (class)
  (interactive (list (read-string "Create .t for: " 
                                  (module-on-point-or-current-module))))
  (airwave-shell-function (format "mkclass -t %s" class) "discard output")
  (let ((file class))
    (if (string-match "::" file) (setq file (string-replace-match "::" file "/" 'nil t)))
    (airwave-find-file 'nil (format "%s.t" file))))

(defun airwave-make-pm (class)
  (interactive (list (read-string "Create .pm for: " 
                                  (module-on-point-or-current-module))))
  (airwave-shell-function (format "mkclass %s" class) "discard output")
  (airwave-find-module class))

(defun airwave-shrink-copyright ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (search-forward-regexp "^# *All Rights Reserved\.[ 	]*$" nil t)
    (beginning-of-line 2)
    (while (empty-line-suffix) (forward-line 1))
    (if (looking-at "use strict;") (forward-line 1))
    (while (empty-line-suffix) (forward-line 1))

    (let ((overlay (make-overlay (point-min) (point))))
      (overlay-put overlay 'face '(:height 10)))
    )
  )

(defun airwave-iswitchb-otherwindow ()
  (interactive)
  "Toggle new buffer in otherwindow setting"
  (let ((buffer  (car iswitchb-matches)))
    (if (not (eq iswitchb-method 'otherwindow))
        (progn 
          (message "Other window: %s" buffer) 
          (setq iswitchb-method 'otherwindow))
      (progn (message "Same window:  %s" buffer) 
             (setq iswitchb-method 'always-frame)))))

(defun airwave-kill-old-buffers (&optional seconds)
  "Kill buffers that haven't been visited recently"
  (interactive)
  (if (not seconds) (setq seconds 600))
  (let ((time (time-to-seconds (current-time))) 
        (curbuf (buffer-name))
        bufname)
    (loop for buf in (buffer-list) do
          (with-current-buffer buf
            (setq bufname (buffer-name))
            ;; (message "buffer: %s" bufname)
            (when (and
                   (not (equal bufname "*scratch*"))
                   (not (equal bufname "*Messages*"))
                   (not (equal bufname "AP.pm"))
                   (not (equal bufname curbuf))
                   (or 
                    (string-match "^\\*.*\\*$" bufname)
                    (is-svn-buffer bufname)
                    (not buffer-display-time)
                    (> (- time (time-to-seconds buffer-display-time)) seconds))
                   (or 
                    (string-match "^\\*.*\\*$" bufname) 
                    (not (buffer-modified-p))))
              (kill-buffer buf)))))
  (message "Killing old buffers"))

(defun airwave-kill-least-significant-buffers ()
  "Kill buffers that haven't been modified much"
  (interactive)
  (let ((curbuf (buffer-name))
        bufname tokill modfiles buffile)
    (setq modfiles 
          (string-split "\n" 
           (shell-command-to-string "sorted_diffstat -q -p /tmp/current_cdiff")))
    (loop for buf in (buffer-list) do
          (setq bufname (buffer-name buf))
          ;; (message "buffer: %s" bufname)
          (when (and
                 (not (equal bufname "*scratch*"))
                 (not (equal bufname "*Messages*"))
                 (not (equal bufname "AP.pm"))
                 (not (equal bufname curbuf)))
            (setq tokill 1)
            (setq buffile (buffer-file-name buf))
            (when buffile
              (loop for file in modfiles do
                    (if (string= buffile (mercmerc (format "/%s" file)))
                        (setq tokill 'nil))))
            (if tokill (kill-buffer buf)))))
  (message "Killing least significant buffers"))

;;;;;; cvs navigation ;;;;;;

(defun airwave-hide-copyleft (&optional noundo)
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (not (search-forward "\^M" nil t))
        (progn
          (setq hide-copyleft-mode 'nil)
          (setq selective-display 'nil)
          ))
    (hide-copyleft-mode)
    (if noundo (setq buffer-undo-list 'nil))
    ))

(defun airwave-rdiff-backward ()
  (interactive)
  (let ((revs (airwave-diff-revision-numbers)) r1 r2 file prev)
    (setq r1 (pop revs))
    (setq r2 (pop revs))
    (setq file (airwave-file-being-diffed))
    (when (string= r1 "1") (error "r1 is as low as it goes buddy."))
    (progn
      (clear-vc-diff-buffer)
      (airwave-vc-revision-diff file r1))))
(defun airwave-svn-version-before (rev file)
  (shell-command-to-string (format "previous_svn_revision.pl -p -i %s %s" file rev)))
(defun airwave-svn-version-after (rev file)
  (shell-command-to-string (format "previous_svn_revision.pl -n -i %s %s" file rev)))
(defun airwave-svn-version-first (file)
  (shell-command-to-string (format "previous_svn_revision.pl -f -i %s" file)))
(defun airwave-svn-version-last (file)
  (shell-command-to-string (format "previous_svn_revision.pl -l -i %s" file)))
(defun airwave-show-first-checkin-diff (file)
  (airwave-shell-function-insert 
   (format "build_initial_svn_diff.pl %s" file))
  (goto-char (point-min))
  )

(defun airwave-rdiff-forward ()
  (interactive)
  (let ((revs (airwave-diff-revision-numbers)) r1 r2 r3 file)
    (setq r1 (pop revs))
    (setq r2 (pop revs))
    (setq file (airwave-file-being-diffed))
    (if (or (not r2)
            (>= (string-to-int r2) (string-to-int (vc-workfile-version file))))
        (airwave-diff-jump-to-line file)
      (airwave-vc-revision-diff-after file r2))))

(defun airwave-diff-jump-to-line (&optional file)
  (interactive)
  (if (not file) (setq file (airwave-file-being-diffed)))
  (let ((window-line (window-line))
        (source-char (airwave-diff-source-char-at-point))
        (linenum (airwave-diff-source-linenum))
        (column (1- (current-column)))
        (diff-buffer (current-buffer)))
    (if (< column 0) (setq column 0))
    (if (= linenum -1) (progn (setq column 0) (setq linenum 1)))
    (find-file file)
    (airwave-set-diff-buffer diff-buffer)
    (if (and source-char 'nil) ;;; linenum/column is more acurate, i think... 
        (if (>= source-char (point-max))
            (progn (goto-line linenum) (move-to-column column))
          (goto-char source-char))
      (progn (goto-line linenum) (move-to-column column)))
    (recenter window-line)
    ))
(defun airwave-set-diff-buffer (buffer)
  (if buffer 
      (progn
        (make-local-variable 'airwave-diff-buffer)
        (setq airwave-diff-buffer buffer))))

(defun airwave-load-latest-diff-buffer ()
  (interactive)
  (switch-to-buffer airwave-diff-buffer))

(defun clear-vc-diff-buffer ()
  (airwave-select-empty-output-buffer "*vc-diff*")
  (diff-mode))

(defun airwave-file-being-diffed ()
  (or (save-excursion
        (and (airwave-goto-diff-index-line)
             (airwave-parse-next-rcs-file-line)))
      (save-excursion
        (airwave-parse-file-from-triple-signs))))

(defun airwave-parse-file-from-triple-signs ()
  (let (file)
    (beginning-of-line)
    (if (looking-at "\\+\\+\\+\\|\\-\\-\\-\\|Index:\\|===")
        (search-forward "@@"))
    (search-backward-regexp "^\\+\\+\\+" 'nil t)
    (setq file (airwave-parse-file-from-diff-file-line))
    (when (string= file "/dev/null")
      (search-backward-regexp "^\\-\\-\\-" 'nil t)
      (setq file (airwave-parse-file-from-diff-file-line))
      )
    file))

(defun airwave-parse-file-from-diff-file-line ()
  (beginning-of-line)
  (search-forward " ")
  (let ((beg (point)))
    (search-forward-regexp "[ \t]")
    (forward-char -1)
    (airwave-full-cvsfile (buffer-substring beg (point)))))
(defun airwave-parse-next-rcs-file-line ()
  (if (search-forward-regexp "^RCS file: " 'nil t)
    (let ((beg (point)) cvsfile)
      (end-of-line)
      (search-backward-regexp ",v" (- (point) 5) t)
      (airwave-full-cvsfile (buffer-substring beg (point))))
    'nil))
(defun airwave-full-cvsfile (file)
  (if (not (string-match "^/" file))
      (format "%s/%s" (root) file)
    (or (string-replace-match "/usr/local/cvsroot/mercury-code" 
                              file 
                              (root))
        (string-replace-match "/usr/local/cvsroot/" 
                              file 
                              (home "/svn/"))
        file)))
(defun airwave-goto-diff-index-line ()
  (beginning-of-line)
  (if (not (looking-at "Index: ")) (search-backward-regexp "^Index: " nil t))
  (looking-at "Index: "))
(defun airwave-diff-revision-numbers ()
  (save-excursion
    (airwave-goto-diff-index-line)
     (let (r1 r2 rv lastrev)
       (setq r1 (airwave-parse-next-revision-line))
       (forward-line 1)
       (if (string-match "revision " (current-line))
           (setq r2 (airwave-parse-next-revision-line))
         (when (string-match "working copy" (current-line))
           (setq lastrev (airwave-svn-version-last (airwave-file-being-diffed)))
           (if (> (string-to-int r1) (string-to-int lastrev)) (setq r1 lastrev))))
       (if (and r1 r2 (eq (1+ (string-to-int r1)) (string-to-int r2)))
           (setq r1 (airwave-svn-version-before r2 (airwave-file-being-diffed))))
       (push r2 rv) (push r1 rv) rv)))
(defun end-of-line-ignore-whitespace ()
  (interactive)
  (goto-char (line-end-position-ignore-whitespace)))
(defun line-end-position-ignore-whitespace ()
  (save-excursion
    (end-of-line)
    (while (or (string= (previous-char) " ")
               (string= (previous-char) "	"))
      (backward-char 1))
    (point)))

(defun airwave-delete-trailing-whitespace-from-line ()
  (save-excursion
    (end-of-line)
    (while (or (string= (previous-char) " ")
               (string= (previous-char) "	"))
      (delete-backward-char 1))))  
(defun airwave-delete-trailing-whitespace ()
  (interactive)
  (setq initial-buffer (current-buffer))
  (if (is-svn-buffer (buffer-name initial-buffer))
    (airwave-diff-jump-to-line))
  (airwave-delete-trailing-whitespace-from-line)
  (if (is-svn-buffer (buffer-name initial-buffer))
    (progn
      (save-buffer)
      (switch-to-buffer initial-buffer)
      (airwave-delete-trailing-whitespace-from-line))))

(defun airwave-parse-next-revision-line ()
  (interactive)
  (let ((start (point)) indexpoint revisionpoint)
    (forward-line 1)
    (condition-case nil (search-forward-regexp "^Index: ")
      (error (goto-char (point-max))))
    (setq indexpoint (point))
    (goto-char start)
    
    (search-forward-regexp "revision " nil t)
    (setq revisionpoint (point))
    (if (> revisionpoint indexpoint) 'nil
      (progn
        (end-of-line-ignore-whitespace)
        (skip-chars-backward " ()")
        (buffer-substring revisionpoint (point))))))

(defun airwave-cvs-log ()
  (interactive)
  (let (rev revs)
    (if (string-match "Annotate" (buffer-name))
        (progn
         (setq rev (airwave-diff-rev-for-annotate-line))
         (find-file (airwave-diff-file-being-annotated)))
      (if (or (string= (buffer-name) "*vc-diff*")
              (string= (buffer-name) "*svn revision diff*")
              (string= (buffer-name) "*svn diff span*"))
          (progn
            (setq revs (airwave-diff-revision-numbers))
            (setq rev (pop revs))
            (setq rev (pop revs))
            (find-file (airwave-file-being-diffed)))
        (if (or (string= (buffer-name) "*svn diff*")
                (string= (buffer-name) "*sorted svn diff*"))
            (progn
              (save-excursion
                (beginning-of-line)
                (if (looking-at "+") (error "That line is yours, my friend..."))
                )
              (setq revs (airwave-diff-revision-numbers))
              (setq rev (pop revs))
              (find-file (airwave-file-being-diffed))))))

    (if (not (buffer-file-name))
        (if (point-on-svn-revision)
            (airwave-shell-function 
             (format "svn log -r%s %s" (revision-at-point) (svnbase))
             "*svn comment*")
          (message "Can't show log from here???"))
      (progn
        (vc-print-log)
        (delete-other-windows)
        (sit-for 15)
        (when rev 
          (search-forward-regexp (format "^r%s" rev))
          (beginning-of-line)
          (airwave-color-line "magenta"))
        (beginning-of-line)))))
(defun airwave-svn-annotate ()
  (interactive)
  (let (rev revs (linenum -3) namecolor (codeline ""))
    (if (string= (buffer-name) "*vc-change-log*")
        (progn
          (setq rev (airwave-diff-rev-for-svnlog-line))
          (find-file (airwave-diff-file-being-svnlogged)))
      (if (or (string= (buffer-name) "*vc-diff*")
              (string= (buffer-name) "*svn diff span*"))
          (progn
            (setq linenum (airwave-diff-source-linenum))
            (setq codeline
                  (buffer-substring (1+ (line-beginning-position))
                                    (line-end-position)))
            (save-excursion
              (setq revs (airwave-diff-revision-numbers))
              (setq rev (pop revs))
              (beginning-of-line)
              (if (not (looking-at "-")) (setq rev (pop revs))))
            (find-file (airwave-file-being-diffed)))
        (if (or
             (string= (buffer-name) "*svn revision diff*")
             (string= (buffer-name) "*svn diff*")
             (string= (buffer-name) "*sorted svn diff*"))
            (progn 
              (save-excursion
                (beginning-of-line)
                (if (looking-at "+") (error "That line is yours, my friend..."))
                (setq linenum (airwave-diff-source-linenum))
                (setq codeline
                      (buffer-substring (1+ (line-beginning-position))
                                        (line-end-position)))
                (setq revs (airwave-diff-revision-numbers))
                (setq rev (pop revs))
                )
              (find-file (airwave-file-being-diffed)))
          (if (string-match "Annotate" (buffer-name))
              (progn
                (setq linenum (current-line-number))
                (setq codeline (airwave-codeline-in-annotate-buffer))
                (setq rev (airwave-diff-rev-for-annotate-line))
                (find-file (airwave-diff-file-being-annotated))
                )
            (progn
              (setq rev (vc-workfile-version (buffer-file-name)))
              (setq linenum (current-line-number))
              (setq codeline (current-line))
              )))))
    (let ((buf (buffer-name)) matchcount)
      (beginning-of-line)
      (airwave-vc-annotate-version rev)
      (switch-to-buffer (format "*Annotate %s version*" buf))
      (goto-char (point-min))
      (insert (format "*** VERSION %s ***\n" rev))
      (setq linenum (+ linenum 4))
      (goto-line linenum)
      (setq matchcount (lines-in-buffer-matching codeline))
      (if (not (= matchcount 1))
          (message (format "Matching Lines: %s" matchcount)))
      (setq namecolor (or 
                       (and (= matchcount 1) "magenta")
                       (and (> matchcount 1) "blue violet")
                       (and (< matchcount 1) "orange")))
      (airwave-goto-nearest-line-matching codeline matchcount)
      (delete-other-windows)
      )
    (if (> linenum 1)
        (progn
          (beginning-of-line)
          (skip-chars-forward "0-9 ")
          (airwave-color-word namecolor)
          (recenter)
          ))
    ))
(defun airwave-codeline-in-annotate-buffer ()
  (save-excursion
    (beginning-of-line)
    (search-forward ": ")
    (buffer-substring (point) (line-end-position))))
(defun airwave-goto-nearest-line-matching (str &optional matchcount)
  (if (not matchcount) (setq matchcount (lines-in-buffer-matching str)))
  (if (not (string-match str (current-line)))
      (if (= matchcount 1)
          (progn 
            (goto-char (point-min))
            (search-forward str)
            (beginning-of-line))
        (if (> matchcount 1)
            (condition-case nil 
                (search-backward str (- (point) 5000))
              (error (search-forward str (+ (point) 2500) t))))))
  (beginning-of-line))

(defun airwave-svn-log-for-revision ()
  "Shows the log for the revision number you are currently looking at."
  (interactive)
  (let (rev)
    (if (point-on-svn-revision)
        (setq rev (revision-at-point))
      (setq rev (read-string "Log for Revision: ")))
    (switch-to-buffer (generate-new-buffer "*svn-log*"))
    (erase-buffer)
    (insert (shell-command-to-string (format "cd %s; svn log -r %s" (root) rev)))))

(defun airwave-rdiff ()
  (interactive)
  (airwave-vc-revision-diff (buffer-file-name)
                            (vc-workfile-version (buffer-file-name))))
(defun airwave-diff-source-char-at-point ()
  (interactive)
   (let ((rev (not (save-excursion (beginning-of-line) (looking-at "[-<]")))))
     (condition-case nil
         (progn
           (destructuring-bind (buf line-offset pos src dst &optional switched)
               (diff-find-source-location 'nil rev)
             (+ pos (cdr src))))
       (error 'nil))
     ))
(defun airwave-diff-file-being-annotated ()
  (let ((name (buffer-name)))
    (setq name (string-replace-match "\.*Annotate " name ""))
    (setq name (string-replace-match " version\.*" name ""))
    (when (string-match "|" name)
      (setq name (string-replace-match "|.*" name "")))
    (concat default-directory name)))

(defun skip-whitespace-forward ()
  (skip-chars-forward " \r\t\n"))

(defun airwave-diff-rev-for-annotate-line ()
  (save-excursion
    (beginning-of-line)
    (skip-whitespace-forward)
    (current-word)))

(defun airwave-diff-from-annotate ()
  (interactive)
  (let ((file (airwave-diff-file-being-annotated))
        (r2 (airwave-diff-rev-for-annotate-line))
        (linenum (airwave-line-in-annotation))
        (codeline (airwave-codeline-being-annotated))
        (annotated-revision (airwave-annotated-revision)))
    (airwave-vc-revision-diff file r2)
    (sit-for 15)
    (if (string= r2 annotated-revision)
        (airwave-goto-linenum-in-diff linenum)
      (progn
        (search-forward-regexp (format "^+%s$" codeline) 'nil t)
        (beginning-of-line)
        )
      )))
(defun airwave-diff-left ()
  (interactive)
  (if (string-match "Annotate" (buffer-name))
      (airwave-diff-from-annotate)
    (if (string= (buffer-name) "*vc-change-log*")
        (airwave-diff-from-svnlog)
      (if (or (string= (previous-key-string) "<s-left>")
              (and (string= (previous-key-string 5) "ESC")
                   (string= (previous-key-string 4) "O")
                   (string= (previous-key-string 3) "t")))
          (airwave-rdiff)
        (airwave-cvs-file-diff)))))
(defun airwave-cvs-file-diff ()
  (interactive)
  (let ((linenum (current-line-number)) (diffrv (vc-diff 'nil 't)))
    (if (not (or (not diffrv) (stringp diffrv)))
        (progn
          (setq buffer-read-only 'nil)
          (delete-other-windows)
          (airwave-goto-linenum-in-diff linenum)))))
(defun airwave-svn-file-diff ()
  (interactive)
  (let ((linenum (current-line-number)))
    (airwave-shell-function (airwave-build-svn-diff-cmd (buffer-file-name)) "*svn-diff*")
    (diff-mode)
    (setq buffer-read-only 'nil)
    (airwave-goto-linenum-in-diff linenum)))

(defun airwave-diff-file-being-svnlogged ()
  (save-excursion
    (goto-char (point-min))
    (search-forward ": ")
    (concat default-directory (buffer-substring (point) (line-end-position)))))
(defun airwave-diff-rev-for-svnlog-line ()
  (save-excursion
    (search-backward (make-string 72 ?-))
    (forward-line 1)
    (revision-at-point)))
(defun airwave-diff-from-svnlog ()
  (interactive)
  (airwave-vc-revision-diff (airwave-diff-file-being-svnlogged)
                            (airwave-diff-rev-for-svnlog-line)))
(defun airwave-vc-annotate-version (rev)
  "Annotate a specific revision of a file"
  (vc-ensure-vc-buffer)
  (let* ((temp-buffer-name (concat "*Annotate " (buffer-name) " version*"))
         (temp-buffer-show-function 'vc-annotate-display)
         (vc-annotate-ratio 2.0)
         (vc-annotate-backend (vc-backend (buffer-file-name))))
    (message "Annotating...")
    (with-output-to-temp-buffer temp-buffer-name
      (vc-call-backend vc-annotate-backend 'annotate-command
		       (file-name-nondirectory (buffer-file-name))
		       (get-buffer temp-buffer-name)
                       rev))
    (setq vc-annotate-buffers
	  (append vc-annotate-buffers
		  (list (cons (get-buffer temp-buffer-name) vc-annotate-backend))))
    (message "Annotating... done")))
(defun airwave-diff-back-to-source ()
  (interactive)
  (let ((window-line (window-line)))
    (if (string= (buffer-name) "*vc-change-log*")
        (find-file (airwave-diff-file-being-svnlogged))
      (if (string-match "Annotate" (buffer-name))
          (find-file (airwave-diff-file-being-annotated))))
    (airwave-recenter window-line)
    ))
(defun airwave-view-version ()
  "Visit version REV of the current file in another window."
  (interactive)
  (let (rev)
    (if (string= (buffer-name) "*vc-change-log*")
        (progn
          (setq rev (airwave-diff-rev-for-svnlog-line))
          (find-file (airwave-diff-file-being-svnlogged)))
      (if (string= (buffer-name) "*vc-diff*")
          (progn 
            (setq revs (airwave-diff-revision-numbers))
            (setq rev (pop revs))
            (setq rev (pop revs))
            (find-file (airwave-file-being-diffed))
            )
        (if (string-match "Annotate" (buffer-name))
            (setq rev (airwave-diff-rev-for-annotate-line))
          (setq rev (read-from-minibuffer "Version to visit: ")))))
    (vc-version-other-window rev)
    (delete-other-windows)))

(defun airwave-diff-source-linenum ()
  (interactive)
  (let (minus orig beg hasprevhunk (start (point)) (linenum -1))
    (save-excursion
      (search-backward "@@" 'nil 't)
      (setq hasprevhunk (looking-at "@@"))
      (goto-char start)
      (beginning-of-line)
      (if hasprevhunk
          (progn
            (setq minus (looking-at "-"))
            (setq orig (point))
            (airwave-diff-hunk-prev-safe)
            (search-forward (if minus "-" "+"))
            (setq beg (point))
            (while (looking-at "[0-9]") (forward-char 1))
            (setq linenum (1- (string-to-int (buffer-substring beg (point)))))
            (while (< (point) orig)
              (progn
                (forward-line 1)
                (if (looking-at "-")
                    (if minus (setq linenum (1+ linenum)))
                  (if (looking-at "+")
                      (if (not minus) (setq linenum (1+ linenum)))
                    (setq linenum (1+ linenum))))))
            ))
      linenum)))
(defun airwave-color-region (beg end color)
  (when font-lock-mode (font-lock-mode -1))
  (let ((face (intern (concat "fg:" color))))
    (or (facemenu-get-face face) (error "Unknown color: %s" color))
    (facemenu-add-face face beg end)
    ))
(defun airwave-color-line (color)
  (interactive)
  (airwave-color-region (line-beginning-position) (line-end-position) color))
(defun airwave-color-word (color)
  (interactive)
  (let ((start (point)))
    (save-excursion
      (airwave-forward-word)
      (airwave-color-region start (point) color))))

(defun airwave-color-regexp (regexp color &optional color1 color2 color3)
  (interactive)
  (save-excursion
    (let ((face (intern (concat "fg:" color)))
          (face1 (intern (concat "fg:" color1)))
          (face2 (intern (concat "fg:" color2)))
          (face3 (intern (concat "fg:" color3)))
          )
      (goto-char (point-min))
      (or (facemenu-get-face face)
          (error "Unknown color: %s" color))
      (or (facemenu-get-face face1)
          (error "Unknown color: %s" color1))
      (or (facemenu-get-face face2)
          (error "Unknown color: %s" color2))
      (or (facemenu-get-face face3)
          (error "Unknown color: %s" color3))
      (while (search-forward-regexp regexp nil t)
        (if color
            (facemenu-add-face face (match-beginning 0) (match-end 0)))
        (if color1
            (facemenu-add-face face1 (match-beginning 1) (match-end 1)))
        (if color2
            (facemenu-add-face face2 (match-beginning 2) (match-end 2)))
        (if color3
            (facemenu-add-face face3 (match-beginning 3) (match-end 3))))
      )))
(defun airwave-line-in-annotation ()
  (- (current-line-number) 1))
(defun airwave-goto-linenum-in-diff (linenum)
  (airwave-goto-diff-index-line)
  (let ((curline 0) lasthunk)
    (condition-case nil
        (while (<= curline linenum)
          (progn
            (diff-hunk-next)
            (forward-line 1)
            (setq curline (airwave-diff-source-linenum))))
      (error (setq lasthunk t)))
    (if (not lasthunk) 
        (progn (forward-line -2) 
               (airwave-diff-hunk-prev-safe)
               (forward-line 1)))
    (setq curline (airwave-diff-source-linenum))
    (forward-line 1)
    (while (and (< curline linenum) (not (eobp)))
      (progn
        (while (looking-at "-") (forward-line 1))
        (setq curline (airwave-diff-source-linenum))
        (forward-line 1)
        ))
    (forward-line -1)
    )
  (if (eobp) (goto-char (point-min)))
  )
(defun airwave-diff-hunk-prev-safe ()
  (let (hasprevhunk)
    (save-excursion
      (beginning-of-line)
      (setq hasprevhunk (search-backward  "@@" 'nil 't)))
    (if hasprevhunk (diff-hunk-prev))))
(defun airwave-annotated-revision ()
  (save-excursion
    (goto-char (point-min))
    (search-forward "VERSION ")
    (let ((beg (point)))
      (while (looking-at "[0-9\.]") (forward-char 1))
      (buffer-substring beg (point)))))
(defun airwave-codeline-being-annotated ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (forward-word 2)
    (forward-char 1)
    (buffer-substring (point) (line-end-position))))
(defun airwave-kill-cvs-buffers()
  "Kills all buffers doing cvs stuff (annotations, logs, diffs)"
  (interactive)
  (let* ((list (buffer-list)) (buffer (car list)))
    (loop for buffer in (buffer-list) do
          (if (is-svn-buffer (buffer-name buffer))
              (kill-buffer buffer))))
  (message "Killing cvs related buffers")
  )
(defun airwave-what-file ()
  (interactive)
  (if (and (buffer-file-name) (not (in-diff-mode)))
      (message (buffer-file-name))
    (airwave-show-diff-file)))
(defun in-diff-mode () (equal major-mode 'diff-mode))
(defun airwave-show-diff-file ()
  (let (indexfile)
    (save-excursion
      (airwave-goto-diff-index-line)
      (search-forward "Index: ")
      (setq indexfile (buffer-substring (point) (line-end-position)))
      )
    (message "You are looking at %s" indexfile)))
(defun airwave-diff-hunk-next ()
  (interactive) (diff-hunk-next) (airwave-recenter 7))
(defun airwave-diff-hunk-prev ()
  (interactive) (diff-hunk-prev) (airwave-recenter 7))
(defun airwave-diff-file-next ()
  (interactive) (diff-file-next) (airwave-recenter 5))
(defun airwave-diff-file-prev ()
  (interactive) (diff-file-prev) (airwave-recenter 5))

(defun airwave-recenter (n) (if (not (at-bottom)) (recenter n)))
(defun at-bottom () (= (window-end) (point-max)))
(defun window-line () (airwave-count-lines (window-start) (point)))
(defun window-line-from-bottom () (airwave-count-lines (point) (window-end)))
(defun is-svn-buffer (name)
  (or (string-match "^\\*.*\\(vc\\|svn\\|Annotate\\|diff\\).*\\*$" name)
      (string-match "\\.diff$" name)
      (string-match "\\.checkin_comments$" name)
      (string-match "\\.~[0-9.]+~$" name)))

(defun airwave-cdiff-what-else ()
  (interactive)
  (save-some-buffers)
  (let ((newrev (cadr (airwave-diff-revision-numbers))))
    (airwave-show-revision-diff newrev "*svn diff span*")
    ))
(defun airwave-diff-for-revision (rev &optional proj branch)
   "Show an svn revision diff"
   (interactive (list (read-string "Diff for Revision: " (current-number))))
   (let (frombase)
     (when (string-match "^r" rev)
       (setq rev (substring rev 1))
       (setq frombase t))
     (airwave-show-revision-diff rev "*svn revision diff*" frombase proj branch)))

(defun airwave-diff-for-revision-at-point (&optional checkbranch)
  (let (proj branch)
    (save-excursion
      (when (looking-at "r") (forward-char 1))
      (skip-chars-backward "0-9")
      (when (previous-string ":r") 
        (backward-char 2)
        (setq proj (previous-string "mercury" "devtoys" "amp_license" "build-centos"
                                    "svn-migration" "thirdparty" "amp-install" "client"
                                    "linux-java-devel" "aml" "assertparse")
              )))
    (when (string-match "Branch: [A-Za-z0-9_\/]+\]" (current-line-prefix))
      (setq branch (parse-branch-from-string (current-line-prefix))))
    (save-excursion
      (skip-chars-forward "r0-9")
      (when (looking-at " | [a-z]+ | ")
        (setq branch (parse-branch-from-svnlog))))
    (airwave-diff-for-revision (concat (if checkbranch "r" "") (revision-at-point))
                               proj branch)))
(defun parse-branch-from-svnlog ()
  (save-excursion
    (let ((beg (point)))
      (search-forward "-------------------------------------------------------")
      (parse-branch-from-string (buffer-substring beg (point))))))
(defun parse-branch-from-string (string)
  (if (string-match "Branch: [A-Za-z0-9_\/]+\]" string)
      (substring string 
                 (+ 8 (match-beginning 0)) 
                 (- (match-end 0) 1))
    nil))
(defun point-on-svn-revision ()
  (save-excursion
    (skip-chars-backward "r0-9")
    (or (looking-at "r[0-9]+[^a-zA-Z0-9]")
        (and (empty-line-prefix) (looking-at "[0-9]+[^a-zA-Z0-9]"))
        (progn (safe-backward-char 9) (looking-at "revision ")))))
(defun revision-at-point ()
  (save-excursion
    (let (end)
      (skip-chars-forward "r0-9")
      (setq end (point))
      (skip-chars-backward "0-9")
      (buffer-substring (point) end))))
(defun point-on-devamp ()
  (devamp-at-point))
(defun airwave-diff-for-devamp ()
  (airwave-your-diff (devamp-at-point)))
(defun devamp-at-point ()
  (let (devamp)
    (save-excursion
      (skip-chars-forward "a-zA-Z-_")
      (setq devamp (previous-string (devamps)))
      (if devamp 
          (progn
            (skip-chars-backward "a-zA-Z-_")
            (and (looking-at devamp) devamp))
        'nil))))

(defun airwave-show-revision-diff (rev bufname &optional frombase proj branch)
  (airwave-select-empty-output-buffer bufname)
  (diff-mode)
  (let (path (svndiffstr (airwave-build-svn-diff-cmd 'nil
          (format "-r%s:%s" (- (string-to-int rev) 1) rev))))
    (when frombase
      (setq svndiffstr (format "%s %s" svndiffstr (svnbase))))
    (when (or proj branch)
      (when (not proj) (setq proj "mercury"))
      (if (string-match "^checkins/" branch)
          (setq path (concat "/" branch))
        (if branch
            (setq path (concat "/branches/" branch))
          (setq path "/trunk")))
      (if (string= proj "devtoys") (setq path ""))
      (setq svndiffstr (format "%s %s" svndiffstr (svnurl proj path))))
    (shell-command
     (format "cd %s; %s" (root) svndiffstr)
     bufname)
    (airwave-diffstat-on-buffer)))

(defun airwave-vc-revision-diff (file r &optional prevrev)
  (let ((cachefile (airwave-cached-diff-filename file r)))
    (when (not (airwave-diff-is-cached cachefile))
      (airwave-cache-revision-diff file r prevrev))
    (clear-vc-diff-buffer)
    (insert-file cachefile)
    (setq buffer-read-only 'nil)
    (goto-char (point-min))
    (delete-other-windows)))
(defun airwave-cache-revision-diff (file r prevrev)
  (when (not prevrev) (setq prevrev (airwave-svn-version-before r file)))
  (airwave-vc-version-diff file prevrev r)
  (sit-for 10)
  (let ((cachefile (airwave-cached-diff-filename file r)))
    (make-directory (string-replace-match "/[^/]*$" cachefile "") t)
    (write-region (point-min) (point-max) cachefile)))
(defun airwave-diff-is-cached (cachefile)
  (and (file-exists-p cachefile)
       (> (nth 7 (file-attributes cachefile)) 0)))
(defun airwave-cached-diff-filename (file r)
  (concat "/tmp/time_machine_cache" file ".diff.~" r "~"))
(defun airwave-vc-revision-diff-after (file r)
  (airwave-vc-revision-diff file (airwave-svn-version-after r file) r))
(defun airwave-vc-version-diff (file r1 r2)
  (airwave-select-empty-output-buffer "*vc-diff*")
  (if (string= r1 "") (airwave-show-first-checkin-diff file)
    (vc-version-diff file r1 r2)))

;;;;;;;;
(defun airwave-copy-buffer ()
  (interactive)
  (airwave-save-and-save-some-buffers)
  (let (new (orig (buffer-file-name)) (line (current-line-number))
            (tmpfile "/tmp/.emacs.tmpbuffersave")
            (column (current-column)) origbuffer)
    (if orig 
        (setq new (read-string "Copy file to: " (format "%s" orig)))
      (progn
        (setq orig tmpfile)
        (setq origbuffer (buffer-name))
        (setq new (read-string "Write buffer to: "))
        (write-file tmpfile 'nil)
        (rename-buffer origbuffer)
        ))
    (shell-command (format "cp %s %s" orig new))
    (find-file new)
    (goto-line line)
    (move-to-column column)))
;;;;;;;;
(defun airwave-bookmark ()
  "Manage your bookmarks:
.         => current-word
bookmark! => set default
bookmark+ => move
bookmark* => move
bookmark- => delete
"
  (interactive)
  (let (bookmark prompt toset tomove todel)
    (if (not (null airwave-current-bookmark))
        (setq prompt (format "Bookmark (%s): " airwave-current-bookmark))
      (progn
        (setq prompt "Bookmark: ")
        (setq toset t)))
    (setq bookmark (completing-read prompt
       bookmark-alist nil nil nil bookmark-history))
    (while (string-match "[!*+-]$" bookmark)
      (if (string-match "!$" bookmark) (setq toset t))
      (if (string-match "[*+]$" bookmark) (setq tomove t))
      (if (string-match "-$" bookmark) (setq todel t))
      (setq bookmark (substring bookmark 0 -1)))
    (if (string= bookmark ".") (setq bookmark (current-word)))
    (if (string= bookmark "") 
        (if (null airwave-current-bookmark) 
            (error "You have no default bookmark")
          (setq bookmark airwave-current-bookmark)))
    (if toset (setq airwave-current-bookmark bookmark))
    (if todel
        (progn
          (bookmark-delete bookmark)
          (if (string= airwave-current-bookmark bookmark)
              (setq airwave-current-bookmark (car (bookmark-all-names)))))
      (if (and (assoc bookmark bookmark-alist)
               (not tomove))
          (bookmark-jump bookmark)
        (bookmark-set bookmark)))))
(defun airwave-bookmark-jump ()
  (interactive)
  (if (null airwave-current-bookmark) 
      (error "You have no default bookmark")
    (bookmark-jump airwave-current-bookmark)))
(defun drop-bookmarks ()
  (interactive)
  (setq airwave-current-bookmark 'nil)
  (loop for bookmark in (bookmark-all-names) do
        (bookmark-delete bookmark)))
;;;;;;;;
(defun lines-in-buffer-matching (str)
  (let ((linecount 0))
    (setq str (regexp-quote str))
    (save-excursion
      (goto-char (point-min))
      (if (string-match str (current-line))
          (setq linecount (1+ linecount)))
      (while (and (forward-line 1) (not (eobp)))
        (if (string-match str (current-line))
            (setq linecount (1+ linecount)))))
    linecount))

(defun airwave-toggle-pounds ()
  (interactive)
  (if (not mark-active)
      (error "Highlight region to toggle comments")
    (let ((beg (point)) (end (mark)) (end-marker (make-marker)))
      (if (> beg end) (progn (setq beg (mark)) (setq end (point))))
      (set-marker end-marker end)
      (goto-char beg)
      (while (< (point) end-marker)
        (back-to-indentation)
        (if (not (empty-line-suffix))
            (if (looking-at (substring comment-start 0 1))
                (uncomment-region (line-beginning-position) (line-end-position))
              (comment-region (line-beginning-position) (line-end-position))))
        (forward-line 1))
      (set-marker end-marker 'nil))))
;;;;;;;;;;;;;;
(defun airwave-difflists ()
  (interactive)
  (airwave-update-buffers)
  (find-file "/tmp/iteration_diffs"))
(defun airwave-suggestion-list ()
  (interactive)
  (airwave-update-buffers)
  (find-file "/tmp/use_line_suggestions"))
(defun airwave-failedlists ()
  (interactive)
  (airwave-update-buffers)
  (find-file "/tmp/testrunner_failed_tests_reason"))

(defun airwave-sort-buffer-diff ()
  (interactive)
  (if (in-diff-mode)
      (progn
        (write-file "~/diffs/local_emacs.diff" 'nil)
        (airwave-kill-this-buffer)
        (airwave-select-empty-output-buffer "*sorted svn diff*")
        (diff-mode)
        (insert-file (airwave-sort-diff-file "~/diffs/local_emacs.diff"))
        (airwave-diffstat-on-buffer)
        )))

(defun airwave-set-charmap-for-nox ()
  "Set keybindings for use in non X environments"
  (interactive)
  (require 'console)
  )
(defun airwave-set-charmap-for-x-and-nox ()
  "Set keybindings that are useful in non X environments, 
   but desired on X as well"
  (interactive)
  (setq keyboard-translate-table
        (make-char-table 'keyboard-translate-table nil))
  ;; (message "s-a:%s, other:%s" (kbd "s-a") (kbd "C-a"))
  (aset keyboard-translate-table ?\^q 8388705)
  (define-key function-key-map "\M-s" 'superify)
  (define-key function-key-map [kp-begin] 'superify)
  (define-key function-key-map [kp-5] 'superify)
  )
;;;;;;;;;;;;;;;;;;
(require 'uniquify)
(defadvice kill-buffer (after reuniquify-buffernames activate compile)
  (uniquify-rationalize-file-buffer-names))

;; see http://www.gnu.org/software/emacs/elisp-manual/html_node/elisp_693.html
(defun superify (prompt)
  (let ((e (read-event)))
    (vector (if (numberp e)
                (logior (lsh 1 23) e)
              (if (memq 'super (event-modifiers e))
                  e
                (add-event-modifier "s-" e))))))
(defun add-event-modifier (string e)
  (let ((symbol (if (symbolp e) e (car e))))
    (setq symbol (intern (concat string
                                 (symbol-name symbol))))
    (if (symbolp e)
        symbol
      (cons symbol (cdr e)))))
(defun airwave-goto-superclass ()
  (interactive)
  (airwave-find-module (airwave-perl-superclass)))

(defun airwave-blink-matching-char ()
  (interactive)
  (save-excursion
    (airwave-goto-matching-char)
    (if blink-matching-paren-on-screen
        (if (pos-visible-in-window-p) (sit-for 0.5)
          (message "Matches: %s" (current-line))))))

(defun airwave-goto-matching-char ()
  (interactive)
  (let ((pos (airwave-find-matching-position)))
    (goto-char pos)))

(defun airwave-find-matching-position ()
  (save-excursion
    (let ((matchchar (current-char)) closechar backward pos)
      (when (string= matchchar "(") (setq closechar ")"))
      (when (string= matchchar ")") (setq closechar "(") (setq backward 't))
      (when (string= matchchar "[") (setq closechar "]"))
      (when (string= matchchar "]") (setq closechar "[") (setq backward 't))
      (when (string= matchchar "{") (setq closechar "}"))
      (when (string= matchchar "}") (setq closechar "{") (setq backward 't))
      (when (string= matchchar "<") (setq closechar ">"))
      (when (string= matchchar ">") (setq closechar "<") (setq backward 't))
      (if (not closechar) (error "Not on a blinkable char, try one of '(){}[]<>'"))
      (condition-case nil 
          (airwave-matching-char-position matchchar closechar backward)
        (error (error "Couldn't find matching '%s'." closechar))))))

(defun airwave-matching-char-position (matchchar closechar backward)
  (save-excursion
    (let ((count 0) currchar pos 
          (regexp (concat (regexp-quote matchchar) "\\|" (regexp-quote closechar)))
          (myface (face-at-point))
          )
      (while (not pos)
        (if backward
            (search-backward-regexp regexp)
          (progn
            (forward-char 1)
            (search-forward-regexp regexp)
            (backward-char 1)))
        (if (not (equal (face-at-point) myface)) 'nil
          (if (string= (current-char) matchchar)
              (setq count (1- count))
            (if (= count 0) (progn (setq pos (point)))
              (setq count (1+ count))))))
      pos
      )))
(defun airwave-clean-droppings ()
  (interactive)
  (airwave-shell-function "cleanalldroppings" "discard output")
  )

(defun airwave-region-replace (&optional from to)
  (interactive)
  (airwave-region-replace-func 'search-forward 'nil from to))
(defun airwave-region-replace-regex (&optional from to)
  (interactive)
  (airwave-region-replace-func 'search-forward-regexp t from to))
(defun airwave-region-replace-func (func asregex &optional from to)
  (let ((beg (point-min)) (end (point-max)) (region "Buffer") begend lastm done)
    (when mark-active
      (setq region "Region")
      (setq beg (point))
      (setq end (mark))
      (when (> beg end)
        (setq begend beg)
        (setq beg end)
        (setq end begend)))
    (if asregex (setq region (format "%s regexp" region)))
    (if (not from)
        (setq from (read-string (format "%s replace: " region))))
    (if (not to)
        (setq to (read-string (format "%s replace %s with: " region from))))
    (goto-char beg)
    (while (and (< (point) end) (not done))
      (setq lastm (point))
      (if (not (funcall func from nil t))
          (setq done 1))
      (if (< (point) end) (replace-match to nil nil)))
    (goto-char lastm)
    ))

; a way to find out how to name a key in elisp code
(defun get-key-name ()
  "Prompts user to type a key, and prints the key's name."
  (interactive)
  (let ((key (read-key-sequence-vector "Key: ")))
    (message "%s => %s" (airwave-chord-for-key key) key)
    ))

;;;;;;;;;;;;;;;;;;
; fancy delete-other-windows 
; from: http://www.cs.berkeley.edu/~smcpeak/elisp/scott.emacs.el
(defvar my-saved-window-config-list nil)
(defun mdi-maximize-restore-toggle ()
  "When called in a multi-window frame it will save the window
  configuration by calling `current-window-configuration', then call
  `delete-other-windows'.  When called in a single-window frame it will
  restore the frame configuration by calling `set-window-configuration'."
  (interactive)
  (if (> (count-windows) 1)
      (progn
        (gc-my-window-config-list (selected-frame))
        (setq my-saved-window-config-list
              (cons (list (buffer-name) (current-window-configuration))
                    my-saved-window-config-list))
        (delete-other-windows))
    (restore-applicable-window-configuration my-saved-window-config-list)))
(defvar saved-window-config-subwindow nil)
(make-variable-buffer-local 'saved-window-config-subwindow) 
(defun maximize-restore-toggle-with-subbuffer ()
  "This will always minimize the subbuffer, not the one the point is in"
  (interactive)
  (if (> (count-windows) 1)
      (progn 
        (when saved-window-config-subwindow 
          (if (buffer-file-name) (save-buffer))
          (other-window 1))
        (mdi-maximize-restore-toggle)
        )
    (progn
      (mdi-maximize-restore-toggle)
      (when (> (count-windows) 1) ; some windows got restored
        (setq saved-window-config-subwindow 'nil)
        (other-window 1)
        (setq saved-window-config-subwindow t)))))

(defun gc-my-window-config-list (frame)
  "Remove any saved configs that apply to deleted frames or to
  the 'frame' argument."
  (setq my-saved-window-config-list
    (filter-list my-saved-window-config-list
      #'(lambda (config)
          (and
            (member (window-configuration-frame (car (cdr config))) (frame-list))
            (not (eq (window-configuration-frame (car (cdr config))) frame))
          ))
    )))
(defun restore-applicable-window-configuration (list)
  "Look through 'list' for a window config that applies to the selected
  frame.  If found, restore via that config.  If not, say so."
  (if (not list)
      (princ "There is no saved window config for this buffer.")
    (let ((bufname (car (car list)))
          (windowconfig (car (cdr (car list)))))
      (if (and (eq (window-configuration-frame windowconfig) (selected-frame))
               (eq bufname (buffer-name)))
          ; restore it
          (set-window-configuration windowconfig)
        ; else, proceed down list
        (restore-applicable-window-configuration (cdr list))))))
(defun filter-list (list predicate)
  "Return a list containing only those elements from 'list' which
  cause 'predicate' to return true."
  (if (not list)
      nil          ; recursion base case
      (if (funcall predicate (car list))
          ; keep the item
          (cons (car list) (filter-list (cdr list) predicate))
          ; else, remove it
          (filter-list (cdr list) predicate)
      )))
(defun delete-window-restorable ()
  (interactive)
  (if (eq (count-windows) 2)
      (progn
        (other-window 1)
        (mdi-maximize-restore-toggle))
    (delete-window)))
;;;;;;;;;;;;;;;;;;

(defun airwave-fix-folding ()
  (interactive)
  (airwave-region-replace "\r" "\n")
  (save-buffer))

(defun airwave-help ()
  (interactive)
  (switch-to-buffer (get-buffer-create "*airwave help*"))
  (setq truncate-lines t)
  (delete-region (point-min) (point-max))
  (let ((menu-list (cdr (cdr airwave-menu))))
    (loop for menu in menu-list do
          (let ((menu-name (car menu)) 
                (menu-items (cdr (cdr (car (cdr (cdr (cdr menu))))))))
            (insert (format "%s\n" menu-name))
            (loop for menu-item in menu-items do
                  (airwave-display-menu-item (cdr menu-item))
                  )))
    (goto-char (point-min))
    ))
(defun airwave-display-menu-item (item)
  (let (title command keys)
    (setq title (car (cdr item)))
    (setq command (car (cdr (cdr item))))
    (setq keys (last-element item))
    (when (listp keys) (setq keys (cdr keys)))
    (when (not (stringp keys)) (setq keys (airwave-chord-for-command command)))
    (setq keys (or (string-replace-match "^ +" keys "") keys))
    (setq keys (or (string-replace-match "^(" keys "") keys)) 
    (setq keys (or (string-replace-match ")$" keys "") keys))
    (setq keys (or (string-replace-match ">$" keys "") keys))
    (setq keys (or (string-replace-match "^<" keys "") keys))
    (when (listp keys) (setq keys (list-join keys)))
    (insert (format "  %-29s %-15s %s\n" title keys command))
    ))
(defun airwave-chord-for-command (command)
  (airwave-chord-for-key (car (where-is-internal command))))
(defun airwave-chord-for-key (key)
  (key-description key))

(defvar aw-font-picked 6)
(defvar aw-font-list)
(defun airwave-change-font (&optional font-index)
  (interactive)
  (if (not font-index) (setq font-index (1+ aw-font-picked)))
  (let (width height)
    (if (>= font-index (length aw-font-list)) (setq font-index 0))
    (setq aw-font-picked font-index)
    (setq width (airwave-frame-current-width))
    (setq height (airwave-frame-current-height))
    (set-default-font (airwave-font-number font-index))
    (airwave-set-frame-size width height)
    ))
(defun airwave-font-number (font-index)
  (let ((count 0) (font aw-font-list))
    (while (< count font-index)
      (setq font (cdr font))
      (setq count (1+ count)))
    (car font)))
(defun airwave-default-font-teenier ()
  (interactive) (airwave-change-font 0))
(defun airwave-default-font-teeny ()
  (interactive) (airwave-change-font 1))
(defun airwave-default-font-tinier ()
  (interactive) (airwave-change-font 2))
(defun airwave-default-font-tiny ()
  (interactive) (airwave-change-font 3))
(defun airwave-default-font-small ()
  (interactive) (airwave-change-font 4))
(defun airwave-default-font-medium ()
  (interactive) (airwave-change-font 5))
(defun airwave-default-font-big ()
  (interactive) (airwave-change-font 6))
(defun airwave-default-font-huge ()
  (interactive) (airwave-change-font 7))
(defun airwave-default-font-huge-wide ()
  (interactive) (airwave-change-font 8))

(defun airwave-bigger-font ()
  (interactive)
  (let ((fontsize (1+ aw-font-picked)))
    (when (>= fontsize (length aw-font-list))
      (message "This is the biggest font I've got")
      (setq fontsize (1- (length aw-font-list))))
    (airwave-change-font fontsize)))
(defun airwave-smaller-font ()
  (interactive)
  (let ((fontsize (1- aw-font-picked)))
    (when (<= fontsize 0)
      (message "This is the smallest font I've got")
      (setq fontsize 0))
    (airwave-change-font fontsize)))
(defun airwave-pretty-print ()
  (interactive)
  (save-excursion
    (let (beg localtime timestamp)
      (skip-chars-backward "0-9")
      (setq beg (point))
      (skip-chars-forward "0-9")
      (setq timestamp (buffer-substring beg (point)))
      (setq localtime (shell-command-to-string 
                  (format "perl -e 'print scalar localtime(%s)'" 
                          timestamp)))
      (message "Timestamp (%s) => %s" timestamp localtime)
      )))
(require 'bw)
(require 'scroll-bar)
(require 'follow)
(defvar aw-birds-eye-font aw-font-picked)
(defun airwave-birds-eye (&optional max-windows)
  (interactive "p")
  (if (eq max-windows 1) (setq max-windows 15))
  (if (not follow-mode) (setq aw-birds-eye-font aw-font-picked))
  (delete-other-windows)
  (goto-char (point-min))
  (let ((lines (count-lines (point-min) (point-max)))
        (height (- (frame-parameter 'nil 'height) 2))
        (windows 1)
        (window-pixel-width (airwave-window-pixel-width))
        (min-chars 40)
        (min-comfortable-chars 50)
        pages columns
        )
    ; pick a relatively small font
    (setq pages (/ lines (float height)))
    (setq columns (/ (/ window-pixel-width
                        (airwave-font-pixel-width
                         (airwave-font-number aw-font-picked))) 
                     min-comfortable-chars))
    (while (and (> pages columns)
                (> aw-font-picked 0))
      (setq aw-font-picked (1- aw-font-picked))
      (setq height (- (/ (frame-pixel-height) 
                         (airwave-font-pixel-height 
                          (airwave-font-number aw-font-picked))) 2))                             
      (setq pages (/ lines (float height)))
      (setq columns (/ (/ window-pixel-width
                          (airwave-font-pixel-width
                           (airwave-font-number aw-font-picked))) 
                       min-comfortable-chars))
      )
    (airwave-change-font aw-font-picked)
    ; turn on follow mode settings
    (scroll-bar-mode -1)
    (follow-mode t)
    (setq truncate-lines t)
    ; split window multiple times
    (while (and (> pages 1) 
                (< windows max-windows)
                (> (window-width) min-chars))
      (split-window-horizontally)
      (bw-balance)
      (setq windows (1+ windows))
      (setq pages (1- pages)))
    ))
(defun airwave-turtles-eye ()
  (interactive)
  (follow-mode 'nil)
  (delete-other-windows)
  (set-scroll-bar-mode 'right)
  (setq truncate-lines 'nil)
  (airwave-change-font aw-birds-eye-font))
(defun airwave-birds-eye-pgup ()
  (interactive)
  (scroll-down (airwave-birds-eye-scroll-size)))
(defun airwave-birds-eye-pgdn ()
  (interactive)
  (scroll-up (airwave-birds-eye-scroll-size)))
(defun airwave-birds-eye-scroll-size ()
  (- (* (frame-parameter 'nil 'height) (count-windows))
     (+ (* 2 (count-windows) 2))))
(defun my-follow-mode-hook ()
  (global-set-key (kbd "<prior>") 'airwave-birds-eye-pgup)
  (global-set-key (kbd "<next>") 'airwave-birds-eye-pgdn)
  (global-set-key [(control super ,)] 'airwave-birds-eye-scroll-left)
  (global-set-key [(control super .)] 'airwave-birds-eye-scroll-right)
  )
(defun my-follow-mode-off-hook ()
  (global-set-key (kbd "<prior>") 'scroll-down)
  (global-set-key (kbd "<next>") 'scroll-up)
  (global-unset-key [(control super ,)])
  (global-unset-key [(control super .)])
  )
(add-hook 'follow-mode-hook 'my-follow-mode-hook)
(add-hook 'follow-mode-off-hook 'my-follow-mode-off-hook)
(defun airwave-birds-eye-add-column ()
  (interactive)
  (when (not follow-mode) 
    (setq aw-birds-eye-font aw-font-picked)
    (scroll-bar-mode -1)
    (follow-mode t)
    (setq truncate-lines t)
    )
  (split-window-horizontally)
  (bw-balance))
(defun airwave-birds-eye-delete-column ()
  (interactive)
  (if (eq (count-windows) 2)
      (airwave-turtles-eye)
    (progn (delete-window) (bw-balance))))
(defun airwave-birds-eye-scroll-right ()
  (interactive)
  (walk-windows
   (function
    (lambda (win)
      (set-window-hscroll win (+ (window-hscroll win) 10))))))
(defun airwave-birds-eye-scroll-left ()
  (interactive)
  (walk-windows
   (function
    (lambda (win)
      (set-window-hscroll win (+ (window-hscroll win) -10))))))
(defun airwave-font-pixel-height (font)
  (aref (airwave-font-info font) 3))
(defun airwave-font-pixel-width (font)
  (aref (airwave-font-info font) 2))
(defun airwave-font-info (font)
  (let ((font-info (font-info font)))
    (when (not font-info)
      (make-face 'fooface)
      (set-face-font 'fooface font)
      (setq font-info (font-info font)))
    font-info))
(defun airwave-window-pixel-width ()
  (* (window-width)
     (airwave-font-pixel-width (frame-parameter nil 'font)))) 
    
(defun airwave-tests-for-method () 
  "Run tests_for_method on current method."
  (interactive)
  (let ((method (airwave-perl-currentmethod)))
    (airwave-shell-function 
     (format "tests_for_method %s::%s" (airwave-perl-currentclass) method)
     (format "*tfm:%s*" method))
    (goto-char (point-min))))
(defun airwave-perl-currentmethod ()
  (save-excursion
    (beginning-of-line)
    (if (not (looking-at "^ *sub [A-Za-z_]")) (search-backward-regexp "^ *sub [A-Za-z_]"))
    (search-forward-regexp " *sub *")
    (current-function)
    ))
(defun airwave-method-jump (method)
  "Lookup Mercury::CurrentClass->method, then jump to it"
  (interactive (list (read-string "method to lookup: ")))
  (airwave-find-module-with-sub 
   (format "%s->%s" 
           (airwave-perl-currentclass) 
           method)))

(defun airwave-keyboard-quit ()
  (interactive)
  (when (not mark-active) (widen))
  (keyboard-quit))
 
(defun airwave-kill-this-buffer ()
  (interactive)
  (if (window-minibuffer-p) (keyboard-escape-quit)
    (if (string= "*Buffer List*" (buffer-name)) (keyboard-quit)
      (progn
        (kill-buffer (current-buffer))
        (if (> (count-windows) 1) (delete-window))
        ))))
(defun airwave-kill-other-buffer ()
  (interactive)
  (other-window 1)
  (airwave-kill-this-buffer))

(defun airwave-beginning-or-toindent ()
  (interactive)
  (if (and (eq (current-column) 0)
           (string= (previous-key-string) "C-a"))
      (back-to-indentation)
    (beginning-of-line)))

(defun airwave-ending-or-nextline-end ()
  (interactive)
  (if (string= (previous-key-string) "C-e") (forward-line 1))
  (end-of-line-ignore-whitespace))

(defun airwave-goto-end (&optional ARG)
  (interactive)
  (let ((prevkey (previous-key-string)))
    (if (or (string= prevkey "<end>")
            (string= prevkey "<kp-end>")
            (and (string= (previous-key-string 7) "ESC")
                 (string= (previous-key-string 6) "[")
                 (string= (previous-key-string 5) "4")
                 (string= (previous-key-string 4) "~")))
        (end-of-buffer ARG)
      (end-of-line ARG))))
  
(defun airwave-goto-beg (&optional ARG)
  (interactive)
  (let ((prevkey (previous-key-string)))
    (if (or (string= prevkey "<home>") 
            (string= prevkey "<kp-home>")
            (and (string= (previous-key-string 7) "ESC")
                 (string= (previous-key-string 6) "[")
                 (string= (previous-key-string 5) "1")
                 (string= (previous-key-string 4) "~")))
        (beginning-of-buffer ARG)
      (beginning-of-line ARG))))

;;;;;;;;;;;;;;;;;; keyboard macros
(require 'macros)
(require 'edmacro)

(defun airwave-toggle-kbd-macro-recording ()
  (interactive)
  (if defining-kbd-macro (end-kbd-macro) (start-kbd-macro 'nil)))

(defun airwave-edit-kbd-macro (macro)
  (interactive "CName of macro to edit (last-kbd-macro): ")
  (if (or (not macro) (string= macro "") (string= macro "last"))
      (setq macro "last-kbd-macro"))
  (if (string= macro "last-kbd-macro")
      (edit-kbd-macro 'call-last-kbd-macro)
    (if (or (string= macro "view-lossage") (string= macro "lossage"))
        (edit-kbd-macro 'view-lossage)
      (edit-kbd-macro macro))))
(defun lossage () (interactive) (view-lossage))

(defun airwave-assign-to-last-kbd-macro (symbol)
  "Assign a named macro to the last keyboard macro defined."
  (interactive "CName of stored macro to save as last-macro: ")
  (or symbol (error "No keyboard macro defined"))
  (and (fboundp symbol)
       (not (stringp (symbol-function symbol)))
       (not (vectorp (symbol-function symbol)))
       (error "Function %s is already defined and not a keyboard macro"
	      symbol))
  (if (string-equal symbol "") (error "No command name given"))
  (setq last-kbd-macro (symbol-function symbol)))

;; macro for searching for 'ok(' in test code... use like: 'C-u COUNT M-x ok'   
(fset 'ok [?\C-s ?o ?k ?\( ?\C-m right right])
(fset 'id_param [right right right ?\C-r ?$ right ?s ?e ?l ?f ?- ?> 
  ?i ?d ?_ ?p ?a ?r ?a ?m ?\( ?$ right ?, ?\C-z ?\( delete ?  ?\C-r ?$ ?\C-r ?\C-m])

(defun airwave-rally-diff (&optional fid)
  (interactive)
  (setq fid (or fid (read-string "Rally FID: ")))
  (airwave-shell-function (format "revisions_for_thing -w %s > /tmp/combined.diff" fid) "discard output")
  (airwave-update-buffers)
  (find-file "/tmp/combined.diff")
  (setq buffer-read-only 'nil)
  (airwave-diffstat-on-buffer))

(defvar aw-delta-diff-history 'nil)
(defun airwave-delta-diff (&optional diff_file)
  (interactive)
  (airwave-save-and-save-some-buffers)
  (setq diff_file (or diff_file (airwave-read-diff-name "Delta diff from: " )))
  (when (string-match "\\.diff$" diff_file)
    (setq diff_file (string-replace-match "\\.diff$" diff_file "")))
  (airwave-shell-function (format "delta_diff %s" diff_file) "discard output")
  (airwave-update-buffers)
  (find-file (home "/diffs/delta_diff.diff"))
  (setq buffer-read-only 'nil)
  (airwave-diffstat-on-buffer))
(defun airwave-make-diff-marker (&optional diff_file)
  (interactive)
  (airwave-save-and-save-some-buffers)
  (setq diff_file (or diff_file (airwave-read-diff-name "Name for diff marker: " )))
  (airwave-shell-function-basic "make_diff_marker" diff_file))
(defun airwave-read-diff-name (prompt)
  (let (diff_file)
    (setq diff_file (completing-read prompt (aw-diff-alist) nil nil 
                                     (car aw-delta-diff-history) 
                                     '(aw-delta-diff-history . 1)))
    (if (string= diff_file "") (error "No diff file given"))
    diff_file))

(defun aw-diff-alist ()
  (let (list)
    (loop for file in (directory-files (home "/diffs") 'nil "\\.diff$") do
          (setq file (string-replace-match "\\.diff" file ""))
          (setq list (cons (cons file 1) list)))
    list))

(defun airwave-accept-useline-suggestions ()
  (interactive)
  (beginning-of-line)
  (when (not (looking-at "Useline suggestions"))
    (search-backward "Useline suggestions"))
  (search-forward "for ")
  (let (useclass (file (buffer-substring (point) (1- (line-end-position)))))
    (message "%s" file)
    (forward-line 1)
    (while (looking-at "  -")
      (setq useclass (buffer-substring (+ 4 (point)) (1- (line-end-position))))
      (airwave-remove-useline-from-file file useclass)
      (forward-line 1))
    (while (looking-at "use")
      (setq useclass (buffer-substring (+ 4 (point)) (1- (line-end-position))))
      (airwave-add-useline-to-file file useclass)
      (forward-line 1))
    (save-window-excursion
      (airwave-find-file 'nil file)
      (save-buffer)))
  (forward-line 1))
(defun airwave-remove-useline-from-file (file useclass)
  (message "Removing %s from %s" useclass file)
  (save-window-excursion
    (airwave-find-file 'nil file)
    (goto-char (point-min))
    (search-forward (format "use %s;" useclass))
    (airwave-delete-whole-line)
    ))
(defun airwave-add-useline-to-file (file useclass)
  (message "Adding %s from %s" useclass file)
  (save-window-excursion
    (airwave-find-file 'nil file)
    (airwave-add-export-use-line useclass)
    ))
(defun airwave-get-scratch-buffer ()
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

;;;;;;;;;;;;;; from simple.el of emacs 22
(defun process-file (program &optional infile buffer display &rest args)
  "Process files synchronously in a separate process.
Similar to `call-process', but may invoke a file handler based on
`default-directory'.  The current working directory of the
subprocess is `default-directory'.

File names in INFILE and BUFFER are handled normally, but file
names in ARGS should be relative to `default-directory', as they
are passed to the process verbatim.  \(This is a difference to
`call-process' which does not support file handlers for INFILE
and BUFFER.\)

Some file handlers might not support all variants, for example
they might behave as if DISPLAY was nil, regardless of the actual
value passed."
  (let ((fh (find-file-name-handler default-directory 'process-file))
        lc stderr-file)
    (unwind-protect
        (if fh (apply fh 'process-file program infile buffer display args)
          (when infile (setq lc (file-local-copy infile)))
          (setq stderr-file (when (and (consp buffer) (stringp (cadr buffer)))
                              (make-temp-file "emacs")))
          (prog1
              (apply 'call-process program
                     (or lc infile)
                     (if stderr-file (list (car buffer) stderr-file) buffer)
                     display args)
            (when stderr-file (copy-file stderr-file (cadr buffer)))))
      (when stderr-file (delete-file stderr-file))
      (when lc (delete-file lc)))))

;;;;;;; from byte-run.el in 22
(put 'with-no-warnings 'lisp-indent-function 0)
(defun with-no-warnings (&rest body)
  "Like `progn', but prevents compiler warnings in the body."
  ;; The implementation for the interpreter is basically trivial.
  (car (last body)))

;;; from vc.el in 22... modified to not trim filename
(defun vc-do-command-fullfile (buffer okstatus command file &rest flags)
  "Execute a VC command, notifying user and checking for errors.
Output from COMMAND goes to BUFFER, or *vc* if BUFFER is nil or the
current buffer if BUFFER is t.  If the destination buffer is not
already current, set it up properly and erase it.  The command is
considered successful if its exit status does not exceed OKSTATUS (if
OKSTATUS is nil, that means to ignore errors, if it is 'async, that
means not to wait for termination of the subprocess).  FILE is the
name of the working file (may also be nil, to execute commands that
don't expect a file name).  If an optional list of FLAGS is present,
that is inserted into the command line before the filename."
  (and file (setq file (expand-file-name file)))
  (if vc-command-messages
      (message "Running %s on %s..." command file))
  (save-current-buffer
    (unless (or (eq buffer t)
                (and (stringp buffer)
                     (string= (buffer-name) buffer))
                (eq buffer (current-buffer)))
      (vc-setup-buffer buffer))
    (let ((squeezed (remq nil flags))
	  (inhibit-read-only t)
	  (status 0)
          (orig-default-directory default-directory)
          (file-rel-to-vcroot file))
      (when vc-root
        (setq file-rel-to-vcroot (or (string-replace-match 
                                      (format "^%s/" vc-root)
                                      file "" 't) 
                                     file)))
      (when file
	;; FIXME: file-relative-name can return a bogus result because
	;; it doesn't look at the actual file-system to see if symlinks
	;; come into play.
	(setq squeezed (append squeezed (list file-rel-to-vcroot))))
      (let ((exec-path (append vc-path exec-path))
	    ;; Add vc-path to PATH for the execution of this command.
	    (process-environment
	     (cons (concat "PATH=" (getenv "PATH")
			   path-separator
			   (mapconcat 'identity vc-path path-separator))
		   process-environment))
	    (w32-quote-process-args t))
        (if vc-root (setq default-directory (format "%s/" vc-root)))
	(if (and (eq okstatus 'async) (file-remote-p default-directory))
	    ;; start-process does not support remote execution
	    (setq okstatus nil))
	(if (eq okstatus 'async)
	    (let ((proc
		   (let ((process-connection-type nil))
		     (apply 'start-process command (current-buffer) command
			    squeezed))))
              (unless (active-minibuffer-window)
                (message "Running %s in the background..." command))
	      ;;(set-process-sentinel proc (lambda (p msg) (delete-process p)))
	      (set-process-filter proc 'vc-process-filter)
	      (vc-exec-after
	       `(unless (active-minibuffer-window)
                  (message "Running %s in the background... done" ',command)))
              )
          (setq status (apply 'process-file command nil t nil squeezed))
	  (when (or (not (integerp status)) (and okstatus (< okstatus status)))
	    (pop-to-buffer (current-buffer))
	    (goto-char (point-min))
	    (shrink-window-if-larger-than-buffer)
	    (error "Running %s...FAILED (%s)" command
		   (if (integerp status) (format "status %d" status) status))))
	(if vc-command-messages
	    (message "Running %s...OK" command)))
      (setq default-directory orig-default-directory)
      (vc-exec-after
       `(run-hook-with-args 'vc-post-command-functions ',command ',file ',flags))
      status)))
;;; from files.el of 22
(defun file-remote-p (file)
  "Test whether FILE specifies a location on a remote system.
Return an identification of the system if the location is indeed
remote.  The identification of the system may comprise a method
to access the system and its hostname, amongst other things.

For example, the filename \"/user@host:/foo\" specifies a location
on the system \"/user@host:\"."
  (let ((handler (find-file-name-handler file 'file-remote-p)))
    (if handler
	(funcall handler 'file-remote-p file)
      nil)))
;;;;; airwave class picker
(defun airwave-class-picker ()
  "Switch to a perlclass matching a substring."
  (interactive)
  (let (prompt buf class)
    (setq prompt (format "pmswitch "))
    (setq class (airwave-pmswitchb-read-buffer prompt))
    (airwave-find-module class)))
(defun airwave-pmswitchb-read-buffer (prompt &optional default require-match)
  "Copy 'n Paste of iswitchb-read-buffer with buffer picking replaced with class picking"
  (let
      (initial-substring
       buf-sel
       iswitchb-final-text
       (icomplete-mode nil) ;; prevent icomplete starting up
       ;; can only use fonts if they have been bound.
       (iswitchb-use-fonts (and iswitchb-use-fonts
				(boundp 'font-lock-comment-face)
				(boundp 'font-lock-function-name-face))))

    (iswitchb-define-mode-map)
    (setq iswitchb-exit nil)
    (setq iswitchb-rescan t)
    (setq iswitchb-text "")
    (setq iswitchb-default
	  (if (bufferp default)
	      (buffer-name default)
	    default))
    (setq initial-substring (read-string "Substrings in class: " (current-selection)))
    (airwave-pmswitchb-make-buflist initial-substring) ; this one line changed from iswitchb
    (iswitchb-set-matches)
    (let
	((minibuffer-local-completion-map iswitchb-mode-map)
	 (iswitchb-prepost-hooks t)
	 ;; Record the minibuffer depth that we expect to find once
	 ;; the minibuffer is set up and iswitchb-entryfn-p is called.
	 (iswitchb-minibuf-depth (1+ (minibuffer-depth)))
	 (iswitchb-require-match require-match))
      ;; prompt the user for the buffer name
      (setq iswitchb-final-text (completing-read
				 prompt	;the prompt
				 '(("dummy" . 1)) ;table
				 nil	;predicate
				 nil	;require-match [handled elsewhere]
				 nil	;initial-contents
				 'iswitchb-history)))
    ;; Handling the require-match must be done in a better way.
    (if (and require-match (not (iswitchb-existing-buffer-p)))
	(error "Must specify valid buffer"))

    (if (or
	 (eq iswitchb-exit 'takeprompt)
	 (null iswitchb-matches))
	(setq buf-sel iswitchb-final-text)
      ;; else take head of list
      (setq buf-sel (car iswitchb-matches)))
    
    ;; Or possibly choose the default buffer
    (if  (equal iswitchb-final-text "")
	(setq buf-sel
	      (car iswitchb-matches)))

    buf-sel))
(defun airwave-pmswitchb-make-buflist (substring)
  (let (str)
    (setq str (shell-command-to-string (format "perl_classes_matching -s %s" substring)))
    (if (string= str "") (error "Couldn't find any classes matching %s" substring))
    (setq iswitchb-buflist (string-split "\n" (string-replace-match "\n\$" str "")))))
(defun rebuild-perl-class-cache ()
  (interactive)
  (airwave-shell-function-basic "perl_classes_matching" "-r -q -d"))

(defun airwave-c-toggle-code-test-buffer ()
  "Switch buffer from foo.t.c <--> foo.c"
  (interactive)
  (let ((new-filename (airwave-c-code-test-map (buffer-file-name))))
    (if (file-exists-p new-filename) 
        (find-file new-filename)
      (error "File doesn't exist %s" new-filename)))
  )

(defun airwave-c-code-test-map (filename)
  (or (string-replace-match "\\.t\\.c" filename ".c")
      (string-replace-match "\\.c" filename ".t.c")
      filename
      )
  )

(defun airwave-rerun-test (&optional n)
  (interactive "P")
  (if (buffer-file-name) (save-buffer))
  (airwave-jump-to-last-test n)
  (airwave-run-perl-file))

(defun airwave-rerun-debugger ()
  (interactive)
  (if (buffer-file-name) (save-buffer))
  (airwave-jump-to-last-test)
  (airwave-perldb))

(defun airwave-jump-to-last-test (&optional n)
  (interactive "P")
  (if (null n)
      (when (boundp 'airwave-last-test-executed)
        (airwave-find-file 't airwave-last-test-executed))
    (when (boundp 'airwave-next-to-last-test-executed)
      (airwave-find-file 't airwave-next-to-last-test-executed))))

(defun isearch-occur ()
  "Invoke `occur' from within isearch."
  (interactive)
  (let ((case-fold-search isearch-case-fold-search))
    (occur (if isearch-regexp isearch-string (regexp-quote isearch-string)))))

(defun airwave-first-line-of-method (variable)
  (interactive)
  (insert "my (" variable ", ) = @_;")
  (backward-char (length ") = @_;"))
  (cperl-indent-line)
  )

(defun airwave-my-self ()
  (interactive)
  (airwave-first-line-of-method "$self")
  )

(defun airwave-my-class ()
  (interactive)
  (airwave-first-line-of-method "$class")
  )

(defun current-selection ()
  (if mark-active (buffer-substring (point) (mark)) 'nil))

(require 'avoid)
(defun airwave-banish-mouse ()
  (interactive)
  (mouse-avoidance-banish-mouse))

(defun airwave-snmpwalk-to-hash ()
  (interactive)
  (skip-whitespace-forward)
  (skip-chars-backward ".0-9")          ; find beginning of next oid
  (while (and (looking-at ".[0-9]") (airwave-oid-to-hash-pair))
    (progn (cperl-indent-line)
           (skip-chars-forward " \t\n"))))

(defun delete-if-looking-at (regex)
  (if (looking-at regex)
      (progn (delete-char (- (match-end 0) (match-beginning 0))) t)
      nil))

(defun airwave-oid-to-hash-pair ()
  (interactive)
  (insert "'")
  (skip-chars-forward "^=")
  (save-excursion
    (skip-chars-backward " \t")
    (insert "'"))
  (forward-char)
  (insert ">")
  (skip-whitespace-forward)
  (if (looking-at "Wrong Type")
      (progn (zap-to-char 1 ?:)
             (delete-char 1)))
  (cond ((or (delete-if-looking-at "STRING: ")
             (delete-if-looking-at "IpAddress: ")
             (delete-if-looking-at "Network Address: ")
             (delete-if-looking-at "OID: "))
         (insert "'")
         (end-of-line)
         (insert "',")
         t)
        ((or (delete-if-looking-at "INTEGER: ")
             (delete-if-looking-at "Counter\\([0-9]*\\): ")
             (delete-if-looking-at "Gauge\\([0-9]*\\): ")
             (delete-if-looking-at "Timeticks: "))
         (if (not (looking-at "[-0-9]"))
             (progn (zap-to-char 1 40)  ; left paren; using ?( screws up indentation
                    (skip-chars-forward "-0-9")
                    (kill-line)))
         (end-of-line)
         (insert ",")
         t)
        ((delete-if-looking-at "Hex-STRING: ")
         (insert "pack('H*', '")
         (airwave-remove-spaces-from-hex-string)
         (insert "'),")
         t)
        ((delete-if-looking-at "BITS: ")
         (insert "pack('B*', '")
         (airwave-remove-spaces-from-hex-string)
         (kill-line)
         (insert "'),")
         t)
        (nil)))

(defun airwave-remove-spaces-from-hex-string ()
  (skip-chars-forward "0-9A-F")
  (while (looking-at " ")
    (delete-char 1)
    (skip-chars-forward "0-9A-F")))

(defun airwave-snmp-walk ()
  "Walk a given OID"
  (interactive)
  (let ((oid-to-walk (read-string "Oid to walk: " (current-oid))))
    (if (not (boundp 'host-to-walk))
        (setq host-to-walk (read-string "Host to walk: ")))
    (if (not (boundp 'community-string))
        (setq community-string (read-string "Community string: ")))
    (shell-command
     (concat "s2w " host-to-walk " " community-string " " oid-to-walk))))

(defun airwave-forget-snmp-info ()
  "Forget cached host and community string"
  (interactive)
  (makunbound 'host-to-walk)
  (makunbound 'community-string))

(defun airwave-open-ap ()
  "Open AP.pm"
  (interactive)
  (airwave-open-mercfile "/lib/perl/Mercury/AP.pm"))

(defun airwave-open-console-elisp ()
  "Open console.el"
  (interactive)
  (airwave-open-mercfile "/lib/conf/elisp/console.el"))

(defun airwave-open-amp-js ()
  "Open amp.js"
  (interactive)
  (airwave-open-mercfile "/lib/html/amp.js"))

(defun airwave-open-utility-datamanip ()
  "Open DataManip.pm"
  (interactive)
  (airwave-open-mercfile "/lib/perl/Mercury/Utility/DataManip.pm"))
(defun airwave-open-mercfile (partialfile)
  (find-file (mercmerc partialfile)))

(defun airwave-open-managed-config ()
  "Open AP.pm"
  (interactive)
  (airwave-open-mercfile "/lib/perl/Mercury/Role/ManagedConfiguration.pm"))

(defun airwave-long-skinny ()
  "Go to the long skinny method"
  (interactive)
  (airwave-open-managed-config)
  (goto-char (point-min))
  (search-forward "sub compute_desired_dbconfig")
  (search-forward "my %args")
  (recenter))

(defun airwave-copy-word ()
  (interactive)
  (kill-new (current-word)))

(defun airwave-delete-new-trailing-whitespace ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (= 0 (forward-line))
      (if (and (string= (current-char) "+")
               (not (string= (next-char) "+")))
          (airwave-delete-trailing-whitespace)))))

(defun airwave-whitespace-as-underline ()
  (interactive)
  (set-face-attribute 'trailing-whitespace 'nil :background "black" :underline "grey20"))
(defun airwave-whitespace-as-block ()
  (interactive)
  (set-face-attribute 'trailing-whitespace 'nil :background "grey30"))
(defun airwave-toblakem ()
  (interactive)
  (if (> (x-display-pixel-height) 900)
      (airwave-default-font-huge-wide)
    (airwave-default-font-big))
  (airwave-set-frame-size 0.87 0.95)
  (airwave-whitespace-as-underline)
  )

(defun airwave-tobelden ()
  (interactive)
  (airwave-default-font-tiny)
  (airwave-set-frame-size 0.65 0.93)
  )

(defun airwave-topsanford ()
  (interactive)
  (global-set-key [(super a) ?c ?a] 'airwave-svn-annotate)
  (global-set-key [(super a) ?a ?c] 'airwave-cleanap))

(defun airwave-recent-checkin-comments (rcc-cmd)
  (interactive
    (list (read-string "Run as: " "recent_checkin_comments")))
  (airwave-shell-function rcc-cmd "*recent_checkin_comments*"))
(defun airwave-your-diff (machine)
  (interactive
   (list (read-string (format "Show %s diff on: " (svnproj)))))
  (let (file)
    (setq file (format "%s/remote_diff_on_%s_in_%s.diff" 
                       (if (file-directory-p (home "/diffs")) (home "/diffs") "/tmp")
                       machine
                       (svnproj)))
    (update_ssh_key machine)
    
    (airwave-generate-empty-output-buffer "*remote_diff*")
    (start-process-shell-command "your_diff" (format "*remote_diff*" machine) 
     (format "ssh %s \"root=%s summarize_svn_diffs -u 3\" > %s; cat %s" 
             machine (root) file file))
    (set-process-sentinel (get-process "your_diff") 'finish-your-diff)
    (message "Building diff from %s in *remote_diff*" machine)
    ))
(defun finish-your-diff (process event)
  (airwave-update-buffers)
  (switch-to-buffer "*remote_diff*")
  (diff-mode)
  (goto-char (point-min))
  )
(defun fetch_and_save_all_ssh_known_hosts ()
  (interactive)
  (airwave-shell-function "fetch_and_save_all_ssh_known_hosts" "*fasaskh*"))
(defun update_ssh_key (machine)
  (interactive
   (list (read-string "Update ssh key for machine: ")))
  (airwave-shell-function (format "update_ssh_key %s" machine) "discard output" 't))
(defun airwave-ampboard (&optional n)
  (interactive "P")
  (if (buffer-exists "*amp-board*")
      (if (string= (buffer-name) "*amp-board*")
          (progn 
            (kill-buffer (get-buffer-create "*old-board*"))
            (rename-buffer "*old-board*") 
            (airwave-build-ampboard n))
        (progn (switch-to-buffer "*amp-board*") (goto-char (point-min))))
    (airwave-build-ampboard n)))
(defun airwave-build-ampboard (n)
  (let (cmd)
    (kill-buffer (get-buffer-create "*amp-board*"))
    (message "Building AMP Board in the background")
    (if n (setq cmd "amp_board 2> /dev/null")
      (setq cmd "fetch_and_save_all_ssh_known_hosts > /tmp/fasaskh_em; amp_board 2> /dev/null"))
    (start-process-shell-command "amp_board" "*amp-board*" cmd)
    (set-process-sentinel (get-process "amp_board") 'finish-amp-board)
    ))

(defun finish-amp-board (process event)
  ; (princ (format "Process: %s had the event `%s'" process event))
  (message "AMP Board has been built in the \"*amp-board*\" buffer")
  )
(defun buffer-exists (buffer) (buffer-live-p (get-buffer buffer)))

(defun airwave-resolve-svn-conflict ()
  (interactive)
  (let ((file (buffer-file-name))
        (conflictfile) (conflictfile2))
    (if (and file (vc-svn-responsible-p file))
        (if (airwave-svn-conflict-point)
            (progn
              (goto-char (airwave-svn-conflict-point))
              (message "Fix this first..."))
          (progn
            (setq conflictfile (concat file ".mine"))
            (setq conflictfile2 (concat file ".working"))
            (if (or (file-exists-p conflictfile) (file-exists-p conflictfile2))
                (progn
                  (airwave-save-and-save-some-buffers)
                  (airwave-shell-function-no-output 
                   (format "svn resolved %s" (buffer-file-name)))
                  (if (or (file-exists-p conflictfile) (file-exists-p conflictfile2))
                      (message "Failed to resolve conflict")
                    (message "Resolved conflict.")))
              (message "No existing conflict to resolve."))))
      (message "That buffer isn't checked in"))))
(defun airwave-svn-conflict-point ()
  (save-excursion
    (goto-char (point-min))
    (if (search-forward-regexp "^>>>>>>> \\|^=======$\\|^<<<<<<<" 'nil 't)
        (line-beginning-position)
      'nil)))

(defun airwave-comment-dwim ()
  (interactive)
  (if (not mark-active)
      (progn
        (if (eq last-command 'airwave-comment-dwim)
            (kill-append (current-line-full) 'nil)
          (kill-new (current-line-full)))
        (comment-region (line-beginning-position) (line-end-position))
        (forward-line 1)
        )
    (comment-dwim nil)))

(defun airwave-yesterdays-checkins (&optional n)
  (interactive "P")
  (when (not n)
    (let ((day (format-time-string "%a" (current-time))))
      (if (string= day "Sat") (setq n 2)
        (if (string= day "Sun") (setq n 3)
          (setq n 1)))))
  (airwave-shell-function 
   (format "ssh %s@svn.corp.airwave.com standup_cheat_sheet.pl -r -p -s %s" (towho) n)
   "*yesterdays_checkins*")
  (goto-char (point-min))
  )

(defun cicomm ()
  (interactive)
  (delete-other-windows)
  (airwave-sorted-cdiff)
  (when (> (count-windows) 1)
    (other-window 1)
    (delete-window))
  (split-window-vertically)
  (other-window 1)
  (find-file (home "/cicomm"))
  (when (> (window-height (selected-window)) 10)
    (shrink-window (- (window-height (selected-window)) 10)))
  (other-window 1)
  (goto-char (point-min))
  (other-window 1)
  (setq saved-window-config-subwindow 't)
  )

(defun airwave-swap-windows ()
  (interactive)
  (let ((i 1) (windowcount (count-windows)) (firstwindow (buffer-name)) otherwindow)
    (while (< i windowcount)
      (setq i (1+ i))
      (other-window -1)
      (setq otherwindow (buffer-name))
      (other-window 1)
      (switch-to-buffer otherwindow)
      (other-window -1))
    (switch-to-buffer firstwindow)))
  
(defun hide-indented-lines ()
  (interactive)
  (show-all-invisible)
  (hide-matching-lines (concat "\\(\\^ *\\$\\)\\|\\(^" 
                               (make-string (1+ (current-column)) ?\ ) 
                               "\\)"))
  )

(defun airwave-fix-tabstops (width)
  (interactive "P")
  (when (null width) (setq width 2))
  (set-variable 'tab-width width))

(defun airwave-super-call ()
  "Insert a call to ->SUPER::method(method args)"
  (interactive)
  (airwave-up-call "SUPER"))

(defun airwave-next-call ()
  "Insert a call to ->NEXT::method(method args)"
  (interactive)
  (airwave-up-call "NEXT"))

(defun airwave-up-call (up-name)
  (let (method-name invocant remaining-args)
    (save-excursion
      (re-search-backward "sub\\s-+\\(\\sw+\\)\\s-+{")
      (airwave-forward-word)
      (setq method-name (current-word))
      (search-forward "{")
      (search-forward "(")
      (let ((start-of-invocant (point)))
        (search-forward-regexp "[,)]")
        (setq invocant (buffer-substring-no-properties start-of-invocant (1- (point)))))
      (backward-char 1)
      (if (looking-at ",")
          (let ((start-of-remaining-args (+ 2 (point))))
            (search-forward ")")
            (setq remaining-args (buffer-substring-no-properties start-of-remaining-args (1- (point)))))))
      (insert invocant "->" up-name "::" method-name (if remaining-args
                                                   (concat "(" remaining-args ")")
                                                 ""))))

(defun airwave-ensure-trailing-comma ()
  "Add a trailing comma to the line if one is absent"
  (interactive)
  (save-excursion
    (end-of-line)
    (unless (bobp)
      (backward-char 1)
      (cond
       ((looking-at ",")
        t)
       ((looking-at ";")
        (delete-char 1)
        (insert ","))
       (t
        (forward-char 1)
        (insert ",")))))
  (next-line 1))

(defun airwave-warn-data-dumper ()
  "Put in a pretty printed warn statement"
  (interactive)
  (let (variable string)
    (setq variable (current-variable))
    (setq string (current-word))
    (end-of-line)
    (newline)
    (insert (format "warn Data::Dumper->Dump([%s], [qw(%s)]);" variable string))
    (airwave-backward-word)
    (airwave-backward-word)
    (backward-char 4)
    ))
