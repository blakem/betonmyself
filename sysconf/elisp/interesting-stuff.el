; Good stuff at: http://www.cs.berkeley.edu/~smcpeak/elisp/scott.emacs.el

;;;;;;;;;;;;;;;;;;;;;
;; (require 'highlight-completion)
;; (highlight-completion-mode 1)
;; (define-key minibuffer-local-completion-map [tab] 'hc-completing-insert-file-name)
;; (require 'cycle-mini)

;; http://www.cua.dk/ido.el
;;  (require 'ido)
;;  (ido-mode t)
;;  (ido-mode 'nil)

;; http://homepage1.nifty.com/bmonkey/emacs/elisp/mcomplete.el
;;  (autoload 'mcomplete-mode "mcomplete" "minibuffer completion" t nil)
;;  (autoload 'turn-on-mcomplete-mode "mcomplete" "mcomplete on" t nil)
;;  (autoload 'turn-off-mcomplete-mode "mcomplete" "mcomplete off" t nil)
;;  (turn-on-mcomplete-mode)
;;  (load-library "mcomplete")
;;  (turn-off-mcomplete-mode)

;; cycle-mini
;; (load "cycle-mini")

; http://www-db.stanford.edu/~manku/dotemacs.html

;; http://www.emacswiki.org/cgi-bin/wiki/EmacsNiftyTricks
;; new macro declare-abbrevs -- similar to define-abbrev-table
(require 'cl)
(defvar my-abbrev-tables nil)
(defun my-abbrev-hook ()
  (let ((def (assoc (symbol-name last-abbrev) my-abbrev-tables)))
    (when def
      (execute-kbd-macro (cdr def)))
    t))
(put 'my-abbrev-hook 'no-self-insert t)
(defmacro declare-abbrevs (table abbrevs)
  (if (consp table)
      `(progn ,@(loop for tab in table
                      collect `(declare-abbrevs ,tab ,abbrevs)))
    `(progn
       ,@(loop for abbr in abbrevs
               do (when (third abbr)
                    (push (cons (first abbr) (read-kbd-macro (third abbr)))
                          my-abbrev-tables))
               collect `(define-abbrev ,table
                          ,(first abbr) ,(second abbr) ,(and (third abbr)
                                                             ''my-abbrev-hook))))))
(put 'declare-abbrevs 'lisp-indent-function 2)

;;; sample abbrev definitions
(eval-after-load "c-mode"
  '(declare-abbrevs (c-mode-abbrev-table c++-mode-abbrev-table)
       (("#s"    "#include <>" "C-b")
        ("#i"    "#include \"\"" "C-b")
        ("#ifn"  "#ifndef")
        ("#e"    "#endif /* */" "C-3 C-b")
        ("#ifd"  "#ifdef")
        ("imain" "int\nmain (int ac, char **av[])\n{\n\n}" "C-p TAB")
        ("if"    "if () {\n}\n" "C-M-b C-M-q C-- C-M-d")
        ("else"  "else {\n}\n"  "C-M-b C-M-q C-M-d RET")
        ("while" "while () {\n}\n" "C-M-b C-M-q C-- C-M-d")
        ("for"   "for (;;) {\n}\n" "C-M-b C-M-q C-M-b C-M-d")
        ("pr"    "printf (\"\")" "C-2 C-b"))))


; (frame-parameter nil 'font) ; show current font
; interesting fonts
(set-default-font "-baekmuk-gulimbdf-bold-r-normal--24-240-75-75-m-240-ksc5601.1987-0")
(set-default-font "-vga-fixed-medium-r-normal--24-230-75-75-c-120-iso8859-1")
(set-default-font "-adobe-courier-bold-r-normal--24-240-75-75-m-150-iso8859-2")
(set-default-font "-biznet-fotinos-medium-r-normal-sans-24-240-75-75-p-136-iso8859-2")
(set-default-font "-biznet-kathimerini-bold-r-normal--24-240-75-75-p-132-iso8859-2")

(progn
  (insert "\n\n\n")
  (loop for font in (x-list-fonts 
;                     "-*-Fixed-*-r-*--24-*-*"
                     "-*-24-*"
                     )
        do (insert (format "(set-default-font \"%s\")\n" font))))
