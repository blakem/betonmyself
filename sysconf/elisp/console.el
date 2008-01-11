(provide 'console)
; f keys
(define-key esc-map "[24~" '[f12])
(define-key esc-map "[25~" 'pop-global-mark)
(define-key esc-map "[26~" 'airwave-super-follow)
(define-key esc-map "[28~" 'airwave-toggle-code-test-buffer)
(define-key esc-map "[29~" 'previous-error-recenter)
(define-key esc-map "[31~" 'delete-window)
(define-key esc-map "[33~" 'airwave-perldb-ask)
(define-key esc-map "[34~" 'uncomment-region)
(define-key esc-map "[15;2~" '[S-f5])
(define-key esc-map "[15;5~" '[C-f5])
(define-key esc-map "[17;2~" '[S-f6])
(define-key esc-map "[18;2~" '[S-f7])

;alt-f12 - putty is not allowing shift for f11 and f12
(define-key esc-map "\e[24~" 'font-lock-mode)

; special Function key cases
(define-key esc-map "[13~" '(lambda () (interactive) (set-mark-command t)))

; piotr's gnome-terminal uses these
(define-key esc-map "[1;5A" '[C-up]) ; control-movement
(define-key esc-map "[1;5B" '[C-down])
(define-key esc-map "[1;5C" '[C-right])
(define-key esc-map "[1;5D" '[C-left])
(define-key esc-map "[1;6A" '[s-up]) ; control-shift-up = super-up
(define-key esc-map "[1;6B" '[s-down])
(define-key esc-map "[1;6C" '[s-right])
(define-key esc-map "[1;6D" '[s-left])

; ctrl and alt arrow keys
(define-key esc-map "[A" '[C-up])
(define-key esc-map "[B" '[C-down])
(define-key esc-map "[C" '[C-right])
(define-key esc-map "[D" '[C-left])
(define-key esc-map "\eOA" '[M-up])
(define-key esc-map "\eOB" '[M-down])
(define-key esc-map "\eOC" '[M-right])
(define-key esc-map "\eOD" '[M-left])
(define-key esc-map "O3A" '[M-up])
(define-key esc-map "O3B" '[M-down])
(define-key esc-map "O3C" '[s-right])
(define-key esc-map "O3D" '[s-left])

; M-S-<arrow> does M-S-<arrow> (which seems to be useless)
(define-key esc-map "O4A" '[M-S-up])
(define-key esc-map "O4B" '[M-S-down])
(define-key esc-map "O4C" '[M-S-right])
(define-key esc-map "O4D" '[M-S-left])

; C-<arrow> does C-<arrow>
(define-key esc-map "O5A" '[C-up])
(define-key esc-map "O5B" '[C-down])
(define-key esc-map "O5C" '[C-right])
(define-key esc-map "O5D" '[C-left])

; C-S-<arrow> does s-<arrow>
(define-key esc-map "O6A" '[s-up])
(define-key esc-map "O6B" '[s-down])
(define-key esc-map "O6C" '[s-right])
(define-key esc-map "O6D" '[s-left])

; alt combos
(define-key esc-map "9" 'airwave-toggle-vertical-horizontal-list)
; this one's annoying for typos
;(define-key esc-map "0" 'airwave-end-and-format-list)
(define-key esc-map "5" 'airwave-region-replace)
; use ctrl-numpad arrow keys for super keys
(define-key esc-map "Ot" '[s-left])
(define-key esc-map "Ov" '[s-right])
(define-key esc-map "Or" '[s-down])
(define-key esc-map "Ox" '[s-up])
; other combos
(define-key esc-map "." 'dot-mode-execute) ; alt-. for repeat
; home and end keys
(define-key esc-map "[1~" 'airwave-goto-beg)
(define-key esc-map "[4~" 'airwave-goto-end)
(define-key esc-map "[6;5~" '[C-next])
(define-key esc-map "[5;5~" '[C-prior])
