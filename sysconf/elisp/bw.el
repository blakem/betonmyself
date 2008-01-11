;;; bw.el --- Balance windows using split tree

;; Copyright (C) 2005 by Lennart Borgman

;; Author:     Lennart Borgman <lennart DOT borgman DOT 073 AT student DOT lu DOT se>
;; Created: 2005-09-15
;; Version: 0.52
;; Keywords: tools hypermedia html

;; This file is not part of Emacs

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.


;;; Commentary:
;; 
;; This is a rewrite of balance-window.  The main reason for the
;; rewrite was that balance-window could not handle all cases.  One of
;; the reasons for this was that the window split tree was not taken
;; into account (at least I believe so). 
;;
;; In this implementation the window split tree is used. However it is
;; not directly available (since Emacs does not give access to it at
;; elisp level).  Instead it is assumed that failures to enlarge
;; windows is caused by wrong assumptions for the split dimension.
;; Then the other split dimension is tried.
;;
;; Balancing is trying to use the way the user sees the windows, not
;; how the splits actually where made.  An example that is more
;; precise: If the user splits windows two times in the same
;; directions then the three resulting windows gets the same size
;; after balancing.  (The sizes may of course be one column or row
;; off.)
;;
;; Usage:
;;     (require 'bw)
;;     M-x bw-balance
;;
;;     To use bw-balance as a drop-in balance-window replacement:
;;     (define-key ctl-x-map "+" 'bw-balance)

;; Todo: weights, count windows
;; I planned to implement some weights to be able to give windows
;; further down the window split tree larger size, but I have not had
;; time to finish that.
;;


;;; History:
;; 

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Debugging helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(setq bw-debug t)  ;
;;(setq bw-debug nil)  ;
(defvar bw-debug nil
  "If non-nil debugging is turned on.
This writes some messages to *Messages* buffer and use `sit-for'
to show what is happening during the balance phase.")

(defun bw-sit-for(seconds &optional nodisp)
  (when bw-debug
    (sit-for seconds nodisp)))

(defun bw-message(fmt &rest args)
  (when bw-debug
    (let* ((str (apply 'format fmt args))
           (indent "")
           (len (length str)))
      (while (< 0 (length str))
        (setq len 140)
        (when (> len (length str)) (setq len (length str)))
        (message "%s%s" indent (substring str 0 len))
        (setq str (substring str len))
        (setq indent "    ")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bw-firstw(obj)
  "Get first window in current"
  ;;(message "bw-firstw %s" obj)(sit-for 5)
  (if (windowp obj) obj
    (bw-firstw (car (assq 'childs obj)))))
   
(defun bw-checkobj(obj)
  (unless (or (windowp obj)
              (and (assq 'r obj)
                   (listp (assq 'childs obj))))
    (error "Bad obj=%s" obj)))

(defun bw-chkdim(dimension)
  (unless (memq dimension '(hor ver))
    (error "Internal error: bad dimension: %s" dimension)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Window or object edges
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bw-l(obj)
  (if (windowp obj) (nth 0 (window-edges obj)) (cdr (assq 'l obj))))
(defun bw-t(obj)
  (if (windowp obj) (nth 1 (window-edges obj)) (cdr (assq 't obj))))
(defun bw-r(obj)
  (if (windowp obj) (nth 2 (window-edges obj)) (cdr (assq 'r obj))))
(defun bw-b(obj)
  (if (windowp obj) (nth 3 (window-edges obj)) (cdr (assq 'b obj))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sorting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bw-sort(windows dimension)
  (bw-chkdim dimension)
  (bw-message "sort %s %s" windows dimension)
  (let ((s (sort (reverse windows)
                 (lambda(l r)
                   (if (eq 'hor dimension)
                       (if (< (bw-l l) (bw-l r))
                           t
                         (when (= (bw-l l) (bw-l r))
                           (when (< (bw-t l) (bw-t r)) t)))
                     (if (< (bw-t l) (bw-t r))
                         t
                       (when (= (bw-t l) (bw-t r))
                         (when (< (bw-l l) (bw-l r))))))))))
    (bw-message "<<< sort result=%s" s)
    s))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Weight (not yet used)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar bw-weight 0.7)

(defun bw-set-weight(obj weight)
  (bw-checkobj obj)
  (when (windowp obj) (error "Can't set weight on window"))
  (assq-delete-all 'weight obj)
  (assoc 'weight weight))

(defun bw-get-weight(obj)
  (bw-checkobj obj)
  (when (windowp obj) (error "Can't get weight on window"))
  (car (assoc 'weight obj)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Split directions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bw-dir(obj)
  (if (symbolp obj)
      obj
    (if (windowp obj)
        'both
      (let ((dir (cdr (assq 'dir obj))))
        (unless (memq dir '(hor ver both))
          (error "Can't find dir in %s" obj))
        dir))))

(defun bw-eqdir(obj1 obj2)
  (let ((dir1 (bw-dir obj1))
        (dir2 (bw-dir obj2)))
    (bw-message "dir1=%s, dir2=%s" dir1 dir2)
    (or (eq dir1 dir2)
        (eq dir1 'both)
        (eq dir2 'both))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Building split tree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bw-refresh-edges(obj)
  (unless (windowp obj)
    (bw-message "bw-re obj=%s" obj)
    (let ((childs (cdr (assq 'childs obj)))
          (ol 1000)
          (ot 1000)
          (or -1)
          (ob -1))
      (dolist (o childs)
        (when (> ol (bw-l o)) (setq ol (bw-l o)))
        (when (> ot (bw-t o)) (setq ot (bw-t o)))
        (when (< or (bw-r o)) (setq or (bw-r o)))
        (when (< ob (bw-b o)) (setq ob (bw-b o))))
      (setq obj (delq 'l obj))
      (setq obj (delq 't obj))
      (setq obj (delq 'r obj))
      (setq obj (delq 'b obj))
      (add-to-list 'obj (cons 'l ol))
      (add-to-list 'obj (cons 't ot))
      (add-to-list 'obj (cons 'r or))
      (add-to-list 'obj (cons 'b ob))
      (bw-checkobj obj)
      (bw-message "bw-re 2 obj=%s" obj)))
  obj)

(defun bw-merge(obj lst direction)
  (bw-message ">>>> bw-merge obj=%s" obj)
  (bw-message ">> lst=%s" lst)
  (bw-checkobj obj)
  (dolist (o lst) (bw-checkobj o))
  (let ((newobj nil)
        (objdir (bw-dir obj))
;;         (ol (bw-l obj))
;;         (ot (bw-t obj))
;;         (or (bw-r obj))
;;         (ob (bw-b obj))
        )
;;     (dolist (o lst)
;;       (when (> ol (bw-l o)) (setq ol (bw-l o)))
;;       (when (> ot (bw-t o)) (setq ot (bw-t o)))
;;       (when (< or (bw-r o)) (setq or (bw-r o)))
;;       (when (< ob (bw-b o)) (setq ob (bw-b o))))
;;     (add-to-list 'newobj (cons 'l ol))
;;     (add-to-list 'newobj (cons 't ot))
;;     (add-to-list 'newobj (cons 'r or))
;;     (add-to-list 'newobj (cons 'b ob))
    (add-to-list 'newobj (cons 'dir direction))
    (bw-message "direction=%s objdir=%s" direction objdir)
    (if (bw-eqdir obj direction) ;;(windowp obj)
        (progn
          ;;(setq newobj (car newobj))
          (bw-message "merging similar")
          (let ((allchilds (reverse lst)))
            (if (windowp obj)
                (progn
                  (add-to-list 'allchilds obj))
              (setq allchilds (append (cdr (assq 'childs obj)) allchilds)))
            (add-to-list 'newobj (cons 'childs (bw-sort allchilds direction))))
          (bw-message "newobj=%s" newobj))
      (bw-message "merging split")
      ;;(when (bw-eqdir obj direction) (error "Unexpected directions in merge"))
      (add-to-list 'newobj (cons 'childs (bw-sort (cons obj lst) direction)))
      )
    ;;(error "%s" (assq 'childs newobj))
    (setq newobj (bw-refresh-edges newobj))
    ;;(message "beofre checkobj newobj=%s" newobj)
    (bw-checkobj newobj)
    (bw-message "   <<<< bw-merge return=%s" newobj)
    newobj))

(defun bw-in-line(p1 p2 dimension)
  (let ((max1 (if (eq 'hor dimension) (bw-b p1) (bw-r p1)))
        (min1 (if (eq 'hor dimension) (bw-t p1) (bw-l p1)))
        (max2 (if (eq 'hor dimension) (bw-b p2) (bw-r p2)))
        (min2 (if (eq 'hor dimension) (bw-t p2) (bw-l p2))))
    ;;(bw-message "(bw-in-line %s %s %s), 1=%s, %s; 2=%s %s" p1 p2 dimension min1 max1 min2 max2)
    (and (= max1 max2)
         (= min1 min2))))

(defun bw-get-sib(sibling possible dimension)
  (bw-chkdim dimension)
  (let ((pos (bw-sort possible dimension))
        (sib-max (if (eq 'hor dimension) (bw-r sibling) (bw-b sibling)))
        (sib-min (if (eq 'hor dimension) (bw-l sibling) (bw-t sibling)))
        sibs
        (is-hor (eq 'hor dimension))
        )
    (bw-message "(bw-get-sib: is-hor=%s min,max=%s,%s pos=%s" is-hor sib-min sib-max pos)
    (dolist (p pos)
      (when (bw-in-line sibling p dimension)
        (when (= sib-max (if is-hor (bw-l p) (bw-t p)))
          (setq sib-max (if is-hor (bw-r p) (bw-b p)))
          (setq sibs (cons p sibs)))))
    (dolist (p (reverse pos))
      (when (bw-in-line sibling p dimension)
        (when (= sib-min (if is-hor (bw-r p) (bw-b p)))
          (setq sib-min (if is-hor (bw-l p) (bw-t p)))
          (setq sibs (cons p sibs)))))
    sibs))

(defvar bw-hv nil
  "List of tried resulotions of + formed border conflicts.")

(defun bw-build-tree()
  (bw-message ">>>> bw-build-tree")
  (select-window (frame-first-window))
  (let ((wtree (reverse (window-list nil 0)))
        (no-changes 0))
    (while (< no-changes (length wtree))
      (setq prev-len (length wtree))
      (let* ((curr (car (last wtree)))
             (cl (bw-l curr))
             (ct (bw-t curr))
             (cr (bw-r curr))
             (cb (bw-b curr))
             hsib vsib)
        (bw-checkobj curr)
        (bw-message "")
        (bw-message "                          --------")
        (bw-message "wtree 1=%s" wtree)
        (setq wtree (delq curr wtree))
        (bw-message "wtree 2=%s" wtree)
        (bw-message "curr=%s %s %s %s %s" curr cl ct cr cb)

        ;; Same top and bottom?
        (setq hsib (bw-get-sib curr wtree 'hor))
        (bw-message "hsib=%s" hsib) ;;(bw-sit-for 2)

        ;; Same left and right?
        (setq vsib (bw-get-sib curr wtree 'ver))
        (bw-message "vsib=%s" vsib) ;;(bw-sit-for 2)

        ;;(bw-message "wtree after same=%s" wtree)
        ;; Both can't be possible
        (when (and hsib vsib)
          ;;(error "Got both horizontal and vertical siblings, can't handle that!")
          ;; TODO: record tries here and loop
          (if (assoc hsib bw-hv)
              (setq hsib nil)
            (add-to-list 'bw-hv (cons hsib t))
            (setq vsib nil))
          )
        ;; Merge
        (when hsib
          (dolist (s hsib) (setq wtree (delq s wtree)))
          (push (bw-merge curr hsib 'hor) wtree))
        (when vsib
          (dolist (s vsib) (setq wtree (delq s wtree)))
          (push (bw-merge curr vsib 'ver) wtree))
        (if (or hsib vsib)
            (setq no-changes 0)
          (setq no-changes (1+ no-changes))
          (push curr wtree))
        ))
    ;; It should be here now
    (bw-message "   <<< bw-build-tree end %s" wtree)
    (bw-message "")
    ;; Return tree
    (let ((tree (if (= 1 (length wtree))
                    (car wtree)
                  (error "All not merged - have the screen crashed . . ")
                  )))
      (bw-checkobj tree)
      (if (windowp tree)
          nil
        tree))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Balancing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bw-balance()
  "Balance windows heights and widths."
  (interactive)
  (setq bw-hv nil)
  (let ((sw (selected-window))
        (wt (bw-build-tree))
        (w)
        (h)
        (bw-did-enlarge t)
        (bw-couldnt-enlarge nil)
        (counter 0))
    (bw-message "")
    (bw-message "wt=%s" wt)
    (when wt
      (bw-checkobj wt)
      (while (and (> 20 (setq counter (1+ counter)))
                  (or bw-did-enlarge bw-couldnt-enlarge))
        (when bw-couldnt-enlarge
          (setq bw-couldnt-enlarge nil)
          (setq wt (bw-build-tree)))
        (setq bw-did-enlarge nil)
        (when (eq 'hor (bw-dir wt))
          (setq w (- (bw-r wt) (bw-l wt))))
        (when (eq 'ver (bw-dir wt))
          (setq h (- (bw-b wt) (bw-t wt))))
        (bw-balance-sub wt w h)
        ;;(when bw-did-enlarge (message "was enlarged") (bw-sit-for 2) (setq bw-did-enlarge nil))
        ))
    (select-window sw)))

(defun bw-enlarge-window(arg side preserve-before)
  (let ((lrtb (window-edges (selected-window))))
    ;; In 21.3 enlarge-window takes only two args:
    (if (fboundp 'cua-mode)
        (enlarge-window arg side preserve-before)
      (enlarge-window arg side))
    (let ((lrtb2 (window-edges (selected-window))))
      (unless (equal lrtb lrtb2)
        (setq bw-did-enlarge t))
      (dolist (i '(0 1 2 3))
        (when (< 1 (abs (- (nth i lrtb) (nth i lrtb2))))
          ;;(message "couldnt %s %s %s" i lrtb lrtb2)(sit-for 3)
          (setq bw-couldnt-enlarge t)))
      )))

(defun bw-balance-sub(wt w h)
  (bw-message ">>>> bw-balance-sub %s %s %s" wt w h)
  (bw-checkobj wt)
  (setq wt (bw-refresh-edges wt))
  (unless w (setq w (- (bw-r wt) (bw-l wt))))
  (unless h (setq h (- (bw-b wt) (bw-t wt))))
  (if (windowp wt)
      (progn
        (select-window wt)
        (bw-message "***** window RESIZE %s %s %s %s" wt w h (window-edges wt))
        (when w
          (let ((dw (- w (- (bw-r wt) (bw-l wt)))))
            (bw-message "dw=%s edges=%s" dw (window-edges wt))
            (when (/= 0 dw)
              (bw-enlarge-window dw t t)
              (bw-sit-for 1)
              )))
        (when h
          (let ((dh (- h (- (bw-b wt) (bw-t wt)))))
            (bw-message "dh=%s edges=%s" dh (window-edges wt))
            (when (/= 0 dh)
              (bw-enlarge-window dh nil t)
              (bw-sit-for 1)
              )
            (bw-message "   => %s" (mapcar (lambda(elt) (window-edges elt)) (window-list)))
            )))
    (let* ((childs (cdr (assq 'childs wt)))
           (dummy (bw-message "Not window %s %s %s lc=%s" wt w h (length childs)))
           (cw (when w (/ w (if (bw-eqdir 'hor wt) (length childs) 1))))
           (ch (when h (/ h (if (bw-eqdir 'ver wt) (length childs) 1)))))
      (dolist (c childs) (bw-balance-sub c cw ch)))))
  

(provide 'bw)

;;(let ((s)) (walk-windows (lambda(w) (setq s (concat s (format "%s: %s\n" w (window-edges w)))))) (message s))
;;(list (bw-l (selected-window)) (bw-t (selected-window)) (bw-r (selected-window)) (bw-b (selected-window)))

;;; bw.el ends here
