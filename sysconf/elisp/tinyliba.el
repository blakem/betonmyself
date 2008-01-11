;;; @(#) tinyliba.el --- Library for (a)utoload definitions
;;; @(#) $Id: tinyliba.el 2138 2003-05-19 02:37:26Z blakem $

;;{{{ Id

;; Copyright (C)    1998-2002 Jari Aalto
;; Author:          Jari Aalto <jari.aalto@poboxes.com>
;; Maintainer:      Jari Aalto <jari.aalto@poboxes.com>
;; Created:         1998-03
;; Keywords:        extensions
;;
;; To get information on this program use ident(1) or do M-x tinyliba-version
;; Look at the code with folding.el, tinybm.el

;; LCD Archive Entry:
;; tinyliba|Jari Aalto|jari.aalto@poboxes.com|
;; library for (a)utoload definitions|
;; 2001-12-31|$Revision: 2138 $|~/misc/tinyliba.el.Z|

;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs.  If you did not, write to the Free Software Foundation,
;; Inc., 675 Mass Ave., Cambridge, MA 02139, USA.

;;}}}
;;{{{ Install

;; ........................................................ &t-install ...
;; DO NOT LOAD THIS FILE, but load the central library "m". It loads this
;; file and backward compatible library "b"
;;
;;     	(require 'tinylibm)

;;}}}
;;{{{ Documentation

;; ..................................................... &t-commentary ...

;;; Commentary:

;;  Preface, 1998
;;
;;	This is lisp function library, package itself does nothing.
;;	This library defines autoload functions and few emacs version
;;      detection functions.
;;
;;	The autoloads are automatically generated and you should not
;;	fix them by hand. To add or update autoloads from a package,
;;	do it like this:
;;
;;	o   Load library tinylisp.el
;;	o   Generate autoloads to separate buffer with
;;	    command C-u M-x tinylisp-autoload-generate-library
;;	o   At the end of buffer *tinylisp-autoloads* cut'n paste
;;	    the definititions to this file.

;;}}}

;;; Change Log:

;;; Code:

;;{{{ code: Init


;; Older byte compiler's don't allow putting these inside
;; `eval-and-compile':
;;
;;   ** The compiler ignores `autoload' except at top level.  You should
;;      probably put the autoload of the macro `with-timeout' at top-level.

(autoload 'with-timeout	     "timer"	    "" nil 'macro)
(autoload 'easy-menu-define  "easymenu"	    "" nil 'macro)


(defun tinyliba-file-version (file)
  "Find Version: string from lisp FILE."
  (let* ((lib    (locate-library file))
	 (buffer (and lib (find-file-noselect lib)))
	 find-file-hooks
	     version)
    (save-excursion
      (if (null find-file-hooks)       ;; No-op, byte compiler silencer
	      (setq find-file-hooks nil))
      (set-buffer buffer)
      (goto-char (point-min))
	  (if (re-search-forward
	       "^;+[ \t]+Version:[ \t]+\\(.+\\)" nil t)
	      (setq version (match-string 1)))
	  (kill-buffer buffer)
	  version)))


(eval-and-compile

  (require 'cl)

  (unless (fboundp 'return)
    ;;  cl.el version 3.0 does not define macro `return' cl 2.02(19.34) is ok.
    ;;  This was noticed by Sami Khoury <skhoury@cse.dnd.ca>
    (let ((location (locate-library "cl"))
	  (version  (tinyliba-file-version "cl.el")))
      (error "\
** tinyliba.el: Your 'cl package [%s] is dysfunctional. Get some other version.
                After the (require 'cl), it dind't provide standard CL
                `return'. This may be a problem in your `load-path'.
                Re-arrange? The package `cl' was found at %s"
	     location version)))

  (require 'backquote)


  ;; ..................................................... &presettings ...


  ;; defvar silences Byte Compiler

  (defvar byte-compile-dynamic nil "")   ;; Introduced in 19.29
  (make-local-variable 'byte-compile-dynamic)
  (setq byte-compile-dynamic t)         ;19.29+


  ;; Predeclare functions for byte compiler, so that we can later use
  ;;
  ;; (eval-and-compile
  ;;   (when (emacs-p)
  ;;       ...))

  (defsubst xemacs-p (&optional version-string)
    "Check if running XEmacs. Optionally at least VERSION-STRING.
Version string is like  \"20.4\". Value t is returned if version of
emacs is equal or greater than VERSION-STRING."
    ;; `emacs-version' can't be modified, be bomb sure
    (when (string-match "xemacs" (emacs-version))
      ;;  These caould be "mistakenly" get defined. Unlikely,
      ;;  but we won't even get here unless `emacs-version' say so.
      (if (or (boundp 'xemacs-logo)
	      (featurep 'xemacs))		;Appeared in 20.2+
	  (cond
	   ((null version-string)
	    emacs-version)
	   ((not (string< emacs-version version-string ))
	    emacs-version)))))


  (defsubst emacs-p (&optional version-string)
    "Check if running Emacs. Optionally at least VERSION-STRING.
Version string is like  \"20.4\". Value t is returned if version of
emacs is equal or greater than VERSION-STRING."
    (if (not (boundp 'xemacs-logo))
	(cond
	 ((null version-string)
	  emacs-version)
	 ((not (string< emacs-version version-string ))
	  emacs-version))))


  (defsubst emacs-version-number-as-string ()
    "Emacs and XEmacs compatibility. Return plain version number string."
    (if (emacs-p)
	emacs-version			; "19.34"
      ;; XEmacs return "19.14 XEmacs Lucid", get only version
      (and (string-match "^\\([0-9]+\\.[0-9.]+\\)" emacs-version)
	   (substring emacs-version 0 (match-end 1)) )))

  (defsubst emacs-version-number-as-string-major ()
    "Return major version number string. 20.4.1 --> 20.4"
    (and (string-match "^\\([0-9]+\\.[0-9]+\\)" emacs-version)
	 (substring emacs-version 0 (match-end 1))))


  ;;  Note: While Emacs would return 20.4.1 for version number,
  ;;  The installation directory is not emacs-20.4.1 but 20.4 for
  ;;  official releases.
  ;;
  ;;  Win32: (getenv "emacs_dir"))
  ;;  emacs_dir is one of the variables that are taken from
  ;;  the registry and mapped into the environment during startup
  ;;  of the emacs binary.
  ;;
  ;;  See also `invocation-directory', The directory in which the Emacs
  ;;  executable was found
  ;;
  ;;  See also `data-directory' Directory of machine-independent files that
  ;;  come with GNU Emacs. These are files intended for Emacs to use while
  ;;  it runs.

  (defun emacs-install-root ()
    "Return Emacs install ROOT by searching emacs version number from `load-path'."
    (let ((regexp
	   (concat
	    ".*" (regexp-quote (emacs-version-number-as-string-major))
	    "[.0-9]*"))
	  ret)
      (dolist (elt load-path)
	(when (and (stringp elt)
		   (string-match regexp elt)
		   (setq ret (match-string 0 elt))
		   ;;  load-path may contain whatever directories, but
		   ;;  is it on disk too?
		   (file-directory-p ret))
	  (return)))
      ret))

  (defun win32-p ()
    "Check if running under Win32 system."
    (cond
     ((memq system-type '(ms-dos windows-nt)))  ;; Emacs
     ((fboundp 'console-type)
      ;; Quiet Emacs byte compiler
      (memq (funcall (symbol-function 'console-type))
	    '(win32 w32 mswindows)))
     ((boundp 'window-system)
      (memq (symbol-value 'window-system) '(win32 w32 mswindows)))
     ((error "Internal alert, contact maintainer of TinyLib."))))


  (unless (fboundp 'w32-system-shell-p)    ;; Emacs function
    (defun w32-system-shell-p (shell-name)
      "Emacs an XEmacs compatibility."
      (or (and (fboundp 'w32-system-shell-p)
	       (w32-system-shell-p shell-name))
	  ;;  This is simplistic alternative if the above function
	  ;;  is not avilable.
	  (string-match "cmdproxy"
			(or shell-name "")))))

  (defun win32-shell-p ()
    "Check if shell filename is traditional win32 shell."
    (w32-system-shell-p (or shell-file-name "")))

  (defsubst win32-9x-p ()
    "Check windows 9x."
    (and (win32-p)
	 ;;#todo: Should use %SystemRoot% or something
	 ;; User may have multiboot drive, where c: is not the right drive
	 (file-exists-p "c:/windows/command.com")))

  (defsubst win32-nt-p ()
    "Check windows NT."
    (or (and (fboundp 'w32-using-nt)
	     ;;  Emacs has this in w32-fns.el
	     (funcall (symbol-function 'w32-using-nt)))
	(let ((nt-root  (getenv "systemroot")))
	  (and nt-root
	       (win32-p)
	       (or (string-match "windows.*NT"  (or (getenv "OS") "" ))
		   (file-exists-p
		    (concat
		     (file-name-as-directory nt-root)
		     "system32/cnd.exe")))))))


  (defun win32-cygwin-p (&optional use-cache)
    "Return path if cygwin1.dll is found from `exec-path'.
If USE-CACHE is non-nil, retrieve cached value."
    (let (ret)
      (cond
       ((and use-cache
	     (get 'win32-cygwin-p 'cache-set))
	(get 'win32-cygwin-p 'cache-value))
       (t
	(put 'win32-cygwin-p 'cache-set t)
	(dolist (path exec-path)
	  ;; "E:/USR/LOCAL/CYGNUS/B19/H-I386-CYGWIN32/BIN"
	  ;;
	  (when (or (string-match "CYGNUS[/\\]\\([^/\\]+\\)[/\\].*cygwin32" path)
		    (when (file-exists-p
			   (concat
			    (file-name-as-directory path) "cygwin1.dll"))
		      ;;  The root directory is one DIR. ../bin/cygwin1.dll
		      ;;
		      ;;  1) Drop the trailing slash  ../bin
		      ;;  2) Give one directory up    ..
		      ;;
		      ;;  We have to leave trailing slash, because the resulting
		      ;;  directory may be in the worst case C:/
		      ;;  (which is NOT recommended place for cygwin install)
		      ;;
		      (if (string-match "^.*[^/\\]" path)
			  (setq path
				(file-name-directory
				 (match-string 0 path))))))
	    (setq ret path)
	    (put 'win32-cygwin-p 'cache-value ret)
	    (return)))))
      ret))


  (defun turn-on-emacs-debug ()
    "Turn on Emacs or XEmacs debug."
    (interactive)
    (emacs-debug-mode 1))

  (defun turn-off-emacs-debug ()
    "Turn off Emacs or XEmacs debug."
    (interactive)
    (emacs-debug-mode 0))

  (defun emacs-debug-mode (&optional mode)
    "Toggle XEmacs/Emacs debug on and off."
    (interactive "P")

    ;;  The normal debug flag

    (cond
     ((null mode)
      (setq debug-on-error (not debug-on-error)))
     ((and (integerp mode) (> mode 0))
      (setq debug-on-error t))
     (t
      (setq debug-on-error nil)))

    (when (boundp 'debug-ignored-errors)
      (unless  (get 'debug-ignored-errors 'tinyliba)
	(put 'debug-ignored-errors 'tinyliba t)
	(put 'debug-ignored-errors 'tinyliba-saved debug-ignored-errors)))

    (cond
     (debug-on-error
      ;;
      ;;   Emacs 20. You want to see all errors
      ;;
      (when (boundp 'debug-ignored-errors)
	(set 'debug-ignored-errors nil))
      (setq debug-on-error t)
      ;;
      ;;  Must be nil, otherwise it get's on your nervers
      ;;  too much when yo hit C-g to interrupt inputs.
      ;;  This only exists in New emacs releases.
      ;;
      (if (boundp 'debug-on-quit)
	  (setq debug-on-quit nil))
      ;;
      (if (boundp 'debug-on-signal);;  This must *not* be on!
	  (setq debug-on-signal nil))
      ;;
      (if (boundp 'stack-trace-on-error)         ;; Xemacs
	  (set 'stack-trace-on-error t))
      (message "TinyLib: Emacs debug is ON"))
     (t
      (when (boundp 'debug-ignored-errors)
	(set 'debug-ignored-errors
	     (get 'debug-ignored-errors 'tinyliba-value)))
      (if (boundp 'stack-trace-on-error)         ;; Xemacs
	  (set 'stack-trace-on-error nil))
      (message "TinyLib: Emacs debug is OFF"))))


  )

;;}}}
;;{{{ code: utility functions

;; These are from SEMI::APEL::poe.el

(put 'defun-maybe    'lisp-indent-function 'defun)
(defmacro defun-maybe (name &rest everything-else)
  (or (and (fboundp name)
           (not (get name 'defun-maybe)))
      (` (or (fboundp (quote (, name)))
             (progn
               (defun (, name) (,@ everything-else))
               (put (quote (, name)) 'defun-maybe t))))))

(put 'defsubst-maybe 'lisp-indent-function 'defun)
(defmacro defsubst-maybe (name &rest everything-else)
  (or (and (fboundp name)
           (not (get name 'defsubst-maybe)))
      (` (or (fboundp (quote (, name)))
             (progn
               (defsubst (, name) (,@ everything-else))
               (put (quote (, name)) 'defsubst-maybe t))))))

(put 'defmacro-maybe 'lisp-indent-function 'defun)
(defmacro defmacro-maybe (name &rest everything-else)
  (or (and (fboundp name)
           (not (get name 'defmacro-maybe)))
      (` (or (fboundp (quote (, name)))
             (progn
               (defmacro (, name) (,@ everything-else))
               (put (quote (, name)) 'defmacro-maybe t))))))

(defmacro defalias-maybe (sym newdef)
  "Make defalias SYM if it does not exist and NEWDEF exists."
  (`
   (when (and (not (fboundp (, sym)))
	      (fboundp (, newdef)))
     (defalias (, sym) (, newdef)))))

(defmacro defconst-maybe (name &rest everything-else)
  (or (and (boundp name)
           (not (get name 'defconst-maybe)))
      (` (or (boundp (quote (, name)))
             (progn
               (defconst (, name) (,@ everything-else))
               (put (quote (, name)) 'defconst-maybe t))))))

;;}}}

(eval-and-compile

;; XEmacs and Emacs differ here

;; (if (locate-library "rsz-mini")
;;     (autoload 'resize-minibuffer-mode "rsz-mini")
;;   (autoload 'resize-minibuffer-mode "rsz-minibuf"))
;;

;;{{{ code: Autoload easymenu.el

;;  These are from XEmacs 19.14, they should suffice

(autoload 'easy-menu-do-define                  "easymenu" "" nil)
(autoload 'easy-menu-add                        "easymenu" "" nil)
(autoload 'easy-menu-remove                     "easymenu" "" nil)


;;; ..  ..  ..  ..  ..  ..  ..  ..  ..  ..  ..  ..  ..  .. Emacs 19.30  ..

;; (autoload 'easy-menu-define                     "easymenu" "" nil 'macro)
;; (autoload 'easy-menu-do-define                  "easymenu" "" t)
;; (autoload 'easy-menu-create-keymaps             "easymenu" "" nil)
;; (autoload 'easy-menu-change                     "easymenu" "" nil)
;; (autoload 'easy-menu-remove                     "easymenu" "" nil)
;; (autoload 'easy-menu-add                        "easymenu" "" nil)

;;}}}
;;{{{ code: Autoload skeleton.el

(autoload 'define-skeleton                      "skeleton" "" t 'macro)
(autoload 'skeleton-proxy-new                   "skeleton" "" t)
(autoload 'skeleton-proxy                       "skeleton" "" t)
(autoload 'skeleton-abbrev-cleanup              "skeleton" "" nil)
(autoload 'skeleton-insert                      "skeleton" "" nil)
(autoload 'skeleton-read                        "skeleton" "" nil)
(autoload 'skeleton-internal-list               "skeleton" "" nil)
(autoload 'skeleton-internal-1                  "skeleton" "" nil)
(autoload 'skeleton-pair-insert-maybe           "skeleton" "" t)

;;}}}
;;{{{ code: Autoload cl

;; cl-compat.el Emacs 19.34

(autoload 'defkeyword                           "cl-compat" "" nil 'macro)
(autoload 'keywordp                             "cl-compat" "" nil)
(autoload 'keyword-of                           "cl-compat" "" nil)
(autoload 'values                               "cl-compat" "" nil)
(autoload 'values-list                          "cl-compat" "" nil)
(autoload 'multiple-value-list                  "cl-compat" "" nil 'macro)
(autoload 'multiple-value-call                  "cl-compat" "" nil 'macro)
(autoload 'multiple-value-bind                  "cl-compat" "" nil 'macro)
(autoload 'multiple-value-setq                  "cl-compat" "" nil 'macro)
(autoload 'multiple-value-prog1                 "cl-compat" "" nil 'macro)
(autoload 'build-klist                          "cl-compat" "" nil)
(autoload 'extract-from-klist                   "cl-compat" "" nil)
(autoload 'keyword-argument-supplied-p          "cl-compat" "" nil)
(autoload 'elt-satisfies-test-p                 "cl-compat" "" nil)
(autoload 'cl-floor                             "cl-compat" "" nil)
(autoload 'cl-ceiling                           "cl-compat" "" nil)
(autoload 'cl-round                             "cl-compat" "" nil)
(autoload 'cl-truncate                          "cl-compat" "" nil)
(autoload 'safe-idiv                            "cl-compat" "" nil)
(autoload 'pair-with-newsyms                    "cl-compat" "" nil)
(autoload 'zip-lists                            "cl-compat" "" nil)
(autoload 'unzip-lists                          "cl-compat" "" nil)
(autoload 'reassemble-argslists                 "cl-compat" "" nil)
(autoload 'duplicate-symbols-p                  "cl-compat" "" nil)
(autoload 'setnth                               "cl-compat" "" nil)
(autoload 'setnthcdr                            "cl-compat" "" nil)
(autoload 'setelt                               "cl-compat" "" nil)

;; cl-extra.el 19.34

;; (autoload 'cl-push                              "cl-extra" "" nil 'macro)
;; (autoload 'cl-pop                               "cl-extra" "" nil 'macro)
(autoload 'coerce                               "cl-extra" "" nil)
(autoload 'equalp                               "cl-extra" "" nil)
(autoload 'cl-mapcar-many                       "cl-extra" "" nil)
(autoload 'map                                  "cl-extra" "" nil)
(autoload 'maplist                              "cl-extra" "" nil)
(autoload 'mapc                                 "cl-extra" "" nil)
(autoload 'mapl                                 "cl-extra" "" nil)
(autoload 'mapcan                               "cl-extra" "" nil)
(autoload 'mapcon                               "cl-extra" "" nil)
(autoload 'some                                 "cl-extra" "" nil)
(autoload 'every                                "cl-extra" "" nil)
(autoload 'notany                               "cl-extra" "" nil)
(autoload 'notevery                             "cl-extra" "" nil)
(autoload 'cl-map-keymap                        "cl-extra" "" nil)
(autoload 'cl-map-keymap-recursively            "cl-extra" "" nil)
(autoload 'cl-map-intervals                     "cl-extra" "" nil)
(autoload 'cl-map-overlays                      "cl-extra" "" nil)
(autoload 'cl-set-frame-visible-p               "cl-extra" "" nil)
(autoload 'cl-progv-before                      "cl-extra" "" nil)
(autoload 'cl-progv-after                       "cl-extra" "" nil)
(autoload 'gcd                                  "cl-extra" "" nil)
(autoload 'lcm                                  "cl-extra" "" nil)
(autoload 'isqrt                                "cl-extra" "" nil)
(autoload 'cl-expt                              "cl-extra" "" nil)
(autoload 'floor*                               "cl-extra" "" nil)
(autoload 'ceiling*                             "cl-extra" "" nil)
(autoload 'truncate*                            "cl-extra" "" nil)
(autoload 'round*                               "cl-extra" "" nil)
(autoload 'mod*                                 "cl-extra" "" nil)
(autoload 'rem*                                 "cl-extra" "" nil)
(autoload 'signum                               "cl-extra" "" nil)
(autoload 'random*                              "cl-extra" "" nil)
(autoload 'make-random-state                    "cl-extra" "" nil)
(autoload 'random-state-p                       "cl-extra" "" nil)
(autoload 'cl-finite-do                         "cl-extra" "" nil)
(autoload 'cl-float-limits                      "cl-extra" "" nil)
(autoload 'subseq                               "cl-extra" "" nil)
(autoload 'concatenate                          "cl-extra" "" nil)
(autoload 'revappend                            "cl-extra" "" nil)
(autoload 'nreconc                              "cl-extra" "" nil)
(autoload 'list-length                          "cl-extra" "" nil)
(autoload 'tailp                                "cl-extra" "" nil)
(autoload 'cl-copy-tree                         "cl-extra" "" nil)
(autoload 'get*                                 "cl-extra" "" nil)
(autoload 'getf                                 "cl-extra" "" nil)
(autoload 'cl-set-getf                          "cl-extra" "" nil)
(autoload 'cl-do-remf                           "cl-extra" "" nil)
(autoload 'cl-remprop                           "cl-extra" "" nil)
(autoload 'make-hash-table                      "cl-extra" "" nil)
(autoload 'hash-table-p                         "cl-extra" "" nil)
(autoload 'cl-not-hash-table                    "cl-extra" "" nil)
(autoload 'cl-hash-lookup                       "cl-extra" "" nil)
(autoload 'cl-gethash                           "cl-extra" "" nil)
(autoload 'cl-puthash                           "cl-extra" "" nil)
(autoload 'cl-remhash                           "cl-extra" "" nil)
(autoload 'cl-clrhash                           "cl-extra" "" nil)
(autoload 'cl-maphash                           "cl-extra" "" nil)
(autoload 'hash-table-count                     "cl-extra" "" nil)
(autoload 'cl-prettyprint                       "cl-extra" "" nil)
(autoload 'cl-do-prettyprint                    "cl-extra" "" nil)
(autoload 'cl-macroexpand-all                   "cl-extra" "" nil)
(autoload 'cl-macroexpand-body                  "cl-extra" "" nil)
(autoload 'cl-prettyexpand                      "cl-extra" "" nil)


;; cl-seq.el 19.34
;; Hm. Sometimemes you find this message:
;;    "Tried to load `cl-seq' before `cl'!"
;;
;; These are commented for now

(when nil

(autoload 'cl-push                              "cl-seq" "" nil 'macro)
(autoload 'cl-pop                               "cl-seq" "" nil 'macro)
(autoload 'cl-parsing-keywords                  "cl-seq" "" nil 'macro)
(autoload 'cl-check-key                         "cl-seq" "" nil 'macro)
(autoload 'cl-check-test-nokey                  "cl-seq" "" nil 'macro)
(autoload 'cl-check-test                        "cl-seq" "" nil 'macro)
(autoload 'cl-check-match                       "cl-seq" "" nil 'macro)
(autoload 'reduce                               "cl-seq" "" nil)
(autoload 'fill                                 "cl-seq" "" nil)
(autoload 'replace                              "cl-seq" "" nil)
(autoload 'remove*                              "cl-seq" "" nil)
(autoload 'remove-if                            "cl-seq" "" nil)
(autoload 'remove-if-not                        "cl-seq" "" nil)
(autoload 'delete*                              "cl-seq" "" nil)
(autoload 'delete-if                            "cl-seq" "" nil)
(autoload 'delete-if-not                        "cl-seq" "" nil)
(autoload 'remove                               "cl-seq" "" nil)
(autoload 'remq                                 "cl-seq" "" nil)
(autoload 'remove-duplicates                    "cl-seq" "" nil)
(autoload 'delete-duplicates                    "cl-seq" "" nil)
(autoload 'cl-delete-duplicates                 "cl-seq" "" nil)
(autoload 'substitute                           "cl-seq" "" nil)
(autoload 'substitute-if                        "cl-seq" "" nil)
(autoload 'substitute-if-not                    "cl-seq" "" nil)
(autoload 'nsubstitute                          "cl-seq" "" nil)
(autoload 'nsubstitute-if                       "cl-seq" "" nil)
(autoload 'nsubstitute-if-not                   "cl-seq" "" nil)
(autoload 'find                                 "cl-seq" "" nil)
(autoload 'find-if                              "cl-seq" "" nil)
(autoload 'find-if-not                          "cl-seq" "" nil)
(autoload 'position                             "cl-seq" "" nil)
(autoload 'cl-position                          "cl-seq" "" nil)
(autoload 'position-if                          "cl-seq" "" nil)
(autoload 'position-if-not                      "cl-seq" "" nil)
(autoload 'count                                "cl-seq" "" nil)
(autoload 'count-if                             "cl-seq" "" nil)
(autoload 'count-if-not                         "cl-seq" "" nil)
(autoload 'mismatch                             "cl-seq" "" nil)
(autoload 'search                               "cl-seq" "" nil)
(autoload 'sort*                                "cl-seq" "" nil)
(autoload 'stable-sort                          "cl-seq" "" nil)
(autoload 'merge                                "cl-seq" "" nil)
(autoload 'member*                              "cl-seq" "" nil)
(autoload 'member-if                            "cl-seq" "" nil)
(autoload 'member-if-not                        "cl-seq" "" nil)
(autoload 'cl-adjoin                            "cl-seq" "" nil)
(autoload 'assoc*                               "cl-seq" "" nil)
(autoload 'assoc-if                             "cl-seq" "" nil)
(autoload 'assoc-if-not                         "cl-seq" "" nil)
(autoload 'rassoc*                              "cl-seq" "" nil)
(autoload 'rassoc-if                            "cl-seq" "" nil)
(autoload 'rassoc-if-not                        "cl-seq" "" nil)
(autoload 'union                                "cl-seq" "" nil)
(autoload 'nunion                               "cl-seq" "" nil)
(autoload 'intersection                         "cl-seq" "" nil)
(autoload 'nintersection                        "cl-seq" "" nil)
(autoload 'set-difference                       "cl-seq" "" nil)
(autoload 'nset-difference                      "cl-seq" "" nil)
(autoload 'set-exclusive-or                     "cl-seq" "" nil)
(autoload 'nset-exclusive-or                    "cl-seq" "" nil)
(autoload 'subsetp                              "cl-seq" "" nil)
(autoload 'subst-if                             "cl-seq" "" nil)
(autoload 'subst-if-not                         "cl-seq" "" nil)
(autoload 'nsubst                               "cl-seq" "" nil)
(autoload 'nsubst-if                            "cl-seq" "" nil)
(autoload 'nsubst-if-not                        "cl-seq" "" nil)
(autoload 'sublis                               "cl-seq" "" nil)
(autoload 'cl-sublis-rec                        "cl-seq" "" nil)
(autoload 'nsublis                              "cl-seq" "" nil)
(autoload 'cl-nsublis-rec                       "cl-seq" "" nil)
(autoload 'tree-equal                           "cl-seq" "" nil)
(autoload 'cl-tree-equal-rec                    "cl-seq" "" nil)


;; cl-indent.el 19.34

(autoload 'common-lisp-indent-function          "cl-indent" "" nil)
(autoload 'lisp-indent-report-bad-format        "cl-indent" "" nil)
(autoload 'lisp-indent-259                      "cl-indent" "" nil)
(autoload 'lisp-indent-tagbody                  "cl-indent" "" nil)
(autoload 'lisp-indent-do                       "cl-indent" "" nil)
(autoload 'lisp-indent-function-lambda-hack     "cl-indent" "" nil)

) ;; when-nil


;; assoc.el 20.4

(autoload 'asort                                "assoc" "" nil)
(autoload 'aelement                             "assoc" "" nil)
(autoload 'aheadsym                             "assoc" "" nil)
(autoload 'anot-head-p                          "assoc" "" nil)
(autoload 'aput                                 "assoc" "" nil)
(autoload 'adelete                              "assoc" "" nil)
(autoload 'aget                                 "assoc" "" nil)
(autoload 'amake                                "assoc" "" nil)


;;}}}
;;{{{ code: Autoload 'main' lib

;;; ........................................................ &autoload ...

(autoload 'ti::assoc-replace-maybe-add        "tinylibm" "" nil)
(autoload 'ti::kill-buffer-safe                     "tinylibm" "" nil)
(autoload 'ti::use-package-emacs                    "tinylibm" "" nil 'macro)
(autoload 'ti::use-prefix-key                       "tinylibm" "" nil);;defsubst
(autoload 'ti::re-search-check                      "tinylibm" "" nil)

;;; tinylib.el

(autoload 'tinylib-version                      "tinylib" "" t)
(autoload 'tinylib-submit-feedback              "tinylib" "" t)
(autoload 'ti::s-trim-blanks                    "tinylib" "" nil)
(autoload 'ti::s-verify-ends                    "tinylib" "" nil)
(autoload 'ti::s-add-space                      "tinylib" "" nil);;defsubst
(autoload 'ti::s-sqz                            "tinylib" "" nil)
(autoload 'ti::s-mangle                         "tinylib" "" nil)
(autoload 'ti::s-regexp-delete                  "tinylib" "" nil);;defsubst
(autoload 'ti::m-tmp-dir                        "tinylib" "" nil);;defsubst
(autoload 'ti::c-format-string                  "tinylib" "" nil)
(autoload 'ti::c-url-to-ange-ftp                "tinylib" "" nil)
(autoload 'ti::c-upcase-words-to-variable-names "tinylib" "" t)
(autoload 'ti::c-get-nth-str                    "tinylib" "" nil);;defsubst
(autoload 'ti::c-time-list                      "tinylib" "" t)
(autoload 'ti::c-ch2escape                      "tinylib" "" nil)
(autoload 'ti::c-str2re                         "tinylib" "" nil)
(autoload 'ti::c-int-to-access-mode-string      "tinylib" "" nil)
(autoload 'ti::m-rcs-delta-get-revisions        "tinylib" "" nil);;defsubst
(autoload 'ti::m-rcs-delta-get-file             "tinylib" "" nil)
(autoload 'ti::m-rcs-delta-lock-status          "tinylib" "" nil)
(autoload 'ti::m-rcs-delta-lock-status-user     "tinylib" "" nil);;defsubst
(autoload 'ti::m-rcs-delta-highest-version      "tinylib" "" t);;defsubst
(autoload 'ti::m-rcs-read-val                   "tinylib" "" nil);;defsubst
(autoload 'ti::m-rcs-look-id                    "tinylib" "" nil)
(autoload 'ti::m-cvs-to-cvs-dir                 "tinylib" "" nil);;defsubst
(autoload 'ti::m-cvs-to-cvs-dir-p               "tinylib" "" nil);;defsubst
(autoload 'ti::m-cvs-to-cvs-file                "tinylib" "" nil)
(autoload 'ti::m-cvs-to-cvs-file-content        "tinylib" "" nil)
(autoload 'ti::m-cvs-file-exists-p              "tinylib" "" nil)
(autoload 'ti::m-cvs-entry-split                "tinylib" "" nil);;defsubst
(autoload 'ti::m-cvs-entry-type                 "tinylib" "" nil);;defsubst
(autoload 'ti::m-cvs-entry-split-info           "tinylib" "" nil);;defsubst
(autoload 'ti::m-rcs-file-p                     "tinylib" "" nil);;defsubst
(autoload 'ti::m-rcs-make-filename              "tinylib" "" nil)
(autoload 'ti::m-rcs-file-exists-p              "tinylib" "" nil);;defsubst
(autoload 'ti::m-rcs-normal-file                "tinylib" "" nil);;defsubst
(autoload 'ti::m-rcs-sort-same-level-list       "tinylib" "" nil)
(autoload 'ti::m-rcs-files-in-dir               "tinylib" "" nil)
(autoload 'ti::m-rcs-head-version               "tinylib" "" nil);;defsubst
(autoload 'ti::m-rcs-guess-buffer-version       "tinylib" "" nil)
(autoload 'ti::m-rcs-buffer-version             "tinylib" "" nil)
(autoload 'ti::m-rcs-rlog-get-revisions         "tinylib" "" nil);;defsubst
(autoload 'ti::m-rcs-all-versions               "tinylib" "" nil);;defsubst
(autoload 'ti::m-rcs-previous-version           "tinylib" "" nil)
(autoload 'ti::m-rcs-get-all-branches           "tinylib" "" nil)
(autoload 'ti::m-rcs-str-find                   "tinylib" "" nil)
(autoload 'ti::m-rcs-str-find-buffer            "tinylib" "" nil);;defsubst
(autoload 'ti::d-standard-date                  "tinylib" "" t)
(autoload 'ti::d-s-month-n                      "tinylib" "" t)
(autoload 'ti::d-time-difference                "tinylib" "" nil);;defsubst
(autoload 'ti::d-time-diff-days                 "tinylib" "" nil)
(autoload 'ti::d-parse-date                     "tinylib" "" nil)
(autoload 'ti::s-repeat                         "tinylib" "" nil)
(autoload 'ti::s-syntax-info                    "tinylib" "" t)
(autoload 'ti::s-syntax-kill-double-quote       "tinylib" "" t)
(autoload 'ti::s-tabify                         "tinylib" "" nil)
(autoload 'ti:s-match-string-subs               "tinylib" "" nil)
(autoload 'ti:s-match-string-list               "tinylib" "" nil)
(autoload 'ti::s-case-replace                   "tinylib" "" nil)
(autoload 'ti::s-index                          "tinylib" "" nil)
(autoload 'ti::s-index-substring                "tinylib" "" nil)
(autoload 'ti::s-1-space                        "tinylib" "" nil)
(autoload 'ti::s-listify                        "tinylib" "" nil)
(autoload 'ti::b-get-ange-buffer-list           "tinylib" "" nil);;defsubst
(autoload 'ti::b-find-ange-buffer               "tinylib" "" nil)
(autoload 'ti::b-find-ange-to-dired-buffer      "tinylib" "" nil)
(autoload 'ti::b-uu-area                        "tinylib" "" nil)
(autoload 'ti::b-uu-line-p                      "tinylib" "" t)
(autoload 'ti::b-area-bounds                    "tinylib" "" nil)
(autoload 'ti::b-join-region                    "tinylib" "" t)
(autoload 'ti::b-read-if-solid                  "tinylib" "" nil)
(autoload 'ti::b-read-whitespace                "tinylib" "" nil)
(autoload 'ti::b-read-line                      "tinylib" "" nil)
(autoload 'ti::b-grep-lines                     "tinylib" "" nil)
(autoload 'ti::b-looking-back-at                "tinylib" "" nil)
(autoload 'ti::b-read-char                      "tinylib" "" nil)
(autoload 'ti::b-read-word                      "tinylib" "" nil)
(autoload 'ti::b-read-space-word                "tinylib" "" nil)
(autoload 'ti::b-read-syntax-word               "tinylib" "" nil)
(autoload 'ti::b-read-nth-word                  "tinylib" "" nil)
(autoload 'ti::b-replace-keywords-with-table    "tinylib" "" t)
(autoload 'ti::b-replace-region-with            "tinylib" "" nil);;defsubst
(autoload 'ti::b-zap-to-regexp                  "tinylib" "" t)
(autoload 'ti::b-leave-nth-word                 "tinylib" "" t)
(autoload 'ti::b-kill-line                      "tinylib" "" t)
(autoload 'ti::b-strip-control-m                "tinylib" "" nil)
(autoload 'ti::b-u2d-crlf                       "tinylib" "" t)
(autoload 'ti::b-arrow-control                  "tinylib" "" nil)
(autoload 'ti::b-insert-line-numbers            "tinylib" "" t)
(autoload 'ti::b-remove-line-numbers            "tinylib" "" t);;defsubst
(autoload 'ti::b-randomize-lines                "tinylib" "" t)
(autoload 'ti::b-make-dup-line                  "tinylib" "" t)
(autoload 'ti::b-inc-string-nbr                 "tinylib" "" t)
(autoload 'ti::b-copy-line-and-inc-numbers      "tinylib" "" t)
(autoload 'ti::b-copy-word                      "tinylib" "" t)
(autoload 'ti::b-add-newlines-to-region         "tinylib" "" t)
(autoload 'ti::b-cnv-empty-lines                "tinylib" "" t)
(autoload 'ti::b-del-dup-lines                  "tinylib" "" t)
(autoload 'ti::b-delete-until-non-empty-line    "tinylib" "" t)
(autoload 'ti::b-trim-blanks                    "tinylib" "" t)
(autoload 'ti::b-replace-regexp                 "tinylib" "" nil)
(autoload 'ti::b-diff-type-p                    "tinylib" "" nil)
(autoload 'ti::b-xtra-open-outline              "tinylib" "" t)
(autoload 'ti::b-get-re                         "tinylib" "" t)
(autoload 'ti::b-buffer-list-files              "tinylib" "" nil)
(autoload 'ti::w-frame-list                     "tinylib" "" nil)
(autoload 'ti::w-list                           "tinylib" "" nil)
(autoload 'ti::w-get-buffer-window-other-frame  "tinylib" "" nil)
(autoload 'ti::w-find-bottom                    "tinylib" "" nil)
(autoload 'ti::w-match-buffers                  "tinylib" "" nil)
(autoload 'ti::k-single-key-definition-p        "tinylib" "" nil)
(autoload 'ti::k-define-key-backspace           "tinylib" "" t)
(autoload 'ti::k-function-bind-info             "tinylib" "" nil)
(autoload 'ti::k-reinstall-minor-mode           "tinylib" "" nil)
(autoload 'ti::k-add-minor-mode                 "tinylib" "" nil)
(autoload 'ti::k-map-bind-control               "tinylib" "" nil)
(autoload 'ti::k-xlat-table                     "tinylib" "" nil)
(autoload 'ti::k-put-abc-map                    "tinylib" "" nil)
(autoload 'ti::k-put-map                        "tinylib" "" nil)
(autoload 'ti::k-mapkeys                        "tinylib" "" nil)
(autoload 'ti::t-wipe-properties                "tinylib" "" t)
(autoload 'ti::t-try-set-colors                 "tinylib" "" nil)
(autoload 'ti::m-word-move                      "tinylib" "" t)
(autoload 'ti::m-find-duplicate-same-word       "tinylib" "" t)
(autoload 'ti::m-count-words                    "tinylib" "" t)
(autoload 'ti::m-count-chars-in-delimited-area  "tinylib" "" t)
(autoload 'ti::m-move-paragraph-to-column       "tinylib" "" t)
(autoload 'ti::m-move-to-col                    "tinylib" "" t);;defsubst
(autoload 'ti::m-fwd-line                       "tinylib" "" t);;defsubst
(autoload 'ti::m-surround-with-char             "tinylib" "" t)
(autoload 'ti::m-add-spaces-80                  "tinylib" "" t)
(autoload 'ti::m-quote-words-in-region          "tinylib" "" t);;defsubst
(autoload 'ti::m-long-line                      "tinylib" "" nil)
(autoload 'ti::m-scramble-reg                   "tinylib" "" t)
(autoload 'ti::m-add-str-reg                    "tinylib" "" t)
(autoload 'ti::m-sort-regexp-fields             "tinylib" "" nil)
(autoload 'ti::m-passwd-grep-user-alist         "tinylib" "" nil)
(autoload 'ti::m-passwd-build-alist             "tinylib" "" nil)
(autoload 'ti::m-passwd-read-entry              "tinylib" "" nil)
(autoload 'ti::m-make-2D-array                  "tinylib" "" nil)
(autoload 'ti::m-make-ND-array                  "tinylib" "" nil)
(autoload 'ti::m-momentary-string-flash         "tinylib" "" nil)
(autoload 'ti::m-find-function-name             "tinylib" "" nil)
(autoload 'ti::m-reindent                       "tinylib" "" t)
(autoload 'ti::f-days-old                       "tinylib" "" nil);;defsubst
(autoload 'ti::f-touch                          "tinylib" "" nil)
(autoload 'ti::f-ange-completed-message         "tinylib" "" nil)
(autoload 'ti::f-ange-status                    "tinylib" "" nil)
(autoload 'ti::f-ange-download-file             "tinylib" "" nil)
(autoload 'ti::f-ange-file-handle               "tinylib" "" nil)
(autoload 'ti::f-chmod-w-toggle                 "tinylib" "" nil)
(autoload 'ti::f-find-shadows                   "tinylib" "" t)
(autoload 'ti::f-dir-subdirectory-list          "tinylib" "" nil)
(autoload 'ti::f-dir-recursive-do               "tinylib" "" nil)
(autoload 'ti::f-dir-up                         "tinylib" "" nil)
(autoload 'ti::f-dir-subdirs                    "tinylib" "" nil)
(autoload 'ti::f-dir-files                      "tinylib" "" nil)
(autoload 'ti::f-files-only                     "tinylib" "" nil)
(autoload 'ti::f-newer-exist                    "tinylib" "" nil)
(autoload 'ti::f-get-extension                  "tinylib" "" nil)
(autoload 'ti::f-path-and-line-info             "tinylib" "" nil)
(autoload 'ti::f-path-to-unix                   "tinylib" "" nil)
(autoload 'ti::f-path-to-msdos                  "tinylib" "" nil)
(autoload 'ti::f-make-path                      "tinylib" "" nil)
(autoload 'ti::f-get-load-path                  "tinylib" "" t)
(autoload 'ti::f-user-home                      "tinylib" "" nil)
(autoload 'ti::f-file-list                      "tinylib" "" nil)
(autoload 'ti::f-complete-file-name             "tinylib" "" nil)
(autoload 'ti::f-complete-file-name-word        "tinylib" "" t)
(autoload 'ti::f-complete-filename-minibuffer   "tinylib" "" t 'macro)
(autoload 'ti::f-read-file-list                 "tinylib" "" nil)
(autoload 'ti::unix--man-path-root              "tinylib" "" nil)
(autoload 'ti::p-finger-error                   "tinylib" "" nil)
(autoload 'ti::p-finger                         "tinylib" "" t)
(autoload 'ti::p-http-request                   "tinylib" "" t)
(autoload 'ti::p-zip                            "tinylib" "" t)
(autoload 'ti::p-zip-view-command               "tinylib" "" t)
(autoload 'ti::p-tar-zip-view-maybe-command     "tinylib" "" nil)
(autoload 'ti::p-tar-view-command               "tinylib" "" t)
(autoload 'ti::p-tar-read-listing-forward       "tinylib" "" nil)
(autoload 'ti::q-read-input-invisible           "tinylib" "" nil)
(autoload 'ti::q-read-input-as-password         "tinylib" "" nil)
(autoload 'ti::m-selective-display-copy-to      "tinylib" "" t)
(autoload 'ti::m-selective-display-print        "tinylib" "" t)
(autoload 'ti::m-ad-control                     "tinylib" "" nil)
(autoload 'ti::m-pkg-autoload-loaddefs-build-recursive "tinylib" "" nil)
(autoload 'ti::m-pkg-submit-feedback            "tinylib" "" t)
(autoload 'ti::m-pkg-submit-bug-report          "tinylib" "" t)
(autoload 'ti::m-pkg-version-info               "tinylib" "" t)
(autoload 'ti::m-pkg-get-header                 "tinylib" "" nil)
(autoload 'ti::m-pkg-ins-example                "tinylib" "" t)
(autoload 'ti::m-pkg-rip                        "tinylib" "" t)
(autoload 'ti::m-pkg-rip-magic                  "tinylib" "" t)
(autoload 'ti::m-pkg-make-mode-magic            "tinylib" "" t)
(autoload 'ti::m-pkg-make-mode                  "tinylib" "" t)
(autoload 'ti::m-pkg-make-var                   "tinylib" "" nil)
(autoload 'ti::m-pkg-make                       "tinylib" "" nil)
(autoload 'ti::m-pkg-autoload-create-on-file    "tinylib" "" t)
(autoload 'ti::m-pkg-autoload-create-on-directory "tinylib" "" nil)
(autoload 'ti::m-pkg-install-pgp-tar            "tinylib" "" t)
(autoload 'ti::xe-installation-root             "tinylib" "" nil)
(autoload 'ti::xe-overlay-some                  "tinylib" "" nil)
(autoload 'ti::xe-overlay-properties            "tinylib" "" nil)
(autoload 'ti::xe-overlays-at                   "tinylib" "" nil)
(autoload 'ti::xe-overlay-put                   "tinylib" "" nil)
(autoload 'ti::xe-overlay-move                  "tinylib" "" nil)
(autoload 'ti::xe-activate-region               "tinylib" "" nil)
(autoload 'ti::xe-read-password                 "tinylib" "" nil)
(autoload 'ti::xe-key-local-map                 "tinylib" "" nil)
(autoload 'ti::xe-key-call-original             "tinylib" "" nil)
(autoload 'ti::xe-mouse-key                     "tinylib" "" nil)
(autoload 'ti::xe-mouse-call-original-function  "tinylib" "" nil)
(autoload 'ti::xe-mouse-call-original           "tinylib" "" t)
(autoload 'ti::xe-popup                         "tinylib" "" t)
(autoload 'ti::xe-display-depth                 "tinylib" "" nil)
(autoload 'ti::xe-read-event                    "tinylib" "" nil)
(autoload 'ti::xe-executing-macro               "tinylib" "" nil)
(autoload 'ti::xe-make-x-popup-event            "tinylib" "" nil)
(autoload 'ti::xe-make-fake-event               "tinylib" "" nil)
(autoload 'ti::xe-modeline-update               "tinylib" "" nil)
(autoload 'ti::xe-set-frame-name                "tinylib" "" nil)
(autoload 'ti::xe-set-frame-parameter           "tinylib" "" t)
(autoload 'ti::xe-frame-window-config           "tinylib" "" nil)
(autoload 'ti::xe-window-system                 "tinylib" "" nil)
(autoload 'ti::xe-timer-list-control            "tinylib" "" nil)
(autoload 'ti::xe-timer-control                 "tinylib" "" nil)
(autoload 'ti::xe-timer-elt                     "tinylib" "" nil)
(autoload 'ti::xe-timer-process-status          "tinylib" "" nil)
(autoload 'ti::xe-timer-cancel                  "tinylib" "" nil)
(autoload 'ti::xe-timer-cancel-function         "tinylib" "" nil)
(autoload 'ti::xe-set-mode-line-format          "tinylib" "" nil)
(autoload 'ti::m-vmacro-minor-mode              "tinylib" "" nil 'macro)
(autoload 'ti::m-vmacro-minor-mode-1            "tinylib" "" nil)
(autoload 'ti::m-fmacro-minor-mode              "tinylib" "" nil 'macro)
(autoload 'ti::m-fmacro-minor-mode-1            "tinylib" "" t)
(autoload 'ti::m-fmacro-minor-mode-on           "tinylib" "" t)
(autoload 'ti::m-fmacro-minor-mode-off          "tinylib" "" t)
(autoload 'ti::m-fmacro-minor-mode-help         "tinylib" "" t)
(autoload 'ti::m-fmacro-minor-mode-commentary   "tinylib" "" t)
(autoload 'ti::m-fmacro-minor-mode-viper-attach "tinylib" "" t)
(autoload 'ti::m-fmacro-minor-mode-install      "tinylib" "" nil 'macro)
(autoload 'ti::m-fmacro-minor-mode-install-1    "tinylib" "" t)
(autoload 'ti::m-fmacro-define-keys             "tinylib" "" nil 'macro)
(autoload 'ti::m-fmacro-define-keys-1           "tinylib" "" nil)
(autoload 'ti::m-fmacro-version-bug-report-1    "tinylib" "" t)
(autoload 'ti::m-fmacro-version-bug-report      "tinylib" "" nil 'macro)
(autoload 'ti::m-fmacro-debug-1                 "tinylib" "" t)
(autoload 'ti::m-fmacro-debug-lowlevel          "tinylib" "" nil 'macro)
(autoload 'ti::m-fmacro-debug-standard          "tinylib" "" nil 'macro)
(autoload 'ti::m-fmacro-install-pgp-tar-1       "tinylib" "" t)
(autoload 'ti::m-fmacro-install-pgp-tar         "tinylib" "" nil 'macro)
(autoload 'ti::m-fmacro-minor-mode-wizard       "tinylib" "" nil 'macro)
(autoload 'ti::m-fmacro-minor-mode-wizard-1     "tinylib" "" nil)

;;}}}
;;{{{ code: Autoload 'mt' lib -- mail tools

;;; tinylibmt.el

(autoload 'ti::mt-pgp-signature-begin-line      "tinylibmt" "" nil);;defsubst
(autoload 'ti::mt-pgp-signature-end-line        "tinylibmt" "" nil);;defsubst
(autoload 'ti::mt-pgp-signed-begin-line         "tinylibmt" "" nil);;defsubst
(autoload 'ti::mt-pgp-signed-end-line           "tinylibmt" "" nil);;defsubst
(autoload 'ti::mt-pgp-pkey-begin-line           "tinylibmt" "" nil);;defsubst
(autoload 'ti::mt-pgp-pkey-end-line             "tinylibmt" "" nil);;defsubst
(autoload 'ti::mt-pgp-msg-begin-line            "tinylibmt" "" nil);;defsubst
(autoload 'ti::mt-pgp-msg-end-line              "tinylibmt" "" nil);;defsubst
(autoload 'ti::mt-pgp-any-pgp-line-regexp       "tinylibmt" "" nil);;defsubst
(autoload 'ti::mt-news-group                    "tinylibmt" "" nil);;defsubst
(autoload 'tinylibmt-version                    "tinylibmt" "" t)
(autoload 'tinylibmt-submit-feedback            "tinylibmt" "" t)
(autoload 'ti::mt-signature-p                   "tinylibmt" "" nil)
(autoload 'ti::mt-body-empty-p                  "tinylibmt" "" nil)
(autoload 'ti:mt-body-clear                     "tinylibmt" "" nil)
(autoload 'ti::mt-set-region                    "tinylibmt" "" nil 'macro)
(autoload 'ti::mt-point-in-header-macro         "tinylibmt" "" nil 'macro)
(autoload 'ti::mt-message-length                "tinylibmt" "" nil)
(autoload 'ti::mt-get-2re                       "tinylibmt" "" nil)
(autoload 'ti::mt-required-headers              "tinylibmt" "" nil)
(autoload 'ti::mt-mail-mode-p                   "tinylibmt" "" nil)
(autoload 'ti::mt-mailbox-p                     "tinylibmt" "" nil)
(autoload 'ti::mt-mail-p                        "tinylibmt" "" nil)
(autoload 'ti::mt-header-area-size              "tinylibmt" "" nil)
(autoload 'ti::mt-hmax                          "tinylibmt" "" nil)
(autoload 'ti::mt-text-start                    "tinylibmt" "" nil)
(autoload 'ti::mt-point-at-header-p             "tinylibmt" "" nil)
(autoload 'ti::mt-point-at-body-p               "tinylibmt" "" nil)
(autoload 'ti::mt-narrow                        "tinylibmt" "" nil)
(autoload 'ti::mt-mail-buffer-name              "tinylibmt" "" nil)
(autoload 'ti::mt-generate-buffer-name          "tinylibmt" "" t)
(autoload 'ti::m-mail-simple-p                  "tinylibmt" "" nil)
(autoload 'ti::mt-to-list-p                     "tinylibmt" "" nil)
(autoload 'ti::mt-vm-macro                      "tinylibmt" "" nil 'macro)
(autoload 'ti::mt-mh-macro                      "tinylibmt" "" nil 'macro)
(autoload 'ti::mt-gnus-macro                    "tinylibmt" "" nil 'macro)
(autoload 'ti::mt-rmail-macro                   "tinylibmt" "" nil 'macro)
(autoload 'ti::mt-rmail-do-message-macro        "tinylibmt" "" nil 'macro)
(autoload 'ti::mt-rmail-copy-message            "tinylibmt" "" t)
(autoload 'ti::mt-pgp-v3xx-p                    "tinylibmt" "" nil)
(autoload 'ti::mt-pgp-p                         "tinylibmt" "" nil)
(autoload 'ti::mt-pgp-signed-conventional-p     "tinylibmt" "" nil)
(autoload 'ti::mt-pgp-signature-detached-p      "tinylibmt" "" nil)
(autoload 'ti::mt-pgp-signed-conventional-multi-p "tinylibmt" "" nil)
(autoload 'ti::mt-pgp-signed-xpgp-p             "tinylibmt" "" nil)
(autoload 'ti::mt-pgp-signed-p                  "tinylibmt" "" nil)
(autoload 'ti::mt-pgp-public-key-p              "tinylibmt" "" nil)
(autoload 'ti::mt-pgp-remail-p                  "tinylibmt" "" nil)
(autoload 'ti::mt-pgp-comment-file-p            "tinylibmt" "" nil)
(autoload 'ti::mt-pgp-encrypted-p               "tinylibmt" "" nil)
(autoload 'ti::mt-pgp-normal-p                  "tinylibmt" "" nil)
(autoload 'ti::mt-pgp-headers-p                 "tinylibmt" "" nil)
(autoload 'ti::mt-pgp-re                        "tinylibmt" "" nil)
(autoload 'ti::mt-pgp-block-area-kill-forward   "tinylibmt" "" nil)
(autoload 'ti::mt-pgp-block-area                "tinylibmt" "" nil)
(autoload 'ti::mt-pgp-re-search                 "tinylibmt" "" nil)
(autoload 'ti::mt-pgp-exe-version-string        "tinylibmt" "" nil)
(autoload 'ti::mt-pgp-data-type                 "tinylibmt" "" nil)
(autoload 'ti::mt-pgp-trim-buffer               "tinylibmt" "" nil)
(autoload 'ti::mt-pgp-chop-region               "tinylibmt" "" nil)
(autoload 'ti::mt-pgp-header-kill-in-body       "tinylibmt" "" nil)
(autoload 'ti::mt-pgp-data-char-to-int          "tinylibmt" "" nil)
(autoload 'ti::mt-pgp-data-string-to-bin-string "tinylibmt" "" nil)
(autoload 'ti::mt-pgp-data-bin-string-to-int-list "tinylibmt" "" nil)
(autoload 'ti::mt-pgp-data-ascii-armor-convert  "tinylibmt" "" nil);;defsubst
(autoload 'ti::mt-pgp-data-study-ctb-byte       "tinylibmt" "" nil)
(autoload 'ti::mt-pgp-stream-study-1-ver        "tinylibmt" "" nil);;defsubst
(autoload 'ti::mt-pgp-stream-study-1-key-id     "tinylibmt" "" nil 'macro)
(autoload 'ti::mt-pgp-stream-study-1-time       "tinylibmt" "" nil)
(autoload 'ti::mt-pgp-stream-study-enc          "tinylibmt" "" nil)
(autoload 'ti::mt-pgp-stream-study-signed       "tinylibmt" "" nil)
(autoload 'ti::mt-pgp-stream-study-pring        "tinylibmt" "" nil)
(autoload 'ti::mt-pgp-stream-study              "tinylibmt" "" nil)
(autoload 'ti::mt-pgp-stream-forward-xpgp       "tinylibmt" "" nil)
(autoload 'ti::mt-pgp-stream-forward            "tinylibmt" "" nil)
(autoload 'ti::mt-pgp-stream-forward-and-study  "tinylibmt" "" t)
(autoload 'ti::mt-pgp-stream-forward-info       "tinylibmt" "" nil)
(autoload 'ti::mt-pgp-stream-data-elt           "tinylibmt" "" nil)
(autoload 'ti::mt-pgpk-id-lines-in-region       "tinylibmt" "" nil)
(autoload 'ti::mt-pgpk-id-0x-lines-in-region    "tinylibmt" "" nil)
(autoload 'ti::mt-pgpk-public-get-region        "tinylibmt" "" nil)
(autoload 'ti::mt-pgp-signature-remove          "tinylibmt" "" nil)
(autoload 'ti::mt-pgp-signature-normal-do-region "tinylibmt" "" nil 'macro)
(autoload 'ti::mt-get-article-buffer            "tinylibmt" "" nil);;defsubst
(autoload 'ti::mt-with-article-buffer           "tinylibmt" "" nil 'macro)
(autoload 'ti::mt-pgp-signature-normal-info     "tinylibmt" "" nil)
(autoload 'ti::mt-pgp-sig-header-info-v2xx      "tinylibmt" "" nil)
(autoload 'ti::mt-pgp-signature-header-info-v3xx "tinylibmt" "" nil)
(autoload 'ti::mt-pgp-signature-header-info     "tinylibmt" "" nil)
(autoload 'ti::mt-mime-parse-header             "tinylibmt" "" nil)
(autoload 'ti::mt-pgp-pkey-read                 "tinylibmt" "" nil)
(autoload 'ti::mt-pgpr-close                    "tinylibmt" "" nil)
(autoload 'ti::mt-pgpr-anonymize-headers        "tinylibmt" "" nil)
(autoload 'ti::mt-pgpr-reply-type               "tinylibmt" "" nil)
(autoload 'ti::mt-pgpr-block                    "tinylibmt" "" nil)
(autoload 'ti::mt-pgpr-reply-block              "tinylibmt" "" nil)
(autoload 'ti::mt-pgpr-parse-levien-list        "tinylibmt" "" nil)
(autoload 'ti::mt-email-domain                  "tinylibmt" "" nil)
(autoload 'ti::mt-email-domain-canonilize       "tinylibmt" "" nil)
(autoload 'ti::mt-email-find-region             "tinylibmt" "" nil)
(autoload 'ti::mt-email-from-string             "tinylibmt" "" nil)
(autoload 'ti::mt-t-parse-name                  "tinylibmt" "" nil)
(autoload 'ti::mt-parse-name                    "tinylibmt" "" nil)
(autoload 'ti::mt-parse-email                   "tinylibmt" "" nil)
(autoload 'ti::mt-parse-received                "tinylibmt" "" nil)
(autoload 'ti::mt-nslookup                      "tinylibmt" "" nil)
(autoload 'ti::mt-get-buffer                    "tinylibmt" "" nil)
(autoload 'ti::mt-signature-insert-break        "tinylibmt" "" nil)
(autoload 'ti::mt-yank                          "tinylibmt" "" nil)
(autoload 'ti::mt-trim-buffer                   "tinylibmt" "" nil)
(autoload 'ti::mt-field-space-count             "tinylibmt" "" nil);;defsubst
(autoload 'ti::mt-field-start                   "tinylibmt" "" nil)
(autoload 'ti::mt-next-field-start              "tinylibmt" "" nil)
(autoload 'ti::mt-current-field-name            "tinylibmt" "" nil)
(autoload 'ti::mt-field-email-send-p            "tinylibmt" "" nil)
(autoload 'ti::mt-kill-field-in-body            "tinylibmt" "" nil)
(autoload 'ti::mt-kill-field                    "tinylibmt" "" nil)
(autoload 'ti::mt-get-field-1                   "tinylibmt" "" nil)
(autoload 'ti::mt-get-field                     "tinylibmt" "" nil)
(autoload 'ti::mt-add-field                     "tinylibmt" "" nil)
(autoload 'ti::mt-add-to-field-string           "tinylibmt" "" nil)
(autoload 'ti::mt-kill-field-elt                "tinylibmt" "" nil)
(autoload 'ti::mt-kill-non-rfc-fields           "tinylibmt" "" nil)
(autoload 'ti::mt-get-all-email-addresses       "tinylibmt" "" nil)
(autoload 'ti::mt-set-recipients                "tinylibmt" "" nil)
(autoload 'ti::mt-news-buffer-p                 "tinylibmt" "" t)
(autoload 'ti::mt-article-regexp-read-line      "tinylibmt" "" nil)
(autoload 'ti::mt-news-reply-p                  "tinylibmt" "" nil)
(autoload 'ti::mt-anon-penet-p                  "tinylibmt" "" nil)
(autoload 'ti::mt-anon-penet-to-p               "tinylibmt" "" nil)
(autoload 'ti::mt-nymserver-email-convert       "tinylibmt" "" nil)
(autoload 'ti::mt-mime-tm-featurep-p            "tinylibmt" "" nil);;defsubst
(autoload 'ti::mt-mime-semi-featurep-p          "tinylibmt" "" nil);;defsubst
(autoload 'ti::mt-mime-feature-p                "tinylibmt" "" nil);;defsubst
(autoload 'ti::mt-mime-tm-edit-p                "tinylibmt" "" nil);;defsubst
(autoload 'ti::mt-mime-semi-edit-p              "tinylibmt" "" nil);;defsubst
(autoload 'ti::mt-mime-tm-edit-mode-macro       "tinylibmt" "" nil 'macro)
(autoload 'ti::mt-mime-semi-edit-mode-macro     "tinylibmt" "" nil 'macro)
(autoload 'ti::mt-mime-funcall-0-macro          "tinylibmt" "" nil 'macro)
(autoload 'ti::mt-mime-funcall-2-macro          "tinylibmt" "" nil 'macro)
(autoload 'ti::mt-mime-turn-on-mode             "tinylibmt" "" t)
(autoload 'ti::mt-mime-turn-off-mode            "tinylibmt" "" t)
(autoload 'ti::mt-mime-sign-region              "tinylibmt" "" t)
(autoload 'ti::mt-mime-encrypt-region           "tinylibmt" "" t)
(autoload 'ti::mt-mime-tm-split-macro           "tinylibmt" "" nil 'macro)
(autoload 'ti::mt-mime-maybe-p                  "tinylibmt" "" nil)
(autoload 'ti::mt-mime-p                        "tinylibmt" "" t)
(autoload 'ti::mt-mime-qp-decode                "tinylibmt" "" nil)
(autoload 'ti::mt-qp-mime-prepare               "tinylibmt" "" t)
(autoload 'ti::mt-plugged-p                     "tinylibmt" "" nil)
(autoload 'ti::mt-sendmail-reset-send-hooks     "tinylibmt" "" nil)
(autoload 'ti::mt-sendmail-pure-env-macro       "tinylibmt" "" nil 'macro)
(autoload 'ti::mt-sendmail-macro                "tinylibmt" "" nil 'macro)
(autoload 'ti::mt-abbrev-table                  "tinylibmt" "" nil)
(autoload 'ti::mt-abbrev-expand-mail-aliases    "tinylibmt" "" t)
(autoload 'ti::mt-abbrev-get-alist              "tinylibmt" "" nil)
(autoload 'ti::m-mail-abbrevs-email-list        "tinylibmt" "" nil)


;;}}}
;;{{{ code: Autoload 'y' lib -- system

;;; tinyliby.el

(autoload 'ti::y-autoload-function-list         "tinyliby" "" nil)
(autoload 'ti::y-autoload-function-file-list    "tinyliby" "" nil)
(autoload 'tinyliby-version                     "tinyliby" "" t)
(autoload 'tinyliby-submit-feedback             "tinyliby" "" t)
(autoload 'ti::y-package-where-is-source        "tinyliby" "" nil)
(autoload 'ti::y-load-cleanup                   "tinyliby" "" nil)
(autoload 'ti::y-lh-emacs-lisp-files            "tinyliby" "" nil)
(autoload 'ti::y-lh-where-exactly               "tinyliby" "" nil)
(autoload 'ti::y-describe-symbol-find-file      "tinyliby" "" nil)
(autoload 'ti::y-lh-where-1                     "tinyliby" "" nil)
(autoload 'ti::y-doc-where-is-source            "tinyliby" "" nil)
(autoload 'ti::y-lh-where-is-source             "tinyliby" "" nil)
(autoload 'ti::y-lh-get                         "tinyliby" "" nil)
(autoload 'ti::y-feature-kill                   "tinyliby" "" nil)
(autoload 'ti::y-unload-symbols                 "tinyliby" "" nil)
(autoload 'ti::y-unload                         "tinyliby" "" nil)
(autoload 'ti::y-unload-feature                 "tinyliby" "" t)
(autoload 'ti::y-feature-list-unload            "tinyliby" "" nil)
(autoload 'ti::y-map-hook-macro                 "tinyliby" "" nil 'macro)
(autoload 'ti::y-remove-from-hooks              "tinyliby" "" nil)
(autoload 'ti::y-match-in-hooks                 "tinyliby" "" t)
(autoload 'ti::y-get-symbols                    "tinyliby" "" nil)
(autoload 'ti::y-get-file-documentation         "tinyliby" "" t)
(autoload 'ti::y-describe-symbols-i-args        "tinyliby" "" nil)
(autoload 'ti::y-describe-symbols               "tinyliby" "" t)
(autoload 'ti::y-symbol-summary                 "tinyliby" "" t)

;;}}}
;;{{{ code: Autoload 'o' lib -- overlays

;;; tinylibo.el

(autoload 'tinylibo-version                     "tinylibo" "" t)
(autoload 'tinylibo-feedback                    "tinylibo" "" t)
(autoload 'ti::o-make                           "tinylibo" "" nil);;defsubst
(autoload 'ti::o-makec                          "tinylibo" "" nil);;defsubst
(autoload 'ti::o-make-match                     "tinylibo" "" nil)
(autoload 'ti::o-buffer-substring               "tinylibo" "" nil);;defsubst
(autoload 'ti::o-mouse-on-p                     "tinylibo" "" nil)
(autoload 'ti::o-get-mouse                      "tinylibo" "" nil)
(autoload 'ti::o-get-prop                       "tinylibo" "" nil)
(autoload 'ti::o-get-prop-val                   "tinylibo" "" nil)
(autoload 'ti::o-re-search                      "tinylibo" "" nil)
(autoload 'ti::o-re-search-move                 "tinylibo" "" nil)
(autoload 'ti::o-get-within-area                "tinylibo" "" nil)
(autoload 'ti::o-remove-region                  "tinylibo" "" t)

;;}}}
;;{{{ code: Autoload 't lib  -- Text property library

;;; tinylibt.el

(autoload 'ti::t-search-face-reset              "tinylibt" "" nil 'macro)
(autoload 'ti::t-search-face-set                "tinylibt" "" nil 'macro)
(autoload 'ti::t-face                           "tinylibt" "" nil 'macro)
(autoload 'ti::t-stack-clear                    "tinylibt" "" nil);;defsubst
(autoload 'ti::t-stack-length                   "tinylibt" "" nil);;defsubst
(autoload 'ti::t-stack-full-p                   "tinylibt" "" nil);;defsubst
(autoload 'ti::t-stack-p                        "tinylibt" "" nil);;defsubst
(autoload 'ti::t-save-data                      "tinylibt" "" nil)
(autoload 'ti::t-undo                           "tinylibt" "" t)
(autoload 'ti::t-clear-buffer-properties        "tinylibt" "" t)
(autoload 'ti::t-clear-region-properties        "tinylibt" "" t)
(autoload 'ti::t-get-mouse-property             "tinylibt" "" nil)
(autoload 'ti::t-match-level                    "tinylibt" "" nil)
(autoload 'ti::t-re-search                      "tinylibt" "" t)
(autoload 'ti::t-property-search-and-modify     "tinylibt" "" nil)
(autoload 'ti::t-read-regexp                    "tinylibt" "" nil)
(autoload 'ti::t-looking-at                     "tinylibt" "" t)
(autoload 'ti::t-buffer                         "tinylibt" "" t)
(autoload 'ti::t-re-search-forward              "tinylibt" "" t)
(autoload 'ti::t-re-search-backward             "tinylibt" "" t)
(autoload 'ti::t-mouse-mark-region              "tinylibt" "" t)
(autoload 'ti::t-mouse-unmark-region            "tinylibt" "" t)
(autoload 'ti::t-unmark-region                  "tinylibt" "" t)
(autoload 'ti::t-mark-region                    "tinylibt" "" t)

;;}}}

;;{{{ code: Autoload other 'tiny tools'

;;; tinylibck.el

(autoload 'ti::ck-maybe-activate	    "tinylibm")
(autoload 'ti::ck-advice-control	    "tinylibck")

;;; tinylibid.el

(autoload 'ti::id-info			    "tinylibid")
(autoload 'ti::id-cnv-txt2comment	    "tinylibid")

;;; tinylibmenu.el

(autoload 'ti::menu-menu		    "tinylibmenu")

;;; tinytab.el

(autoload 'tinytab-mode                         "tinytab" "" t)
(autoload 'turn-on-tinytab-mode                 "tinytab" "" t)
(autoload 'turn-off-tinytab-mode                "tinytab" "" t)

;;; tinyurl.el

(autoload 'turn-on-tinyurl-mode-maybe           "tinyurl" "" nil)
(autoload 'turn-on-tinyurl-mode-mail            "tinyurl" "" nil)
(autoload 'turn-on-tinyurl-mode-1               "tinyurl" "" t)
(autoload 'turn-off-tinyurl-mode-1              "tinyurl" "" t)
(autoload 'tinyurl-mode-1                       "tinyurl" "" t)
(autoload 'turn-on-tinyurl-mode                 "tinyurl" "" t)
(autoload 'turn-off-tinyurl-mode                "tinyurl" "" t)
(autoload 'tinyurl-mode                         "tinyurl" "" t)
(autoload 'tinyurl-mode-action                  "tinyurl" "" nil)
(autoload 'tinyurl-install                      "tinyurl" "" t)
(autoload 'tinyurl-mark-line                    "tinyurl")
(autoload 'tinyurl-overlay-get                  "tinyurl")
(autoload 'tinyurl-dispatcher                   "tinyurl")
(autoload 'tinyurl-agent-funcall                "tinyurl")


;;}}}

;;{{{ code: autoload other

(autoload 'byte-compile			    "bytecomp")
(autoload 'occur			    "replace" "" t)

(autoload 'folding-open-buffer              "folding" "" t)


(autoload 'browse-url                       "browse-url")
(autoload 'browse-url-w3                    "browse-url")
(autoload 'browse-url-netscape              "browse-url")
(autoload 'browse-url-lynx-emacs            "browse-url")

(autoload 'display-time                     "time")
(autoload 'shuffle-vector                   "cookie1")
(autoload 'name-last-kbd-macro              "macros")
(autoload 'mail-extract-address-components  "mail-extr")


;;  This is special case. if there is Igrep package available, it
;;  will define autoload to "grep" and we must reflect the
;;  situation accordingly. See `igrep-insinuate'

(unless (fboundp 'grep)
  (if (locate-library "igrep")
      (autoload 'grep "igrep" "" t)
    (autoload 'grep "grep" "" t)))

(autoload 'compile           "compile" "" t)
(autoload 'compile-internal  "compile")


;; Emacs 20.6 sort.el

(autoload 'sort-subr                            "sort" "" nil)
(autoload 'sort-build-lists                     "sort" "" nil)
(autoload 'sort-reorder-buffer                  "sort" "" nil)
(autoload 'sort-lines                           "sort" "" t)
(autoload 'sort-paragraphs                      "sort" "" t)
(autoload 'sort-pages                           "sort" "" t)
(autoload 'sort-numeric-fields                  "sort" "" t)
(autoload 'sort-fields                          "sort" "" t)
(autoload 'sort-fields-1                        "sort" "" nil)
(autoload 'sort-skip-fields                     "sort" "" nil)
(autoload 'sort-regexp-fields-next-record       "sort" "" nil)
(autoload 'sort-regexp-fields                   "sort" "" t)
(autoload 'sort-columns                         "sort" "" t)
(autoload 'reverse-region                       "sort" "" t)

;; tabify.el

(autoload 'tabify                           "tabify" "" t)
(autoload 'untabify                         "tabify" "" t)

;; pp.el

(autoload 'pp-to-string                         "pp" "" nil)
(autoload 'pp                                   "pp" "" nil)
(autoload 'pp-eval-expression                   "pp" "" t)
(autoload 'pp-eval-last-sexp                    "pp" "" t)

;; thingatpt.el

(autoload 'forward-thing			"thingatpt" "" nil)
(autoload 'bounds-of-thing-at-point             "thingatpt" "" nil)
(autoload 'thing-at-point                       "thingatpt" "" nil)
(autoload 'beginning-of-thing                   "thingatpt" "" nil)
(autoload 'end-of-thing                         "thingatpt" "" nil)
(autoload 'in-string-p                          "thingatpt" "" nil)
(autoload 'end-of-sexp                          "thingatpt" "" nil)
(autoload 'forward-whitespace                   "thingatpt" "" t)
(autoload 'forward-symbol                       "thingatpt" "" t)
(autoload 'forward-same-syntax                  "thingatpt" "" t)
(autoload 'word-at-point                        "thingatpt" "" nil)
(autoload 'sentence-at-point                    "thingatpt" "" nil)
(autoload 'read-from-whole-string               "thingatpt" "" nil)
(autoload 'form-at-point                        "thingatpt" "" nil)
(autoload 'sexp-at-point                        "thingatpt" "" nil)
(autoload 'symbol-at-point                      "thingatpt" "" nil)
(autoload 'number-at-point                      "thingatpt" "" nil)
(autoload 'list-at-point                        "thingatpt" "" nil)


;; rect.el

(autoload 'operate-on-rectangle                 "rect" "" nil)
(autoload 'delete-rectangle-line                "rect" "" nil)
(autoload 'delete-extract-rectangle-line        "rect" "" nil)
(autoload 'extract-rectangle-line               "rect" "" nil)
(autoload 'spaces-string                        "rect" "" nil)
(autoload 'delete-rectangle                     "rect" "" t)
(autoload 'delete-extract-rectangle             "rect" "" nil)
(autoload 'extract-rectangle                    "rect" "" nil)
(autoload 'kill-rectangle                       "rect" "" t)
(autoload 'yank-rectangle                       "rect" "" t)
(autoload 'insert-rectangle                     "rect" "" nil)
(autoload 'open-rectangle                       "rect" "" t)
(autoload 'open-rectangle-line                  "rect" "" nil)
(autoload 'string-rectangle                     "rect" "" t)
(autoload 'string-rectangle-line                "rect" "" nil)
(autoload 'clear-rectangle                      "rect" "" t)
(autoload 'clear-rectangle-line                 "rect" "" nil)

;; jka-compr.el

(autoload 'jka-compr-info-regexp                "jka-compr"   "" nil)
(autoload 'jka-compr-info-compress-message      "jka-compr"   "" nil)
(autoload 'jka-compr-info-compress-program      "jka-compr"   "" nil)
(autoload 'jka-compr-info-compress-args         "jka-compr"   "" nil)
(autoload 'jka-compr-info-uncompress-message    "jka-compr"   "" nil)
(autoload 'jka-compr-info-uncompress-program    "jka-compr"   "" nil)
(autoload 'jka-compr-info-uncompress-args       "jka-compr"   "" nil)
(autoload 'jka-compr-info-can-append            "jka-compr"   "" nil)
(autoload 'jka-compr-info-strip-extension       "jka-compr"   "" nil)
(autoload 'jka-compr-get-compression-info       "jka-compr"   "" nil)
(autoload 'jka-compr-error                      "jka-compr"   "" nil)
(autoload 'jka-compr-partial-uncompress         "jka-compr"   "" nil)
(autoload 'jka-compr-call-process               "jka-compr"   "" nil)
(autoload 'jka-compr-make-temp-name             "jka-compr"   "" nil)
(autoload 'jka-compr-delete-temp-file           "jka-compr"   "" nil)
(autoload 'jka-compr-write-region               "jka-compr"   "" nil)
(autoload 'jka-compr-insert-file-contents       "jka-compr"   "" nil)
(autoload 'jka-compr-file-local-copy            "jka-compr"   "" nil)
(autoload 'jka-compr-load                       "jka-compr"   "" nil)
(autoload 'jka-compr-byte-compiler-base-file-name "jka-compr" "" nil)
(autoload 'jka-compr-handler                    "jka-compr"   "" nil)
(autoload 'jka-compr-run-real-handler           "jka-compr"   "" nil)
(autoload 'toggle-auto-compression              "jka-compr"   "" t)
(autoload 'jka-compr-build-file-regexp          "jka-compr"   "" nil)
(autoload 'jka-compr-install                    "jka-compr"   "" nil)
(autoload 'jka-compr-uninstall                  "jka-compr"   "" nil)
(autoload 'jka-compr-installed-p                "jka-compr"   "" nil)

;; Advice.el (partial autolaods only)

(autoload 'ad-disable-advice			"advice")
(autoload 'ad-enable-advice			"advice")
(autoload 'ad-activate				"advice")

;; finder.el


(autoload 'finder-compile-keywords              "finder" "" nil)
(autoload 'finder-compile-keywords-make-dist    "finder" "" nil)
(autoload 'finder-insert-at-column              "finder" "" nil)
(autoload 'finder-mouse-face-on-line            "finder" "" nil)
(autoload 'finder-list-keywords                 "finder" "" t)
(autoload 'finder-list-matches                  "finder" "" nil)
(autoload 'finder-find-library                  "finder" "" nil)
(autoload 'finder-commentary                    "finder" "" t)
(autoload 'finder-current-item                  "finder" "" nil)
(autoload 'finder-select                        "finder" "" t)
(autoload 'finder-mouse-select                  "finder" "" t)
(autoload 'finder-by-keyword                    "finder" "" t)
(autoload 'finder-mode                          "finder" "" t)
(autoload 'finder-summary                       "finder" "" t)
(autoload 'finder-exit                          "finder" "" t)

;; lisp-mnt.el

(autoload 'lm-get-header-re                     "lisp-mnt" "" nil);;defsubst
(autoload 'lm-get-package-name                  "lisp-mnt" "" nil);;defsubst
(autoload 'lm-section-mark                      "lisp-mnt" "" nil)
(autoload 'lm-code-mark                         "lisp-mnt" "" nil);;defsubst
(autoload 'lm-commentary-mark                   "lisp-mnt" "" nil);;defsubst
(autoload 'lm-history-mark                      "lisp-mnt" "" nil);;defsubst
(autoload 'lm-header                            "lisp-mnt" "" nil)
(autoload 'lm-header-multiline                  "lisp-mnt" "" nil)
(autoload 'lm-summary                           "lisp-mnt" "" nil)
(autoload 'lm-crack-address                     "lisp-mnt" "" nil)
(autoload 'lm-authors                           "lisp-mnt" "" nil)
(autoload 'lm-maintainer                        "lisp-mnt" "" nil)
(autoload 'lm-creation-date                     "lisp-mnt" "" nil)
(autoload 'lm-last-modified-date                "lisp-mnt" "" nil)
(autoload 'lm-version                           "lisp-mnt" "" nil)
(autoload 'lm-keywords                          "lisp-mnt" "" nil)
(autoload 'lm-adapted-by                        "lisp-mnt" "" nil)
(autoload 'lm-commentary                        "lisp-mnt" "" nil)
(autoload 'lm-insert-at-column                  "lisp-mnt" "" nil)
(autoload 'lm-verify                            "lisp-mnt" "" t)
(autoload 'lm-synopsis                          "lisp-mnt" "" t)
(autoload 'lm-report-bug                        "lisp-mnt" "" t)


;; reporter.el

(autoload 'reporter-update-status               "reporter" "" nil)
(autoload 'reporter-beautify-list               "reporter" "" nil)
(autoload 'reporter-lisp-indent                 "reporter" "" nil)
(autoload 'reporter-dump-variable               "reporter" "" nil)
(autoload 'reporter-dump-state                  "reporter" "" nil)
(autoload 'reporter-calculate-separator         "reporter" "" nil)
(autoload 'reporter-mail                        "reporter" "" nil)
(autoload 'reporter-compose-outgoing            "reporter" "" nil)
(autoload 'reporter-submit-bug-report           "reporter" "" nil)
(autoload 'reporter-bug-hook                    "reporter" "" nil)
(autoload 'define-mail-user-agent               "reporter" "" nil)


;; vc-hook.el from Emacs 20.7


(autoload 'vc-mistrust-permissions              "vc-hooks" "" nil)
(autoload 'vc-error-occurred                    "vc-hooks" "" nil 'macro)
(autoload 'vc-file-setprop                      "vc-hooks" "" nil)
(autoload 'vc-file-getprop                      "vc-hooks" "" nil)
(autoload 'vc-file-clearprops                   "vc-hooks" "" nil)
(autoload 'vc-match-substring                   "vc-hooks" "" nil)
(autoload 'vc-lock-file                         "vc-hooks" "" nil)
(autoload 'vc-parse-buffer                      "vc-hooks" "" nil)
(autoload 'vc-insert-file                       "vc-hooks" "" nil)
(autoload 'vc-parse-locks                       "vc-hooks" "" nil)
(autoload 'vc-simple-command                    "vc-hooks" "" nil)
(autoload 'vc-parse-cvs-status                  "vc-hooks" "" nil)
(autoload 'vc-fetch-master-properties           "vc-hooks" "" nil)
(autoload 'vc-consult-rcs-headers               "vc-hooks" "" nil)
(autoload 'vc-backend-subdirectory-name         "vc-hooks" "" nil)
(autoload 'vc-name                              "vc-hooks" "" nil)
(autoload 'vc-backend                           "vc-hooks" "" nil)
(autoload 'vc-checkout-model                    "vc-hooks" "" nil)
(autoload 'vc-cvs-status                        "vc-hooks" "" nil)
(autoload 'vc-master-locks                      "vc-hooks" "" nil)
(autoload 'vc-master-locking-user               "vc-hooks" "" nil)
(autoload 'vc-lock-from-permissions             "vc-hooks" "" nil)
(autoload 'vc-user-login-name                   "vc-hooks" "" nil)
(autoload 'vc-file-owner                        "vc-hooks" "" nil)
(autoload 'vc-rcs-lock-from-diff                "vc-hooks" "" nil)
(autoload 'vc-locking-user                      "vc-hooks" "" nil)
(autoload 'vc-latest-version                    "vc-hooks" "" nil)
(autoload 'vc-your-latest-version               "vc-hooks" "" nil)
(autoload 'vc-master-workfile-version           "vc-hooks" "" nil)
(autoload 'vc-fetch-properties                  "vc-hooks" "" nil)
(autoload 'vc-workfile-version                  "vc-hooks" "" nil)
(autoload 'vc-registered                        "vc-hooks" "" nil)
(autoload 'vc-sccs-project-dir                  "vc-hooks" "" nil)
(autoload 'vc-search-sccs-project-dir           "vc-hooks" "" nil)
(autoload 'vc-find-cvs-master                   "vc-hooks" "" nil)
(autoload 'vc-buffer-backend                    "vc-hooks" "" nil)
(autoload 'vc-toggle-read-only                  "vc-hooks" "" t)
(autoload 'vc-after-save                        "vc-hooks" "" nil)
(autoload 'vc-mode-line                         "vc-hooks" "" t)
(autoload 'vc-status                            "vc-hooks" "" nil)
(autoload 'vc-follow-link                       "vc-hooks" "" nil)
(autoload 'vc-find-file-hook                    "vc-hooks" "" nil)
(autoload 'vc-file-not-found-hook               "vc-hooks" "" nil)
(autoload 'vc-kill-buffer-hook                  "vc-hooks" "" nil)

(autoload 'vc-backend-dispatch                  "vc" "" nil 'macro)
(autoload 'vc-backend-release                   "vc" "" nil)
(autoload 'vc-release-greater-or-equal          "vc" "" nil)
(autoload 'vc-backend-release-p                 "vc" "" nil)
(autoload 'vc-trunk-p                           "vc" "" nil)
(autoload 'vc-branch-p                          "vc" "" nil)
(autoload 'vc-branch-part                       "vc" "" nil)
(autoload 'vc-minor-part                        "vc" "" nil)
(autoload 'vc-previous-version                  "vc" "" nil)
(autoload 'vc-clear-context                     "vc" "" t)
(autoload 'vc-file-clear-masterprops            "vc" "" nil)
(autoload 'vc-head-version                      "vc" "" nil)
(autoload 'vc-latest-on-branch-p                "vc" "" nil)
(autoload 'with-vc-file                         "vc" "" nil 'macro)
(autoload 'edit-vc-file                         "vc" "" nil 'macro)
(autoload 'vc-ensure-vc-buffer                  "vc" "" nil)
(autoload 'vc-find-binary                       "vc" "" nil)
(autoload 'vc-do-command                        "vc" "" nil)
(autoload 'vc-position-context                  "vc" "" nil)
(autoload 'vc-find-position-by-context          "vc" "" nil)
(autoload 'vc-context-matches-p                 "vc" "" nil)
(autoload 'vc-buffer-context                    "vc" "" nil)
(autoload 'vc-restore-buffer-context            "vc" "" nil)
(autoload 'vc-revert-buffer1                    "vc" "" t)
(autoload 'vc-buffer-sync                       "vc" "" nil)
(autoload 'vc-workfile-unchanged-p              "vc" "" nil)
(autoload 'vc-next-action-on-file               "vc" "" nil)
(autoload 'vc-next-action-dired                 "vc" "" nil)
(autoload 'vc-next-action                       "vc" "" t)
(autoload 'vc-checkout-writable-buffer          "vc" "" nil)
(autoload 'vc-register                          "vc" "" t)
(autoload 'vc-resynch-window                    "vc" "" nil)
(autoload 'vc-resynch-buffer                    "vc" "" nil)
(autoload 'vc-start-entry                       "vc" "" nil)
(autoload 'vc-admin                             "vc" "" nil)
(autoload 'vc-checkout                          "vc" "" nil)
(autoload 'vc-steal-lock                        "vc" "" nil)
(autoload 'vc-finish-steal                      "vc" "" nil)
(autoload 'vc-checkin                           "vc" "" nil)
(autoload 'vc-comment-to-change-log             "vc" "" t)
(autoload 'vc-finish-logentry                   "vc" "" t)
(autoload 'vc-previous-comment                  "vc" "" t)
(autoload 'vc-next-comment                      "vc" "" t)
(autoload 'vc-comment-search-reverse            "vc" "" t)
(autoload 'vc-comment-search-forward            "vc" "" t)
(autoload 'vc-diff                              "vc" "" t)
(autoload 'vc-version-diff                      "vc" "" t)
(autoload 'vc-version-other-window              "vc" "" t)
(autoload 'vc-insert-headers                    "vc" "" t)
(autoload 'vc-clear-headers                     "vc" "" nil)
(autoload 'vc-merge                             "vc" "" t)
(autoload 'vc-resolve-conflicts                 "vc" "" t)
(autoload 'vc-dired-toggle-terse-mode           "vc" "" t)
(autoload 'vc-dired-mark-locked                 "vc" "" t)
(autoload 'vc-fetch-cvs-status                  "vc" "" nil)
(autoload 'vc-dired-state-info                  "vc" "" nil)
(autoload 'vc-dired-reformat-line               "vc" "" nil)
(autoload 'vc-dired-hook                        "vc" "" nil)
(autoload 'vc-dired-purge                       "vc" "" nil)
(autoload 'vc-directory                         "vc" "" t)
(autoload 'vc-add-triple                        "vc" "" nil)
(autoload 'vc-record-rename                     "vc" "" nil)
(autoload 'vc-lookup-triple                     "vc" "" nil)
(autoload 'vc-snapshot-precondition             "vc" "" nil)
(autoload 'vc-create-snapshot                   "vc" "" t)
(autoload 'vc-retrieve-snapshot                 "vc" "" t)
(autoload 'vc-print-log                         "vc" "" t)
(autoload 'vc-revert-buffer                     "vc" "" t)
(autoload 'vc-cancel-version                    "vc" "" t)
(autoload 'vc-rename-file                       "vc" "" t)
(autoload 'vc-update-change-log                 "vc" "" t)
(autoload 'vc-annotate-mode-variables           "vc" "" nil)
(autoload 'vc-annotate-mode                     "vc" "" t)
(autoload 'vc-annotate-display-default          "vc" "" t)
(autoload 'vc-annotate-add-menu                 "vc" "" t)
(autoload 'vc-annotate                          "vc" "" t)
(autoload 'vc-annotate-car-last-cons            "vc" "" nil)
(autoload 'vc-annotate-time-span                "vc" "" nil)
(autoload 'vc-annotate-compcar                  "vc" "" nil)
(autoload 'vc-annotate-display                  "vc" "" nil)
(autoload 'vc-backend-admin                     "vc" "" nil)
(autoload 'vc-backend-checkout                  "vc" "" nil)
(autoload 'vc-backend-logentry-check            "vc" "" nil)
(autoload 'vc-backend-checkin                   "vc" "" nil)
(autoload 'vc-backend-revert                    "vc" "" nil)
(autoload 'vc-backend-steal                     "vc" "" nil)
(autoload 'vc-backend-uncheck                   "vc" "" nil)
(autoload 'vc-backend-print-log                 "vc" "" nil)
(autoload 'vc-backend-assign-name               "vc" "" nil)
(autoload 'vc-backend-diff                      "vc" "" nil)
(autoload 'vc-backend-merge-news                "vc" "" nil)
(autoload 'vc-backend-merge                     "vc" "" nil)
(autoload 'vc-check-headers                     "vc" "" t)
(autoload 'vc-log-mode                          "vc" "" t)
(autoload 'vc-file-tree-walk                    "vc" "" nil)
(autoload 'vc-file-tree-walk-internal           "vc" "" nil)


;; font-lock from Emacs 20.6

(autoload 'font-lock-mode                       "font-lock" "" t)
(autoload 'turn-on-font-lock                    "font-lock" "" nil)

;; Not necessarily in XEmacs font-lock.el
;; (autoload 'global-font-lock-mode                "font-lock" "" t)
;; (autoload 'font-lock-add-keywords               "font-lock" "" nil)

(autoload 'font-lock-change-major-mode          "font-lock" "" nil)
(autoload 'turn-on-font-lock-if-enabled         "font-lock" "" nil)
(autoload 'font-lock-turn-on-thing-lock         "font-lock" "" nil)
(autoload 'font-lock-turn-off-thing-lock        "font-lock" "" nil)
(autoload 'font-lock-after-fontify-buffer       "font-lock" "" nil)
(autoload 'font-lock-after-unfontify-buffer     "font-lock" "" nil)
(autoload 'font-lock-fontify-buffer             "font-lock" "" t)
(autoload 'font-lock-unfontify-buffer           "font-lock" "" nil)
(autoload 'font-lock-fontify-region             "font-lock" "" nil)
(autoload 'font-lock-unfontify-region           "font-lock" "" nil)
(autoload 'font-lock-default-fontify-buffer     "font-lock" "" nil)
(autoload 'font-lock-default-unfontify-buffer   "font-lock" "" nil)
(autoload 'font-lock-default-fontify-region     "font-lock" "" nil)
(autoload 'font-lock-default-unfontify-region   "font-lock" "" nil)
(autoload 'font-lock-after-change-function      "font-lock" "" nil)
(autoload 'font-lock-fontify-block              "font-lock" "" t)
(autoload 'font-lock-prepend-text-property      "font-lock" "" nil)
(autoload 'font-lock-append-text-property       "font-lock" "" nil)
(autoload 'font-lock-fillin-text-property       "font-lock" "" nil)
(autoload 'font-lock-apply-syntactic-highlight  "font-lock" "" nil)
(autoload 'font-lock-fontify-syntactic-anchored-keywords "font-lock" "" nil)
(autoload 'font-lock-fontify-syntactic-keywords-region "font-lock" "" nil)
(autoload 'font-lock-fontify-syntactically-region "font-lock" "" nil)
(autoload 'font-lock-apply-highlight            "font-lock" "" nil);;defsubst
(autoload 'font-lock-fontify-anchored-keywords  "font-lock" "" nil);;defsubst
(autoload 'font-lock-fontify-keywords-region    "font-lock" "" nil)
(autoload 'font-lock-compile-keywords           "font-lock" "" nil)
(autoload 'font-lock-compile-keyword            "font-lock" "" nil)
(autoload 'font-lock-eval-keywords              "font-lock" "" nil)
(autoload 'font-lock-value-in-major-mode        "font-lock" "" nil)
(autoload 'font-lock-choose-keywords            "font-lock" "" nil)
(autoload 'font-lock-set-defaults               "font-lock" "" nil)
(autoload 'font-lock-unset-defaults             "font-lock" "" nil)
(autoload 'font-lock-match-c-style-declaration-item-and-skip-to-next "font-lock" "" nil)
(autoload 'font-lock-match-c++-style-declaration-item-and-skip-to-next "font-lock" "" nil)


;; imenu.el 20.6, Not in XEmacs.

(when (locate-library "imenu")
  (autoload 'imenu--subalist-p                    "imenu" "" nil)
  ;; ** The compiler ignores `autoload' except at top level.
  ;; (autoload 'imenu-progress-message               "imenu" "" nil 'macro)
  (autoload 'imenu-example--name-and-position     "imenu" "" nil)
  (autoload 'imenu-example--lisp-extract-index-name "imenu" "" nil)
  (autoload 'imenu-example--create-lisp-index     "imenu" "" nil)
  (autoload 'imenu-example--create-c-index        "imenu" "" nil)
  (autoload 'imenu--sort-by-name                  "imenu" "" nil)
  (autoload 'imenu--sort-by-position              "imenu" "" nil)
  (autoload 'imenu--relative-position             "imenu" "" nil)
  (autoload 'imenu--split                         "imenu" "" nil)
  (autoload 'imenu--split-menu                    "imenu" "" nil)
  (autoload 'imenu--split-submenus                "imenu" "" nil)
  (autoload 'imenu--truncate-items                "imenu" "" nil)
  (autoload 'imenu--make-index-alist              "imenu" "" nil)
  (autoload 'imenu--cleanup                       "imenu" "" nil)
  (autoload 'imenu--create-keymap-2               "imenu" "" t)
  (autoload 'imenu--create-keymap-1               "imenu" "" nil)
  (autoload 'imenu--in-alist                      "imenu" "" nil)
  (autoload 'imenu-default-create-index-function  "imenu" "" nil)
  (autoload 'imenu--replace-spaces                "imenu" "" nil)
  (autoload 'imenu--generic-function              "imenu" "" nil)
  (autoload 'imenu--completion-buffer             "imenu" "" nil)
  (autoload 'imenu--mouse-menu                    "imenu" "" nil)
  (autoload 'imenu-choose-buffer-index            "imenu" "" nil)
  (autoload 'imenu-add-to-menubar                 "imenu" "" t)
  (autoload 'imenu-add-menubar-index              "imenu" "" t)
  (autoload 'imenu-update-menubar                 "imenu" "" nil)
  (autoload 'imenu--menubar-select                "imenu" "" nil)
  (autoload 'imenu-default-goto-function          "imenu" "" nil)
  (autoload 'imenu                                "imenu" "" t))

;;}}}
;;{{{ code: SEMI libraries: APEL::alist.el

(if (locate-library "alist")
    (progn
      (autoload 'put-alist                            "alist" "" nil)
      (autoload 'del-alist                            "alist" "" nil)
      (autoload 'set-alist                            "alist" "" nil)
      (autoload 'remove-alist                         "alist" "" nil)
      (autoload 'modify-alist                         "alist" "" nil)
      (autoload 'set-modified-alist                   "alist" "" nil)))

;; set-modified-alist                   sym modifier
;; modify-alist                         modifier default
;; remove-alist                         symbol item
;; set-alist                            symbol item value
;; del-alist                            item alist
;; put-alist                            item value alist

;;}}}

;;{{{ code: Xemacs emulation.

(when (locate-library "timer")
  ;; XEmacs provides xemacs-packages\lisp\fsf-compat\timer.el
  ;;
  ;; These functions are the "common denominator" of XEmacs 21.2
  ;; And Emacs 20.4
  ;;
  (autoload 'cancel-function-timers               "timer" "" t)
  (autoload 'cancel-timer                         "timer" "" nil)
  (autoload 'run-at-time                          "timer" "" t)
  (autoload 'run-with-idle-timer                  "timer" "" t)
  (autoload 'run-with-timer                       "timer" "" t)
  (autoload 'timer-activate                       "timer" "" nil)
  (autoload 'timer-activate-when-idle             "timer" "" nil)
  (autoload 'timer-duration                       "timer" "" nil)
  (autoload 'timer-inc-time                       "timer" "" nil)
  (autoload 'timer-relative-time                  "timer" "" nil)
  (autoload 'timer-set-function                   "timer" "" nil)
  (autoload 'timer-set-idle-time                  "timer" "" nil)
  (autoload 'timer-set-time                       "timer" "" nil)
  (autoload 'timer-set-time-with-usecs            "timer" "" nil)
  (autoload 'with-timeout-handler                 "timer" "" nil)
  (autoload 'y-or-n-p-with-timeout                "timer" "" nil)
  )

(when (xemacs-p)

  (autoload 'set-cursor-color			"tinylibxe" "" t)
  (autoload 'set-foreground-color		"tinylibxe" "" t)
  (autoload 'set-background-color		"tinylibxe" "" t)
  (autoload 'transient-mark-mode		"tinylibxe" "" t)

  (unless (fboundp 'run-at-time)
    (autoload 'run-at-time			"tinylibxe"))

  (unless (fboundp 'cancel-timer)
    (autoload 'cancel-timer 			"tinylibxe"))

  (autoload 'posn-window			"tinylibxe")
  (autoload 'posn-point				"tinylibxe")
  (autoload 'posn-timestamp			"tinylibxe")
  (autoload 'window-edges			"tinylibxe")

  (autoload 'event-start			"tinylibxe")
  (autoload 'event-x				"tinylibxe")
  (autoload 'event-y				"tinylibxe")
  (autoload 'posn-x-y				"tinylibxe")

  (autoload 'frame-parameters			"tinylibxe")


  (eval-when-compile
    ;;  emulation in xe library
    (put 'frame-parameters 'byte-obsolete-variable nil))


  (autoload 'dired-unmark			"tinylibxe")
  (autoload 'dired-mark				"tinylibxe")
  (autoload 'dired-get-marked-files		"tinylibxe")
  (autoload 'dired-map-over-marks		"tinylibxe")

  )

;;}}}
;;{{{ code: XEmacs and Emacs autoloads

(defvar   view-mode nil)

(cond
 ;; XEmacs 21.x changed package name
 ((locate-library "view-less")
  (autoload 'view-exit  "view-less" "" t)
  (autoload 'view-mode  "view-less" "" t))
 (t
  (autoload 'view-exit "view" "" t)
  (autoload 'view-mode "view" "" t)))

(when (locate-library "overlay")   ;; Xemacs has emulation lib
  ;; overlay.el
  ;; xemacs-packages/lisp/fsf-compat/overlay.el
  (autoload 'overlayp                             "overlay" "" nil)
  (autoload 'make-overlay                         "overlay" "" nil)
  (autoload 'move-overlay                         "overlay" "" nil)
  (autoload 'delete-overlay                       "overlay" "" nil)
  (autoload 'overlay-start                        "overlay" "" nil)
  (autoload 'overlay-end                          "overlay" "" nil)
  (autoload 'overlay-buffer                       "overlay" "" nil)
  (autoload 'overlay-properties                   "overlay" "" nil)
  (autoload 'overlays-at                          "overlay" "" nil)
  (autoload 'overlays-in                          "overlay" "" nil)
  (autoload 'next-overlay-change                  "overlay" "" nil)
  (autoload 'previous-overlay-change              "overlay" "" nil)
  (autoload 'overlay-lists                        "overlay" "" nil)
  (autoload 'overlay-recenter                     "overlay" "" nil)
  (autoload 'overlay-get                          "overlay" "" nil)
  (autoload 'overlay-put                          "overlay" "" nil))

;;}}}


) ;; eval-and-compile

(provide 'tinyliba)

;;; tinyliba.el ends here
