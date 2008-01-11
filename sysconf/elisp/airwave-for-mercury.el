(provide 'airwave-for-mercury)
(require 'airwave-func)
(require 'cperl-mode)

(defun airwave-get-test-object (class)
  (interactive (cons (read-from-minibuffer
                           "Class: "
                           "Mercury::DB::"
                           perlclass-minibuffer-map) '()))
  (insert "destroyer(" class "->get_test_object(\n\n));")
  (save-excursion
    (let ((end (point)))
      (backward-line 2)
      (indent-region (point) end nil)
      )
    )
  (backward-line 1)
  (cperl-indent-line)
  )

(defun airwave-newly-failing-tests-list ()
  (interactive)
  (airwave-update-buffers)
  (find-file "/tmp/newly_failing_tests"))

(defun airwave-expand-security-mode ()
  (interactive)
  (save-excursion
    (skip-chars-backward "0123456789")
    (let ((replacement
           (car (filter-list
                 (mapcar '(lambda (x)
                            (let ((numeric (number-to-string (car x))) (string (cdr x)))
                              (if (looking-at numeric) (cons (length numeric) string) nil)))
                         '((13 . "SECURITY_MODE_STATIC_CKIP")
                           (12 . "SECURITY_MODE_WPA2_PSK")
                           (11 . "SECURITY_MODE_WPA2")
                           (10 . "SECURITY_MODE_WPA_PSK")
                           (9 . "SECURITY_MODE_WPA")
                           (8 . "SECURITY_MODE_LEAP_WEP")
                           (7 . "SECURITY_MODE_8021X_LEAP")
                           (6 . "SECURITY_MODE_8021X_WEP")
                           (5 . "SECURITY_MODE_REQUIRE_LEAP")
                           (4 . "SECURITY_MODE_REQUIRE_8021X")
                           (3 . "SECURITY_MODE_REQUIRE_WEP")
                           (2 . "SECURITY_MODE_OPTIONAL_WEP")
                           (1 . "SECURITY_MODE_NO_ENCRYPTION")))
                 '(lambda (x) x)))))
      (if replacement
          (progn
            (delete-char (car replacement))
            (insert (cdr replacement))
            (when (point-on-hash-keyp) (insert "()")))))))

(defun airwave-expand-oid-to-snmp-config-map-entry ()
  "Expand an OID's symbolic name into an snmp_config_map entry"
  (interactive)
  (let ((oid (current-oid)) (start-pos (point)))
    (skip-chars-forward "a-zA-Z0-9")
    (insert
     " => {\n oid => \'"
     (car (airwave-get-oid-translation oid))
     "\',\n type => '"
     (snmp-config-map-type (airwave-type-from-oid-info (airwave-get-oid-info oid)))
     "',\n},")
    (cperl-indent-region start-pos (point))))

(defun airwave-show-module-isa (module &optional sub)
   "Show inheritance tree of module under point"
   (interactive (current-method-call))
   (shell-command 
    (if sub (format "isa %s %s" module sub) (format "isa %s" module)) 
    "*isa*"))

(defun dbap (apid)
  "Run the debugger on an AP test object"
  (interactive (list (read-string "ap_id: ")))
  (airwave-save-and-save-some-buffers)
  (airwave-make-dbap-file apid)
  (perldb "perl -d -MMercury::AP -MData::Dumper /tmp/dbap.pl"))

(defun airwave-uimake () 
  "Pushes changes made to the templates out so Apache can see them."
  (interactive)
  (airwave-save-and-save-some-buffers)
  (call-process "sh" nil "*uimake*" nil "-c" 
                (format "cd %s; make ui" (home "/svn/mercury")))
  (message "uimake output is in *uimake*")) ; undesirable?

(defun airwave-make-libperl-and-restart-apache ()
  "Execute airwave-make-libperl and airwave-restart-apache."
  (interactive)
  (airwave-make-libperl)
  (airwave-restart-apache))

(defun airwave-make-libperl () 
  "Pushes changes made to the perl modules out so Apache can see them."
  (interactive)
  (airwave-save-and-save-some-buffers)
  (message "making libperl, please wait...")
  (call-process "sh" nil "*libperl*" nil "-c" 
                (format "cd %s; time make libperl; telinit q;" (home "/svn/mercury")))
  (message "libperl output is in *libperl*"))

(defun airwave-update-db ()
  "ML and updatedb"
  (interactive)
  (airwave-make-frontendperl)
  (message "running update_db.pl...")
  (call-process "sh" nil "*update_db*" nil "-c"
                (format "cd %s; update_db.pl;" (home "/svn/mercury")))
  (message "update_db.pl output is in *update_db*"))

(defun airwave-make-frontendperl ()
  "ml without the restart deamons"
  (interactive)
  (airwave-save-and-save-some-buffers)
  (message "making frontendperl, please wait...")
  (call-process "sh" nil "*libperl*" nil "-c"
                (format "cd %s; time make frontendperl;" (home "/svn/mercury")))
  (message "frontendperl output is in *libperl*"))

(defun airwave-restart-apache () 
  "Restart apache."
  (interactive)
  (message "restarting apache, please wait...")
  (call-process "sh" nil "*restart apache*" nil "-c" 
                "service httpd restart")
  (message "ra output is in *restart apache*"))

(defun airwave-restart-daemons () 
  "Restart daemons."
  (interactive)
  (message "restarting demons, please wait...")
  (call-process "sh" nil "*restart daemons*" nil "-c" 
                "restart_daemons.pl")
  (message "rd output is in *restart daemons*"))

(defun airwave-cleanap () 
  "Run cleanap."
  (interactive)
  (message "Running cleanap, please wait...")
  (airwave-shell-function-basic "cleanap")
  )

(defun airwave-make-confcommon () 
  "Push out apache,shell,emacs,etc configuration files."
  (interactive)
  (airwave-save-and-save-some-buffers)
  (message "making conf_common, please wait...")
  (call-process "sh" nil "*conf_common*" nil "-c" 
                (format "cd %s; make conf_common" (home "/svn/mercury")))
  (message "conf_common output is in *conf_common*"))

(defun url2handler (url)
  "Determine a handler from a url"
  (interactive (list (read-string "url: ")))
  (airwave-shell-function-basic "url2handler" url)
  )

(defun airwave-check-use-lines ()
  (interactive)
  (airwave-save-and-save-some-buffers)
  (airwave-shell-function-basic "check_use_lines" (buffer-file-name)))

(defun airwave-delete-newest-schema-change ()
  (interactive)
  (airwave-shell-function-basic "delete_newest_schema_change"))

(defun airwave-switch-project (proj)
  (setq vc-root  (home "/svn/" proj))
  (setenv "root" vc-root))
(defun switch-to-mercury ()
  (interactive)
  (airwave-switch-project "mercury"))
(defun switch-to-project (proj)
  (interactive "sProject: ")
  (airwave-switch-project proj))
(defun switch-to-devtoys ()
  (interactive)
  (airwave-switch-project "devtoys"))
(defun switch-to-svnhook_testbed ()
  (interactive)
  (airwave-switch-project "svnhook_testbed"))
(defun switch-to-build-centos ()
  (interactive)
  (airwave-switch-project "build-centos"))

; keep you in the SVN sandbox
(defadvice find-file (after to-sandbox activate compile)
  (when (or (string-match "/usr/local/airwave/lib/" (buffer-file-name))
            (string-match (home "/.e") (buffer-file-name)))
      (airwave-leaving-sandbox)))
(defadvice find-function (after to-sandbox activate compile)
  (when (string-match (home "/.e") (buffer-file-name))
    (airwave-leaving-sandbox)))
(defadvice help-follow (after to-sandbox activate compile)
  (when (string-match (home "/.e") (buffer-file-name))
    (airwave-leaving-sandbox)))
(defun airwave-leaving-sandbox ()
  (setq buffer-read-only t)
  (local-set-key [s-right] 'airwave-to-sandbox)
  (message "Warning: You have left the SVN sandbox, <super-right> will take you back.")
  )

(defun airwave-sandbox-filename (filename)
  (or (string-replace-match "/usr/local/airwave/lib/" 
                            filename 
                            (root "/lib/"))
      (string-replace-match (home "/.emacs") 
                            filename 
                            (root "/lib/conf/emacs"))
      (string-replace-match (home "/.elisp") 
                            filename 
                            (root "/lib/conf/elisp"))
      ))
(defun airwave-to-sandbox ()
  (interactive)
  (let ((linenum (current-line-number))
        (filename (airwave-sandbox-filename (buffer-file-name))))
    (kill-buffer (current-buffer))
    (find-file filename)
    (goto-line linenum)))

(defun replace-char-under-point (new-string)
  (delete-char 1)
  (insert new-string))

(defun airwave-columndb-to-column-data ()
  (interactive)
  (search-forward "Mercury::Handler::Table::ColumnDB")
  (beginning-of-line)
  (airwave-zap-to-char 1 (string-to-char "("))
  (save-excursion
    (airwave-goto-matching-char)
    (replace-char-under-point "}"))
  (replace-char-under-point "{")
  (let (column-name)
    (save-excursion
      (search-forward "column =>")
      (search-forward-regexp "[\"\']")
      (setq column-name (current-word))
      (airwave-kill-whole-line))
    (backward-char 1)
    (insert column-name " => "))
  (cperl-indent-line)
  (airwave-goto-matching-char))




