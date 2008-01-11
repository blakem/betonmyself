(provide 'airwave-rally)

(defvar rally-current-pair nil)
(defvar rally-current-task nil)
(defvar rally-last-keys nil)
(defvar rally-timer nil)

(defun rally-start-task-and-pick-pair ()
  (interactive)
  (rally-set-current-task)
  (setq rally-current-pair (read-string "Pair (or nothing for singleton): " rally-current-pair))
  (when (string= rally-current-pair "") (setq rally-current-pair 'nil))
  (rally-start-task rally-current-task))
  
(defun rally-start-task (&optional current-task)
  "Start work on a task"
  (interactive)
  (rally-set-current-task current-task)
  (start-process-shell-command "rally" nil "/usr/local/bin/start_task" rally-current-task "-p" rally-current-pair)
  (rally-setup-timer)
  (rally-set-mode-line))

(defun rally-set-current-task (&optional task)
  (if task (setq rally-current-task task)
    (setq rally-current-task (read-string "Task: " "TA")))
  (setq rally-current-task (or (string-replace-match "TATA" rally-current-task "TA") rally-current-task)))

(defun rally-pause-task ()
  "Pause a task"
  (interactive)
  (when rally-timer
    (start-process-shell-command "rally" nil "/usr/local/bin/pause_task" rally-current-task "--pause")
    (rally-teardown-timer))
  (rally-set-mode-line))

(defun rally-resume-task ()
  "Resume a task"
  (interactive)
  (when rally-current-task
    (start-process-shell-command "rally" nil "/usr/local/bin/pause_task" rally-current-task "--resume")
    (rally-setup-timer))
  (rally-set-mode-line))

(defun rally-complete-task ()
  "Complete work on a task"
  (interactive)
  (when rally-current-task
    (start-process-shell-command "rally" nil "/usr/local/bin/complete_task" rally-current-task)
    (rally-teardown-timer)
    (setq rally-current-task nil))
  (rally-set-mode-line))

(defun rally-forget-task ()
  "Make emacs forget this task"
  (interactive)
  (when rally-current-task
    (rally-teardown-timer)
    (setq rally-current-task nil)
    (setq rally-current-pair nil))
  (rally-set-mode-line))

(defun rally-pair-mode (&optional new-pair)
  "Set your pair"
  (interactive)
  (setq rally-current-pair (or new-pair (read-string "Pair: ")))
  (when rally-current-task
    (start-process-shell-command "rally" nil "/usr/local/bin/set_pair" rally-current-task rally-current-pair))
  (when rally-timer
    (rally-setup-timer))
  (rally-set-mode-line))

(defun rally-singleton-mode ()
  "Work alone"
  (interactive)
  (setq rally-current-pair nil)
  (when rally-current-task
    (start-process-shell-command "rally" nil "/usr/local/bin/set_pair" rally-current-task))
  (when rally-timer
    (rally-setup-timer))
  (rally-set-mode-line))

; Functions to handle ticking
(defun rally-setup-timer ()
  (rally-teardown-timer)
  (setq rally-last-keys (recent-keys))
  (setq rally-timer (run-at-time (rally-tick-period) (rally-tick-period) 'rally-tick-task)))

(defun rally-teardown-timer ()
  (when rally-timer 
    (cancel-timer rally-timer)
    (setq rally-timer nil)))

(defun rally-tick-task ()
  (when (and rally-current-task (not (equal (recent-keys) rally-last-keys)))
    (start-process-shell-command "rally" nil "/usr/local/bin/set_task_time" rally-current-task "--add" "--time" ".01" "--must_be_in_progress"))
  (setq rally-last-keys (recent-keys)))

(defun rally-tick-period ()
  (if rally-current-pair 288 576))

(defun rally-idle-period ()
  (- (rally-tick-period) 1))

(defun rally-set-idle ()
  (while (sit-for (rally-idle-period))
    (setq rally-idle-state 1)))

(defun rally-set-mode-line ()
  (setq global-mode-string rally-current-task)
  (when rally-current-pair
    (setq global-mode-string (concat global-mode-string (when global-mode-string " ") "w/" rally-current-pair)))
  (when (and (not rally-timer) rally-current-task)
    (setq global-mode-string (concat "***" global-mode-string "***")))
  (force-mode-line-update))
