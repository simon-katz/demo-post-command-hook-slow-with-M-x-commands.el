;;; demo-post-command-hook-slow-with-m-x-commands-v2.el --- Demo that post-command hooks are slow with M-x commands      -*- lexical-binding: t; -*-

;; This demonstrates an issue with:
;;   `(add-hook 'post-command-hook ...)`.
;; It's a follow-up to an already-reported and partially-fixed problem -- see
;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=50042

;; To reproduce the problem:
;;
;; - Run `emacs -q -l <this-file>`
;;
;; - Enter `M-x **-msg-cmd-without-key-binding`.
;; - Observe that it takes many seconds (~15 for me) to produce output to the
;;   "my-output" buffer, and that the "*Messages*" buffer has the message:
;;     You can run the command ‘**-msg-cmd-without-key-binding’ with M-x -witho RET
;;
;; - Enter `M-x **-msg-cmd-without-key-binding` again.
;; - Observe that both the "*Messages*" buffer and the "my-output" buffer update
;;   immediately.
;; - Repeatedly enter `M-x **-msg-cmd-without-key-binding` and observe the same
;; - result.
;;
;; - Enter `M-x **-msg-cmd-with-key-binding`.
;; - Observe that it takes 2 seconds for output to appear in the "my-output"
;;   buffer.
;; - Observe that the behaviour is the same if the command is repeated.
;;
;; - Use the `C-c C-c` key binding, to invoke `**-msg-cmd-with-key-binding`.
;; - Observe that there are no delays.


;; Emacs displays the "*GNU Emacs*" buffer after loading this file, so use
;; `run-at-time` to delay setting up which buffers are displayed.
(run-at-time 1
             nil
             (lambda ()
               (switch-to-buffer "*Messages*" nil t)
               (display-buffer (generate-new-buffer "my-output"))
               ;; Arrange things so that we see the output as it appears:
               (select-window (get-buffer-window "my-output"))))

(defun date-time ()
  (format-time-string "%Y-%m-%d %H:%M:%S"))

(defun post-command-message ()
  (when (member this-command
                '(**-msg-cmd-with-key-binding
                  **-msg-cmd-without-key-binding))
    (with-current-buffer "my-output"
      (goto-char (point-max))
      (insert (date-time)
              " The command `"
              (format "%s" this-command)
              "` was executed\n")
      (goto-char (point-max)))))

(add-hook 'post-command-hook 'post-command-message)

(defun **-msg-cmd-without-key-binding ()
  (interactive)
  (message "%s Hello from `**-msg-cmd-without-key-binding`"
           (date-time)))

(defun **-msg-cmd-with-key-binding ()
  (interactive)
  (message "%s Hello from `**-msg-cmd-with-key-binding`"
           (date-time)))

(define-key global-map (kbd "C-c C-c") '**-msg-cmd-with-key-binding)

(when t ; Whether to load the changes in Emacs commit 42a98feb5b

  (defvar execute-extended-command--binding-timer nil)

  (defun execute-extended-command (prefixarg &optional command-name typed)
    ;; Based on Fexecute_extended_command in keyboard.c of Emacs.
    ;; Aaron S. Hawley <aaron.s.hawley(at)gmail.com> 2009-08-24
    "Read a command name, then read the arguments and call the command.
To pass a prefix argument to the command you are
invoking, give a prefix argument to `execute-extended-command'."
    (declare (interactive-only command-execute))
    ;; FIXME: Remember the actual text typed by the user before completion,
    ;; so that we don't later on suggest the same shortening.
    (interactive
     (let ((execute-extended-command--last-typed nil))
       (list current-prefix-arg
             (read-extended-command)
             execute-extended-command--last-typed)))
    ;; Emacs<24 calling-convention was with a single `prefixarg' argument.
    (unless command-name
      (let ((current-prefix-arg prefixarg) ; for prompt
            (execute-extended-command--last-typed nil))
        (setq command-name (read-extended-command))
        (setq typed execute-extended-command--last-typed)))
    (let* ((function (and (stringp command-name) (intern-soft command-name)))
           (binding (and suggest-key-bindings
                         (not executing-kbd-macro)
                         (where-is-internal function overriding-local-map t))))
      (unless (commandp function)
        (error "`%s' is not a valid command name" command-name))
      ;; Some features, such as novice.el, rely on this-command-keys
      ;; including M-x COMMAND-NAME RET.
      (set--this-command-keys (concat "\M-x" (symbol-name function) "\r"))
      (setq this-command function)
      ;; Normally `real-this-command' should never be changed, but here we really
      ;; want to pretend that M-x <cmd> RET is nothing more than a "key
      ;; binding" for <cmd>, so the command the user really wanted to run is
      ;; `function' and not `execute-extended-command'.  The difference is
      ;; visible in cases such as M-x <cmd> RET and then C-x z (bug#11506).
      (setq real-this-command function)
      (let ((prefix-arg prefixarg))
        (command-execute function 'record))
      ;; If enabled, show which key runs this command.
      ;; But first wait, and skip the message if there is input.
      (let* ((waited
              ;; If this command displayed something in the echo area;
              ;; wait a few seconds, then display our suggestion message.
              ;; FIXME: Wait *after* running post-command-hook!
              ;; FIXME: If execute-extended-command--shorter were
              ;; faster, we could compute the result here first too.
              (when (and suggest-key-bindings
                         (or binding
                             (and extended-command-suggest-shorter typed)))
                (sit-for (cond
                          ((zerop (length (current-message))) 0)
                          ((numberp suggest-key-bindings) suggest-key-bindings)
                          (t 2))))))
        (when (and waited (not (consp unread-command-events)))
          (unless (or (not extended-command-suggest-shorter)
                      binding executing-kbd-macro (not (symbolp function))
                      (<= (length (symbol-name function)) 2))
            ;; There's no binding for CMD.  Let's try and find the shortest
            ;; string to use in M-x.
            ;; FIXME: Can be slow.  Cache it maybe?
            (while-no-input
              (setq binding (execute-extended-command--shorter
                             (symbol-name function) typed))))
          (when binding
            ;; This is normally not necessary -- the timer should run
            ;; immediately, but be defensive and ensure that we never
            ;; have two of these timers in flight.
            (when execute-extended-command--binding-timer
              (cancel-timer execute-extended-command--binding-timer))
            (setq execute-extended-command--binding-timer
                  (run-at-time
                   0 nil
                   (lambda ()
                     (with-temp-message
                         (format-message "You can run the command `%s' with %s"
                                         function
                                         (if (stringp binding)
                                             (concat "M-x " binding " RET")
                                           (key-description binding)))
                       (sit-for (if (numberp suggest-key-bindings)
                                    suggest-key-bindings
                                  2))))))))))))

(provide 'demo-post-command-hook-slow-with-m-x-commands-v2)
;;; demo-post-command-hook-slow-with-m-x-commands-v2.el ends here
