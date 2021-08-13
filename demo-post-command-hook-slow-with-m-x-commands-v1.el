;;; demo-post-command-hook-slow-with-m-x-commands.el --- Demo that post-command hooks are slow with M-x commands      -*- lexical-binding: t; -*-

;; This demonstrates an issue with `(add-hook 'post-command-hook ...)`.

;; To reproduce the problem:
;; - Run `emacs -q -l demo-post-command-hook-slow-with-m-x-commands.el` (that's
;;   this file).
;; - Enter `M-x next-line`.
;; - Observe that the message displayed by the `say-something` function
;;   takes a while to appear.
;; - Contrast with hitting the down-arrow key, when the message appears
;;   immediately.

;;; Code:

(require 'package)

(setq debug-on-error t
      no-byte-compile t
      package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/"))
      package-user-dir (expand-file-name (make-temp-name "flycheck-tmp-elpa")
                                         user-emacs-directory)
      custom-file (expand-file-name "custom.el" package-user-dir))

(defun say-something ()
  (let ((inhibit-message t))
    (message
     "With `M-x next-line` (for example), this message is slow to appear")))

(add-hook 'post-command-hook 'say-something)

(display-buffer "*Messages*")

(provide 'demo-post-command-hook-slow-with-m-x-commands)
;;; demo-post-command-hook-slow-with-m-x-commands.el ends here
