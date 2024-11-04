;;; multi-vterm.el --- manager multi vterm           -*- lexical-binding: t; -*-

;; Copyright (C) 2024  lizqwer scott

;; Author: lizqwer scott <lizqwerscott@gmail.com>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'cl-lib)
(require 'vterm)
(require 'project)

(defgroup multi-vterm nil
  "Multi term manager"
  :group 'vterm)

(defcustom multi-vterm-program nil
  "The shell program run by vterm.
If nil, this defaults to the SHELL environment variable."
  :type 'string
  :group 'multi-vterm)

(defcustom multi-vterm-buffer-name "vterminal"
  "The vterm buffer name."
  :type 'string
  :group 'multi-vterm)

;; Contants

;; Variables
(defvar multi-vterm-buffer-list nil
  "The list of non-dedicated terminal buffers managed by `multi-vterm'.")

(defun string-hash-case-sensitive (a)
  (sxhash a))
(defun string-equal-case-sensitive (a b)
  (string= a b))
(define-hash-table-test 'case-sensitive
                        'string-equal-case-sensitive
                        'string-hash-case-sensitive)
(defvar *multi-vterm-dir* (make-hash-table :test 'case-sensitive))

;; Interactive Functions
;;;###autoload
(defun multi-vterm ()
  "Create new vterm buffer."
  (interactive)
  (let* ((vterm-buffer (multi-vterm-get-buffer)))
    (setq multi-vterm-buffer-list (nconc multi-vterm-buffer-list (list vterm-buffer)))
    (set-buffer vterm-buffer)
    (multi-vterm-internal)
    (pop-to-buffer-same-window vterm-buffer)))

;;;###autoload
(defun multi-vterm-open ()
  "Create new vterm buffer or open buffer"
  (interactive)
  (let ((res (gethash (file-truename default-directory)
                      *multi-vterm-dir*)))
    (if res
        (pop-to-buffer-same-window res)
      (let* ((vterm-buffer (multi-vterm-get-buffer)))
        (setq multi-vterm-buffer-list (nconc multi-vterm-buffer-list (list vterm-buffer)))
        (set-buffer vterm-buffer)
        (multi-vterm-internal)
        (setf (gethash (file-truename default-directory)
                       *multi-vterm-dir*)
              vterm-buffer)
        (pop-to-buffer-same-window vterm-buffer)))))

;;;###autoload
(defun multi-vterm-project ()
  "Create new project vterm buffer."
  (interactive)
  (if (multi-vterm-project-root)
      (if (buffer-live-p (get-buffer (multi-vterm-project-get-buffer-name)))
          (if (string-equal (buffer-name (current-buffer)) (multi-vterm-project-get-buffer-name))
              (delete-window (selected-window))
            (switch-to-buffer-other-window (multi-vterm-project-get-buffer-name)))
        (let* ((vterm-buffer (multi-vterm-get-buffer 'project))
               (multi-vterm-buffer-list (nconc multi-vterm-buffer-list (list vterm-buffer))))
          (set-buffer vterm-buffer)
          (multi-vterm-internal)
          (switch-to-buffer-other-window vterm-buffer)))
    (message "This file is not in a project")))

;;;###autoload
(defun multi-vterm-run (run-command)
  "Open a project or local vterm to run command"
  (interactive)
  (if (project-current nil)
      (call-interactively #'multi-vterm-project)
    (call-interactively #'multi-vterm-open))
  (vterm-send-M-w)
  (vterm-send-string run-command t)
  (vterm-send-return))

(defun multi-vterm-get-buffer (&optional dedicated-window)
  "Get vterm buffer name based on DEDICATED-WINDOW.
Optional argument DEDICATED-WINDOW: There are three types of DEDICATED-WINDOW: dedicated, project, default."
  (with-temp-buffer
    (let ((index 1)
          vterm-name)
      (cond ((eq dedicated-window 'dedicated) (setq vterm-name (multi-vterm-dedicated-get-buffer-name)))
            ((eq dedicated-window 'project) (progn
                                              (setq vterm-name (multi-vterm-project-get-buffer-name))
                                              (setq default-directory (multi-vterm-project-root))))
            (t (progn
                 (while (buffer-live-p (get-buffer (multi-vterm-format-buffer-index index)))
                   (setq index (1+ index)))
                 (setq vterm-name (multi-vterm-format-buffer-index index)))))
      (let ((buffer (get-buffer vterm-name)))
        (if buffer
            buffer
          (let ((buffer (generate-new-buffer vterm-name)))
            (set-buffer buffer)
            (vterm-mode)
            buffer))))))

(defun multi-vterm-project-root ()
  "Get `default-directory' for project using projectile or project.el."
  (unless (boundp 'multi-vterm-projectile-installed-p)
    (setq multi-vterm-projectile-installed-p (require 'projectile nil t)))
  (if multi-vterm-projectile-installed-p
      (projectile-project-root)
    (project-root
     (or (project-current) `(transient . ,default-directory)))))

(defun multi-vterm-project-get-buffer-name ()
  "Get project buffer name."
  (multi-vterm-format-buffer-name (multi-vterm-project-root)))

(defun multi-vterm-rename-buffer (name)
  "Rename vterm buffer to NAME."
  (interactive "MRename vterm buffer: ")
  (rename-buffer (multi-vterm-format-buffer-name name)))

(defun multi-vterm-format-buffer-name (name)
  "Format vterm buffer NAME."
  (format "*%s - %s*" multi-vterm-buffer-name name))

(defun multi-vterm-format-buffer-index (index)
  "Format vterm buffer name with INDEX."
  (format "*%s<%s>*" multi-vterm-buffer-name index))

(defun multi-vterm-handle-close ()
  "Close current vterm buffer when `exit' from vterm buffer."
  (when (ignore-errors (get-buffer-process (current-buffer)))
    (set-process-sentinel (get-buffer-process (current-buffer))
                          (lambda (proc change)
                            (when (string-match "\\(finished\\|exited\\)" change)
                              (kill-buffer (process-buffer proc)))))))

(defun multi-vterm-next (&optional offset)
  "Go to the next term buffer.
If OFFSET is `non-nil', will goto next term buffer with OFFSET."
  (interactive "P")
  (multi-vterm-switch 'NEXT (or offset 1)))

(defun multi-vterm-prev (&optional offset)
  "Go to the previous term buffer.
If OFFSET is `non-nil', will goto next term buffer with OFFSET."
  (interactive "P")
  (multi-vterm-switch 'PREVIOUS (or offset 1)))

(defun multi-vterm-switch (direction offset)
  "Internal `multi-vterm' buffers switch function.
If DIRECTION is `NEXT', switch to the next term.
If DIRECTION `PREVIOUS', switch to the previous term.
Option OFFSET for skip OFFSET number term buffer."
  (unless (multi-vterm-switch-internal direction offset)
    (multi-vterm)))

;; Utility Functions
(defun multi-vterm-internal ()
  "Internal handle for `multi-vterm' buffer."
  (multi-vterm-handle-close)
  (add-hook 'kill-buffer-hook #'multi-vterm-kill-buffer-hook))

(defun multi-vterm-kill-buffer-hook ()
  "Function that hook `kill-buffer-hook'."
  (when (eq major-mode 'vterm-mode)
    (let ((killed-buffer (current-buffer)))
      (setq multi-vterm-buffer-list
            (delq killed-buffer multi-vterm-buffer-list)))))

(defun multi-vterm-shell-name ()
  "Get shell-name based on var `multi-vterm-program' or env SHELL or default `shell-file-name'."
  (or multi-vterm-program
      (getenv "SHELL")
      shell-file-name))

(defun multi-vterm-dedicated-get-window ()
  "Get `multi-vterm' dedicated window."
  (setq multi-vterm-dedicated-window
	    (split-window
	     (selected-window)
	     (- (multi-vterm-current-window-height) (multi-vterm-dedicated-calc-window-height)))))

(defun multi-vterm-current-window-height (&optional window)
  "Return the height the `window' takes up.
Not the value of `window-height', it returns usable rows available for WINDOW.
If `window' is nil, get current window."
  (let ((edges (window-edges window)))
    (- (nth 3 edges) (nth 1 edges))))

(defun multi-vterm-window-exist-p (window)
  "Return non-nil if WINDOW exist."
  (and window (window-live-p window)))

(defun multi-vterm-buffer-exist-p (buffer)
  "Return non-nil if BUFFER exist.
Otherwise return nil."
  (and buffer (buffer-live-p buffer)))

(defun multi-vterm-switch-internal (direction offset)
  "Internal `multi-vterm' buffers switch function.
If DIRECTION is `NEXT', switch to the next term.
If DIRECTION `PREVIOUS', switch to the previous term.
Option OFFSET for skip OFFSET number term buffer."
  (when multi-vterm-buffer-list
    (let ((buffer-list-len (length multi-vterm-buffer-list))
          (my-index (cl-position (current-buffer) multi-vterm-buffer-list)))
      (if my-index
          (let ((target-index (if (eq direction 'NEXT)
                                  (mod (+ my-index offset) buffer-list-len)
                                (mod (- my-index offset) buffer-list-len))))
            (switch-to-buffer (nth target-index multi-vterm-buffer-list)))
        (switch-to-buffer (car multi-vterm-buffer-list))))))

(provide 'multi-vterm)
;;; multi-vterm.el ends here
