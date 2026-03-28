;;; dino-prettierd-cleanup.el --- cleanup prettierd daemons   -*- coding: utf-8; lexical-binding: t;  -*-
;;
;; Author: Dino Chiesa
;; Created: Tuesday, March 24, 2026
;; Package-Requires: (cl-lib)
;; Keywords: utility, processes
;; License: New BSD

;;; Commentary:
;;
;; This package provides a mechanism to periodically clean up prettierd
;; daemon processes that have been orphaned or are no longer needed.
;; It is particularly useful on Windows where process-attributes often
;; returns nil for the 'args' attribute.

;;; License:
;;
;; This code is distributed under the New BSD License.
;;
;; Copyright (c) 2026, Dino Chiesa
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; Redistributions of source code must retain the above copyright
;; notice, this list of conditions and the following disclaimer.
;;
;; Redistributions in binary form must reproduce the above copyright
;; notice, this list of conditions and the following disclaimer in the
;; documentation and/or other materials provided with the distribution.
;;
;; Neither the name of the author or any contributors, nor the names of
;; any organizations they belong to, may be used to endorse or promote
;; products derived from this software without specific prior written
;; permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
;; BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
;; OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
;; AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY
;; WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.
;;

;;; Code:

(require 'cl-lib)

(defun dino/prettierd--get-command-line (pid)
  "Retrieve the command line for PID using wmic or pwsh.
This is necessary on Windows where ='process-attributes'= may return
nil for the ='args'= attribute."
  (let ((wmic-path (or (executable-find "wmic") "C:\\Windows\\System32\\wbem\\wmic.exe"))
        (ps-path (or (executable-find "pwsh.exe") (executable-find "powershell.exe"))))
    (let ((wmic-result (let ((cmd (shell-command-to-string
                                   (format "\"%s\" process where processid=%d get commandline /format:list" wmic-path pid))))
                         (if (string-match "CommandLine=\\(.+\\)" cmd)
                             (match-string 1 cmd)
                           nil))))
      (if wmic-result
          wmic-result
        ;; Try PowerShell as backup
        (when ps-path
          (let ((cmd (shell-command-to-string
                      (format "\"%s\" -NoProfile -Command \"(Get-CimInstance Win32_Process -Filter 'ProcessId = %d').CommandLine\"" ps-path pid))))
            (if (and cmd (not (string-empty-p (string-trim cmd))))
                (string-trim cmd)
              nil)))))))

(defun dino/is-prettierd-process-p (pid attrs)
  "Return non-nil if PID with ATTRS is a prettierd daemon.
PID is the process ID. ATTRS is an alist of process attributes as
returned by ='process-attributes'=.

Checks for ='node'= in the command and ='prettierd'= + ='daemon.js'= in
the arguments."

  (let* ((comm (cdr (assoc 'comm attrs)))
         (is-node-comm (and comm (string-match-p "node" comm)))
         (args (cdr (assoc 'args attrs)))
         (full-args (if (and (null args) (eq system-type 'windows-nt) is-node-comm)
                        (dino/prettierd--get-command-line pid)
                      args)))
    (when (and full-args (not (string-empty-p (format "%s" full-args))))
      (let* ((full-args-str (if (listp full-args) (mapconcat 'identity full-args " ") (format "%s" full-args)))
             ;; Tight check: must be node and must be the prettierd daemon
             (is-node (string-match-p "node\\(\\.exe\\)? " full-args-str))
             (is-prettierd (and (string-match-p "prettierd" full-args-str)
                                (string-match-p "daemon\\.js" full-args-str))))
        (and is-node is-prettierd)))))

(defun dino/kill-prettierd-daemons (&optional any-parent dry-run)
  "Kill prettierd daemon processes started by this Emacs instance.
If ANY-PARENT is non-nil (or if called interactively with a prompt),
kill all prettierd processes regardless of their parent process.
If DRY-RUN is non-nil (or called with a prefix arg), only log actions."
  (interactive (list (y-or-n-p "Kill ALL prettierd processes (regardless of parent)? ")
                     current-prefix-arg))
  (let ((emacs-pid (emacs-pid))
        (killed-count 0))
    (dolist (pid (list-system-processes))
      (let* ((attrs (process-attributes pid))
             (ppid (cdr (assoc 'ppid attrs))))
        (when (or any-parent (eq ppid emacs-pid))
          (when (dino/is-prettierd-process-p pid attrs)
            (if dry-run
                (message "[DRY-RUN] Would kill prettierd process: %d" pid)
              (message "Killing prettierd process: %d" pid)
              (signal-process pid 'sigkill)) ;; sigterm does not work
            (setq killed-count (1+ killed-count))))))
    (if (> killed-count 0)
        (message "%s %d prettierd processes%s."
                 (if dry-run "Would have cleaned up" "Cleaned up")
                 killed-count
                 (if any-parent " (system-wide)" ""))
      (when (called-interactively-p 'any)
        (message "No prettierd processes found to kill.")))))

;;;###autoload
(defvar dino/prettierd-cleanup-timer
  (run-at-time "10 min" 600 #'dino/kill-prettierd-daemons)
  "Timer that periodically cleans up prettierd daemon processes.")

(provide 'dino-prettierd-cleanup)

;;; dino-prettierd-cleanup.el ends here
