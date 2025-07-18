;;; -*- coding: utf-8; lexical-binding: t;  -*-
;;; restclient.el --- An interactive HTTP client for Emacs
;;
;; Public domain.

;; Author: Pavel Kurnosov <pashky@gmail.com>
;; Maintainer: Pavel Kurnosov <pashky@gmail.com>
;; Created: 01 Apr 2012
;; Keywords: http

;; slightly modified by Dino.
;; 2024-11-05T14:33:11

;; This file is not part of GNU Emacs.
;; This file is public domain software. Do what you want.

;;; Commentary:
;;
;; This is a tool to manually explore and test HTTP REST
;; webservices.  Runs queries from a plain-text query sheet, displays
;; results as a pretty-printed XML, JSON and even images.

;;; Code:
;;
(require 'url)
(require 'json)
(require 's)
;;(require 'dash)

(defgroup restclient nil
  "An interactive HTTP client for Emacs."
  :group 'tools)

(defcustom restclient-log-request t
  "Log restclient requests to *Messages*."
  :group 'restclient
  :type 'boolean)

(defcustom restclient-same-buffer-response t
  "Re-use same buffer for responses or create a new one each time."
  :group 'restclient
  :type 'boolean)

(defcustom restclient-same-buffer-response-name "*HTTP Response*"
  "Name for response buffer."
  :group 'restclient
  :type 'string)

(defcustom restclient-inhibit-cookies nil
  "Inhibit restclient from sending cookies implicitly."
  :group 'restclient
  :type 'boolean)

(defvar restclient-within-call nil)

(defvar restclient-request-time-start nil)
(defvar restclient-request-time-end nil)

(defvar restclient-response-loaded-hook nil
  "Hook run after response buffer created and data loaded.")

(defvar restclient-http-do-hook nil
  "Hook to run before making request.")


;; this is the new advice pashky has recommended to avoid the
;; warnings like:
;;     Warning (bytecomp): assignment to free variable ‘success’

;; (defadvice url-http-handle-authentication (around restclient-fix)
;;   (if restclient-within-call
;;       (setq ad-return-value t)
;;     ad-do-it))
;; (ad-activate 'url-http-handle-authentication)
;;
;; (defadvice url-cache-extract (around restclient-fix-2)
;;   (unless restclient-within-call
;;     ad-do-it))
;; (ad-activate 'url-cache-extract)

(defvar success nil
  "placeholder to allow the advice below to not throw warnings")


;; The following disables the interactive request for user name and
;; password should an API call encounter a permission-denied response.
;; This API is meant to be usable without constant asking for username
;; and password.
(defadvice url-http-handle-authentication (around restclient-fix)
  (let ((have-success (boundp 'success)))
    (if restclient-within-call
        (setq success t ad-return-value t)
      ad-do-it)))

(ad-activate 'url-http-handle-authentication)

(defadvice url-cache-extract (around restclient-fix-2)
  (if restclient-within-call
      (setq success t)
    ad-do-it))
(ad-activate 'url-cache-extract)

(defadvice url-http-user-agent-string (around restclient-fix-3)
  (if restclient-within-call
      (setq ad-return-value nil)
    ad-do-it))
(ad-activate 'url-http-user-agent-string)

(defun restclient-restore-header-variables ()
  (url-set-mime-charset-string)
  (setq url-mime-language-string nil)
  (setq url-mime-encoding-string nil)
  (setq url-mime-accept-string nil)
  (setq url-personal-mail-address nil))

(defun restclient-http-do (method url headers entity &rest handle-args)
  "Send ENTITY and HEADERS to URL as a METHOD request."
  (if restclient-log-request
      (message "HTTP %s %s Headers:[%s] Body:[%s]" method url headers entity))
  (let ((url-request-method method)
        (url (replace-regexp-in-string "#" "%23" (s-trim url)))
        (url-request-extra-headers '())
        (url-request-data (encode-coding-string entity 'utf-8)))

    (restclient-restore-header-variables)

    (dolist (header headers)
      (let* ((mapped (assoc-string (downcase (car header))
                                   '(("from" . url-personal-mail-address)
                                     ("accept-encoding" . url-mime-encoding-string)
                                     ("accept-charset" . url-mime-charset-string)
                                     ("accept-language" . url-mime-language-string)
                                     ("accept" . url-mime-accept-string)))))

        (if mapped
            (set (cdr mapped) (cdr header))
          (setq url-request-extra-headers (cons header url-request-extra-headers)))
        ))

    (setq restclient-within-call t)
    (setq restclient-request-time-start (current-time))
    (run-hooks 'restclient-http-do-hook)
    (url-retrieve url 'restclient-http-handle-response
                  (append (list method url (if restclient-same-buffer-response
                                               restclient-same-buffer-response-name
                                             (format "*HTTP %s %s*" method url))) handle-args) nil restclient-inhibit-cookies)))

(defvar restclient-content-type-regexp "^Content-[Tt]ype: \\(\\w+\\)/\\(?:[^\\+\r\n]*\\+\\)*\\([^;\r\n]+\\)")

(defun restclient-prettify-response (method url)
  (save-excursion
    (let ((start (point)) (guessed-mode) (end-of-headers))
      (while (and (not (looking-at "^\\s-*$"))
                  (eq (progn
                        (when (looking-at restclient-content-type-regexp)
                          (setq guessed-mode
                                (cdr (assoc-string (concat
                                                    (buffer-substring-no-properties (match-beginning 1) (match-end 1))
                                                    "/"
                                                    (buffer-substring-no-properties (match-beginning 2) (match-end 2))
                                                    )
                                                   '(("text/xml" . xml-mode)
                                                     ("application/xml" . xml-mode)
                                                     ("application/json" . js-mode)
                                                     ("application/x-yaml" . yaml-mode)
                                                     ("image/png" . image-mode)
                                                     ("image/jpeg" . image-mode)
                                                     ("image/jpg" . image-mode)
                                                     ("image/gif" . image-mode)
                                                     ("text/html" . html-mode))))))
                        (forward-line)) 0)))
      (setq end-of-headers (point))
      (while (and (looking-at "^\\s-*$")
                  (eq (forward-line) 0)))
      (unless guessed-mode
        (setq guessed-mode
              (or (assoc-default nil
                                 ;; magic mode matches
                                 '(("<\\?xml " . xml-mode)
                                   ("{\\s-*\"" . js-mode))
                                 (lambda (re _dummy)
                                   (looking-at re))) 'js-mode)))
      (let ((headers (buffer-substring-no-properties start end-of-headers)))
        (when guessed-mode
          (delete-region start (point))
          (unless (eq guessed-mode 'image-mode)
            (apply guessed-mode '())
            (if (fboundp 'font-lock-flush)
                (font-lock-flush)
              (with-no-warnings
                (font-lock-fontify-buffer))))

          (cond
           ((eq guessed-mode 'xml-mode)
            (goto-char (point-min))
            (while (search-forward-regexp "\>[ \\t]*\<" nil t)
              (backward-char) (insert "\n"))
            (indent-region (point-min) (point-max)))

           ((eq guessed-mode 'image-mode)
            (let* ((img (buffer-string)))
              (delete-region (point-min) (point-max))
              (fundamental-mode)
              (insert-image (create-image img nil t))))

           ((eq guessed-mode 'js-mode)
            (let ((json-special-chars (remq (assoc ?/ json-special-chars) json-special-chars)))
              (ignore-errors (json-pretty-print-buffer)))
            (restclient-prettify-json-unicode)))

          (goto-char (point-max))
          (or (eq (point) (point-min)) (insert "\n"))
          (let ((hstart (point)))
            (insert method " " url "\n" headers)
            (insert (format "Request duration: %fs\n" (float-time (time-subtract restclient-request-time-end restclient-request-time-start))))
            (unless (eq guessed-mode 'image-mode)
              (comment-region hstart (point))
              (indent-region hstart (point)))))))))

(defun restclient-prettify-json-unicode ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\\\[Uu]\\([0-9a-fA-F]\\{4\\}\\)" nil t)
      (replace-match (char-to-string (decode-char 'ucs (string-to-number (match-string 1) 16))) t nil))))

(defun restclient-http-handle-response (status method url bufname raw stay-in-window)
  "Switch to the buffer returned by `url-retreive'.
The buffer contains the raw HTTP response sent by the server."
  (setq restclient-within-call nil)
  (setq restclient-request-time-end (current-time))
  (if (= (point-min) (point-max))
      (signal (car (plist-get status :error)) (cdr (plist-get status :error)))
    (restclient-restore-header-variables)
    (when (buffer-live-p (current-buffer))
      (with-current-buffer (restclient-decode-response
                            (current-buffer)
                            bufname
                            restclient-same-buffer-response)
        (unless raw
          (restclient-prettify-response method url))
        (buffer-enable-undo)
        (run-hooks 'restclient-response-loaded-hook)
        (if stay-in-window
            (display-buffer (current-buffer) t)
          (switch-to-buffer-other-window (current-buffer)))))))

(defun restclient-decode-response (raw-http-response-buffer target-buffer-name same-name)
  "Decode the HTTP response using the charset (encoding) specified in the Content-Type header. If no charset is specified, default to UTF-8."
  (let* ((charset-regexp "Content-Type.*charset=\\([-A-Za-z0-9]+\\)")
         (image? (save-excursion
                   (search-forward-regexp "Content-Type.*[Ii]mage" nil t)))
         (encoding (if (save-excursion
                         (search-forward-regexp charset-regexp nil t))
                       (intern (downcase (match-string 1)))
                     'utf-8)))
    (if image?
        ;; Dont' attempt to decode. Instead, just switch to the raw HTTP response buffer and
        ;; rename it to target-buffer-name.
        (with-current-buffer raw-http-response-buffer
          ;; We have to kill the target buffer if it exists, or `rename-buffer'
          ;; will raise an error.
          (when (get-buffer target-buffer-name)
            (kill-buffer target-buffer-name))
          (rename-buffer target-buffer-name)
          raw-http-response-buffer)
      ;; Else, switch to the new, empty buffer that will contain the decoded HTTP
      ;; response. Set its encoding, copy the content from the unencoded
      ;; HTTP response buffer and decode.
      (let ((decoded-http-response-buffer
             (get-buffer-create
              (if same-name target-buffer-name (generate-new-buffer-name target-buffer-name)))))
        (with-current-buffer decoded-http-response-buffer
          (setq buffer-file-coding-system encoding)
          (save-excursion
            (erase-buffer)
            (insert-buffer-substring raw-http-response-buffer))
          (kill-buffer raw-http-response-buffer)
          (condition-case nil
              (decode-coding-region (point-min) (point-max) encoding)
            (error
             (message (concat "Error when trying to decode http response with encoding: "
                              (symbol-name encoding)))))
          decoded-http-response-buffer)))))

(defconst restclient-method-url-regexp
  "^\\(GET\\|POST\\|DELETE\\|PUT\\|HEAD\\|OPTIONS\\|PATCH\\) \\(.*\\)$")

(defconst restclient-header-regexp
  "^\\([^ :]+\\): \\(.*\\)$")

(defconst restclient-var-regexp
  "^\\(:[^: ]+\\)\\s-+\\(:?\\)=\\s-+\\(.+\\)$")

(defconst restclient-evar-regexp
  "^\\(:[^: ]+\\)\\s-+:=\\s-+\\(.+\\)$")

(defun restclient-current-min ()
  (save-excursion
    (beginning-of-line)
    (if (looking-at "^#")
        (if (re-search-forward "^[^#]" (point-max) t)
            (point-at-bol) (point-max))
      (if (re-search-backward "^#" (point-min) t)
          (point-at-bol 2)
        (point-min)))))

(defun restclient-current-max ()
  (save-excursion
    (if (re-search-forward "^#" (point-max) t)
        (max (- (point-at-bol) 1) 1)
      (point-max))))

(defun restclient-replace-all-in-string (replacements s)
  (if replacements
      (replace-regexp-in-string (regexp-opt (mapcar 'car replacements))
                                (lambda (key)
                                  (or (cdr (assoc key replacements))
                                      key))
                                s nil t)
    s))


(defun restclient-replace-all-in-header (replacements header)
  (cons (car header)
        (restclient-replace-all-in-string replacements (cdr header))))

(defun restclient-replace-all-in-headers (replacements headers)
  (mapcar (apply-partially 'restclient-replace-all-in-header replacements) headers))

(defun restclient--unique-vars (vars)
  (let ((unique-keys (delete-dups (mapcar 'car vars))))
    (mapcar (lambda (key) (assoc key vars)) unique-keys)))

(defun restclient-find-vars-before-point ()
  (let ((vars nil)
        (bound (point)))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp restclient-var-regexp bound t)
        (let ((name (buffer-substring-no-properties (match-beginning 1) (match-end 1)))
              (should-eval (> (length (match-string 2)) 0))
              (value (buffer-substring-no-properties (match-beginning 3) (match-end 3))))
          (setq vars (cons (cons name (if should-eval (restclient-eval-var value) value)) vars))))
      (restclient--unique-vars vars))))

(defun restclient-var (name)
  "return the value of a var in the restclient context.
eg, :basicauth := (base64-encode-string
                   (concat (restclient-var \":userNAME\")
                      \":\" (restclient-var \":pwd\"))
"
  (cdr (assoc name vars)))

(defun restclient-eval-var (string)
  (with-output-to-string (princ (eval (read string)))))

(defun restclient--trim-right (s)
  "Remove whitespace at the end of S."
  (if (string-match "[ \t\n\r]+\\'" s)
      (replace-match "" t t s)
    s))

(defun restclient-http-parse-current-and-do (func &rest args)
  (save-excursion
    (goto-char (restclient-current-min))
    (when (re-search-forward restclient-method-url-regexp (point-max) t)
      (let ((method (buffer-substring-no-properties (match-beginning 1) (match-end 1)))
            (url (buffer-substring-no-properties (match-beginning 2) (match-end 2)))
            (headers '()))
        (forward-line)
        (while (re-search-forward restclient-header-regexp (point-at-eol) t)
          (setq headers (cons (cons (buffer-substring-no-properties (match-beginning 1) (match-end 1))
                                    (buffer-substring-no-properties (match-beginning 2) (match-end 2)))
                              headers))
          (forward-line))
        (when (looking-at "^\\s-*$")
          (forward-line))
        (let* ((cmax (restclient-current-max))
               (entity (restclient--trim-right (buffer-substring-no-properties (min (point) cmax) cmax)))
               (vars (restclient-find-vars-before-point))
               (url (restclient-replace-all-in-string vars url))
               (headers (restclient-replace-all-in-headers vars headers))
               (entity (restclient-replace-all-in-string vars entity))
               ;; dino - modified 20241105-1500
               ;; To compactify json optionally.
               ;; This is necessary for some API servers. (5g Apigee portals)
               (ctype-hdr (or
                           (cdr (assoc "content-type" headers))
                           (cdr (assoc "Content-Type" headers))))
               ;; rely on a variable
               (compact-var (cdr (assoc ":-compact-json" vars)))
               (want-compact-json (and
                                   ctype-hdr
                                   compact-var
                                   (string-match "^application/json.*" ctype-hdr)
                                   (string-match "^\\(true\\|True\\)$" compact-var)
                                   )))
          (if want-compact-json
              (setq entity
                    (with-temp-buffer
                      (insert entity)
                      (goto-char (point-min))
                      (let ((json-encoding-pretty-print nil))
                        (json-encode (json-read))))))

          (apply func method url headers entity args))))))

(defun restclient-copy-curl-command ()
  "Formats the request as a curl command and copies the command to the clipboard."
  (interactive)
  (restclient-http-parse-current-and-do
   '(lambda (method url headers entity)
      (kill-new (format "curl -i %s -X %s \\\n '%s'%s"
                        (mapconcat (lambda (header) (format "-H '%s: %s' \\\n" (car header) (cdr header))) headers " ")
                        method
                        url
                        (if (> (string-width entity) 0)
                            (format "\\\n -d '%s'" entity) "")))
      (message "curl command copied to clipboard."))))

(defun restclient--find-extract-commands ()
  "Finds all '# rc-extract' commands immediately following the current request."
  (save-excursion
    (goto-char (restclient-current-max))
    (unless (eobp) (forward-line 1))
    (let ((commands '())
          (extract-re "^\\s-*#\\s-*rc-extract\\s-+\\(:[^: ]+\\)\\s-*=\\s-*\\$(\\(.*\\))")
          (comment-re "^\\s-*#")
          (empty-comment-re "^\\s-*#\\s-*$"))
      (catch 'stop-scanning
        (while (and (not (eobp)) (looking-at comment-re))
          (if (looking-at extract-re)
              (let ((varname (match-string-no-properties 1))
                    (command (match-string-no-properties 2)))
                (push (list varname (s-trim command)) commands))
            ;; not an extract comment
            (unless (looking-at empty-comment-re)
              ;; not an empty comment either, so stop
              (throw 'stop-scanning nil)))
          (forward-line 1)))
      (nreverse commands))))


(defun restclient--process-extract-commands (commands)
  "Process extract commands, adding all results as a single kill-ring entry."
  (when commands
    (let ((response-buffer (current-buffer))
          (temp-buffer (generate-new-buffer " *rc-temp-filtered*")))
      (unwind-protect
          (let ((results '()))
            (with-current-buffer temp-buffer
              (insert-buffer-substring response-buffer)
              (goto-char (point-min))
              (flush-lines "^//" (point-min) (point-max)))

            ;; 2. Loop through commands and collect results into the `results` list
            (dolist (command-spec commands)
              (let* ((varname (car command-spec))
                     (command (cadr command-spec))
                     (output-buffer (generate-new-buffer " *rc-extract-output*")))
                (unwind-protect
                    (progn
                      (with-current-buffer temp-buffer
                        (shell-command-on-region (point-min) (point-max) command output-buffer nil))
                      (with-current-buffer output-buffer
                        (let ((result (s-trim (buffer-substring-no-properties (point-min) (point-max)))))
                          (push (format "%s = %s" varname result) results))))
                  (when (buffer-live-p output-buffer)
                    (kill-buffer output-buffer)))))

            (when results
              (kill-new (concat (mapconcat #'identity (nreverse results) "\n") "\n"))
              (message "rc-extract: yank to get all extracted variables.")))

        (when (buffer-live-p temp-buffer)
          (kill-buffer temp-buffer))))))

;;;###autoload
(defun restclient-http-send-current (&optional raw stay-in-window)
  "Sends current request.
Optional argument RAW don't reformat response if t.
Optional argument STAY-IN-WINDOW do not move focus to response buffer if t."
  (interactive)
  (let ((extract-commands (restclient--find-extract-commands)))
    (when extract-commands
      (let ((extract-commands extract-commands)
            (hook-fn nil))
        (setq hook-fn
              (lambda ()
                (restclient--process-extract-commands extract-commands)
                (remove-hook 'restclient-response-loaded-hook hook-fn)))
        (add-hook 'restclient-response-loaded-hook hook-fn nil nil))))
  (restclient-http-parse-current-and-do 'restclient-http-do raw stay-in-window))

;;;###autoload
(defun restclient-http-send-current-raw ()
  "Sends current request and get raw result (no reformatting or syntax highlight of XML, JSON or images)."
  (interactive)
  (restclient-http-send-current t))

;;;###autoload
(defun restclient-http-send-current-stay-in-window ()
  "Send current request and keep focus in request window."
  (interactive)
  (restclient-http-send-current nil t))

(defun restclient-jump-next ()
  "Jump to next request in buffer."
  (interactive)
  (let ((last-min nil))
    (while (not (eq last-min (goto-char (restclient-current-min))))
      (goto-char (restclient-current-min))
      (setq last-min (point))))
  (goto-char (+ (restclient-current-max) 1))
  (goto-char (restclient-current-min)))

(defun restclient-jump-prev ()
  "Jump to previous request in buffer."
  (interactive)
  (let* ((current-min (restclient-current-min))
         (end-of-entity
          (save-excursion
            (progn (goto-char (restclient-current-min))
                   (while (and (or (looking-at "^\s*\\(#.*\\)?$")
                                   (eq (point) current-min))
                               (not (eq (point) (point-min))))
                     (forward-line -1)
                     (beginning-of-line))
                   (point)))))
    (unless (eq (point-min) end-of-entity)
      (goto-char end-of-entity)
      (goto-char (restclient-current-min)))))

(defun restclient-mark-current ()
  "Mark current request."
  (interactive)
  (goto-char (restclient-current-min))
  (set-mark-command nil)
  (goto-char (restclient-current-max))
  (backward-char 1)
  (setq deactivate-mark nil))

(defvar restclient-mode-keywords
  (list (list restclient-method-url-regexp '(1 font-lock-keyword-face) '(2 font-lock-function-name-face))
        (list restclient-header-regexp '(1 font-lock-variable-name-face) '(2 font-lock-string-face))
        (list restclient-evar-regexp '(1 font-lock-preprocessor-face) '(2 font-lock-function-name-face))
        (list restclient-var-regexp '(1 font-lock-preprocessor-face) '(3 font-lock-string-face))
        ))

(defvar restclient-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\# "<" table)
    (modify-syntax-entry ?\n ">#" table)
    table))

;;;###autoload
(define-derived-mode restclient-mode fundamental-mode "REST Client"
  "Turn on restclient mode."
  (local-set-key (kbd "C-c C-c") 'restclient-http-send-current)
  (local-set-key (kbd "C-c C-r") 'restclient-http-send-current-raw)
  (local-set-key (kbd "C-c C-v") 'restclient-http-send-current-stay-in-window)
  (local-set-key (kbd "C-c C-n") 'restclient-jump-next)
  (local-set-key (kbd "C-c C-p") 'restclient-jump-prev)
  (local-set-key (kbd "C-c C-.") 'restclient-mark-current)
  (local-set-key (kbd "C-c C-u") 'restclient-copy-curl-command)
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-start-skip) "# *")
  (set (make-local-variable 'comment-column) 48)

  (set (make-local-variable 'font-lock-defaults) '(restclient-mode-keywords)))


(provide 'restclient)
;;; restclient.el ends here
