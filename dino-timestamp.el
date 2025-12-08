;; dino-timestamp.el --- insert timestamp strings -*- lexical-binding: t; -*-

;;; Commentary:
;; utility functions for inserting timestamps into files, while iterating through
;; various formats.  YYYYMMDD, Long form, RFC8601, etc

;;; Code:


(defvar dts--just-deleted-string nil
  "holder of the string most recently deleted by `dts--maybe-delete-time-string-looking-forward'. Used
to determine if we need to rotate through various formats.")

(defconst dts--punctuation-regex "[\\!\?\"\.'#$%&*+/;<=>@^`|~]"
  "regexp for punctuation")

(defconst dts--monthnames-and-numbers '(("jan" . 1) ("feb" . 2) ("mar" . 3)
                                        ("apr" . 4) ("may" . 5) ("jun" . 6)
                                        ("jul" . 7) ("aug" . 8) ("sep" . 9)
                                        ("oct" . 10) ("nov" . 11) ("dec" . 12)
                                        ("january" . 1) ("february" . 2)
                                        ("march" . 3) ("april" . 4) ("june" . 6)
                                        ("july" . 7) ("august" . 8)
                                        ("september" . 9) ("october" . 10)
                                        ("november" . 11) ("december" . 12)))

;; for each
(defconst dts--time-formats '(("%Y%m%d-%H%M"         . dts--parse-YYYYMMDDHHMM-time)
                              ("%A, %e %B %Y, %H:%M" . dts--parse-rfc822-time)
                              ("%Y %B %e"            . dts--parse-YBe-time)
                              ("%H:%M:%S"            . dts--parse-HMS-time)
                              ("%Y-%m-%dT%H:%M:%S"   . dts--parse-YmdHMS-time)
                              )
  "A list of time formats with corresponding parse functions to use in `dino/insert-timeofday' and `dts--maybe-delete-time-string-looking-forward' ")

;; (setq dino-time-formats '(
;;                              ("%Y%m%d-%H%M" . dino-parse-YYYYMMDDHHMM-time)
;;                              ("%A, %e %B %Y, %H:%M" . dino-parse-rfc822-time)
;;                              ("%Y %B %e" . dino-parse-ymd-time)
;;                              ("%H:%M:%S" . dino-parse-hms-time)
;;                              ))

(defun dts--parse-YYYYMMDDHHMM-time (arg)
  "If ARG is a boolean, then return a regex to match a time string
in format YYYYMMDD-HHMM. Example: \"20130820-0848\".
Otherwise, ARG is a string, and this function will parse it with that regex, and
returns the time in emacs internal time format, eg (sec-high sec-low).
"
  (let ((regex "\\(\\(19\\|20\\)[0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)")
        (dt (decode-time (current-time))))
    (if (booleanp arg)
        regex
      (when (string-match regex arg 0)
        (let ((year (string-to-number (match-string 1 arg)))
              (month (string-to-number (match-string 3 arg)))
              (day (string-to-number (match-string 4 arg)))
              (hour (string-to-number (match-string 5 arg)))
              (minute (string-to-number (match-string 6 arg)))
              (seconds (nth 0 dt))
              tz)
          (apply 'encode-time
                 (list seconds minute hour day month year tz)))))))

(defun dts--monthname-to-number (monthname)
  "Maps a monthname to a number, starting with 1 for January.
For invalid monthnames, returns nil."
  (cdr (assoc-string (downcase monthname) dts--monthnames-and-numbers)))

(defun dts--parse-rfc822-time (arg)
  "If ARG is a boolean, then return a regex to match a time string
formatted like: \"Tuesday, 21 November 2017, 12:42\".
Otherwise, ARG is a string, and this function will parse it with that regex, and
returns the time in emacs internal time format, eg (sec-high sec-low).
"
  (let ((regex
         "\\(Sunday\\|Monday\\|Tuesday\\|Wednesday\\|Thursday\\|Friday\\|Saturday\\), +\\([0-9]\\{1,2\\}\\) \\([A-Za-z]\\{3,14\\}\\) \\(\\(19\\|20\\)[0-9]\\{2\\}\\), \\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)")
        (dt (decode-time (current-time))))
    (if (booleanp arg)
        regex
      (when (string-match regex arg 0)

        (let ((day (string-to-number (match-string 2 arg)))
              (month (dts--monthname-to-number (match-string 3 arg)))
              (year (string-to-number (match-string 4 arg)))
              (hour (string-to-number (match-string 6 arg)))
              (minute (string-to-number (match-string 7 arg)))
              (seconds (nth 0 dt))
              tz)
          (apply 'encode-time
                 (list seconds minute hour day month year tz)))))))

(defun dts--parse-YBe-time (arg)
  "If ARG is a boolean, then return a regex to match a time string
formatted like: \"2017 November 21\".
Otherwise, ARG is a string, and this function will parse it with that regex, and
returns the time in emacs internal time format, eg (sec-high sec-low).
"
  (let ((regex "\\(\\(19\\|20\\)[0-9]\\{2\\}\\) \\([A-Za-z]\\{3,14\\}\\) +\\([0-9]\\{1,2\\}\\)")
        (dt (decode-time (current-time))))
    (if (booleanp arg)
        regex
      (when (string-match regex arg 0)

        (let ((year (string-to-number (match-string 1 arg)))
              (month (dts--monthname-to-number (match-string 3 arg)))
              (day (string-to-number (match-string 4 arg)))
              (hour (nth 2 dt))
              (minute (nth 1 dt))
              (seconds (nth 0 dt))
              tz)
          (apply 'encode-time
                 (list seconds minute hour day month year tz)))))))

(defun dts--parse-HMS-time (arg)
  "If ARG is a boolean, then return a regex to match a time string
formatted like: \"14:32:33\".
Otherwise, ARG is a string, and this function will parse it with that regex, and
returns the time in emacs internal time format, eg (sec-high sec-low).
"
  (let ((regex "\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)" )
        (dt (decode-time (current-time))))

    (if (booleanp arg)
        regex
      (when (string-match regex arg 0)

        (let ((hour (string-to-number (match-string 1 arg)))
              (minute (string-to-number (match-string 2 arg)))
              (seconds (string-to-number (match-string 3 arg)))
              (year (nth 5 dt))
              (month (nth 4 dt))
              (day (nth 3 dt))
              tz)
          (apply 'encode-time
                 (list seconds minute hour day month year tz)))))))

(defun dts--parse-YmdHMS-time (arg)
  "If ARG is a boolean, then return a regex to match a time string
formatted like: \"2019-02-12T14:32:33\".
Otherwise, ARG is a string, and this function will parse it with that regex, and
returns the time in emacs internal time format, eg (sec-high sec-low).
"
  (let ((regex "\\(\\(19\\|20\\)[0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)T\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)" )
        (dt (decode-time (current-time))))

    (if (booleanp arg)
        regex
      (when (string-match regex arg 0)
        (let ((year (string-to-number (match-string 1 arg)))
              (month (string-to-number (match-string 2 arg)))
              (day (string-to-number (match-string 3 arg)))
              (hour (string-to-number (match-string 4 arg)))
              (minute (string-to-number (match-string 5 arg)))
              (seconds (string-to-number (match-string 6 arg)))
              tz)
          (apply 'encode-time
                 (list seconds minute hour day month year tz)))))))



(defun dts--maybe-delete-time-string-looking-forward ()
  "if point is looking forward at a time string, delete it.
Return a number (index of the time-format string found) if an
appropriate string has been found and deleted. Else return nil."
  (interactive)
  (let ((ix 0)
        found)
    (setq dts--just-deleted-string nil)
    (while (and (< ix (length dts--time-formats))
                (not found))
      (let ((tf (nth ix dts--time-formats)))
        (if (looking-at (funcall (cdr tf) t))
            (progn
              (setq found ix
                    dts--just-deleted-string (buffer-substring-no-properties (match-beginning 0) (match-end 0)))
              (delete-region (match-beginning 0) (match-end 0)))
          (setq ix (1+ ix)))))
    found))

(defun dts--maybe-delete-time-string-under-point ()
  "if point is on a time string, delete it.
Return a number (index of the time-format string found) if an
appropriate string has been found and deleted. Else return nil."
  (interactive)
  (save-excursion
    (save-match-data
      (or
       ;; 1. try current position.
       (dts--maybe-delete-time-string-looking-forward)
       ;; 2. try searching back to punctuation.
       (and (re-search-backward dts--punctuation-regex (line-beginning-position) t)
            (progn (forward-char)
                   (dts--maybe-delete-time-string-looking-forward)))
       ;; 3. try searching back to whitespace
       (and (re-search-backward "[[:space:]]" (line-beginning-position) t)
            (progn (forward-char)
                   (dts--maybe-delete-time-string-looking-forward)))
       ;; 4. move back to BOL, try there.
       (progn
         (beginning-of-line)
         (dts--maybe-delete-time-string-looking-forward))))))

(defun dts--time-is-within-seconds (the-time delta-seconds)
  "Returns t if THE-TIME is less than DELTA-SECONDS ago."
  (< (float-time (time-subtract (current-time) the-time)) delta-seconds))

(defun dts/insert-timeofday (&optional arg)
  "Inserts a string representing the time of day at point.
If the mode is txt-mode, then the format used is like this:
  Tuesday, 20 August 2013, 08:48

Otherwise, the format used is like this:
  20130820-0848

If you invoke the command while point is on a timestamp string, it
will insert an updated stamp using the same format.

If you invoke this command repeatedly, it cycles through additional formats:

   Tuesday, 20 August 2013, 08:48
   2013 August 20
   08:48:03
   2019-02-12T08:48:03Z

Point is placed at the beginning of the newly inserted timestamp.
"
  (interactive "P")
  (cond
   ((or (window-minibuffer-p) (equal major-mode 'wdired-mode) arg)
    (let ((time-format (nth 0 dts--time-formats)))
      (save-excursion
        (insert (format-time-string (car time-format))))))
   (t
    (let ((ix (dts--maybe-delete-time-string-under-point)))
      (if (numberp ix)
          (let ((previous-time (funcall (cdr (nth ix dts--time-formats)) dts--just-deleted-string)))
            (if (and previous-time
                     (not (dts--time-is-within-seconds previous-time 10))) ;; not recent
                (setq ix (1- ix))))) ;; keep same format

      (setq ix (if (numberp ix) (1+ ix)  ;; increment
                 (if (equal major-mode 'text-mode) 1 0))) ;; start at reasonable defaults

      (if (>= ix (length dts--time-formats)) ;; roll-over
          (setq ix 0))

      (let ((orig-point (point)))
        (insert (format-time-string (car (nth ix dts--time-formats))))
        (push-mark)
        (goto-char orig-point))))))

(defun dts/insert-currentTimeMillis ()
  "function to insert the value like java's currentTimeMillis.

On Sunday, 7 December 2025, for some reason, I observed that calling
this interactively was resulting in a 10-13 second delay.  Calling it
non-interactively does not cause this delay. It was a real
puzzle. Defining a new function with the same logic, but a different
name, allowed it to run without delay.  Restarting emacs allowed it to
run without (much) delay. I never solved this but the delay is mostly
gone; now it takes just 1 second or so now. Still that seems long,but
not worth further investigating. This was one of those debugging and
diagnostic efforts where I spent several hours and learned nothing."
  (interactive)
  (let* ((time-float (float-time))
         (millis-float (* time-float 1000.0))
         (millis-int (truncate millis-float)))
    (insert (number-to-string millis-int))
    ))

(provide 'dino-timestamp)
