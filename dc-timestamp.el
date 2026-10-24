;; dc-timestamp.el --- insert timestamp strings -*- lexical-binding: t; -*-

;;; Commentary:
;; utility functions for inserting timestamps into files, while iterating through
;; various formats.  YYYYMMDD, Long form, RFC8601, etc

;;; Code:


(defvar dc-ts--just-deleted-string nil
  "holder of the string most recently deleted by
`dc-ts--maybe-delete-time-string-looking-forward'. Used
to determine if we need to rotate through various formats.")

(defconst dc-ts--punctuation-regex "[\\!\?\"\.'#$%&*+/;<=>@^`|~]"
  "regexp for punctuation")

(defconst dc-ts--monthnames-and-numbers '(("jan" . 1) ("feb" . 2) ("mar" . 3)
                                        ("apr" . 4) ("may" . 5) ("jun" . 6)
                                        ("jul" . 7) ("aug" . 8) ("sep" . 9)
                                        ("oct" . 10) ("nov" . 11) ("dec" . 12)
                                        ("january" . 1) ("february" . 2)
                                        ("march" . 3) ("april" . 4) ("june" . 6)
                                        ("july" . 7) ("august" . 8)
                                        ("september" . 9) ("october" . 10)
                                        ("november" . 11) ("december" . 12)))

;; for each
(defconst dc-ts--time-formats '(("%Y%m%d-%H%M"         . dc-ts--parse-YYYYMMDDHHMM-time)
                              ("%A, %e %B %Y, %H:%M" . dc-ts--parse-rfc822-time)
                              ("%Y %B %e"            . dc-ts--parse-YBe-time)
                              ("%H:%M:%S"            . dc-ts--parse-HMS-time)
                              ("%Y-%m-%dT%H:%M:%S"   . dc-ts--parse-YmdHMS-time)
                              )
  "A list of time formats with corresponding parse functions to use in
`dc-ts/insert-timeofday' and `dc-ts--maybe-delete-time-string-looking-forward' ")

;; (setq dino-time-formats '(
;;                              ("%Y%m%d-%H%M" . dino-parse-YYYYMMDDHHMM-time)
;;                              ("%A, %e %B %Y, %H:%M" . dino-parse-rfc822-time)
;;                              ("%Y %B %e" . dino-parse-ymd-time)
;;                              ("%H:%M:%S" . dino-parse-hms-time)
;;                              ))

(defun dc-ts--parse-YYYYMMDDHHMM-time (arg)
  "If ARG is a boolean, then return a regex to match a time string
in format YYYYMMDD-HHMM. Example: \"20130820-0848\". Otherwise, ARG is a
string; parse it with that regex, and return the time in emacs internal
time format, eg (sec-high sec-low).
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

(defun dc-ts--monthname-to-number (monthname)
  "Maps a monthname to a number, starting with 1 for January.
For invalid monthnames, returns nil."
  (cdr (assoc-string (downcase monthname) dc-ts--monthnames-and-numbers)))

(defun dc-ts--parse-rfc822-time (arg)
  "If ARG is a boolean, then return a regex to match a time string
formatted like: \"Tuesday, 21 November 2017, 12:42\". Otherwise,
ARG is a string; parse it with that regex, and return the time in
emacs internal time format, eg (sec-high sec-low).
"
  (let ((regex
         "\\(Sunday\\|Monday\\|Tuesday\\|Wednesday\\|Thursday\\|Friday\\|Saturday\\), +\\([0-9]\\{1,2\\}\\) \\([A-Za-z]\\{3,14\\}\\) \\(\\(19\\|20\\)[0-9]\\{2\\}\\), \\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)")
        (dt (decode-time (current-time))))
    (if (booleanp arg)
        regex
      (when (string-match regex arg 0)

        (let ((day (string-to-number (match-string 2 arg)))
              (month (dc-ts--monthname-to-number (match-string 3 arg)))
              (year (string-to-number (match-string 4 arg)))
              (hour (string-to-number (match-string 6 arg)))
              (minute (string-to-number (match-string 7 arg)))
              (seconds (nth 0 dt))
              tz)
          (apply 'encode-time
                 (list seconds minute hour day month year tz)))))))

(defun dc-ts--parse-YBe-time (arg)
  "If ARG is a boolean, then return a regex to match a time string
formatted like: \"2017 November 21\". Otherwise, ARG is a string;
parse it with that regex, and return the time in emacs internal time
format, eg (sec-high sec-low).
"
  (let ((regex "\\(\\(19\\|20\\)[0-9]\\{2\\}\\) \\([A-Za-z]\\{3,14\\}\\) +\\([0-9]\\{1,2\\}\\)")
        (dt (decode-time (current-time))))
    (if (booleanp arg)
        regex
      (when (string-match regex arg 0)

        (let ((year (string-to-number (match-string 1 arg)))
              (month (dc-ts--monthname-to-number (match-string 3 arg)))
              (day (string-to-number (match-string 4 arg)))
              (hour (nth 2 dt))
              (minute (nth 1 dt))
              (seconds (nth 0 dt))
              tz)
          (apply 'encode-time
                 (list seconds minute hour day month year tz)))))))

(defun dc-ts--parse-HMS-time (arg)
  "If ARG is a boolean, then return a regex to match a time string
formatted like: \"14:32:33\". Otherwise, ARG is a string; parse it
with that regex, and return the time in emacs internal time format,
eg (sec-high sec-low).
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

(defun dc-ts--parse-YmdHMS-time (arg)
  "If ARG is a boolean, then return a regex to match a time string
formatted like: \"2019-02-12T14:32:33\". Otherwise, ARG is a string;
parse it with that regex, and return the time in emacs internal time
format, eg (sec-high sec-low).
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



(defun dc-ts--maybe-delete-time-string-looking-forward ()
  "If point is looking forward at a time string, according to
any of the formats in `dc-ts--time-formats', delete it, and return a
number that represents the index of the time-format string found. Else
return nil.

See also `dc-ts--maybe-delete-time-string-under-point'."
  (interactive)
  (let ((ix 0)
        found)
    (setq dc-ts--just-deleted-string nil)
    (while (and (< ix (length dc-ts--time-formats))
                (not found))
      (let ((tf (nth ix dc-ts--time-formats)))
        (if (looking-at (funcall (cdr tf) t))
            (progn
              (setq found ix
                    dc-ts--just-deleted-string (buffer-substring-no-properties (match-beginning 0) (match-end 0)))
              (delete-region (match-beginning 0) (match-end 0)))
          (setq ix (1+ ix)))))
    found))

(defun dc-ts--maybe-delete-time-string-under-point ()
  "if point is on a time string, according to
any of the formats in `dc-ts--time-formats', delete it, and return a
number representing the index of the time-format string found.  Else
return nil.

See also `dc-ts--maybe-delete-time-string-looking-forward'"
  (interactive)
  (save-excursion
    (save-match-data
      (or
       ;; 1. try current position.
       (dc-ts--maybe-delete-time-string-looking-forward)
       ;; 2. try searching back to punctuation.
       (and (re-search-backward dc-ts--punctuation-regex (line-beginning-position) t)
            (progn (forward-char)
                   (dc-ts--maybe-delete-time-string-looking-forward)))
       ;; 3. try searching back to whitespace
       (and (re-search-backward "[[:space:]]" (line-beginning-position) t)
            (progn (forward-char)
                   (dc-ts--maybe-delete-time-string-looking-forward)))
       ;; 4. move back to BOL, try there.
       (progn
         (beginning-of-line)
         (dc-ts--maybe-delete-time-string-looking-forward))))))

(defun dc-ts--time-is-within-seconds (the-time delta-seconds)
  "Returns t if THE-TIME is less than DELTA-SECONDS ago."
  (< (float-time (time-subtract (current-time) the-time)) delta-seconds))

(defun dc-ts/insert-timeofday (&optional arg)
  "Inserts a string representing the time of day at point.
If the mode is txt-mode, then the format used is like this:
  Tuesday, 20 August 2013, 08:48

Otherwise, the format used is like this:
  20130820-0848

If you invoke the command while point is on a timestamp string, it
will insert an updated stamp using the same format.

If you invoke this command repeatedly, it cycles through additional
formats, eg:

   Tuesday, 20 August 2013, 08:48
   2013 August 20
   08:48:03
   2019-02-12T08:48:03Z

It will continue to cycle. Point is placed at the beginning of the newly
inserted timestamp.
"
  (interactive "P")
  (cond
   ((or (window-minibuffer-p) (equal major-mode 'wdired-mode) arg)
    (let ((time-format (nth 0 dc-ts--time-formats)))
      (save-excursion
        (insert (format-time-string (car time-format))))))
   (t
    (let ((ix (dc-ts--maybe-delete-time-string-under-point)))
      (if (numberp ix)
          (let ((previous-time (funcall (cdr (nth ix dc-ts--time-formats)) dc-ts--just-deleted-string)))
            (if (and previous-time
                     (not (dc-ts--time-is-within-seconds previous-time 10))) ;; not recent
                (setq ix (1- ix))))) ;; keep same format

      (setq ix (if (numberp ix) (1+ ix)  ;; increment
                 (if (equal major-mode 'text-mode) 1 0))) ;; start at reasonable defaults

      (if (>= ix (length dc-ts--time-formats)) ;; roll-over
          (setq ix 0))

      (let ((orig-point (point)))
        (insert (format-time-string (car (nth ix dc-ts--time-formats))))
        (push-mark)
        (goto-char orig-point))))))

(defun dc-ts/insert-currentTimeMillis ()
  "Insert the value like java's currentTimeMillis.

On Sunday, 7 December 2025, for some reason, I observed that calling
this interactively was resulting in a 10-13 second delay.  Calling it
non-interactively does not cause this delay. It was a real
puzzle. Defining a new function with the same logic, but a different
name, allowed it to run without delay.  Restarting emacs allowed it to
run without (much) delay. I never solved this but the delay seems to be
gone; now it takes just 1 second or so now. Still that seems long, but
not worth further investigating. This was one of those debugging and
diagnostic efforts where I spent several hours and learned nothing."
  (interactive)
  (let* ((time-float (float-time))
         (millis-float (* time-float 1000.0))
         (millis-int (truncate millis-float)))
    (insert (number-to-string millis-int))
    ))

(provide 'dc-timestamp)
