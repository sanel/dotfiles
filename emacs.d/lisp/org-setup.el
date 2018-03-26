(eval-after-load 'org
  '(let* ((v (split-string (org-version) "\\."))
		  (major (string-to-number (first v)))
		  (minor (string-to-number (second v))))
	(when (>= major 8)
	  ;; allow 'CLOCK' with empty timestamp
	  (defun org-element-timestamp-parser-2 ()
		"Parse time stamp at point.

			Return a list whose CAR is `timestamp', and CDR a plist with
			`:type', `:raw-value', `:year-start', `:month-start',
			`:day-start', `:hour-start', `:minute-start', `:year-end',
			`:month-end', `:day-end', `:hour-end', `:minute-end',
			`:repeater-type', `:repeater-value', `:repeater-unit',
			`:warning-type', `:warning-value', `:warning-unit', `:begin',
			`:end' and `:post-blank' keywords.

			Assume point is at the beginning of the timestamp."
		(save-excursion
		  (let* ((begin (point))
				 (activep (eq (char-after) ?<))
				 (raw-value
				  (progn
					(looking-at "\\([<[]\\(%%\\)?.*?\\)[]>]\\(?:--\\([<[].*?[]>]\\)\\)?")
					(match-string-no-properties 0)))
				 (date-start (or (match-string-no-properties 1) ""))
				 (date-end (match-string 3))
				 (diaryp (match-beginning 2))
				 (post-blank (progn (goto-char (match-end 0))
									(skip-chars-forward " \t")))
				 (end (point))
				 (time-range
				  (and (not diaryp)
					   (string-match
						"[012]?[0-9]:[0-5][0-9]\\(-\\([012]?[0-9]\\):\\([0-5][0-9]\\)\\)"
						date-start)
					   (cons (string-to-number (match-string 2 date-start))
							 (string-to-number (match-string 3 date-start)))))
				 (type (cond (diaryp 'diary)
							 ((and activep (or date-end time-range)) 'active-range)
							 (activep 'active)
							 ((or date-end time-range) 'inactive-range)
							 (t 'inactive)))
				 (repeater-props
				  (and (not diaryp)
					   (string-match "\\([.+]?\\+\\)\\([0-9]+\\)\\([hdwmy]\\)"
									 raw-value)
					   (list
						:repeater-type
						(let ((type (match-string 1 raw-value)))
						  (cond ((equal "++" type) 'catch-up)
								((equal ".+" type) 'restart)
								(t 'cumulate)))
						:repeater-value (string-to-number (match-string 2 raw-value))
						:repeater-unit
						(case (string-to-char (match-string 3 raw-value))
						  (?h 'hour) (?d 'day) (?w 'week) (?m 'month) (t 'year)))))
				 (warning-props
				  (and (not diaryp)
					   (string-match "\\(-\\)?-\\([0-9]+\\)\\([hdwmy]\\)" raw-value)
					   (list
						:warning-type (if (match-string 1 raw-value) 'first 'all)
						:warning-value (string-to-number (match-string 2 raw-value))
						:warning-unit
						(case (string-to-char (match-string 3 raw-value))
						  (?h 'hour) (?d 'day) (?w 'week) (?m 'month) (t 'year)))))
				 year-start month-start day-start hour-start minute-start year-end
				 month-end day-end hour-end minute-end)
			;; Parse date-start.
			(unless (or diaryp
						(string= date-start ""))
			  (let ((date (org-parse-time-string date-start t)))
				(setq year-start (nth 5 date)
					  month-start (nth 4 date)
					  day-start (nth 3 date)
					  hour-start (nth 2 date)
					  minute-start (nth 1 date))))
			;; Compute date-end.  It can be provided directly in time-stamp,
			;; or extracted from time range.  Otherwise, it defaults to the
			;; same values as date-start.
			(unless (or diaryp
						(string= date-start ""))
			  (let ((date (and date-end (org-parse-time-string date-end t))))
				(setq year-end (or (nth 5 date) year-start)
					  month-end (or (nth 4 date) month-start)
					  day-end (or (nth 3 date) day-start)
					  hour-end (or (nth 2 date) (car time-range) hour-start)
					  minute-end (or (nth 1 date) (cdr time-range) minute-start))))
			(list 'timestamp
				  (nconc (list :type type
							   :raw-value raw-value
							   :year-start year-start
							   :month-start month-start
							   :day-start day-start
							   :hour-start hour-start
							   :minute-start minute-start
							   :year-end year-end
							   :month-end month-end
							   :day-end day-end
							   :hour-end hour-end
							   :minute-end minute-end
							   :begin begin
							   :end end
							   :post-blank post-blank)
						 repeater-props
						 warning-props)))))

	  (advice-add #'org-clocktable-indent-string :override #'org-clocktable-indent-string-2)

	  ;; bug in 8.2 which displays '\emsp' instead of '\_'
	  (when (= minor 2)
		(defun org-clocktable-indent-string-2 (level)
		  "Return indentation string according to LEVEL.
			LEVEL is an integer.  Indent by two spaces per level above 1."
		  ;(if (= level 1) ""
		  ;	(let ((str " "))
		  ;	  (dotimes (k (1- level) str)
		  ;		(setq str (concat "\\emsp" str))))))
		  (if (= level 1) ""
			 (concat "\\_" (make-string (* 2 (1- level)) ?\s))))
		(advice-add #'org-element-timestamp-parser :override #'org-element-timestamp-parser-2))
	  )))
