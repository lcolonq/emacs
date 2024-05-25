(defalias 'stdout 'princ)

(require 'calendar)
(require 'solar)
(require 'cal-dst)

(defun lunar-phase (index)
  "Local date and time of lunar phase INDEX.
Integer below INDEX/8 gives the lunation number, counting from
Jan 1, 1900; remainder mod 8 gives the phase: 0 new moon, 1
waxing crescent, 2 first quarter, 3 waxing gibbous, 4 full moon,
5 waning gibbous, 6 last quarter, 7 waning crescent. Returns a
list (DATE TIME PHASE)."
  (let* ((phase (mod index 8))
         (index (/ index 8.0))
         (time (/ index 1236.85))
         (date (+ (calendar-absolute-from-gregorian '(1 0.5 1900))
                  0.75933
                  (* 29.53058868 index) ; FIXME 29.530588853?
                  (* 0.0001178 time time)
                  (* -0.000000155 time time time)
                  (* 0.00033
                     (solar-sin-degrees (+ 166.56
                                           (* 132.87 time)
                                           (* -0.009173 time time))))))
         (sun-anomaly (mod
                       (+ 359.2242
                          (* 29.105356 index)
                          (* -0.0000333 time time)
                          (* -0.00000347 time time time))
                       360.0))
         (moon-anomaly (mod
                        (+ 306.0253
                           (* 385.81691806 index)
                           (* 0.0107306 time time)
                           (* 0.00001236 time time time))
                        360.0))
         (moon-lat (mod
                    (+ 21.2964
                       (* 390.67050646 index)
                       (* -0.0016528 time time)
                       (* -0.00000239 time time time))
                    360.0))
         (adjustment
          (if (memq phase '(0 2))
              (+ (* (- 0.1734 (* 0.000393 time))
                    (solar-sin-degrees sun-anomaly))
                 (* 0.0021 (solar-sin-degrees (* 2 sun-anomaly)))
                 (* -0.4068 (solar-sin-degrees moon-anomaly))
                 (* 0.0161 (solar-sin-degrees (* 2 moon-anomaly)))
                 (* -0.0004 (solar-sin-degrees (* 3 moon-anomaly)))
                 (* 0.0104 (solar-sin-degrees (* 2 moon-lat)))
                 (* -0.0051 (solar-sin-degrees (+ sun-anomaly moon-anomaly)))
                 (* -0.0074 (solar-sin-degrees (- sun-anomaly moon-anomaly)))
                 (* 0.0004 (solar-sin-degrees (+ (* 2 moon-lat) sun-anomaly)))
                 (* -0.0004 (solar-sin-degrees (- (* 2 moon-lat) sun-anomaly)))
                 (* -0.0006 (solar-sin-degrees
                             (+ (* 2 moon-lat) moon-anomaly)))
                 (* 0.0010 (solar-sin-degrees (- (* 2 moon-lat) moon-anomaly)))
                 (* 0.0005 (solar-sin-degrees
                            (+ (* 2 moon-anomaly) sun-anomaly))))
            (+ (* (- 0.1721 (* 0.0004 time))
                  (solar-sin-degrees sun-anomaly))
               (* 0.0021 (solar-sin-degrees (* 2 sun-anomaly)))
               (* -0.6280 (solar-sin-degrees moon-anomaly))
               (* 0.0089 (solar-sin-degrees (* 2 moon-anomaly)))
               (* -0.0004 (solar-sin-degrees (* 3 moon-anomaly)))
               (* 0.0079 (solar-sin-degrees (* 2 moon-lat)))
               (* -0.0119 (solar-sin-degrees (+ sun-anomaly moon-anomaly)))
               (* -0.0047 (solar-sin-degrees (- sun-anomaly moon-anomaly)))
               (* 0.0003 (solar-sin-degrees (+ (* 2 moon-lat) sun-anomaly)))
               (* -0.0004 (solar-sin-degrees (- (* 2 moon-lat) sun-anomaly)))
               (* -0.0006 (solar-sin-degrees (+ (* 2 moon-lat) moon-anomaly)))
               (* 0.0021 (solar-sin-degrees (- (* 2 moon-lat) moon-anomaly)))
               (* 0.0003 (solar-sin-degrees
                          (+ (* 2 moon-anomaly) sun-anomaly)))
               (* 0.0004 (solar-sin-degrees
                          (- sun-anomaly (* 2 moon-anomaly))))
               (* -0.0003 (solar-sin-degrees
                           (+ (* 2 sun-anomaly) moon-anomaly))))))
         (adj (+ 0.0028
                 (* -0.0004 (solar-cosine-degrees
                             sun-anomaly))
                 (* 0.0003 (solar-cosine-degrees
                            moon-anomaly))))
         (adjustment (cond ((= phase 1) (+ adjustment adj))
                           ((= phase 2) (- adjustment adj))
                           (t adjustment)))
         (date (+ date adjustment))
         (date (+ date (/ (- calendar-time-zone
                             (solar-ephemeris-correction
                              (calendar-extract-year
                               (calendar-gregorian-from-absolute
                                (truncate date)))))
                          60.0 24.0)))
         (time (* 24 (- date (truncate date))))
         (date (calendar-gregorian-from-absolute (truncate date)))
         (adj (dst-adjust-time date time)))
    (list (car adj) (apply 'solar-time-string (cdr adj)) phase)))

(defconst lunar-cycles-per-year 12.3685 ; 365.25/29.530588853
  "Mean number of lunar cycles per 365.25 day year.")

(defun lunar-index (date)
  "Return the lunar index for Gregorian date DATE.
This is 8 times the approximate number of new moons since 1 Jan 1900.
The factor of 8 allows (mod INDEX 8) to represent the eight phases."
  (* 8 (truncate
        (* lunar-cycles-per-year
           ;; Years since 1900, as a real.
           (+ (calendar-extract-year date)
              (/ (calendar-day-number date) 366.0)
              -1900)))))

(setq lunar-phase-emojis '(("New" . "🌑")
                     ("Waxing Crescent" . "🌒")
                     ("First Quarter" . "🌓")
                     ("Waxing Gibbous" . "🌔")
                     ("Full" . "🌕")
                     ("Waning Gibbous" . "🌖")
                     ("Last Quarter" . "🌗")
                     ("Waning Crescent" . "🌘")))

(defun lunar-phase-for-date (date)
  "Return the phase of the moon for Gregorian date DATE.
The phase is represented as a (title . emoji) cons cell."
  (let ((offset-from-last-new-moon 0)
	(current-phase-index)
	(current-phase))

    ;; increment the offset until it corresponds to the next phase from now;
    ;; easiest to detect when it's one too many
    (while (calendar-date-compare (lunar-phase (+ offset-from-last-new-moon (lunar-index date)))
				  (list date))
      (setq offset-from-last-new-moon (1+ offset-from-last-new-moon)))

    ;; correct the off-by-one error
    (setq offset-from-last-new-moon (1- offset-from-last-new-moon))

    ;; extract the current phase and look up the corresponding emoji
    (setq current-phase-index (nth 2 (lunar-phase (+ offset-from-last-new-moon (lunar-index date)))))
    (setq current-phase (nth (% (+ 3 current-phase-index) (length lunar-phase-emojis)) lunar-phase-emojis))))

(provide 'moon-phase)
