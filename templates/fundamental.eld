fundamental-mode

(today (format-time-string "%Y-%m-%d"))

;; https://github.com/Crandel/tempel-collection/commit/ba2ae39e66804337f604de69df6ae9b34b821e85
(NOW (format-time-string "<%Y-%0m-%0d %a %H:%0M>"))
(yesterday (format-time-string "<%Y-%0m-%0d %a>" (time-subtract nil (* 24 60 60))))
