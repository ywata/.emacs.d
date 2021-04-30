(provide 'weight-helper)
(defun format-datetime ()
  (format-time-string "%Y-%m-%d %H:%M"))

(defun insert-weight (weight)
  "Insert weight with timestamp"
  (interactive "nWeight: ")
  (let
      ((time (format-datetime)))
    (goto-char (point-max))
    (insert time "," (format "%.2f" weight) "\n")))






