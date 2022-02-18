(provide 'weight-helper)
(defun format-datetime ()
  (format-time-string "%Y-%m-%d %H:%M"))

(defun weight-insert (weight)
  "Insert weight with timestamp"
  (interactive "nWeight: ")
  (let
      ((time (format-datetime)))
    (goto-char (point-max))
    (insert time "," (format "%.2f" weight) "\n")))
(setq weight-file (expand-file-name "~/Memo/weight.csv"))
(defun weight-insert-default-file (weight)
  "Insert weight with timestamp to file"
  (interactive "nWeight: ")
  (if (boundp 'weight-file)
    (let
	((time (format-datetime)))
      (save-excursion
	(find-file weight-file)
	(goto-char (point-max))
	(insert time "," (format "%.2f" weight) "\n")
	(save-buffer)))))
