(defun my-minutes-set-file-name nil
  (interactive)
  (let ((fn (if (boundp 'my-org-minutes-file-name)
		my-org-minutes-file-name
	      "output.txt")))
    (org-entry-put nil "EXPORT_FILE_NAME" (concat (format-time-string "%Y%m%d") fn))))

(defun my-minutes-get-file-name nil
  (org-entry-get nil "EXPORT_FILE_NAME" 'selective))

(defun my-minutes-save nil
  (interactive)
  (save-excursion
    (org-mark-subtree)
    (let ((fn (my-minutes-get-file-name)))
        (if (null fn) (message "EXPORT_FILE_NAME property is not set")
          (write-region (region-beginning) (region-end) fn nil nil nil 'excl)))))
(provide 'my-org)
