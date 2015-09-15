(defun uuid-gen (cnt) 
  (interactive "p")
  (progn 
    (dotimes (qm cnt nil) 
      (call-process "uuidgen" nil (current-buffer) (thing-at-point 'word) "-t"))))

(provide 'uuid-gen)
