(defgroup diff-dired nil
  "Diff Dired buffers."
  :group 'tools)

(defface diff-dired-equal
  '((t (:inherit default :background "pale green")))
  "Face for files equal in both buffers."
  :group 'diff-dired)

(defface diff-dired-different
  '((t (:inherit default :background "salmon")))
  ""
  :group 'diff-dired)

(defface diff-dired-orphan
  '((t (:inherit default :background "white smoke")))
  ""
  :group 'diff-dired)

(defun diff-dired (buf1 buf2)
  (interactive "bCompare buffer: \ni")
  ;;(interactive "bbuffer: \nbbuffer: ")
  (setq buf2 (read-buffer "with buffer: " (buffer-name (other-buffer (current-buffer) t)) t))
  ;;(setq buf2 (read-buffer "with buffer: " (buffer-name (last-buffer buf1 t)) t))
  (let ((done (make-hash-table)))
    (diff-dired-check-buffer buf1 buf2 done)
    (diff-dired-check-buffer buf2 buf1 done)
    )
  )

(defun diff-dired-check-buffer (buf1 buf2 done)
  (set-buffer buf1)
  (goto-char (point-min))
  (dired-goto-next-nontrivial-file)
  (while (setq file1 (dired-get-filename nil t))
    (message file1)
    (let* ((basename (file-name-nondirectory file1))
           (hash (gethash basename done)))
      (diff-dired-color
       file1
       (cond (hash)
             (t (diff-dired-get-color file1 buf2 basename done)))))
    (dired-next-line 1)))

(defun diff-dired-get-color(file1 buf2 basename done)
  (let* ((dir2 (with-current-buffer buf2 default-directory))
         (file2 (expand-file-name basename dir2)))
    ;;(message file2)
    (cond
     ((file-exists-p file2)
      (let* ((outbuf (get-buffer-create "diff-output"))
             (diff (call-process "diff" nil outbuf nil "-s" file1 file2)))
        (puthash basename
                 (if (= 0 diff) 'diff-dired-equal 'diff-dired-different)
                 done)))
     (t 'diff-dired-orphan))))

(defun diff-dired-color(file color)
  ;;(message "%s" color)
  (let ((inhibit-read-only t))
    (message "%s %s %s" (point) (line-end-position)  color)
    (put-text-property (point) (line-end-position)
                       (if font-lock-mode 'font-lock-face 'face) color))
  )

;;(diff-dired (get-buffer "priv|home0012b") (get-buffer "priv|rotter"))
