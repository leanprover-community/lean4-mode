(require 'lean4-util)

(defun lean4-lake-find-dir-in (dir)
  (when dir
    (or (when (f-exists? (f-join dir "lakefile.lean")) dir)
	(lean4-lake-find-dir-in (f-parent dir)))))

(defun lean4-lake-find-dir ()
  (and (buffer-file-name)
       (lean4-lake-find-dir-in (f-dirname (buffer-file-name)))))

(defun lean4-lake-find-dir-safe ()
  (or (lean4-lake-find-dir)
      (error (format "cannot find lakefile.lean for %s" (buffer-file-name)))))

(defun lean4-lake-build ()
  "Call lake build"
  (interactive)
  (let* ((default-directory (file-name-as-directory (lean4-lake-find-dir-safe))))
    (with-existing-directory
      (compile (concat (lean4-get-executable lean4-lake-name) " build")))))

(provide 'lean4-lake)

