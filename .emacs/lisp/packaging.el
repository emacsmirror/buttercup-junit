;;; packaging.el --- Tools for generating Emacs elpa packages

;; Copyright (C) 2019 Ola Nilsson  <ola.nilsson@gmail.com>

;;; Commentary:

;; Useful functions for packaging.

;;; Code:

(autoload 'lm-get-package-name "lisp-mnt" "Return package name by looking at the first line.")
(autoload 'lm-header "lisp-mnt" "Return the contents of the header named HEADER.")

(defun package-version ()
  "Return the version listed in the current buffer.
Prefer the `Package-Version' header over `Version'.  Signal an
error if neither is found."
  (or (lm-header "Package-Version")
      (lm-header "Version")
      (error "No Package-Version or Version header found")))

(defun package-basename ()
  "Return the package basename of the current buffer."
  (file-name-sans-extension (lm-get-package-name)))

(defsubst package--name-impl (suffix)
  "Common implementation of `package-file-name' and `package-archive-name'.
SUFFIX - which should not include a leading `.' - is the file
suffix of the returned file name."
  (let ((name (format "%s-%s.%s"
                      (package-basename)
                      (package-version)
                      suffix)))
    (when noninteractive
      (princ (format "%s\n" name)))
    name))

(defun package-file-name ()
  "Return the name of the package.el singlefile package specified by the current file."
  (package--name-impl "el"))

(defun package-archive-name ()
  "Return the name of the package.el multifile archive specified by the current file."
  (package--name-impl "tar"))

(defun generate-description-file ()
  "Generate a foo-pkg.el file for the current buffer."
  (require 'lisp-mnt)
  (require 'package)
  (require 'pp)
  (let* ((name (intern (package-basename)))
         (pkg-file (format "%s-pkg.el" name))
         (version (version-to-list (package-version)))
         (reqs (mapcar (lambda (req)
                         (list (car req) (version-to-list (cadr req))))
                       (read (or (lm-header "Package-Requires") "()"))))
         extras pkg-desc)
    (let ((maintainer (lm-maintainer))
          (authors (lm-authors))
          (keywords (split-string (or (lm-keywords) "")))
          (homepage (lm-homepage)))
      (when maintainer (push (cons :maintainer maintainer) extras))
      (when authors    (push (cons :authors    authors   ) extras))
      (when keywords   (push (cons :keywords   keywords  ) extras))
      (when homepage   (push (cons :homepage   homepage  ) extras))
      )
    (setq pkg-desc (package-desc-create
                    :name name
                    :version version
                    :summary (or (lm-summary) "")
                    :reqs reqs
                    :extras extras))
    (package-generate-description-file pkg-desc pkg-file)
    (with-current-buffer (find-file-noselect pkg-file)
      (pp-buffer))))

(defun generate-readme ()
  "Generate a foo-readme.txt file."
  (require 'lisp-mnt)
  (let ((start (lm-section-start "Commentary")))
    (when start
      (let* ((end (lm-section-end "Commentary"))
             (rm (buffer-substring start end)))
        (with-temp-buffer
          (insert rm)
          (emacs-lisp-mode)
          (goto-char (point-min))
          (delete-region (point-min) (1+ (line-end-position)))
          (uncomment-region (point-min) (point-max))
          (forward-whitespace 1)
          (delete-region (point-min) (point))
          (goto-char (point-max))
          (forward-whitespace -1)
          (delete-region (point) (point-max))
          (princ (buffer-substring (point-min) (point-max)))
          )))))

;;; packaging.el ends here
;; Local Variables:
;; no-byte-compile: t
;; End:
