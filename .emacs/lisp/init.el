;;; init.el --- Emacs initialization for isolated package testing

;; Copyright (C) 2018 Neil Okamoto  https://github.com/gonewest818
;; Copyright (C) 2019, 2024 Ola Nilsson  <ola.nilsson@gmail.com>

;; Copied from https://github.com/gonewest818/elisp-lint where it had
;; no license.  elisp-lint.el is GPLv3 so I assume this is the same
;; for now.

;;; Commentary:

;; Usage: ${EMACS} -q -l $project_root/.emacs/init.el

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set `user-emacs-directory' to avoid overwriting $HOME/.emacs.d
;; See also: https://debbugs.gnu.org/cgi/bugreport.cgi?bug=15539#66

(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (expand-file-name "../" (file-name-directory user-init-file)))
(setq package-user-dir (locate-user-emacs-file "elpa"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure customize data doesn't land in this file

(setq custom-file (concat user-emacs-directory ".emacs-custom.el"))
(when (file-readable-p custom-file) (load custom-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set load-path to include the .el files for the project under development,
;; which reside in the parent of `user-emacs-directory'.  Adding this path
;; permits tests to require this package itself.
(add-to-list 'load-path (expand-file-name ".." user-emacs-directory))
;; so files in test/ can require each other
(add-to-list 'load-path (expand-file-name "../test" user-emacs-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configure melpa and melpa-stable

(require 'package)
(add-to-list 'package-archives
             '("melpa"        . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(setq package-enable-at-startup nil)

;; Make sure all versions of Emacs can load GNU ELPA,
;;  https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341
(when (and (= emacs-major-version 26)
           (< emacs-minor-version 3))
  (message "Disable TLS 1.3\n Broken on Emacs 26.1 and 26.2 as described in\n  https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341 ")
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;; Create the pgp dir so new keys can be installed
(let ((gnupghome-dir (or (bound-and-true-p package-gnupghome-dir)
                         (expand-file-name "gnupg"
                                           package-user-dir))))
  (mkdir gnupghome-dir t))

;; Disable package signature checking iff 25.1 <= emacs-version <
;; 26.3, because gnu-elpa-keyring-update has to be installed with
;; checking disabled.
(let ((package-check-signature (if (and (version< "25" emacs-version)
                                        (version< emacs-version "26.3"))
                                   nil
                                 (bound-and-true-p package-check-signature))))
  (package-initialize)
  (when (not package-archive-contents)
    (package-refresh-contents))
  ;; update the keyring
  (when (fboundp 'package-import-keyring)
    (unless (package-installed-p 'gnu-elpa-keyring-update)
      (let (package-check-signature)
        (package-install 'gnu-elpa-keyring-update)))))

;; For some reason - in some cases - a call to sit-for is required
;; between package-refresh-contents and the first package-install.
;; The actual sleep time can be _very_ short, so it's probably a case
;; of clearing out timers or callbacks before continuing.
(sit-for 0.000001)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions to install dependencies

(require 'cl-lib)

(cl-defun package-install-maybe (pkg
                                 &key min-version
                                 (min-emacs-version "1"))
  "Install PKG, optionally with MIN-VERSION.
MIN-EMACS-VERSION can be set to a version string.  The package
will not be installed unless the current Emacs instance is at
least that version."
  (if (and (version<= min-emacs-version emacs-version)
           (not (package-installed-p pkg min-version)))
      (package-install pkg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load project dependencies from elsewhere

(load (concat user-emacs-directory "dependencies.el"))

;;; init.el ends here
;; Local Variables:
;; no-byte-compile: t
;; End:
