;;; buttercup-junit.el --- JUnit reporting for Buttercup -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2018  Ola Nilsson

;; Author: Ola Nilsson <ola.nilsson@gmail.com>
;; Maintainer: Ola Nilsson <ola.nilsson@gmail.com>
;; Created: Oct 2, 2016
;; Keywords: tools test unittest buttercup ci
;; Version: 0.6.0
;; Package-Requires: ((emacs "24.3") (buttercup "20170929.512"))
;; URL: http://bitbucket.org/olanilsson/buttercup-junit

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Print buttercup test results to JUnit XML report files.
;;
;; `emacs -batch -L . -f package-initialize -f buttercup-junit-run-discover [buttercup-options]'
;;
;; buttercup-junit-run-discover can be configured with the following
;; command line options:
;;
;;  --xmlfile FILE    Write JUnit report to FILE
;;  --junit-stdout    Write JUnit report to stdout.
;;                    The report file will also be written.
;;  --outer-suite     Add a wrapping testsuite around the outer suites.
;;
;; buttercup tests are grouped into descriptions, and descriptions can
;; be contained in other descriptions creating a tree structure where
;; the tests are leafs.  buttercup-junit will output a testsuite for
;; each buttercup description and a testcase for each `it' testcase.
;; Pending tests will be marked as skipped in the report.

;;; Code:

(require 'pcase)
(require 'cl-lib)
(require 'xml)
(require 'buttercup)

(defgroup buttercup-junit nil
  "buttercup-junit customizations."
  :group 'buttercup)

(defcustom buttercup-junit-result-file "results.xml"
  "Default result file for buttercup-junit."
  :group 'buttercup-junit
  :type 'string)

(defvar buttercup-junit--buffer nil
  "Buffer for buttercup-junit result file.")

(defvar buttercup-junit--indent-level 0
  "Current report indent level.")
(make-variable-buffer-local 'buttercup-junit--indent-level)

(defvar buttercup-junit--state-stack nil
  "Stack for storing state between started and done events.")
(make-variable-buffer-local 'buttercup-junit--state-stack)

(defvar buttercup-junit--to-stdout nil
  "Whether to print the xml file to stdout as well.")

(defvar buttercup-junit-master-suite nil
  "Name of wrapping test suite.
An extra outer testsuite with this name is added to the report if
`buttercup-junit-master-suite' is set to a non-empty string.")

(defsubst buttercup-junit--nonempty-string-p (object)
  "Return non-nil if OBJECT is a non-empty string."
  (and (stringp object) (not (string= object ""))))

(defun buttercup-junit--extract-argument-option (option)
  "Return the item following OPTION in `command-line-args-left'.
OPTION is tyically a string `--option' that should be followed by
a mandatory option argument.  All pairs of `OPTION argument' will
be removed from `command-line-args-left', and the argument of the
last pari will be returned.  Throws an error If OPTION is found
as the last item in `command-line-args-left'."
  (let ((option-elt (member option command-line-args-left))
		argument)
	(while option-elt
	  (unless (cdr option-elt)
		(error "Option %s requires an argument" option))
	  (setq argument (cadr option-elt))
	  (setcdr option-elt (cddr option-elt))
	  (setq command-line-args-left
			(cl-remove option command-line-args-left :test #'string= :count 1))
	  (setq option-elt (member option command-line-args-left)))
	argument))

(defun buttercup-junit--option-set (option)
  "Check `command-line-args-left' for OPTION.  Remove any found."
  (prog1 (member option command-line-args-left)
	(setq command-line-args-left (remove option command-line-args-left))))

(defmacro with-buttercup-junit-reporter (&rest body)
  "Execute BODY with necessary variables bound.
This macro is used to wrap calls to buttercup with relevant
variables set."
  (declare (debug t) (indent defun))
  `(let ((buttercup-junit-result-file
          (or (buttercup-junit--extract-argument-option "--xmlfile")
              buttercup-junit-result-file))
         (buttercup-junit--to-stdout
          (or (buttercup-junit--option-set "--junit-stdout")
              buttercup-junit--to-stdout))
         (buttercup-junit-master-suite
          (or (buttercup-junit--extract-argument-option "--outer-suite")
              buttercup-junit-master-suite))
         (buttercup-reporter #'buttercup-junit-reporter))
     ,@body))

;;;###autoload
(defun buttercup-junit-at-point (&optional outer)
  "Execute `buttercup-run-at-point' with `buttercup-junit-reporter' set.
The JUnit report will be written to thte file specified by
`buttercup-junit-result-file'.  If OUTER or
`buttercup-junit-master-suite' is a non-empty string, a wrapper
testsuite of that name will be added."
  (interactive "souter: ")
  (let ((command-line-args-left (list "--xmlfile" buttercup-junit-result-file)))
	(when outer
	  (setq command-line-args-left (append command-line-args-left
										   (list "--outer-suite" outer))))
    (with-buttercup-junit-reporter
      (buttercup-run-at-point))))

;;;###autoload
(defun buttercup-junit-run-discover ()
  "Execute `buttercup-run-discover' with `buttercup-junit-reporter' set.
The JUnit report will be written to the file specified by
`buttercup-junit-result-file', and to stdout if
`buttercup-junit-to-stdout' is non-nil.  If
`buttercup-junit-master-suite' is set a wrapper testsuite of that
name will be added.  These variables can be overriden by the
options `--xmlfile XMLFILE', `--junit-stdout', and `--outer-suite
SUITE' in `commandline-args-left'."
  (with-buttercup-junit-reporter
    (buttercup-run-discover)))

;;;###autoload
(defun buttercup-junit-run-markdown-buffer (&rest markdown-buffers)
  "Execute `buttercup-run-markdown-buffer' with `buttercup-junit-reporter'.
MARKDOWN-BUFFERS is passed to `buttercup-run-markdown-buffer'.
The JUnit report will be written to the file specified by
`buttercup-junit-result-file', and to stdout if
`buttercup-junit-to-stdout' is non-nil.  If
`buttercup-junit-master-suite' is set a wrapper testsuite of that
name will be added.  These variables can be overriden by the
options `--xmlfile XMLFILE', `--junit-stdout', and `--outer-suite
SUITE' in `commandline-args-left'."
  (interactive)
  (with-buttercup-junit-reporter
    (apply #'buttercup-run-markdown-buffer markdown-buffers)))

;;;###autoload
(defun buttercup-junit-run-markdown ()
  "Execute `buttercyp-run-markdown' with `buttercup-junit-reporter'."
  (with-buttercup-junit-reporter
    (buttercup-run-markdown)))

;;;###autoload
(defun buttercup-junit-run-markdown-file (file)
  "Pass FILE to `buttercup-run-markdown-file' using `buttercup-junit-reporter'."
  (interactive "fMarkdown file: ")
  (with-buttercup-junit-reporter
    (buttercup-run-markdown-file file)))

;;;###autoload
(defun buttercup-junit-run ()
  "Execute `buttercup-run' with `buttercup-junit-reporter'."
  (interactive)
  (with-buttercup-junit-reporter
    (buttercup-run)))

(defsubst buttercup-junit--insert-at (marker &rest insert-args)
  "Go to MARKER, disable MARKER, and `insert' INSERT-ARGS."
  (goto-char marker)
  (setq marker nil)
  (apply #'insert insert-args))

(defun buttercup-junit--open-testsuite (suite)
  "Insert the opening tag of the testsuite element for SUITE.
SUITE is a `buttercup-suite' struct."
  (buttercup-junit--open-testsuite-impl (buttercup-suite-description suite) (list suite)))

(defun buttercup-junit--open-outer-testsuite (name inner-suites)
  "Insert the opening tag of the fake outer testsuite NAME.
INNER-SUITES is a list of `buttercup-suite' structs for all the
suites that will run."
  (buttercup-junit--open-testsuite-impl name inner-suites))

(defun buttercup-junit--escape-string (string)
  "Convert STRING into a string containing valid XML character data.
Convert all non-printable characters in string to a `^A'
sequence, then pass the result to `xml-escape-string'."
  (xml-escape-string
   (with-temp-buffer
	 (insert string)
	 (goto-char 1)
	 (while (not (eobp))
	   (if (aref printable-chars (char-after))
		   (forward-char)
		 (insert (format "^%c" (+ (char-after) ?A -1)))
		 (delete-char 1)))
	 (buffer-string))))

(defun buttercup-junit--open-testsuite-impl (name suites)
    "Insert the opening tag of testsuite NAME.
SUITES is a list of `buttercup-suite' structs for all the
suites that will run."
  (let (failures errors time)
    (insert
     (make-string buttercup-junit--indent-level ?\s)
     (format "<testsuite name=\"%s\" timestamp=\"%s\" hostname=\"%s\" tests=\"%d\" failures=\""
             (buttercup-junit--escape-string name)
             (format-time-string
              "%Y-%m-%d %T%z"
              (buttercup-suite-or-spec-time-started (car suites))) ; timestamp
             (buttercup-junit--escape-string (system-name)) ;hostname
             (buttercup-suites-total-specs-defined suites)))
	(setq failures (point-marker))
	(insert "\" errors=\"")
	(setq errors (point-marker))
	(insert "\" time=\"")
	(setq time (point-marker))
	(insert (format "\" skipped=\"%d\" >"
					(buttercup-suites-total-specs-pending suites))
			"\n")
	(cl-incf buttercup-junit--indent-level)
    (push (list name suites failures errors time)
          buttercup-junit--state-stack)))

(defun buttercup-junit--close-testsuite (suite)
  "Insert the closing tag of the testsuite SUITE."
  (buttercup-junit--close-testsuite-impl (buttercup-suite-description suite) (list suite)))

(defun buttercup-junit--close-outer-testsuite (name suites)
  "Insert the closing tag of the fake outer testsuite NAME.
SUITES is a list of `buttercup-suite' structs for all the suites
that will run."
  (buttercup-junit--close-testsuite-impl name suites))

(defun buttercup-junit--close-testsuite-impl (name suites)
  "Insert the closing tag of the testsuite NAME.
SUITES is a list of `buttercup-suite' structs for all the suites
that will run."
  (cl-decf buttercup-junit--indent-level)
  (cl-destructuring-bind (orig-name orig-suites failures errors time)
	  (pop buttercup-junit--state-stack)
	(ignore orig-suites)
	(unless (string= name orig-name) (error "Corrupted buttercup-junit--state-stack"))
	(save-excursion
	  (buttercup-junit--insert-at failures
								  (number-to-string (buttercup-suites-total-specs-failed suites)))
	  (buttercup-junit--insert-at errors
	                              (number-to-string (buttercup-suites-total-specs-status suites 'error)))
      (buttercup-junit--insert-at
       time
       (format "%f" (float-time
                     (cl-reduce #'time-add
                                (mapcar #'buttercup-elapsed-time suites)))))))
  (insert (make-string buttercup-junit--indent-level ?\s)
		  "</testsuite>\n"))

(defun buttercup-junit--testcase (spec)
  "Print a `testcase' xml element for SPEC to the current buffer."
  (insert (make-string buttercup-junit--indent-level ?\s)
          "<testcase name=\""
          (buttercup-junit--escape-string (buttercup-spec-description spec))
          "\" classname=\"buttercup\" time=\""
          (format "%f" (float-time (buttercup-elapsed-time spec)))
          "\">")
  (cl-incf buttercup-junit--indent-level)
  (pcase (buttercup-spec-status spec)
    (`failed
     (let ((desc (buttercup-spec-failure-description spec))
           (stack (buttercup-spec-failure-stack spec))
           tag message type)
       (cond ((stringp desc) (setq tag "failed"
                                   message desc
                                   ;; TODO: find a proper value for type
                                   type "type"))
             ((eq (car desc) 'error)
              (setq tag "error"
                    message (pp-to-string (cadr desc))
                    type (symbol-name (car desc)))
              (setf (buttercup-spec-status spec) 'error))
             (t (setq tag "failed"
                      message (pp-to-string desc)
                      type "unknown")))
       (insert "\n" (make-string buttercup-junit--indent-level ?\s)
               "<" tag " message=\""
               (buttercup-junit--escape-string message) "\""
               " type=\"" (buttercup-junit--escape-string type) "\">"
               "Traceback (most recent call last):\n")
       (dolist (frame stack)
         (insert (buttercup-junit--escape-string (format "  %S" (cdr frame)))
                 "\n"))
       (insert "</" tag ">\n")))
    (`pending
     (insert "\n" (make-string buttercup-junit--indent-level ?\s)
             "<skipped/>\n")))
  (cl-decf buttercup-junit--indent-level)
  (insert (if (bolp) (make-string buttercup-junit--indent-level ?\s) "")
          "</testcase>\n"))

;;;###autoload
(defun buttercup-junit-reporter (event arg)
  "Insert JUnit tags into the `*junit*' buffer according to EVENT and ARG.
See `buttercup-reporter' for documentation on the values of EVENT
and ARG.  A new output buffer is created on the
`buttercup-started' event, and its contents are written to
`buttercup-junit-result-file' and possibly stdout on the
`buttercup-done' event."
  (when (eq event `buttercup-started)
	 (setq buttercup-junit--buffer (generate-new-buffer (generate-new-buffer-name "*junit*"))))
  (with-current-buffer buttercup-junit--buffer
	(pcase event
      ;; buttercup-started -- The test run is starting. The argument is
      ;; a list of suites this run will execute.
	  (`buttercup-started
	   (set-buffer-file-coding-system 'utf-8)
	   (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
			   "<testsuites>\n")
	   (cl-incf buttercup-junit--indent-level)
	   (when (buttercup-junit--nonempty-string-p buttercup-junit-master-suite)
		 (buttercup-junit--open-outer-testsuite buttercup-junit-master-suite arg)))
	  ;; suite-started -- A suite is starting. The argument is the suite.
	  ;;  See `make-buttercup-suite' for details on this structure.
	  (`suite-started
	   (buttercup-junit--open-testsuite arg))
	  ;; spec-started -- A spec is starting. The argument is the spec.
	  ;;   See `make-buttercup-spec' for details on this structure.
      (`spec-started) ; ignored
      ;; spec-done -- A spec has finished executing. The argument is the spec.
      (`spec-done (buttercup-junit--testcase arg))
	  ;; suite-done -- A suite has finished. The argument is the spec.
	  (`suite-done
	   (buttercup-junit--close-testsuite arg))
	  ;; buttercup-done -- All suites have run, the test run is over.")
	  (`buttercup-done
	   (when (buttercup-junit--nonempty-string-p buttercup-junit-master-suite)
		 (buttercup-junit--close-outer-testsuite buttercup-junit-master-suite arg))
	   (cl-decf buttercup-junit--indent-level)
	   (insert (make-string buttercup-junit--indent-level ?\s)
			   "</testsuites>\n")
	   (when buttercup-junit--to-stdout
		 (send-string-to-terminal (buffer-string)))
	   (when buttercup-junit-result-file
		 (write-file buttercup-junit-result-file))
	   (unless (zerop (+ (buttercup-suites-total-specs-failed arg)
                         (buttercup-suites-total-specs-status arg 'error)))
		 (buttercup-junit--exit-code))
	   ))))

(defun buttercup-junit--exit-code ()
  "Signal error so script return value is failed.
This function exists only for testability reasons."
	(error ""))

(provide 'buttercup-junit)
;;; buttercup-junit.el ends here
;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; tab-width: 4
;; indent-tabs-mode: nil
;; End:
