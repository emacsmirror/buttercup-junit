;;; buttercup-junit.el --- JUnit reporting for Buttercup -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Ola Nilsson

;; Author: Ola Nilsson <ola.nilsson@gmail.com>
;; Maintainer: Ola Nilsson <ola.nilsson@gmail.com>
;; Created: Oct 2, 2016
;; Keywords: tools test unittest buttercup ci
;; Version: 0.3.0
;; Package-Requires: ((buttercup "1.5"))
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
;; emacs -batch -L . -f package-initialize -f buttercup-junit-run-discover [buttercup-options]
;;
;; buttercup-junit-run-discover can be configured with the following command line options:
;;
;;  --xmlfile FILE    Write JUnit report to FILE
;;  --junit-stdout    Write JUnit report to stdout.  The report file will also be written.
;;  --outer-suite     Add a wrapping testsuite around the outer suites.
;;
;; buttercup tests are grouped into descriptions, and descriptions can
;; be contained in other descriptions creating a tree structure where
;; the tests are leafs.  buttercup-junit will output a testsuite for
;; each buttercup description and a testcase for each `it' testcase.
;; Pending tests will be marked as skipped in the report.

;;; Code:

(require 'pcase)
(or (require 'cl-lib nil t) (require 'cl))
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

(defvar buttercup-junit--state-stack nil
  "Stack for storing state between started and done events.")

(defvar buttercup-junit--to-stdout nil
  "Whether to print the xml file to stdout as well.")

(defvar buttercup-junit-master-suite nil
  "An extra outer testsuite with this name is added to the report
  if `buttercup-junit-master-suite' is set to a non-empty
  string.")

(defsubst buttercup-junit--nonempty-string-p (object)
  "Return t if OBJECT is a non-empty string. "
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
			(remove* option command-line-args-left :test #'string= :count 1))
	  (setq option-elt (member option command-line-args-left)))
	argument))

(defun buttercup-junit--option-set (option)
  "Check `command-line-args-left' for OPTION.  Remove any found."
  (prog1 (member option command-line-args-left)
	(setq command-line-args-left (remove option command-line-args-left))))

(defun buttercup-junit-run-discover ()
  "Execute `buttercup-run-discover' with `buttercup-junit-reporter' set.
The JUnit report will be written to the file specified by
`buttercup-junit-result-file', and to stdout if
`buttercup-junit-to-stdout' is non-nil.  If
`buttercup-junit-master-suite' is set a wrapper testsuite of that
name will be added.  These variables can be overriden by the
options `--xmlfile XMLFILE', `--junit-stdout', and `--outer-suite
SUITE' in `commandline-args-left'"
  (let ((buttercup-junit-result-file (or (buttercup-junit--extract-argument-option "--xmlfile")
										 buttercup-junit-result-file))
		(buttercup-junit--to-stdout (or (buttercup-junit--option-set "--junit-stdout")
										buttercup-junit--to-stdout))
		(buttercup-junit-master-suite (or (buttercup-junit--extract-argument-option "--outer-suite")
										  buttercup-junit-master-suite))
		(buttercup-reporter #'buttercup-junit-reporter))
	(buttercup-run-discover)))

(defun buttercup-junit-at-point (&optional outer)
  (interactive "souter: ")
  (let ((command-line-args-left (list "--xmlfile" buttercup-junit-result-file)))
	(when outer
	  (setq command-line-args-left (append command-line-args-left
										   (list "--outer-suite" outer))))
	(let ((buttercup-junit-result-file (or (buttercup-junit--extract-argument-option "--xmlfile")
										   buttercup-junit-result-file))
		  (buttercup-junit--to-stdout (buttercup-junit--option-set "--junit-stdout"))
		  (buttercup-junit-master-suite (or (buttercup-junit--extract-argument-option "--outer-suite")
											buttercup-junit-master-suite))
		  (buttercup-reporter #'buttercup-junit-reporter))
	   (buttercup-run-at-point))))

(defsubst buttercup-junit--insert-at (marker &rest insert-args)
  "Go to MARKER, disable MARKER, and `insert' INSERT-ARGS."
  (goto-char marker)
  (setq marker nil)
  (apply #'insert insert-args))

(defun buttercup-junit--open-testsuite (suite)
  "Insert the opening tag of the testsuite element for SUITE.
SUITE is a `buttercup-suite' struct.  A list containg SUIT, the
marks for the `failures', `errors', and `time' attribute values,
and the current time is pushed on `buttercup-junit--state-stack.
The tag is indented according to `buttercup-junit--indent-level',
and that level is incremented."
  (let (failures errors time)
	(insert (make-string buttercup-junit--indent-level ?\s)
			(format "<testsuite name=\"%s\" timestamp=\"%s\" hostname=\"%s\" tests=\"%d\" failures=\""
					(buttercup-suite-description suite) ;name
					(format-time-string "%Y-%m-%d %T%z" (current-time)) ; timestamp
					(system-name) ;hostname
					(buttercup-suites-total-specs-defined (list suite))))
	(setq failures (point-marker))
	(insert "\" errors=\"")
	(setq errors (point-marker))
	(insert "\" time=\"")
	(setq time (point-marker))
	(insert (format "\" skipped=\"%d\" >"
					(buttercup-suites-total-specs-pending (list suite)))
			"\n")
	(incf buttercup-junit--indent-level)
	(push (list suite failures errors time (current-time)) buttercup-junit--state-stack)))

(defun buttercup-junit-reporter (event arg)
  "Insert JUnit tags into the `*junit*' buffer according to EVENT and ARG.
See `buttercup-reporter' for documentation on the values of EVENT
and ARG.  A new output buffer is created on the
`buttercup-started' event, and its contents are written to
`buttercup-junit-result-file' and possibly stdout on the
`buttercup-done' event."
  (pcase event
	;;buttercup-started -- The test run is starting. The argument is a list of suites this run will execute.
	(`buttercup-started
	 (setq buttercup-junit--buffer (generate-new-buffer "*junit*"))
	 (with-current-buffer buttercup-junit--buffer
	   (set-buffer-file-coding-system 'utf-8)
	   (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
			   "<testsuites>\n")
	   (incf buttercup-junit--indent-level)
	   (when (buttercup-junit--nonempty-string-p buttercup-junit-master-suite)
		 (insert (make-string buttercup-junit--indent-level ?\s)
				 (format "<testsuite name=\"%s\" timestamp=\"%s\" hostname=\"%s\" tests=\"0\" failures=\"0\" skipped=\"0\">\n"
						 buttercup-junit-master-suite
						 (format-time-string "%Y-%m-%d %T%z" (current-time)) ; timestamp
						 (system-name)))
		 (incf buttercup-junit--indent-level))))
	;; suite-started -- A suite is starting. The argument is the suite.
	;;  See `make-buttercup-suite' for details on this structure.
	(`suite-started
	 (with-current-buffer buttercup-junit--buffer
	   (buttercup-junit--open-testsuite arg)))
	;; spec-started -- A spec is starting. The argument is the spec.
	;;   See `make-buttercup-spec' for details on this structure.
	(`spec-started
	 (with-current-buffer buttercup-junit--buffer
	   (insert (make-string buttercup-junit--indent-level ?\s)
			   "<testcase name=\""
			   (buttercup-spec-description arg) ;name
			   "\" classname=\"buttercup\" time=\"")
	   (push (list arg (point-marker) (current-time)) buttercup-junit--state-stack)
	   (insert "\">")
	   (incf buttercup-junit--indent-level)
	   ))
	;; spec-done -- A spec has finished executing. The argument is the
	;;   spec.
	(`spec-done
	 (with-current-buffer buttercup-junit--buffer
	   (destructuring-bind (orig time start-time) (pop buttercup-junit--state-stack)
		 (unless (eq arg orig) (error "Corrupted stack buttercup-junit--state-stack"))
		 (save-excursion
		   (buttercup-junit--insert-at time
									   (format "%f"
											   (float-time (time-subtract (current-time)
																		  start-time))))))
	   (pcase (buttercup-spec-status arg)
		 (`failed
		  (let ((desc (buttercup-spec-failure-description arg)))
			(cond ((stringp desc)
				   (insert "\n" (make-string buttercup-junit--indent-level ?\s)
						   "<failed message=\"test\" type=\"type\">"
						   desc
						   "</failed>\n"))
				  ((and (listp desc)
						(eq (car desc) 'error))
				   (insert "\n" (make-string buttercup-junit--indent-level ?\s)
						   "<error message=\"test\" type=\"type\">")
				   (pp desc buttercup-junit--buffer)
				   (insert "</error>\n"))
				  )))
		 (`pending
		  (insert "\n" (make-string buttercup-junit--indent-level ?\s)
				  "<skipped/>\n")))
	   (decf buttercup-junit--indent-level)
	   (insert (if (bolp) (make-string buttercup-junit--indent-level ?\s) "") "</testcase>\n")))
	;; suite-done -- A suite has finished. The argument is the spec.
	(`suite-done
	 (decf buttercup-junit--indent-level)
	 (with-current-buffer buttercup-junit--buffer
	   (destructuring-bind (orig failures errors time start-time) (pop buttercup-junit--state-stack)
		 (unless (eq arg orig) (error "Corrupted buttercup-junit--state-stack"))
		 (save-excursion
		   (buttercup-junit--insert-at failures
									   (format "%d" (buttercup-suites-total-specs-failed (list arg))))
		   (buttercup-junit--insert-at errors "0")
		   (buttercup-junit--insert-at time
									   (format "%f" (float-time (time-subtract (current-time) start-time))))))
	   (insert (make-string buttercup-junit--indent-level ?\s)
			   "</testsuite>\n")))
	;; buttercup-done -- All suites have run, the test run is over.")
	(`buttercup-done
	 (with-current-buffer buttercup-junit--buffer
	   (decf buttercup-junit--indent-level)
	   (when (buttercup-junit--nonempty-string-p buttercup-junit-master-suite)
		 (insert (make-string buttercup-junit--indent-level ?\s) "</testsuite>\n")
		 (decf buttercup-junit--indent-level))
	   (insert (make-string buttercup-junit--indent-level ?\s)
			   "</testsuites>\n")
	   (when buttercup-junit--to-stdout
		 (send-string-to-terminal (buffer-string)))
	   (when buttercup-junit-result-file
		 (write-file buttercup-junit-result-file))))))

(provide 'buttercup-junit)
;;; buttercup-junit.el ends here
;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; tab-width: 4
;; End:
