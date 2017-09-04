;;; test-buttercup-junit.el --- Tests for buttercup-junit.el -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Ola Nilsson

;; Author: Ola Nilsson <ola.nilsson@gmail.com>
;; Keywords:

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

;; 

;;; Code:

(require 'cl-lib)
(require 'buttercup)
(require 'buttercup-junit)
(require 'cl) ; required by esxml
(require 'esxml)
(require 'utils-test-buttercup-junit)

(describe "Calling buttercup-junit-run-discover"
  :var (command-line-args-left buttercup-junit-result-file buttercup-junit--to-stdout)

  (before-each
	(setq command-line-args-left nil)
	(spy-on 'buttercup-run-discover :and-call-fake (lambda () command-line-args-left)))

  (after-each
	(spy-calls-reset 'buttercup-run-discover))

  (it "Calls buttercup-run-discover"
	(buttercup-junit-run-discover)
	(expect 'buttercup-run-discover :to-have-been-called))
  
  (it "Calls buttercup-run-discover with redirected buttercup-reporter"
	(spy-on 'buttercup-run-discover :and-call-fake (lambda () buttercup-reporter))
	(expect (buttercup-junit-run-discover) :to-be #'buttercup-junit-reporter))
  
  (it "Passes command line argument to buttercup-run-discover"
	(let* ((command-line-args-left '("foo" "bar" "baz"))
		   (before (cl-copy-list command-line-args-left)))
	  (expect (buttercup-junit-run-discover) :to-have-same-items-as before)
	  (expect 'buttercup-run-discover :to-have-been-called)))

  (it "Fails if given an --xmlfile option without argument"
	(setq command-line-args-left '("foo" "bar" "baz" "--xmlfile"))
	(expect (buttercup-junit-run-discover) :to-throw 'error)
	(expect 'buttercup-run-discover :not :to-have-been-called))
  
  (describe "Removes --xmlfile FILE from command-line-args-left before calling buttercup-run-discover"
	(let ((expected '("foo" "baz")))
	  (it "Removes --xmlfile FILE from middle of command line"
		(setq command-line-args-left '("foo" "--xmlfile" "bar" "baz"))
		(expect (buttercup-junit-run-discover) :to-have-same-items-as expected))
	  (it "Removes --xmlfile FILE from beginning of command line"
		(setq command-line-args-left '("--xmlfile" "bar" "foo" "baz"))
		(expect (buttercup-junit-run-discover) :to-have-same-items-as expected))
	  (it "Removes --xmlfile FILE from end of command line"
		(setq command-line-args-left '("foo" "baz" "--xmlfile" "bar"))
		(expect (buttercup-junit-run-discover) :to-have-same-items-as expected))
	  (it "Removes two instances of --xmlfile FILE"
		(setq command-line-args-left '("foo" "--xmlfile" "bar" "baz" "--xmlfile" "qux"))
		(expect (buttercup-junit-run-discover) :to-have-same-items-as expected))
	  ))

  (it "Uses the argument of the last --xmlfile"
	(spy-on 'buttercup-run-discover :and-call-fake (lambda () buttercup-junit-result-file))
	(setq command-line-args-left '("foo" "--xmlfile" "bar" "baz" "--xmlfile" "qux"))
	(expect (buttercup-junit-run-discover) :to-equal "qux"))

  (it "Does not mix up --xmlfile and --outer-suite"
	(spy-on 'buttercup-run-discover :and-call-fake (lambda () buttercup-junit-result-file))
	(setq command-line-args-left '("foo" "--xmlfile" "bar" "baz" "--xmlfile" "qux" "--outer-suite" "outer"))
	(expect (buttercup-junit-run-discover) :to-equal "qux"))

  (describe "Removes --junit-stdout from command-line-args-left before calling buttercup-run-discover"
	(it "Removes --junit-stdout when alone"
	  (setq command-line-args-left '("--junit-stdout"))
	  (expect (buttercup-junit-run-discover) :to-equal '()))
	(it "Removes any number of --junit-stdout"
	  (setq command-line-args-left '("--junit-stdout" "foo" "--junit-stdout" "bar" "--junit-stdout"))
	  (expect (buttercup-junit-run-discover) :to-have-same-items-as '("foo" "bar"))))

  (it "Sets buttercup-junit--to-stdout when --junit-stdout is set"
	(setq command-line-args-left '("--junit-stdout"))
	(spy-on 'buttercup-run-discover :and-call-fake (lambda () buttercup-junit--to-stdout))
	(expect (buttercup-junit-run-discover) :not :to-be nil))
  )

(defmacro buttercup-junit--with-local-vars (&rest body)
  "Execute BODY with all buttercup-junit state vars locally bound."
  (declare (debug t) (indent defun))
  `(let ((buttercup-reporter #'buttercup-junit-reporter)
		 (buttercup-warning-buffer-name " *Buttercup-Junit-Warnings*")
		 buttercup-junit-result-file
		 buttercup-junit--buffer
		 buttercup-junit--to-stdout
		 buttercup-junit-master-suite)
	 ,@body))

(defun test-buttercup-run (suites)
  "Run buttercup for SUITES.
SUITES should be a list of buttercup `description' forms.  One of
them may also be a string, if so `buttercup-junit-master-suite'
will be set to that string value."
  (let (buttercup-suites
		(lexical-binding t))
	(dolist (suite suites)
	  (cond ((stringp suite)
			 (if buttercup-junit-master-suite
				 (warn "buttercup-junit-master-suite already set")
			   (setq buttercup-junit-master-suite suite)))
			(t (eval suite))))
	(buttercup-run)))

(defun buttercup-junit-suite (&rest suites)
  "Run buttercup-junit on SUITES."
  (buttercup-junit--with-local-vars
	(test-buttercup-run suites)))

(defun esxml-buttercup-junit-suite (&rest suites)
  "Run buttercup-junit on SUITES, and convert the JUnit XML to esxml.
let-bind `buttercup-junit-master-suite' to OUTER while running SUITES."
  (buttercup-junit--with-local-vars
	(test-buttercup-run suites)
	(with-current-buffer buttercup-junit--buffer
	  (xml-to-esxml (buffer-string)))))

(defvar test-buttercup-junit-suite1 '(describe "suite1"
									   (it "1.1 should pass"
										 (expect 1 :to-equal 1))
									   (it "1.2 should skip")
									   (it "1.3 should fail"
										 (expect 2 :to-equal 1))))

(defvar test-buttercup-junit-suite2 '(describe "suite1"
									   (it "1.1 should pass"
										 (expect 1 :to-equal 1))
									   (describe "suite2"
										 (it "2.1 should pass"
										   (expect t :to-be-truthy)))
									   (it "1.2 should pass"
										 (expect 2 :to-equal 2))))

(defvar test-buttercup-junit-suite3 '(describe "suite1"
									   (it "should error"
										 (string= 1 2))))

(defvar test-buttercup-junit-suite4 '(describe "suite4"
									   (it "4.1 should pass"
										 (expect 1 :to-equal 1))
									   (it "4.2 should skip")
									   (it "4.3 should fail"
										 (expect 2 :to-equal 1))
									   (it "4.4 should error"
										 (expect (string= 1 2) :to-be-thruthy))))

(defvar test-buttercup-junit-timestamp-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [012][0-9]:[0-5][0-9]:[0-5][0-9]\\+[012][0-9][0-5][0-9]")

(describe "buttercup-junit--escape-string"
  (it "should handle unprintable characters"
	(expect (buttercup-junit--escape-string "") :to-equal "^A^B^Z"))
  (it "should handle xml entities"
	(expect (buttercup-junit--escape-string "\"'&") :to-equal "&quot;&apos;&amp;")))

(cl-defun testsuite-attrs (name &key
								(fail 0) (err 0) (skip 0) (tests (+ fail skip err))
								(stamp test-buttercup-junit-timestamp-re)
								(host ".+") (time "[0-9]+\\.[0-9]+"))
  "Return an esxml attribute alist for testsuite tags."
  `((name . ,name) (timestamp . ,stamp) (hostname . ,host)
	(tests . ,(number-to-string tests)) (failures . ,(number-to-string fail))
	(errors . ,(number-to-string err)) (time . ,time) (skipped . ,(number-to-string skip))))

(describe "JUnit XML output"
  :var ((timestamp (cons 'timestamp test-buttercup-junit-timestamp-re))
		(time (cons 'time "[0-9]+\\.[0-9]+")))
  (before-each (spy-on 'buttercup-junit--exit-code :and-return-value nil ))
  (after-each (spy-calls-reset 'buttercup-junit--exit-code))
  (it "should handle success, failure and pending"
	(expect (esxml-buttercup-junit-suite test-buttercup-junit-suite1) :to-esxml-match
			`(testsuites
			  nil
			  (testsuite ,(testsuite-attrs "suite1" :tests 3 :fail 1 :skip 1)
			   (testcase ((name . "1.1 should pass") (classname . "buttercup") (time . "[0-9]+\\.[0-9]+")))
			   (testcase ((name . "1.2 should skip") (classname . "buttercup") (time . "[0-9]+\\.[0-9]+"))
						 (skipped nil))
			   (testcase ((name . "1.3 should fail") (classname . "buttercup") (time . "[0-9]+\\.[0-9]+"))
						 (failed ((message . "Expected 2 to `equal' 1") (type . "type")) "Traceback .*"))))))
  (it "should handle nested describes"
	(expect (esxml-buttercup-junit-suite test-buttercup-junit-suite2) :to-esxml-match
			`(testsuites
			  nil
			  (testsuite ,(testsuite-attrs "suite1" :tests 3)
			   (testcase ((name . "1.1 should pass") (classname . "buttercup") ,time))
			   (testsuite ,(testsuite-attrs "suite2" :tests 1))
				(testcase ((name . "1.2 should pass") (classname . "buttercup") ,time)))
			  (testcase ((name . "1.3 should fail") (classname . "buttercup") ,time)))))
  (it "should handle erroring testcases"
	(expect (esxml-buttercup-junit-suite test-buttercup-junit-suite3) :to-esxml-match
			`(testsuites
			  nil
			  (testsuite ,(testsuite-attrs "suite1" :err 1)
			   (testcase ((name . "should error") (classname . "buttercup") ,time)
						 (error ((message . "(wrong-type-argument stringp 1)") (type . "error")) "Traceback.*"))))))
  (it "should report correct test state numbers when using outer-suite"
	;; neither the error numbering nor the outer suite works yet
	(expect (esxml-buttercup-junit-suite  "outer" test-buttercup-junit-suite4) :to-esxml-match
			`(testsuites
			  nil
			  (testsuite ,(testsuite-attrs "outer" :tests 4 :fail 1 :skip 1 :err 1)
				(testsuite ,(testsuite-attrs "suite4" :tests 4 :fail 1 :skip 1 :err 1)
			   (testcase ((name . "4.1 should pass") (classname . "buttercup") (time . "[0-9]+\\.[0-9]+")))
			   (testcase ((name . "4.2 should skip") (classname . "buttercup") (time . "[0-9]+\\.[0-9]+"))
						 (skipped nil))
			   (testcase ((name . "4.3 should fail") (classname . "buttercup") (time . "[0-9]+\\.[0-9]+"))
						 (failed ((message . "Expected 2 to `equal' 1") (type . "type")) "Traceback .*"))
			   (testcase ((name . "4.4 should error") (classname . "buttercup") (time . "[0-9]+\\.[0-9]+"))
						 (error ((message . "(wrong-type-argument stringp 1)") (type . "error")) "Traceback.*")))))))
  (it "should handle special XML chars in attributes"
	(expect (esxml-buttercup-junit-suite '(describe "suite with &><\"" (it "should handle &><\"" (expect 1 :to-equal 1))))
			:to-esxml-match
			`(testsuites
			  nil
			  (testsuite ,(testsuite-attrs "suite with &><\"" :tests 1)
						 (testcase ((name . "should handle &><\"") (classname . "buttercup") ,time)))))))

(describe "Return value"
  (before-each (spy-on 'buttercup-junit--exit-code))
  (after-each (spy-calls-reset 'buttercup-junit--exit-code))
  (describe "when all tests pass"
	(it "should be ok"
	  (buttercup-junit-suite test-buttercup-junit-suite2)
	  (expect 'buttercup-junit--exit-code :not :to-have-been-called)))
  (describe "when a test fails"
	(it "should be fail"
	  (buttercup-junit-suite test-buttercup-junit-suite1)
	  (expect 'buttercup-junit--exit-code :to-have-been-called)))
  (describe "when there is an error in a test"
	(it "should be fail"
	  (buttercup-junit-suite test-buttercup-junit-suite3)
	  (expect 'buttercup-junit--exit-code :to-have-been-called))))

(provide 'test-buttercup-junit)
;;; test-buttercup-junit.el ends here
