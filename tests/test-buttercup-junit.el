;;; test-buttercup-junit.el --- Tests for buttercup-junit.el -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2018  Ola Nilsson

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

;; The tests for buttercup-junit, themselves written as buttercup tests.

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
         buttercup-junit-inner-reporter
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

(defmacro buttercup-junit--xml-output (var &rest body)
  "Store junit xml output in VAR while executing BODY.
If `buttercup-junit-reporter' is called with `buttercup-done'
more than once, only the last data will be stored."
  (declare (indent 1) (debug (symbolp body)))
  ;; Uses the buttercup-junit--to-stdout hook to extract xml text
  ;; through send-string-to-terminal
  `(let (,var (buttercup-junit--to-stdout t))
     (cl-letf (((symbol-function 'send-string-to-terminal)
                (lambda (str) (setq ,var str))))
       ,@body)))

(defun esxml-buttercup-junit-suite (&rest suites)
  "Run buttercup-junit on SUITES, and convert the JUnit XML to esxml.
SUITES should be a list of buttercup `description' forms.  One of
them may also be a string, if so `buttercup-junit-master-suite'
will be set to that string value."
  (buttercup-junit--with-local-vars
    (buttercup-junit--xml-output xmlout
      (test-buttercup-run suites)
      (xml-to-esxml xmlout))))

(defvar test-buttercup-junit-suite1 '(describe "suite1"
									   (it "1.1 should pass"
										 (expect 1 :to-equal 1))
									   (it "1.2 should skip")
									   (it "1.3 should fail"
										 (expect 2 :to-equal 1)))
  "Single describe containing 1 passing, 1 pending and 1 failing spec.")

(defvar test-buttercup-junit-suite2 '(describe "suite1"
									   (it "1.1 should pass"
										 (expect 1 :to-equal 1))
									   (describe "suite2"
										 (it "2.1 should pass"
										   (expect t :to-be-truthy)))
									   (it "1.2 should pass"
										 (expect 2 :to-equal 2)))
  "Nested suites containing only passing specs.")

(defvar test-buttercup-junit-suite3 '(describe "suite1"
									   (it "should error"
										 (string= 1 2)))
  "Suite with a single, failing spec.")

(defvar test-buttercup-junit-suite4 '(describe "suite4"
									   (it "4.1 should pass"
										 (expect 1 :to-equal 1))
									   (it "4.2 should skip")
									   (it "4.3 should fail"
										 (expect 2 :to-equal 1))
									   (it "4.4 should error"
										 (expect (string= 1 2) :to-be-truthy)))
  "Suite with 1 passing, 1 pending, 1 failing, and 1 erroring spec.")

(defvar test-buttercup-junit-timestamp-re
  "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [012][0-9]:[0-5][0-9]:[0-5][0-9]\\+[012][0-9][0-5][0-9]"
  "Regex that matches JUnit timestamps; YYY-MM-DD hh:mm::ss+hhmm.")

(describe "buttercup-junit--escape-string"
  (it "should handle unprintable characters"
	(expect (buttercup-junit--escape-string "") :to-equal "^A^B^Z"))
  (it "should handle xml entities"
	(expect (buttercup-junit--escape-string "\"'&") :to-equal "&quot;&apos;&amp;")))

(defun test-buttercup-junit--remove-keys (rest-list &rest keys)
  "Remove all key-value pairs from REST-LIST for all KEYS.
Useful to remove &key arguments from a &rest argument in
`cl-defun's and `cl-defmacro's.

Example:
 (test-buttercup-junit--remove-keys '(1 :foo 2 3) :foo :bar)
 -> '(1 3)"
  (let (filtered elt)
    (while rest-list
      (setq elt (pop rest-list))
      (if (memq elt keys)
          (pop rest-list)
        (push elt filtered)))
    (nreverse filtered)))

(defun testsuites (&rest contains)
  "Return an esxml list for a `testsuites' tag.
CONTAINS should be the contained esxml lists."
  (declare (indent defun))
  (if contains
      `(testsuites nil ,@contains)
    '(testsuites nil)))

(cl-defun testsuite (name
                     &rest contains
                     &key (fail 0) (err 0) (skip 0) (tests (+ fail skip err))
                     (stamp test-buttercup-junit-timestamp-re)
                     (host ".+") (time "[0-9]+\\.[0-9]+")
                     &allow-other-keys)
  "Return an esxml list for a testsuite tag.
NAME is the suite description.
FAIL is the number of failed testcases, default 0.
ERR is the number of testcases that threw an error, default 0.
SKIP is the number of skipped (pending) testcases, default 0.
TESTS is the total number of testcases, defaults to FAIL + ERR + SKIP.
STAMP should be a JUnit timestamp string or a time value as
      returned by `current-time'.  STAMP defaults to
      `test-buttercup-junit-timestamp-re'.
HOST is a hostname, default `.+'.
TIME is the elapsed time in seconds, default `[0-9]+\\.[0-9]+'."
  (declare (indent defun))
  `(testsuite
    ((name . ,name)
     (timestamp . ,(cond ((stringp stamp) stamp)
                         ((listp stamp)
                          (regexp-quote
                           (format-time-string "%Y-%m-%d %T%z" stamp)))
                         (t (error "Unexpected stamp argument type %s"
                                   (type-of stamp)))))
     (hostname . ,host)
     (tests . ,(number-to-string tests))
     (failures . ,(number-to-string fail))
     (errors . ,(number-to-string err)) (time . ,time)
     (skipped . ,(number-to-string skip)))
    ,@(test-buttercup-junit--remove-keys contains :fail :err :skip
                                         :tests :stamp :host :time)))

(cl-defun testcase (name &rest contains &key (class "buttercup") (time "[0-9]+\\.[0-9]+")
						 skip &allow-other-keys)
  "Return an esxml list for a testcase tag.
NAME is the spec description.
CONTAINS is any inner data for the tag.
CLASS is the value if the `class' attribute, default `buttercup'.
TIME is the elapsed time, default `[0-9]+\\.[0-9]+'.
If SKIP is non-nil, include the `skip' attribute."
  (declare (indent defun))
  (let ((attrs `((name . ,name) (classname . ,class) (time . ,time))))
    (setq contains (test-buttercup-junit--remove-keys contains :class :time))
	(cond (skip `(testcase ,attrs (skipped nil)))
		  (contains `(testcase ,attrs ,@contains))
		  (t `(testcase ,attrs)))))

(describe "JUnit XML output"
  :var ((timestamp (cons 'timestamp test-buttercup-junit-timestamp-re))
		(time (cons 'time "[0-9]+\\.[0-9]+")))
  (before-each
    (spy-on 'error :and-call-fake
            (lambda (string &rest args)
              (unless (and (string= "" string)
                           (null args))
                (signal 'error (list (apply #'format-message args)))))))
  (it "should handle success, failure and pending"
	(expect (esxml-buttercup-junit-suite test-buttercup-junit-suite1) :to-esxml-match
            (testsuites
             (testsuite "suite1" :tests 3 :fail 1 :skip 1
                 (testcase "1.1 should pass")
                 (testcase "1.2 should skip" :skip t)
                 (testcase "1.3 should fail"
                   '(failed ((message . "Expected `2' to be `equal' to `1'.*")
                             (type . "type"))
                            "Traceback .*"))))))
  (it "should handle nested describes"
	(expect (esxml-buttercup-junit-suite test-buttercup-junit-suite2) :to-esxml-match
            (testsuites
              (testsuite "suite1" :tests 3
                 (testcase "1.1 should pass")
                 (testsuite "suite2" :tests 1
                   (testcase "2.1 should pass"))
                 (testcase "1.2 should pass")))))
  (it "should handle erroring testcases"
	(expect (esxml-buttercup-junit-suite test-buttercup-junit-suite3) :to-esxml-match
            (testsuites
              (testsuite "suite1" :err 1
                 (testcase "should error"
                   '(error ((message . "(wrong-type-argument stringp 1)")
                            (type . "error")) "Traceback.*"))))))
  (it "should report correct test state numbers when using outer-suite"
	;; neither the error numbering nor the outer suite works yet
	(expect (esxml-buttercup-junit-suite  "outer" test-buttercup-junit-suite4) :to-esxml-match
            (testsuites
             (testsuite "outer" :tests 4 :fail 1 :skip 1 :err 1
                 (testsuite "suite4" :tests 4 :fail 1 :skip 1 :err 1
                   (testcase "4.1 should pass")
                   (testcase "4.2 should skip" :skip t)
                   (testcase "4.3 should fail"
                     '(failed ((message . "Expected `2' to be `equal' to `1'.*")
                               (type . "type")) "Traceback .*"))
                   (testcase "4.4 should error"
                     '(error ((message . "(wrong-type-argument stringp 1)")
                              (type . "error")) "Traceback.*")))))))
  (it "should handle special XML chars in attributes"
	(expect (esxml-buttercup-junit-suite '(describe "suite with &><\"" (it "should handle &><\"" (expect 1 :to-equal 1))))
			:to-esxml-match
            (testsuites
             (testsuite "suite with &><\"" :tests 1
                 (testcase "should handle &><\""))))))

(describe "The timestamps"
  (let (start-time suite spec suite2)
    (cl-labels ((report-suites (&rest suites)
                               (buttercup-junit-reporter 'buttercup-started suites)
                               (dolist (suite suites)
                                 (buttercup-junit-reporter 'suite-started suite)
                                 (buttercup-junit-reporter 'suite-done suite))
                               (buttercup-junit-reporter 'buttercup-done suites))
                (start+ (seconds) (time-add start-time (seconds-to-time seconds))))
      (before-each
        (setq start-time (current-time)
              suite (make-buttercup-suite
                     :description "suite"
                     :time-started start-time
                     :time-ended (start+ 1))
              suite2 (make-buttercup-suite
                      :description "suite2"
                      :time-started (start+ 1.5)
                      :time-ended (start+ 2))
              spec (make-buttercup-spec
                    :description "spec"
                    :time-started (start+ 0.25)
                    :time-ended   (start+ 0.5)))
        (buttercup-suite-add-child suite spec))
      (describe "should hold correct start time"
        (it "for suites"
          (buttercup-junit--with-local-vars
            (report-suites suite)
            (expect (with-current-buffer buttercup-junit--buffer
                      (xml-to-esxml (buffer-string)))
                    :to-esxml-match
                    (testsuites (testsuite "suite" :tests 1 :stamp start-time)))))
        (it "for any outer suite"
          (buttercup-junit--with-local-vars
            (setq buttercup-junit-master-suite "master")
            (report-suites suite suite2)
            (expect (with-current-buffer buttercup-junit--buffer
                      (xml-to-esxml (buffer-string)))
                    :to-esxml-match
                    (testsuites
                      (testsuite "master" :tests 1 :stamp start-time
                         (testsuite "suite" :tests 1 :stamp start-time)
                         (testsuite "suite2" :stamp (start+ 1.5))))))))
      (describe "should report correct elapsed time"
        (it "for suites"
          (buttercup-junit--with-local-vars
            (report-suites suite)
            (expect (with-current-buffer buttercup-junit--buffer
                      (xml-to-esxml (buffer-string)))
                    :to-esxml-match
                    (testsuites
                     (testsuite  "suite" :tests 1 :time "1.000000")))))
        (it "for any outer suite"
          (buttercup-junit--with-local-vars
            (setq buttercup-junit-master-suite "master")
            (report-suites suite suite2)
            (expect (with-current-buffer buttercup-junit--buffer
                      (xml-to-esxml (buffer-string)))
                    :to-esxml-match
                    (testsuites
                     (testsuite "master" :tests 1 :time "2.000000"
                         (testsuite "suite" :tests 1 :time "1.000000")
                         (testsuite "suite2" :time "0.500000")))))))
      (it "should report correct elapsed time for specs"
        (buttercup-junit--with-local-vars
          (buttercup-junit-reporter 'buttercup-started nil)
          (buttercup-junit-reporter 'suite-started suite)
          (buttercup-junit-reporter 'spec-started spec)
          (buttercup-junit-reporter 'spec-done spec)
          (buttercup-junit-reporter 'suite-done suite)
          (buttercup-junit-reporter 'buttercup-done (list suite))
          (expect (with-current-buffer buttercup-junit--buffer
                    (xml-to-esxml (buffer-string)))
                  :to-esxml-match
                  (testsuites
                    (testsuite "." :tests 1
                      (testcase "spec" :time "0.250000")))))))))

(describe "Return value"
  :var (exit-code)
  (before-each
    (setq exit-code nil)
    (spy-on 'error :and-call-fake
            (lambda (&rest args)
              (if (and (string= "" (car args))
                       (null (cdr args)))
                  (setq exit-code t)
                (signal 'error (list (apply #'format-message args)))))))
  (describe "when all tests pass"
	(it "should be ok"
	  (buttercup-junit-suite test-buttercup-junit-suite2)
      (expect exit-code :not :to-be-truthy)))
  (describe "when a test fails"
	(it "should be fail"
	  (buttercup-junit-suite test-buttercup-junit-suite1)
      (expect exit-code :to-be-truthy)))
  (describe "when there is an error in a test"
	(it "should be fail"
	  (buttercup-junit-suite test-buttercup-junit-suite3)
      (expect exit-code :to-be-truthy))))

(describe "The buttercup-junit-inner-reporter"
  (it "should be called for each event"
    ;; Should be cl-flet, but that does not work on 24.3
    (flet ((dummy-reporter (event arg) (ignore event arg)))
      (spy-on 'dummy-reporter)
      (buttercup-junit--with-local-vars
        (let ((buttercup-junit-inner-reporter 'dummy-reporter))
          (buttercup-junit-reporter 'buttercup-started 'foo)
          (expect 'dummy-reporter
                  :to-have-been-called-with 'buttercup-started 'foo)
          (buttercup-junit-reporter 'suite-started 'bar)
          (expect 'dummy-reporter :to-have-been-called-with 'suite-started 'bar)
          (buttercup-junit-reporter 'spec-started 'baz)
          (expect 'dummy-reporter :to-have-been-called-with 'spec-started 'baz)
          (buttercup-junit-reporter 'spec-done 'qux)
          (expect 'dummy-reporter :to-have-been-called-with 'spec-done 'qux)
          (buttercup-junit-reporter 'suite-done 'quux)
          (expect 'dummy-reporter :to-have-been-called-with 'suite-done 'quux)
          ;; arg must be nil here for buttercup-junit-reporter to do nothing
          (buttercup-junit-reporter 'buttercup-done nil)
          (expect 'dummy-reporter
                  :to-have-been-called-with 'buttercup-done nil)))))
    (it "should be ok to set to nil"
      (buttercup-junit--with-local-vars
        (expect buttercup-junit-inner-reporter :to-be nil)
        (dolist (event '(buttercup-started suite-started spec-started
                                           spec-done suite-done buttercup-done))
          (buttercup-junit-reporter event nil)))))

(provide 'test-buttercup-junit)
;;; test-buttercup-junit.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
