;;; utils-test-buttercup-junit.el --- Utility functions for tests -*- lexical-binding: t; -*-

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
(eval-and-compile (require 'buttercup))
(require 'esxml)

(defun esxml-attribs-match (attribs pattern)
  "Compare an esxml ATTRIBS form to PATTERN.
PATTERN should be an esxml attributes form, but the value strings
are interpreted as regexps that are matched against the values in
ATTRIBS.  The elements in ATTRIBS and PATTERN does not have to be
in the same order."
  (if (not (= (length attribs) (length pattern)))
	  (cons nil "attrs are not equal length")
	(let (p)
	  ;; cl-dolist is not in Emacs 24.2, so use a wrapping block.
	  ;; cl-block is not in Emacs 24.2 either.
	  (cl-block nil
		(dolist (a attribs '(t . "Attrs should not match"))
		  (or (and (setq p (assoc (car a) pattern))
				   (string-match (cdr p) (cdr a)))
			  (cl-return (cons nil (format "%s=\"%s\" should match %s=\"%s\""
										   (car a) (cdr a) (car p) (cdr p))))))))))

(defun esxml-tag-match (tag pattern)
  "Match a esxml element TAG against PATTERN."
  (cond ((> 2 (length tag)) (error "%s is malformed for an esxml object" tag))
		((> 2 (length pattern)) (error "%s is malformed for an esxml pattern" pattern))
		((not (eq (car tag) (car pattern))) (cons nil (format "%s and %s should match" (car tag) (car pattern))))
		((not (attrsp (cadr tag))) (error "%s is malformed as an attrs list" (cadr tag)))
		((not (attrsp (cadr pattern))) (error "%s is malformed as an attrs pattern list" (cadr pattern)))
		((not (car (esxml-attribs-match (cadr tag) (cadr pattern)))) (esxml-attribs-match (cadr tag) (cadr pattern)))
		(t
		 (setq tag (cddr tag)
			   pattern (cddr pattern))
		 (if (null tag) (cons t "Element and pattern should not match")
		   (cl-do
			   ((subtag (car tag) (car tag))
				(subpat (car pattern) (car pattern)))
			   ((not (and (pop tag) (pop pattern))) (cons t "Element and pattern should not match"))
			 (let ((val-cons (cond ((stringp subtag)
									(let ((res (string-match subpat subtag)))
									  (if res (cons t (format "%s should not match %s" subpat subtag))
										(cons nil (format "%s should match %s" subpat subtag)))))
								   ((listp subtag) (esxml-tag-match subtag subpat))
								   (t (error "Unknown type")))))
			   (unless (car val-cons)
				 (cl-return val-cons))))))))

(describe "The esxml-matcher"
  (it "should error if either side is less than length 2"
	(expect (esxml-tag-match '(foo) '(foo)) :to-throw 'error)
	(expect (esxml-tag-match '(foo bar) '(foo)) :to-throw 'error))
  (it "should fail if the cars don't match"
	(expect (esxml-tag-match '(foo bar) '(bar baz)) :to-equal `(nil . "foo and bar should match")))
  (it "should throw an error if either sides cadr is not valid attrs"
	(expect (esxml-tag-match '(foo bar) '(foo)) :to-throw 'error)
	(expect (esxml-tag-match '(foo ((bar . "foo"))) '(foo ((bar "baz")))) :to-throw 'error))
  (it "should fail if the attrs dont match"
	(expect (esxml-tag-match '(foo ((bar . "foo"))) '(foo ((bar . "baz") (qux . "q"))))
			:to-equal '(nil . "attrs are not equal length"))
	(expect (esxml-tag-match '(foo ((bar . "foo"))) '(foo ((bar . "baz"))))
			:to-equal '(nil . "bar=\"foo\" should match bar=\"baz\""))
	(expect (esxml-tag-match '(foo ((bar . "foo"))) '(foo ((bar . ".*z"))))
			:to-equal '(nil . "bar=\"foo\" should match bar=\".*z\"")))
  (it "should be ok if the tag name and attrs match"
	(expect (esxml-tag-match '(a ((a . "a"))) '(a ((a . ".")))) :to-equal '(t . "Element and pattern should not match")))
  (it "should call itself once for every sub-element"
	(let ((top '(foo nil))
		  (sub '(bar nil))
		  total)
	  (spy-on 'esxml-tag-match :and-call-through)
	  (dotimes (i 10)
		(setq total (append (cl-copy-list top) (make-list i sub)))
		(expect (esxml-tag-match total total) :to-equal '(t . "Element and pattern should not match"))
		(expect (spy-calls-count 'esxml-tag-match) :to-equal (1+ i))
		(spy-calls-reset 'esxml-tag-match))))
  )

(buttercup-define-matcher :to-esxml-match (value pattern)
  (esxml-tag-match (funcall value) (funcall pattern)))

(provide 'utils-test-buttercup-junit)
;;; utils-test-buttercup-junit.el ends here
;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; no-byte-compile: t
;; tab-width: 4
;; End:
