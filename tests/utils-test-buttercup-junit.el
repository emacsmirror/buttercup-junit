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
(require 'buttercup)
(require 'esxml)

(defun esxml-attribs-match (attribs pattern)
  "Compare an esxml ATTRIBS form to PATTERN.
PATTERN should be an esxml attributes form, but the value strings
are interpreted as regexps that are matched against the values in
ATTRIBS.  The elements in ATTRIBS and PATTERN does not have to be
in the same order."
  (when (= (length attribs) (length pattern))
	(let (p)
	  (cl-dolist (a attribs t)
		(or (and (setq p (assoc (car a) pattern))
				 (string-match (cdr p) (cdr a)))
			(return nil))))))
		
(defun esxml-tag-match (tag pattern)
  "Match a esxml element TAG against PATTERN."
  (and (= (length tag) (length pattern))
	   (eq (pop tag) (pop pattern))
	   (if (and tag
				(attrsp (car tag)))
		   (and (attrsp (car pattern))
				(esxml-attribs-match (pop tag) (pop pattern)))
		 t)
	   (if (null tag) t
		 (cl-do
			 ((subtag (car tag) (car tag))
			  (subpat (car pattern) (car pattern)))
			 ((not (and (pop tag) (pop pattern))) t)
		   (or (cond ((stringp subtag) (string-match subpat subtag))
					 ((listp subtag) (esxml-tag-match subtag subpat))
					 (t (error "Unknown type")))
			   (return nil))))))

(buttercup-define-matcher :to-esxml-match (value pattern)
  (if (esxml-tag-match value pattern)
      (cons t (format "Expected %S not to match pattern %S" value pattern))
    (cons nil (format "Expected %S to match pattern %S" value pattern))))

(provide 'utils-test-buttercup-junit)
;;; utils-test-buttercup-junit.el ends here
