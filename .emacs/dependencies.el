;;; dependencies.el --- project specific package dependencies -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Ola Nilsson

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; List of packages required for development and test.

;;; Code:

(package-install-maybe 'buttercup)
(package-install-maybe 'elisp-lint :min-emacs-version "24.4")
(package-install-maybe 'esxml)
(package-install-maybe 'package-lint)

;;; dependencies.el ends here
;; Local Variables:
;; no-byte-compile: t
;; End:
