;;; dummy-package.el --- a fake package for the marmalade test suite

;; Copyright (C) 2013  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Keywords: lisp
;; Version: 0.0.4
;; Package-requires: ((timeclock "2.6.1"))

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

;; This doesn't do anything, it's just a fake package for Marmalade.

;;; Code:

(provide (quote dummy-package))

;;;###autoload
(defun dummy-package ()
  (interactive)
  (message "dummy-package: hello!"))

(provide 'dummy-package)
;;; dummy-package.el ends here
