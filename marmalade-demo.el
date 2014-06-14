;;; marmalade-demo.el --- a demonstration elpa package

;; Copyright (C) 2014  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Keywords: lisp
;; Version: 0.0.5

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

;; This is just a demonstration package, it doesn't do anything.

;;; Code:

(defun marmalade-demo/private-function ()
  (with-current-buffer (get-buffer-create "*marmalade-demo*")
    (insert "Welcome to the marmalade demo\n")
    (insert "\nThis is just a silly demo. Nothing will come of it.")
    (switch-to-buffer (current-buffer))))

;;;###autoload
(defun marmalade-demo ()
  (interactive)
  (marmalade-demo/private-function))

(provide 'marmalade-demo)

;;; marmalade-demo.el ends here
