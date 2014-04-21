;;; bad-commentary-package.el --- A DSL for generating XML with a bad package header

;; Copyright (C) 2008 Philip Jackson

;; Author: Philip Jackson <phil@shellarchive.co.uk>
;; Version: 0.4

;; This file is not currently part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Note this is intentionally the same as the `xmlgen' package, which
;; has bad commentary.

;; Generate xml using sexps with the function `xmlgen':

;; (xmlgen '(p :class "big"))      => "<p class=\"big\" />")
;; (xmlgen '(p :class "big" "hi")) => "<p class=\"big\">hi</p>")

;; (xmlgen '(html
;;           (head
;;            (title "hello")
;;            (meta :something "hi"))
;;           (body
;;            (h1 "woohhooo")
;;            (p "text")
;;            (p "more text"))))

;; produces this (though wrapped):

;; <html>
;;   <head>
;;     <title>hello</title>
;;     <meta something="hi" />
;;   </head>
;;   <body>
;;     <h1>woohhooo</h1>
;;     <p>text</p>
;;     <p>more text</p>
;;   </body>
;; </html>

(eval-when-compile (require 'cl))

(defvar xmlgen-escape-attribute-vals t
  "When non-nil xmlgen will escape the characters <>'\"&' in an
attribute value.")


;;; bad-commentary-package.el ends here
