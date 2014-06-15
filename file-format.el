;;; file-format.el --- templates with files as the source  -*- lexical-binding: t -*-

;; Copyright (C) 2014  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Keywords: lisp
;; Package-Requires: ((s "1.5.0"))

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

;; File templates let you keep template descriptions in a file and
;; then fill in the template values.

;; This attempts to be efficient, not fetching the file templates
;; unless they have been changed.  They are stored internally, as
;; strings, in a cache.

;; Here's an example, from marmalade:

;; (file-format-html
;;  "package-page.html" marmalade-dir 'aget
;;  `(("package-name" . ,package-name)
;;    ("version" . ,(format "%S" version))
;;    ("author" . ,(if (or (not author)
;;                         (equal author ""))
;;                     "Unknown" author))
;;    ("package-download" . ,package-download)
;;    ("description" . ,description)
;;    ("about" . ,about-text)
;;    ;; Replace safely later
;;    ("header" . ,(propertize 
;;                  (marmalade/page-header httpcon)
;;                  :file-format-html-safe t))))

;; Note the use of propertize to mark the HTML as safe so it isn't
;; escaped.

;;; Code:

(require 'xml)
(require 's)

(defmacro with-transient-file (file-name &rest code)
  "Load FILE-NAME into a buffer and eval CODE.

Then dispose of the buffer.

File loading errors may be generated as by any call to visit a
file."
  (declare (indent 1)
           (debug (sexp &rest form)))
  (let ((fvn (make-symbol "fv"))
        (bvn (make-symbol "bv")))
    `(let* ((,fvn ,file-name)
            (,bvn (get-buffer-create
                   (generate-new-buffer-name "transient"))))
       (unwind-protect
            (with-current-buffer ,bvn
              (insert-file-contents-literally ,fvn)
              ,@code)
         (progn
           (set-buffer-modified-p nil)
           (kill-buffer ,bvn))))))

(defconst file-format/cache (make-hash-table :test 'equal)
  "The cache used for the template objects.")

(defconst file-attributes  (list (cons :file-type 0)
                                 (cons :link-count 1)
                                 (cons :uid 2)
                                 (cons :gid 3)
                                 (cons :atime 4)
                                 (cons :mtime 5)
                                 (cons :stime 6)
                                 (cons :size 7)
                                 (cons :mode 8)
                                 (cons :gid-change 9)
                                 (cons :inode 10)
                                 (cons :fsdevice 11))
  "The indices for `file-attributes' list items.")

(defun file-attr (filename symbolic-name)
  "Get the SYMBOLIC-NAME attribute from FILENAME, eg: `:mtime'."
  ;; We could do with caching this stuff
  (elt (file-attributes (expand-file-name filename))
       (kva symbolic-name file-attributes)))

(defun file-format/template-get (file-name root)
  "Return the template object for the file FILE-NAME under ROOT."
  (let* ((name (file-name-nondirectory file-name))
         (template (gethash name file-format/cache))
         (file-details (file-attributes (expand-file-name name root))))
    (unless file-details
      (signal 'file-error (format "file %s at %s not found" name root)))
    (let ((mtime (file-attr file-name :mtime)))
      (when (or (not template)
                (time-less-p (or (plist-get template :time)
                                 (seconds-to-time 0)) mtime))
        (with-transient-file (expand-file-name name root)
          (puthash name
                   (list :name name
                         :time mtime
                         :content (buffer-string)) file-format/cache)))
      (gethash name file-format/cache))))

(defun file-format (name root func &rest extra)
  "Format the template file specified by NAME under ROOT.

FUNC specifies how to replace the templated values, possibly with
EXTRA.  See `s-format' for more details."
  (apply 's-format (plist-get (file-format/template-get name root) :content)
         func extra))

(defun file-format-html (name root replacer &rest extra)
  "Format as HTML the template file specified by NAME under ROOT.

FUNC specifies how to replace the templated values, possibly with
EXTRA.  See `s-format' for more details.

Each value replaced is escaped for HTML-ness.  See
`xml-escape-string' for details on how we do that."
  (apply 's-format
         (plist-get (file-format/template-get name root) :content)
         (lambda (key &optional extra)
           (xml-escape-string
            (cond
              ((eq replacer 'aget) (s--aget extra key))
              ((eq replacer 'elt) (elt extra key))
              (t
               (if extra
                   (funcall replacer key extra)
                   (funcall replacer key))))))
         extra))

(provide 'file-format)

;;; file-format.el ends here
