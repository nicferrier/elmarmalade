;;; elmarmalade.el --- the marmalade repository in emacs-lisp

;; Copyright (C) 2013  Nic Ferrier

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

;; marmalade-repo.org is an Emacs package repository originally by
;; Nathan Wiezenbaum in node.js.

;; This is a rewrite of marmalade-repo in Emacs-Lisp using Elnode.

;;; Code:

(elnode-app marmalade-dir 
  marmalade-archive)

(defconst marmalade-package-store-dir "~/marmalade/packages"
  "Where the package files are kept.")

(defun marmalade/explode-package-string (package-name)
  (string-match
   "\\(.+\\)-\\([0-9.]+\\)+\\.\\(el\\|tar\\)"
   package-name)
  (list 
   (match-string 1 package-name)
   (match-string 2 package-name)
   (match-string 3 package-name)))

(defun marmalade/package-name->filename (package-name)
  "Convert a package-name to a local filename.

The package root dir is not considered, this is just a structural
transformation of the filename."
  (destructuring-bind (name version type)
      (marmalade/explode-package-string package-name)
    (format
     "%s/%s/%s-%s.%s"
     name
     version
     name version type)))

(defun marmalade/downloader (httpcon)
  "Download a specific package."
  (flet ((elnode-http-mapping (httpcon which)
           (let* ((package (elnode--http-mapping-impl httpcon which))
                  (file (marmalade/package-name->filename package)))
             file)))
    (elnode-docroot-for
        marmalade-package-store-dir
        with target-package
        on httpcon
        do
        (with-current-buffer (find-file-noselect target-package)
          (elnode-http-start httpcon 200 '("Content-type" . "text/elisp"))
          (elnode-http-return httpcon (buffer-string))))))

(defun marmalade-router (httpcon)
  (elnode-hostpath-dispatcher
   httpcon
   '(("^[^/]+//packages/archive-contents" . marmalade-archive-handler)
     ;; We don't really want to send 404's for these if we have them
     ("^[^/]+//packages/.*-readme.txt" . elnode-send-404)
     ("^[^/]+//packages/\\(.*\\)" . marmalade/downloader))))

(provide 'marmalade-service)

;;; marmalade-s.el ends here
