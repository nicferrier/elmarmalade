;;; elmarmalade.el --- the marmalade repository in emacs-lisp

;; Copyright (C) 2012  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Package-Requires: ((dotassoc "0.0.1")(mongo-elnode "0.0.4"))
;; Keywords: hypermedia, lisp, tools

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

(require 'cl)
(require 'elnode)
(require 'mongo-elnode)
(require 'bson) ; provided by mongo
(require 'dotassoc)

(defvar elmarmalade--packages-db
  (elnode-db-make
   '(mongo
     :host "localhost"
     :collection "marmalade.packages"))
  "The Mongo packages database.")

(defvar elmarmalade--files-db
  (elnode-db-make
   '(mongo
     :host "localhost"
     :collection "marmalade.fs.files"))
  "The Mongo files database.")

(defvar elmarmalade--file-chunks-db
  (elnode-db-make
   '(mongo
     :host "localhost"
     :collection "marmalade.fs.chunks"))
  "The Mongo file chunks database.")

(defun elmarmalade--package-list-record (data)
  "Produce a package record from a marmalade db record.

DATA is a marmalade db record.  An example is in
`testrecord.json'."
  (let ((name (dotassoc "_name" data))
        ;; this could be _latestVersion.version but that's a vector?
        (version (coerce (dotassoc "_latestVersion.version" data) 'list))
        (type (dotassoc "_latestVersion.type" data))
        (desc (dotassoc "_latestVersion.description" data)))
    (when (and name version type desc)
      (cons (intern name) `[,version nil ,desc ,(intern type)]))))

(defun elmarmalade--package-list ()
  "Make a list of everything in the package list."
  (let ((package-list
         (elnode-db-map
          'elmarmalade--package-list-record
          elmarmalade--packages-db)))
    (cons 1 package-list)))

(defun elmarmalade-package-archives (httpcon)
  "Produce the archive for Emacs package.el."
  (let ((package-list
         (elmarmalade--package-list)))
    (elnode-http-start httpcon 200)
    (elnode-http-return httpcon (format "%S" package-list))))

(defun elmarmalade--package-filename (package-name)
  "Get a package filename from PACKAGE-NAME."
  (string-match
   "\\(.+\\)-\\([0-9.]+\\)\\.\\(tar\\|el\\)"
   package-name)
  ;; Should we assert here that this was a package name?
  (let* ((name (match-string 1 package-name))
         (version (match-string 2 package-name))
         (type (match-string 3 package-name)))
    (format "%s.%s/%s" name type version)))

(defun elmarmalade--package-get (package-name)
  "Get the package specified by PACKAGE-NAME.

PACKAGE-NAME is expected to be the name and the version and the
type, like:

 fakir-0.0.10.el

We have to retrieve the file record to then lookup the chunks."
  (let*
      ((query
        (list
         (cons
          "filename"
          (elmarmalade--package-filename package-name))))
       (res
        (elnode-db-map
         (lambda (data)
           (bson-oid-to-hex-string (dotassoc "_id" data)))
         elmarmalade--files-db
         query)))
    (when res ; could throw an error on no res?
      (cadr ; second field of the list (first is the subtype)
       (car ; first of the records (there is only one)
        (elnode-db-map
         (lambda (data)
           (dotassoc "data" data))
         elmarmalade--file-chunks-db
         (list
          (cons
           "files_id"
           (bson-oid-of-hex-string (car res))))))))))

(defun elmarmalade-package-download (httpcon)
  (let* ((package-name (elnode-http-mapping httpcon 1))
         (package-text (elmarmalade--package-get "fakir-0.0.10.el")))
    (elnode-http-start httpcon 200)
    (elnode-http-return httpcon package-text)))

(defun elmarmalade-handler (httpcon)
  "The top level handler for marmalade."
  (elnode-hostpath-dispatcher
   httpcon
   '(("^.*//packages/archive-contents" . elmarmalade-package-archives)
     ("^.*//packages/\\(.*\\)" . elmarmalade-package-download))))

(provide 'elmarmalade)

;;; elmarmalade.el ends here
