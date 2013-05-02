;;; elmarmalade.el --- the marmalade repository in emacs-lisp -*- lexical-binding: t -*-

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

(elnode-app marmalade-dir marmalade-archive db s rx)

(defconst marmalade-cookie-name "marmalade-user"
  "The name of the cookie we use for auth.")

(elnode-defauth 'marmalade-auth
  :auth-test 'marmalade-auth-func
  :cookie-name marmalade-cookie-name)

(defconst marmalade/user-db
  (db-make `(db-hash :filename ,(concat marmalade-dir "user")))
  "The user database.")

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

(defun marmalade/package-info (package-file)
  "Return the package-info on the PACKAGE-FILE.

PACKAGE-FILE is either an ELISP or a TAR file to be uploaded to
the package repository."
  (cond
    ((string-match "\\.el$" package-file)
     (with-temp-buffer
       (insert-file-contents-literally package-file)
       (buffer-string) ; leave it in so it's easy to debug
       (package-buffer-info)))
    ((string-match "\\.tar$" package-file)
     (package-tar-file-info package-file))
    (t (error "Unrecognized extension `%s'"
              (file-name-extension package-file)))))

(defun marmalade/package-path (package-file)
  "Turn PACKAGE-FILE into a repository package path."
  (let* ((info (marmalade/package-info package-file))
         (version (elt info 3))
         (package-dir marmalade-package-store-dir)
         (package-name (elt info 0))
         (file-name (file-name-base package-file))
         (file-name-dir (file-name-directory package-file))
         (file-name-type (file-name-extension package-file)))
    (s-lex-format
     "${package-dir}/${package-name}/${version}/${package-name}-${version}.${file-name-type}")))

(defun marmalade/temp-file (base-package-file-name)
  "Make a temp file for storing a package in."
  (make-temp-file
   "marmalade-upload" nil
   (concat "." (file-name-extension
                base-package-file-name))))

(defun marmalade/save-package (package-data package-file-name)
  "Save PACKAGE-DATA as PACKAGE-FILE-NAME in the package store.

PACKAGE-FILE-NAME is used as the basis of a temporary file name
and then the package info is computed and the target package path
name is computed.

The file is moved from the temp name to the target name.

If the target package already exists a `file-error' is produced."
  ;; TODO: if we collect the temp files into a single upload directory
  ;; we could treat that as a sort of queue... anything not moved
  ;; could just be replayed through the package-info stuff because
  ;; temp file nmes don't matter, except the extension, which we
  ;; record.
  (let* ((temp-package-file
          (marmalade/temp-file package-file-name))
         (package-path
          (progn
            ;; First save the uploaded data to a temp package file
            (with-temp-file temp-package-file
              (insert-string
               (substring-no-properties package-data)))
            ;; Now get the real path
            (marmalade/package-path temp-package-file))))
    ;; Try to move the file to the target path
    (when (file-exists-p package-path)
      (signal 'file-error
              (list
               package-path "existing package")))
    (rename-file temp-package-file package-path)
    ;; Return the new path
    package-path))

(defun marmalade/upload (httpcon)
  "Handle uploaded packages."
  ;; FIXME Need to check we have auth here
  (with-elnode-auth httpcon 'marmalade-auth
    (let* ((upload-file (elnode-http-param httpcon "file"))
           (upload-file-name
            (get-text-property 0 :elnode-filename upload-file))
           (base-file-name (file-name-nondirectory upload-file-name)))
      (condition-case err
          (let ((package-file-name
                 (marmalade/save-package upload-file base-file-name)))
            (elnode-send-redirect httpcon package-file-name 201))
        (error (progn
                 (message "marmalade/upload ERROR!")
                 (elnode-send-400
                  httpcon
                  "something went wrong uploading the package")))))))

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
        (with-current-buffer
            (let ((enable-local-variables nil))
              (find-file-noselect target-package))
          (elnode-http-start httpcon 200 '("Content-type" . "text/elisp"))
          (elnode-http-return httpcon (buffer-string))))))

(defun marmalade-auth-func (username)
  "What is the token for the USERNAME?"
  (let ((user (db-get username marmalade/user-db)))
    (when user
      (aget user token))))

(defun marmalade/package-handler (httpcon)
  "Dispatch to the appropriate handler on method."
  (elnode-method httpcon
    (GET (marmalade/downloader httpcon))
    (POST (marmalade/upload httpcon))))

(defun marmalade/packages-index (httpcon)
  "Show a package index in HTML or JSON?"
  (elnode-send-html httpcon "<H1>marmalade repo</H1>"))

(defun marmalade/top-version (package-dir)
  "Return the path to the newest version in PACKAGE-DIR.

PACKAGE-DIR is the top level package directory:

  package-root/package-name

The full path including the package root is returned."
  (let* ((pkg-dir (file-name-as-directory package-dir))
         (versions
              (directory-files pkg-dir nil "[^.].*"))
         (top-version (car versions)))
    (car
     (directory-files
      (concat pkg-dir top-version) t "[^.].*"))))

(defun marmalade/relativize (path root) ; not actually using this right now
  "Return the part of PATH under ROOT or `nil'."
  (save-match-data
    (let ((pt 
           (string-match
            (rx-to-string `(: bos ,root (group (+ anything))))
            path)))
      (when pt
        (match-string 1 path)))))

(defun marmalade/commentary->about (commentary)
  "Transform the COMMENTARY to something we can display."
  (save-match-data
    (mapconcat
     'identity
     (cdr ; first line is a "\n" so drop that
      (-keep 
       (lambda (line)
         (when (not (string-match-p "^;+ Commentary:.*" line))
           (if (string-match "^;* \\(.*\\)" line)
               (match-string 1 line)
               line)))
       (split-string commentary "\n")))
     "\n")))

(defun marmalade/package-blurb (httpcon)
  "Provide an informative description of the package."
  (elnode-docroot-for
      marmalade-package-store-dir
      with target-package
      on httpcon
      do
      (let* ((package-name (elnode-http-mapping httpcon 1))
             (package-file (marmalade/top-version target-package))
             (package-download (file-name-nondirectory package-file))
             (info (marmalade/package-info package-file)))
        (destructuring-bind
              (name depends description version commentary)
            (mapcar 'identity info)
          (let ((about (marmalade/commentary->about commentary)))
            (elnode-http-start httpcon 200 '(Content-type . "text/html"))
            (elnode-http-return
             httpcon
             (s-lex-format
              "<html>
<head>
<link rel=\"stylesheet\" href=\"/-/style.css\" type=\"text/css\"></link>
<title>${package-name} @ Marmalade</title>
</head>
<body>
<h1>${package-name}</h1>
<p class=\"description\">${description}</p>
<a href=\"${package-download}\">download ${package-name}</a>
<pre>${about}</pre>
</body>
<html>")))))))

;; The authentication scheme.
(elnode-defauth 'marmalade-auth
  :auth-test 'marmalade-auth-func
  :cookie-name marmalade-cookie-name)

(defconst marmalade/webserver
  (elnode-webserver-handler-maker (concat marmalade-dir "static"))
  "The webserver for marmalade.")

(defun marmalade-router (httpcon)
  (elnode-hostpath-dispatcher
   httpcon
   `(("^[^/]*//-/\\(.*\\)$" . ,marmalade/webserver)
     ("^[^/]+//packages/archive-contents" . marmalade-archive-handler)
     ;; We don't really want to send 404's for these if we have them
     ("^[^/]+//packages/\\([^/]+\\)" . marmalade/package-blurb)
     ("^[^/]+//packages/.*-readme.txt" . elnode-send-404)
     ("^[^/]+//packages/\\(.*\\)\\.\\(el\\|tar\\)" . marmalade/package-handler)
     ("^[^/]+//packages/" . marmalade/packages-index))
   :log-name "marmalade"
   :auth-scheme 'marmalade-auth))

(provide 'marmalade-service)

;;; marmalade-s.el ends here
