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

(elnode-app marmalade-dir
  marmalade-customs marmalade-archive marmalade-users
  file-format ; should be a separate package
  web htmlize db s 
  dash kv rx lisp-mnt outline)

(defconst marmalade/cookie-name "marmalade-user"
  "The name of the cookie we use for auth.")

(defconst marmalade/page-header "<div class=\"navbar\">
            <div class=\"navbar-inner\">
                <ul class=\"nav pull-right\">
                    <li class=\"active\"><a href=\"/\">marmalade-repo</a></li>
                    <li><a href=\"#\">login</a></li>
                    <li><a href=\"#\">register</a></li>
                </ul>
            </div>
        </div>
        <div id=\"github\">
            <a href=\"https://github.com/nicferrier/elmarmalade\">
                <img style=\"position: absolute; top: 0; left: 0; border: 0;\" 
                     src=\"https://s3.amazonaws.com/github/ribbons/forkme_left_orange_ff7600.png\" 
                     alt=\"Fork me on GitHub\"></img>
            </a>
        </div>")

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
    ((string-match-p "\\.el$" package-file)
     (with-temp-buffer
       (insert-file-contents-literally package-file)
       (buffer-string) ; leave it in so it's easy to debug
       (package-buffer-info)))
    ((string-match-p "\\.tar$" package-file)
     (if (version< emacs-version "24.3.90")
         (package-tar-file-info package-file)
         (let (tar-buf)
           (with-temp-buffer
             (insert-file-contents-literally package-file)
             (tar-mode)
             (package-tar-file-info)))))
    (t (error "Unrecognized extension `%s'"
              (file-name-extension package-file)))))

(defun marmalade-pkname (package-info)
  (elt package-info 0))

(defun marmalade-pkversion (package-info)
  (elt package-info 3))

(defun marmalade/package-path (package-file)
  "Turn PACKAGE-FILE into a repository package path."
  (let ((info (marmalade/package-info package-file)))
    (let* ((props
            (list
             (cons "package-name" (marmalade-pkname info))
             (cons "version" (marmalade-pkversion info))
             (cons "package-dir" marmalade-package-store-dir)
             (cons "file-name" (file-name-base package-file))
             (cons "file-name-dir" (file-name-directory package-file))
             (cons "file-name-type" (file-name-extension package-file))))
           (package-path 
            (s-format
             (concat 
              "${package-dir}/${package-name}"
              "/${version}/${package-name}-${version}.${file-name-type}")
             'aget props)))
      (list
       :info info
       :package-path package-path))))

(defun marmalade/package-meta (package-file)
  (with-transient-file package-file
    (list
     (lm-header "Author")
     (lm-header "Maintainer")
     (lm-header "URL")
     (lm-header "Keywords"))))

(defun marmalade/temp-file (base-package-file-name)
  "Make a temp file for storing a package in."
  (let ((suffix (concat
                 "."
                 (file-name-extension base-package-file-name))))
    (make-temp-file "marmalade-upload" nil suffix)))


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
  ;; temp file names don't matter, except the extension, which we
  ;; record.
  (let* ((temp-package-file
          (marmalade/temp-file package-file-name)))
    ;; First save the uploaded data to a temp package file
    (with-temp-file temp-package-file
      (insert (substring-no-properties package-data)))
    ;; Return the package info, the package-path and the temp file
    (append
     (marmalade/package-path temp-package-file)
     (list :temp-package temp-package-file))))

(defun* marmalade/install-package (&key info package-path temp-package)
  "Take the package and save the package to the package store."
  ;; Try to move the file to the target path
  (when (file-exists-p package-path)
    (signal 'file-error (list package-path "existing package")))
  ;; Really creates a directory for now. Not ideal.
  (make-directory (file-name-directory package-path) t)
  (rename-file temp-package package-path)
  ;; Return the package-info
  info)

(defun marmalade/err->sym (err)
  "Convert an error structure to a sensible symbol."
  (intern
   (concat
    ":"
    (replace-regexp-in-string " " "-" (elt err 2) ))))

(defun marmalade/upload (httpcon)
  "Handle uploaded packages."
  ;;(message "upload starts with %s" (marmalade-get-packages "testuser"))
  (with-elnode-auth httpcon 'marmalade-auth
    (let* ((upload-file
            (elnode-http-param httpcon "package-file"))
           (upload-file-name
            (get-text-property 0 :elnode-filename upload-file))
           (base-file-name
            (file-name-nondirectory upload-file-name)))
      (condition-case err
          (destructuring-bind (&key info package-path temp-package)
              (marmalade/save-package upload-file base-file-name)
            (let* ((package-name (marmalade-pkname info))
                   (package-url (concat "/packages/" package-name))
                   (username (elnode-auth-username httpcon))
                   (user-packages (marmalade-get-packages username)))
              (message "user-packages %s" user-packages)
              (if (not (member package-name user-packages))
                  (elnode-send-400
                   httpcon
                   (format "you aren't authorized to update %s" package-name))
                  ;; Else save the package in the store...
                  (marmalade/install-package :info info
                                             :package-path package-path
                                             :temp-package temp-package)
                  ;; ... send the redirect ...
                  (elnode-send-redirect httpcon package-url 302)
                  ;; ... and send the request to update the cache
                  (elnode-proxy-post
                   httpcon "/packages/archive-contents/update"
                   :data (list (cons "package-info" (format "%S" info)))))))
        (error
         (when (listp err)
           (case (marmalade/err->sym err)
             (:existing-package
              (elnode-send-400
               httpcon (concat base-file-name " already exists"))))))))))

(defun marmalade/downloader (httpcon)
  "Download a specific package."
  (flet ((elnode-http-mapping (httpcon which)
           (let* ((package
                   (elnode--http-mapping-implementation
                    httpcon which))
                  (file (marmalade/package-name->filename package)))
             file)))
    (elnode-docroot-for
        marmalade-package-store-dir
        with target-package
        on httpcon
        do
        (with-transient-file target-package
          (elnode-http-start httpcon 200 '("Content-type" . "text/elisp"))
          (elnode-http-return httpcon (buffer-string))))))

(defun marmalade/package-handler (httpcon)
  "Dispatch to the appropriate handler on method."
  (marmalade/downloader httpcon))

(defun* marmalade/file-sort (files &key (by 5))
  "Sort the list of FILES optional by the specified attribute.

BY, if specified, should be a `file-attributes' number to use to
sort the files by.  By default it is `5', last modification
time."
  (noflet ((kvcmp (a b)
             (let ((fa (file-attributes a))
                   (fb (file-attributes b)))
               (funcall
                (case by
                  ((4 5 6) 'time-less-p)
                  ((1 2 3 7) this-fn)
                  (t (error "cannot compare %s" by)))
                (elt fa by)
                (elt fb by)))))
    (sort files 'kvcmp)))

(defun* marmalade/package-list (&key
                                sorted
                                take)
  "Return the list of package names in the archive.

SORTED is a `file-attributes' field to sort on specified as a
number as per the `file-attributes' help.  If not specified no
sorting is done.


TAKE specifies how many entries to return."
  (let* ((files (directory-files marmalade-package-store-dir t "^[^.]+$"))
	 (package-list
	  (->> (if sorted
		   (reverse (marmalade/file-sort files :by sorted))
                   files)
	    (--map (file-name-nondirectory it))
	    (--filter (not (equal it "archive-contents"))))))
    (if take
        (-take take package-list)
        package-list)))

(defun marmalade/top-version (package-dir)
  "Return the path to the newest version of a package in PACKAGE-DIR.

PACKAGE-DIR is the top level package directory:

  package-root/package-name

The full path including the package root is returned."
  (let* ((pkg-dir (file-name-as-directory package-dir))
         (versions
              (directory-files pkg-dir nil "[^.].*"))
         (top-version (car (reverse versions))))
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

(defun marmalade/uncomment (str)
  "Remove comments from lines in STR."
  (mapconcat
   (lambda (line)
     (if (string-match "^;* \\(.*\\)" line)
         (match-string 1 line)
         line))
   (split-string str "\n")
   "\n"))

(defun marmalade/commentary-grab (buffer)
  "Grab commentary from BUFFER."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let ((start-pos
             (when (re-search-forward "^;+ Commentary:" nil t)
               (re-search-forward "^;+" nil t)
               (line-beginning-position))))
        (if (not start-pos) ""
            ;; Else we do have a commentary
            (let* ((end-pos
                    (progn
                      (or
                       (re-search-forward "^;+ Code:" nil t)
                       ;; if we don't have code we should look for the start of the code
                       (re-search-forward "^(" nil t))
                      (line-beginning-position)))
                   (str (buffer-substring-no-properties
                         start-pos end-pos))
                   (fmted-str (marmalade/uncomment str)))
              (htmlize-protect-string fmted-str)))))))

(defun marmalade/commentary->about (commentary)
  "Transform the COMMENTARY to something we can display.

If COMMENTARY is `nil' the empty string is returned.

See `marmalade/commentary-grab' for details of how the commentary
is grabbed."
  (if commentary
      (save-match-data
        (with-temp-buffer
          (insert commentary)
          (marmalade/commentary-grab (current-buffer))))
      ""))

(defun marmalade/package-blurb (httpcon)
  "Provide an informative description of the package."
  (let (enable-local-variables enable-local-eval)
    (elnode-docroot-for
        marmalade-package-store-dir
        with target-package
        on httpcon
        do
        (let* ((package-name (elnode-http-mapping httpcon 1))
               (package-file (marmalade/top-version target-package))
               (package-download (file-name-nondirectory package-file))
               (info (marmalade/package-info package-file)))
          (destructuring-bind (name depends description version commentary)
              (if (version< emacs-version "24.3.90")
                  (mapcar 'identity info)
                  ;; Else new struct version
                  (list (package-desc-name info)
                        (package-desc-reqs info)
                        (package-desc-summary info)
                        (package-desc-version info)
                        (package-desc-summary info)))
            (destructuring-bind (author maintainer url keywords)
                (marmalade/package-meta package-file)
              (let* ((status 200)
                     (about-text (marmalade/commentary->about commentary))
                     (page
                      (condition-case err
                          (s-format 
                           (file-format-html
                            "package-page.html" marmalade-dir
                            'aget
                            `(("package-name" . ,package-name)
                              ("version" . ,(format "%S" version))
                              ("author" . ,(if (or (not author)
                                                   (equal author ""))
                                               "Unknown" author))
                              ("package-download" . ,package-download)
                              ("description" . ,description)
                              ("about" . ,about-text)
                              ;; Replace safely later
                              ("header" . "${header}")))
                           ;; This is safe to use HTML because it's controlled by us
                           'aget `(("header" . ,marmalade/page-header)))
                        (error
                         (setq status 500) ; set the response for later
                         (format
                          "<html><h3>marmalade error: %S</h3><pre>%S</pre></html>"
                          (xml-escape-string (format "%S" err))
                          (xml-escape-string about-text))))))
                (elnode-http-start httpcon status '(Content-type . "text/html"))
                (elnode-http-return httpcon page))))))))

(defconst marmalade/package-item
  "<li><a href=\"/packages/${name}\">${name}</a></li>"
  "Template for package items.")

(defconst marmalade/login-panel
  "<div id=\"login-panel\">logged in: <span id=\"username\">${username}</span></div>"
  "Login panel template.")

(defun marmalade/login (httpcon)
  (let* ((auth-cookie-cons
          (elnode-auth-get-cookie-value
           httpcon :cookie-name marmalade/cookie-name))
         (username (if (consp auth-cookie-cons) (car auth-cookie-cons) "")))
    (s-format
     "<div id=\"login-panel\">logged in: <span id=\"username\">${username}</span></div>"
     'aget
     `(("username" . ,username)))))

(defconst marmalade/page-file
  (expand-file-name "front-page.html" marmalade-dir))

(defun marmalade/latest-html ()
  "Convert `marmalade/package-list' into HTML LI elements."
  (let* ((latest (marmalade/package-list :sorted 5 :take 10)))
    (mapconcat
     (lambda (name)
       (s-format
        "<li><a href=\"/packages/${name}\">${name}</a></li>" 'aget
        `(("name" . ,(xml-escape-string name)))))
     latest "\n")))

(defun marmalade/packages-index (httpcon)
  "Upload a package of show a package index in HTML."
  (elnode-method httpcon
    (GET
     (elnode-send-html
      httpcon
      (file-format
       marmalade/page-file marmalade-dir 'aget
       `(("login-panel" . ,(marmalade/login httpcon))
         ("latest-html" . ,(marmalade/latest-html))
         ("header" . ,marmalade/page-header)))))
    ;; Or we need to upload
    (POST (marmalade/upload httpcon))))

(defun marmalade/upload-page (httpcon)
  "Send the upload page."
  (with-elnode-auth httpcon 'marmalade-auth
    (elnode-send-file
     httpcon
     (expand-file-name "upload-page.html" marmalade-dir))))

(defun marmalade/login-sender (httpcon _target _redirect)
  "Send the login page."
  (let ((page-file (expand-file-name "login-page.html" marmalade-dir)))
    (elnode-send-file httpcon page-file)))

(defun marmalade/auth-test (username)
  "Test function for marmalade user database."
  (kva "digest" (db-get username marmalade/users)))

(defun marmalade/auth-make-hash (username password)
  "Hash creation function uses the salt from the user database."
  (let* ((record (db-get username marmalade/users))
         (salt (kva "salt" record)))
    (marmalade/user-hash password salt)))

;; The authentication scheme.
(elnode-defauth 'marmalade-auth
  :cookie-name marmalade/cookie-name
  :auth-test 'marmalade/auth-test
  :make-hash 'marmalade/auth-make-hash
  :auth-db marmalade/users
  :sender 'marmalade/login-sender)

(defconst marmalade/webserver
  (elnode-webserver-handler-maker
   (expand-file-name "static" marmalade-dir))
  "The webserver for marmalade.")

(defun marmalade/special-docs (httpcon)
  "Send the login page."
  (let ((matched (elnode-http-mapping httpcon 1)))
    (elnode-send-file
     httpcon
     (expand-file-name (concat matched ".html") marmalade-dir))))

(defun marmalade-router (httpcon)
  "The top level router for marmalade-repo."
  (condition-case err
      (elnode-hostpath-dispatcher
       httpcon
       `(("^[^/]*//-/\\(.*\\)$" . ,marmalade/webserver)
         ("^[^/]*//\\(register-comingsoon\\|terms\\|docs\\)/*$" . marmalade/special-docs)
         ("^[^/]*//packages/new$" . marmalade/upload-page)

         ("^[^/]*//packages/archive-contents$" . marmalade-archive-contents-handler)
         ("^[^/]*//packages/archive-contents/\\([0-9]+\\)" . ,marmalade-archive-cache-webserver)
         ("^[^/]*//packages/archive-contents/update$" . marmalade-archive-update)

         ;; We don't really want to send 404's for these if we have them
         ("^[^/]+//packages/.*-readme.txt" . elnode-send-404)
         ("^[^/]+//packages/\\(.*\\.\\(el\\|tar\\)\\)$" . marmalade/package-handler)
         ("^[^/]+//packages/\\([^/]+\\)" . marmalade/package-blurb)
         ;; we have GET /packages/ and / be the same right now - probably not right
         ("^[^/]+//packages/$" . marmalade/packages-index)

         ("^[^/]+//$" . marmalade/packages-index))
       :log-name "marmalade"
       :auth-scheme 'marmalade-auth)
    (error (message "marmalade: some router error occurred %S" err))))

(provide 'marmalade-service)

;;; marmalade-service.el ends here
