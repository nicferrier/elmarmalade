;;; marmalade-service.el --- the marmalade repository in emacs-lisp -*- lexical-binding: t -*-

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
  marmalade-vars marmalade-customs
  marmalade-archive marmalade-users marmalade-api
  file-format
  web htmlize db s
  noflet dash kv rx lisp-mnt outline)

(defconst marmalade/cookie-name "marmalade-user"
  "The name of the cookie we use for auth.")

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


(defun marmalade/page-header (httpcon)
  "Deliver the appropriate page header.

The page header differs whether you are logged in or not."
  (if-elnode-auth httpcon 'marmalade-auth
    (s-format marmalade/page-header-loggedin
              'aget (list (cons "username" (elnode-auth-username httpcon))))
    ;; Else send the plain thing
    marmalade/page-header))

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

(defun marmalade/package-buffer-info (&optional buffer)
  "Do `package-buffer-info' but with fixes."
  (let ((buf (if (bufferp buffer) buffer (current-buffer))))
    (noflet ((lm-section-end (hdr)
               (if (equal hdr lm-commentary-header)
                   (save-match-data
                     (save-excursion
                       (when (re-search-forward
                              "\\(^;+[ ]*Code:\\|^(\\)" nil t)
                         (line-beginning-position))))
                   (funcall this-fn hdr))))
         (package-buffer-info))))

(defun marmalade/package-info (package-file)
  "Return the package-info on the PACKAGE-FILE.

PACKAGE-FILE is either an ELISP or a TAR file to be uploaded to
the package repository."
  (cond
    ((string-match-p "\\.el$" package-file)
     (with-temp-buffer
       (insert-file-contents package-file)
       ;; Hack to fix broken packages
       (marmalade/package-buffer-info)))
    ((string-match-p "\\.tar$" package-file)
     (if (version< emacs-version "24.3.90")
         (package-tar-file-info package-file)
         ;; Else requires a different API
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
  "Returns a list: author, maintainer, url, keywords."
  (with-transient-file package-file
    (list
     (lm-header "Author")
     (lm-header "Maintainer")
     (lm-header "URL")
     (lm-header "Keywords"))))

(defun marmalade/temp-file (base-package-file-name)
  "Make a temp file for storing a package in."
  (expand-file-name
   base-package-file-name
   (make-temp-file "marmalade-upload" t)))


(defun marmalade/save-package (package-data package-file-name)
  "Save PACKAGE-DATA as PACKAGE-FILE-NAME in a temporary store.

PACKAGE-FILE-NAME is used as the basis of a temporary file name
and then the package info is computed and the target package path
name is computed."
  (let* ((temp-package-file
          (marmalade/temp-file package-file-name))
         (coding-system-for-write 'raw-text))
    ;; First save the uploaded data to a temp package file
    (with-temp-file temp-package-file
      (insert (substring-no-properties package-data)))
    ;; Return the package info, the package-path and the temp file
    (append
     (marmalade/package-path temp-package-file)
     (list :temp-package temp-package-file))))

(defun* marmalade/install-package (&key info package-path temp-package username)
  "Take the package and save the package to the package store.

If the package already exists then `file-error' is signalled."
  ;; Try to move the file to the target path
  (when (file-exists-p package-path)
    (signal 'file-error (list "existing package" (file-name-base package-path))))
  ;; Really creates a directory for now. Not ideal.
  (make-directory (file-name-directory package-path) t)
  (rename-file temp-package package-path)
  ;; Update the user database
  (marmalade-add-packages username (elt info 0))
  ;; Return the package-info
  info)

(defun marmalade/err->sym (err)
  "Convert an error structure to a sensible symbol."
  (intern
   (concat
    ":"
    (replace-regexp-in-string " " "-" (downcase (elt err 1))))))

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
              ;; If the base name dir doesn't exist the package hasn't been uploaded yet
              (if (and
                   (file-exists-p (expand-file-name "../.." package-path))
                   (not (member package-name user-packages)))
                  (elnode-send-400
                   httpcon
                   (format "you aren't authorized to update %s" package-name))
                  ;; Else save the package in the store...
                  (marmalade/install-package
                   :info info
                   :package-path package-path
                   :temp-package temp-package
                   :username username)
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
               httpcon (concat (cadr err) " already exists"))))))))))

(defun marmalade/downloader (httpcon)
  "Download a specific package."
  (noflet ((elnode-http-mapping (httpcon which)
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
         (top-version (car (reverse (-sort 'version< versions)))))
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
  (s-join "\n"
          (--map
           (if (string-match "^;*[ ]*\\(.*\\)" it)
               (match-string 1 it)
               it)
           (split-string str "\n"))))

(defun package-tar-file-info (file)
  "Find package information for a tar file.
FILE is the name of the tar file to examine.
The return result is a vector like `package-buffer-info'."
  (let ((default-directory (file-name-directory file))
	(file (file-name-nondirectory file)))
    (unless (string-match (concat "\\`" package-subdirectory-regexp "\\.tar\\'")
			  file)
      (error "Invalid package name `%s'" file))
    (let* ((pkg-name (match-string-no-properties 1 file))
	   (pkg-version (match-string-no-properties 2 file))
	   ;; Extract the package descriptor.
	   (pkg-def-contents (shell-command-to-string
			      ;; Requires GNU tar.
			      (concat "tar -xOf " file " "

				      pkg-name "-" pkg-version "/"
				      pkg-name "-pkg.el")))
	   (pkg-def-parsed (package-read-from-string pkg-def-contents)))
      (unless (eq (car pkg-def-parsed) 'define-package)
	(error "No `define-package' sexp is present in `%s-pkg.el'" pkg-name))
      (let ((name-str       (nth 1 pkg-def-parsed))
	    (version-string (nth 2 pkg-def-parsed))
	    (docstring      (nth 3 pkg-def-parsed))
	    (requires       (nth 4 pkg-def-parsed))
	    (readme  ; Requires GNU tar.
             (let ((filename
                    (car
                     (s-split
                      "\n"
                      (shell-command-to-string
                       (concat "tar --wildcards -tOf " file " "
                               pkg-name "-" pkg-version "/README*"))))))
               (if filename
                   (propertize 
                    (shell-command-to-string
                     ;; Requires GNU tar.
                     (concat "tar -xOf " file " " filename))
                    :filename filename)))))
	(unless (equal pkg-version version-string)
	  (error "Package has inconsistent versions"))
	(unless (equal pkg-name name-str)
	  (error "Package has inconsistent names"))
	;; Kind of a hack.
	(if (string-match ": Not found in archive" readme)
	    (setq readme nil))
	;; Turn string version numbers into list form.
	(if (eq (car requires) 'quote)
	    (setq requires (car (cdr requires))))
	(setq requires
	      (mapcar (lambda (elt)
			(list (car elt)
			      (version-to-list (cadr elt))))
		      requires))
	(vector pkg-name requires docstring version-string readme)))))

(defun marmalade/commentary-grab (buffer)
  "Grab commentary from BUFFER."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let ((start-pos
             (when (re-search-forward "^;+ Commentary:" nil t)
               (re-search-forward "^;+" nil t)
               (line-beginning-position))))
        (if start-pos
            ;; We do have a commentary...
            (let* ((end-pos
                    (progn
                      (or
                       (re-search-forward "^;+ Code:" nil t)
                       ;; if we don't have code we should look for the start of the code
                       (re-search-forward "^(" nil t)
                       (goto-char (point-max)))
                      (line-beginning-position)))
                   (str (buffer-substring-no-properties
                         start-pos end-pos))
                   (fmted-str (marmalade/uncomment str)))
              (htmlize-protect-string fmted-str))
            ;; Else we don't have a commentary... maybe we have a readme?
            (htmlize-protect-string (buffer-string)))))))

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
      "This package has provided no commentary."))

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
                     (vars
                      `(("package-name" . ,package-name)
                        ("version" . ,(format "%S" version))
                        ("author" . ,(if (or (not author) (equal author "")) "Unknown" author))
                        ("package-download" . ,package-download)
                        ("description" . ,description)
                        ("url" . ,(or url "#blurb"))
                        ("url-text" . ,(or url ""))
                        ("about" . ,about-text)
                        ;; Replace safely later
                        ("header" . ,(propertize 
                                      (marmalade/page-header httpcon)
                                      :file-format-html-safe t))))
                     (page
                      (file-format-html "package-page.html" marmalade-dir 'aget vars)))
                (elnode-http-start httpcon status '(Content-type . "text/html"))
                (elnode-http-return httpcon page))))))))

(defun marmalade/login (httpcon)
  (let* ((auth-cookie-cons
          (elnode-auth-get-cookie-value
           httpcon :cookie-name marmalade/cookie-name))
         (username (if (consp auth-cookie-cons) (car auth-cookie-cons) "")))
    (propertize
     (s-format
      "<div id=\"login-panel\">logged in: <span id=\"username\">${username}</span></div>"
      'aget
      `(("username" . ,username)))
     :file-format-html-safe t)))

(defun marmalade-user-profile (httpcon)
  "Elnode handler for users.

`elnode-http-mapping' 1 should be the username."
  (let ((username (elnode-http-mapping httpcon 1)))
    (noflet ((wrap (str lst)
               (mapconcat
                (lambda (e)
                  (replace-regexp-in-string "%s" e str)) lst "")))
      (elnode-http-start httpcon 200 '(Content-type . "text/html"))
      (elnode-http-return
       httpcon
       (file-format-html
        "profile-page.html" marmalade-dir
        'aget `(("username" . ,username)
                ("package-list"
                 . ,(propertize
                     (wrap
                      "<li><a href=\"/packages/%s\">%s</a></li>"
                      (marmalade-get-packages username))
                     :file-format-html-safe t))
                ("header"
                 . ,(propertize 
                     (marmalade/page-header httpcon)
                     :file-format-html-safe t))))))))

(defconst marmalade/page-file
  (expand-file-name "front-page.html" marmalade-dir))

(defconst marmalade/news
  (propertize
   (with-temp-buffer
     (insert-file-contents
      (expand-file-name "news.html" marmalade-dir))
     (buffer-string)) :file-format-html-safe t)
  "A list of news about marmalade kept in the repository.")

(defun marmalade/latest-html ()
  "Convert `marmalade/package-list' into HTML LI elements."
  (let* ((latest (marmalade/package-list :sorted 5 :take 10)))
    (propertize
     (mapconcat
      (lambda (name)
        (s-format
         "<li><a href=\"/packages/${name}\">${name}</a></li>" 'aget
         `(("name" . ,(xml-escape-string name)))))
      latest "\n") :file-format-html-safe t)))

(defun marmalade/packages-index (httpcon)
  "Upload a package or show a package index in HTML."
  (elnode-method httpcon
    (GET
     (elnode-send-html
      httpcon
      (file-format-html
       marmalade/page-file marmalade-dir 'aget
       `(("login-panel" . ,(marmalade/login httpcon))
         ("latest-html" . ,(marmalade/latest-html))
         ("news" . ,marmalade/news)
         ("header" . ,(propertize
                       (marmalade/page-header httpcon)
                       :file-format-html-safe t))))))
    ;; Or we need to upload
    (POST (marmalade/upload httpcon))))

(defun marmalade/upload-page (httpcon)
  "Send the upload page."
  (with-elnode-auth httpcon 'marmalade-auth
    (elnode-send-file
     httpcon
     (expand-file-name "upload-page.html" marmalade-dir))))

(defconst marmalade/webserver
  (elnode-webserver-handler-maker
   (expand-file-name "static" marmalade-dir))
  "The webserver for marmalade.")

(defconst marmalade/special-docroot
  (expand-file-name "specialpages" marmalade-dir)
  "The docroot of 'special' pages.

Special pages are the ones that aren't part of the main app,
things like terms and conditions and documentation.")

(defun marmalade/special-docs (httpcon)
  "Send special pages."
  (let* ((matched (elnode-http-mapping httpcon 1))
         (text
          (file-format-html
           (concat matched ".html") marmalade/special-docroot
           'aget `(("latest-html" . ,(marmalade/latest-html))
                   ("header" . ,(propertize 
                                 (marmalade/page-header httpcon)
                                 :file-format-html-safe t))))))
    (elnode-send-html httpcon text)))

(defun marmalade-unverified (httpcon)
  "Handle verification of users.

The verified codes are stored in a db (for now).  Users with
verified codes can visit this page and get a form to set their
password.  The form posts back to the same URL.

On completion the verified code is removed from the list and the
list is put back."
  (elnode-method httpcon
    (POST
     (let* ((password (elnode-http-param httpcon "new-password"))
            (confirm (elnode-http-param httpcon "confirm-password"))
            (verify-code (elnode-http-mapping httpcon 1))
            (unverifieds (marmalade/list-unverifieds))
            (found-user (kva verify-code unverifieds)))
       (cond
         ((or (not (equal password confirm))
              (< (length password) 6))
          (elnode-send-400 httpcon "Sorry. The passwords didn't match."))
         ((or (not verify-code)(equal verify-code ""))
          (elnode-send-400 httpcon "Illegal verify code."))
         (t ; Else we're good ...
          (marmalade-set-auth found-user password)
          (marmalade/remove-verified verify-code)
          (elnode-send-redirect httpcon "/login/")))))
    (GET
     (let* ((unverifieds (marmalade/list-unverifieds))
            (requested (elnode-http-mapping httpcon 1))
            (found-user (kva requested unverifieds)))
       (if (not found-user)
           (elnode-send-400 httpcon "We could't find that code. Sorry.")
           ;; Else
           (elnode-send-html
            httpcon
            (file-format-html
             "verify.html" marmalade-dir 'aget
             `(("verify-id" . ,requested)
               ("username" . ,found-user)
               ("header" . ,(propertize
                             (marmalade/page-header httpcon)
                             :file-format-html-safe t))))))))))

(defmacro try (form &rest body)
  "Try FORM and eval BODY if it fails.

Within BODY `backtrace' will return the backtrace including the
stack which caused the problem.  This can be used for actually
debugging the problem."
  (declare (indent 1))
  (let ((ret-var (make-symbol "ret-var")))
    `(let* (,ret-var
            (debug-on-error t)
            (debugger
             (lambda (&rest args)
               (setq ,ret-var (progn ,@body)))))
       (condition-case err
           (setq ,ret-var (progn ,form))
         ((debug error)))
       ,ret-var)))

(defun marmalade-router (httpcon)
  "The top level router for marmalade-repo."
  (try
      (elnode-hostpath-dispatcher
       httpcon
       `(("^[^/]*//-/\\(.*\\)$" . ,marmalade/webserver)
         ("^[^/]*//\\(register-comingsoon\\|terms\\|docs\\)/*$" . marmalade/special-docs)
         ("^[^/]*//packages/new$" . marmalade/upload-page)

         ("^[^/]*//packages/archive-contents$" . marmalade-archive-contents-handler)
         ("^[^/]*//packages/archive-contents/\\([0-9]+\\)" . ,marmalade-archive-cache-webserver)
         ("^[^/]*//packages/archive-contents/update$" . marmalade-archive-update)
         ("^[^/]*//packages/archive-contents/purge$" . marmalade-archive-purge)

         ;; We don't really want to send 404's for these if we have them
         ("^[^/]+//packages/.*-readme.txt" . elnode-send-404)
         ("^[^/]+//packages/\\(.*\\.\\(el\\|tar\\)\\)$" . marmalade/package-handler)
         ("^[^/]+//packages/\\([^/]+\\)" . marmalade/package-blurb)
         ;; we have GET /packages/ and / be the same right now - probably not right
         ("^[^/]+//packages/$" . marmalade/packages-index)

         ("^[^/]+//v1/packages$" .  marmalade-api/upload)
         ("^[^/]+//v1/package/\\(.*\\)$" .  marmalade-api/package)
         ("^[^/]+//v1/users/add/$" .  marmalade-api/add-user)
         ("^[^/]+//v1/users/login/*$" .  marmalade-api/user-login)

         ;; The profile
         ("^[^/]+//profile/\\([^/]+\\)/*" . marmalade-user-profile)
         ("^[^/]+//verify/\\([^/]+\\)/*" . marmalade-unverified)

         ("^[^/]+//$" . marmalade/packages-index))
       :log-name "marmalade"
       :auth-scheme 'marmalade-auth)
    ;; And handle the error by sending an error page otherwise
    (let ((bt (with-output-to-string (backtrace))))
      (elnode-http-start httpcon 400 '(Content-type "text/html"))
      (elnode-http-return
       httpcon
       (format
        "<h1>Error!</h1><p>the backtrace was</p><pre>%s</pre>"
        (xml-escape-string
         (s-join "\n" (-drop 15 (-take 10 (split-string bt "\n"))))))))))

(provide 'marmalade-service)

;;; marmalade-service.el ends here
