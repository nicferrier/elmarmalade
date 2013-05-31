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
  marmalade-archive htmlize db s dash rx lisp-mnt outline)

(defconst marmalade/cookie-name "marmalade-user"
  "The name of the cookie we use for auth.")

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
    ((string-match-p "\\.el$" package-file)
     (with-temp-buffer
       (insert-file-contents-literally package-file)
       (buffer-string) ; leave it in so it's easy to debug
       (package-buffer-info)))
    ((string-match-p "\\.tar$" package-file)
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
    (list
     :info info
     :version version
     :package-name package-name
     :package-path
     (s-lex-format
      "${package-dir}/${package-name}/${version}/${package-name}-${version}.${file-name-type}"))))

(defun marmalade/package-meta (package-file)
  (with-current-buffer (find-file-noselect package-file)
    (list
     (lm-header "Author")
     (lm-header "Maintainer")
     (lm-header "URL")
     (lm-header "Keywords"))))

(defun marmalade/temp-file (base-package-file-name)
  "Make a temp file for storing a package in."
  (let ((suffix (concat
                 "."
                 (file-name-extension
                  base-package-file-name))))
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
  ;; temp file nmes don't matter, except the extension, which we
  ;; record.
  (let* ((temp-package-file
          (marmalade/temp-file package-file-name)))
    ;; First save the uploaded data to a temp package file
    (with-temp-file temp-package-file
      (insert-string
       (substring-no-properties package-data)))
    ;; Now read the file contents to get the real path
    ;; - this part could be done on the consumer side of a queue
    (destructuring-bind
          (&key info version package-name package-path)
        (marmalade/package-path temp-package-file)
      ;; Try to move the file to the target path
      (when (file-exists-p package-path)
        (signal 'file-error
                (list
                 package-path "existing package")))
      ;; Really creates a directory for now. Not ideal.
      (make-directory (file-name-directory package-path) t)
      (rename-file temp-package-file package-path)
      ;; Return the package name, possibly return the version too?
      package-name)))

(defun marmalade/err->sym (err)
  "Convert an error structure to a sensible symbol."
  (intern
   (concat
    ":"
    (replace-regexp-in-string " " "-" (elt err 2) ))))

(defun marmalade/upload (httpcon)
  "Handle uploaded packages."
  (with-elnode-auth httpcon 'marmalade-auth
    (let* ((upload-file
            (elnode-http-param httpcon "package-file"))
           (upload-file-name
            (get-text-property 0 :elnode-filename upload-file))
           (base-file-name
            (file-name-nondirectory upload-file-name)))
      (condition-case err
          (let* ((package-name (marmalade/save-package
                                upload-file base-file-name))
                 (package-url (concat "/packages/" package-name)))
            (elnode-send-redirect httpcon package-url 302))
        (error
         (case (marmalade/err->sym err)
           (:existing-package
            (elnode-send-400
             httpcon (concat base-file-name " already exists")))))))))

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
        (with-current-buffer
            (let ((enable-local-variables nil))
              (find-file-noselect target-package))
          (elnode-http-start httpcon 200 '("Content-type" . "text/elisp"))
          (elnode-http-return httpcon (buffer-string))))))

(defun marmalade/package-handler (httpcon)
  "Dispatch to the appropriate handler on method."
  (marmalade/downloader httpcon))

(defun* marmalade/package-list (&key
                                sorted
                                take)
  "Return the list of packages in the archive.

SORTED is a `file-attributes' field to sort on specified as a
number as per the `file-attributes' help.  If not specified no
sorting is done.

TAKE specifies how many entries to return."
  (flet ((sorter (a b)
           (funcall
            (case sorted
              ((4 5 6) 'time-less-p)
              ((1 2 3 7) 'cmp)
              (t (error "cannot compare %s" sorted)))
            (elt a (+ 1 sorted))
            (elt b (+ 1 sorted))))
         (attribs (f)
           (file-attributes
            (expand-file-name
             (concat
              (file-name-as-directory marmalade-package-store-dir) f)))))
    (let* ((files (directory-files marmalade-package-store-dir nil "^[^.].*"))
           (packages (--map (cons it (attribs it)) files))
           (package-list
            (if sorted
                (reverse (sort packages 'sorter))
                packages)))
      (if take
          (-take take package-list)
          package-list))))

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

(defconst marmalade/package-blurb-page "<html>
<head>
<link rel=\"stylesheet\" href=\"/-/style.css\" type=\"text/css\"></link>
<title>${package-name} @ Marmalade</title>
</head>
<body>
<h1>${package-name} - ${version}</h1>
<p class=\"description\">${description}</p>
<p class=\"author\">${author}</p>
<a href=\"${package-download}\">download ${package-name}</a>
<pre>${about-text}</pre>
</body>
<html>")

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
          (destructuring-bind
                (author maintainer url keywords)
              (marmalade/package-meta package-file)
            (let* ((about-text (marmalade/commentary->about commentary))
                   (page
                    (condition-case err
                        (s-lex-format marmalade/package-blurb-page)
                      (error (format
                              "<html>error: %S<br/><pre>%S</pre></html>"
                              (cdr err)
                              about-text)))))
              (elnode-http-start httpcon 200 '(Content-type . "text/html"))
              (elnode-http-return httpcon page)))))))

(defconst marmalade/package-item
  "<li><a href=\"/packages/${name}\">${name}</a></li>"
  "Template for package items.")

(defconst marmalade/login-panel
  "<div id=\"login-panel\">logged in: <span id=\"username\">${username}</span></div>"
  "Login panel template.")

(defun marmalade/login (httpcon)
  (let* ((auth-cookie-cons (elnode-auth-get-cookie-value
                            httpcon :cookie-name marmalade/cookie-name))
         (username (if (consp auth-cookie-cons) (car auth-cookie-cons) "")))
    (s-lex-format marmalade/login-panel)))

(defmacro with-transient-file (file-name &rest code)
  "Load FILE-NAME into a buffer and eval CODE.

Then dispose of the buffer.

File loading errors may be generated as by any call to visit a
file."
  (declare (indent 2))
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

(defun marmalade/packages-index (httpcon)
  "Upload a package of show a package index in HTML."
  (elnode-method httpcon
    (GET
     (let* ((login-panel (marmalade/login httpcon))
            (latest (marmalade/package-list :sorted 5 :take 10))
            (latest-html-lst
             (loop for (name . rest) in latest
                collect
                  (s-lex-format marmalade/package-item)))
            (latest-html 
             (mapconcat 'identity latest-html-lst "\n"))
            (page-file (concat marmalade-dir "front-page.html"))
            (buffer (find-file-noselect page-file)))
       (with-transient-file page-file
         (elnode-send-html
          httpcon
          (progn
            (s-buffer-lex-format (current-buffer))
            (buffer-string))))))
    ;; Or we need to upload
    (POST (marmalade/upload httpcon))))

(defun marmalade/upload-page (httpcon)
  "Send the upload page."
  (with-elnode-auth httpcon 'marmalade-auth
    (elnode-send-file
     httpcon
     (concat marmalade-dir "upload-page.html"))))

(defun marmalade/login-sender (httpcon target redirect)
  "Send the login page."
  (let ((page-file (concat marmalade-dir "login-page.html")))
    (elnode-send-file httpcon page-file)))

;; The authentication scheme.
(elnode-defauth 'marmalade-auth
  :cookie-name marmalade/cookie-name
  :auth-db marmalade/user-db
  :sender 'marmalade/login-sender)

(defconst marmalade/webserver
  (elnode-webserver-handler-maker
   (concat marmalade-dir "static"))
  "The webserver for marmalade.")

(defun marmalade-router (httpcon)
  (elnode-hostpath-dispatcher
   httpcon
   `(("^[^/]*//-/\\(.*\\)$" . ,marmalade/webserver)
     ("^[^/]*//packages/new$" . marmalade/upload-page)
     ("^[^/]+//packages/archive-contents" . marmalade-archive-handler)
     ;; We don't really want to send 404's for these if we have them
     ("^[^/]+//packages/.*-readme.txt" . elnode-send-404)
     ("^[^/]+//packages/\\(.*\\.\\(el\\|tar\\)\\)" . marmalade/package-handler)
     ("^[^/]+//packages/\\([^/]+\\)" . marmalade/package-blurb)
     ;; we have GET /packages/ and / be the same right now - probably not right
     ("^[^/]+//packages/$" . marmalade/packages-index)
     ("^[^/]+//$" . marmalade/packages-index))
   :log-name "marmalade"
   :auth-scheme 'marmalade-auth))

(provide 'marmalade-service)

;;; marmalade-s.el ends here
