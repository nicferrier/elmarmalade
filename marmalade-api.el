;;; marmalade-api.el --- api for elmarmalade  -*- lexical-binding: t -*-

(require 'elnode)
(require 'db)
(require 'marmalade-users)
(require 'noflet)

(defmacro elnode/err-cond (httpcon condition-message-forms &rest success-form)
  "Allow simple error handling with expressions:

 (elnode/err-cond
  httpcon
  (((not token)  \"no token specified, use login to get a token\")
   ((not (stringp token))  \"only string tokens supported\")
   ((equal token \"\")  \"token is empty string\")
   ((not username)  \"no username specified\")
   ((not (stringp username))  \"only string names supported\")
   ((equal username \"\")  \"username is empty string\")
   ((or (not package-name)(equal package-name \"\")) \"no package-name specified\"))
  (elnode-send-status httpcon 201))

The errors are always sent with `elnode-send-error' with a 400."
  (declare (debug (sexp sexp &rest form))
           (indent 2))
  (let ((httpconv (make-symbol "httpconv")))
    `(let ((,httpconv ,httpcon))
       (cond
         ,@(mapcar
            (lambda (f)
              (list (car f) `(elnode-send-400 ,httpconv ,(cadr f))))
            condition-message-forms)
         (t ,@success-form)))))

(defun marmalade-api/user-login (httpcon)
  "Return the user's authentication token.

The token is required by API v1 for authenticating all calls.

HTTP parameters \"NAME\" and \"PASSWORD\" specify the username
and password to be authenticated." 
  (let ((username (elnode-http-param httpcon "name"))
        (password (elnode-http-param httpcon "password"))
        (method (elnode-http-method httpcon)))
    (noflet ((elnode-send-400 (httpcon &optional message)
               (elnode-http-start httpcon 400 '(content-type . "application/json"))
               (elnode-http-return httpcon (json-encode `((message . ,message))))))
      (cond
        ;; List of constraints
        ((not (equal method "POST")) (elnode-send-400 httpcon "only POST supported"))
        ((equal username "") (elnode-send-400 httpcon "no user specified"))
        ((not username) (elnode-send-400 httpcon "no user specified"))
        ((not (stringp username)) (elnode-send-400 httpcon "don't understand non-string username"))
        ((equal password "") (elnode-send-400 httpcon "no password specified"))
        ((not password) (elnode-send-400 httpcon "no password specified"))
        ((not (stringp  password)) (elnode-send-400 httpcon "don't understand non-string password specified"))
        ((let ((user-record (db-get username marmalade/users)))
           (not (equal
                 (kva "digest" user-record)
                 (marmalade/user-hash password (kva "salt" user-record)))))
         (elnode-send-400 httpcon "bad authentication"))
        ;; Success!
        (t (elnode-send-json
            httpcon
            `((token . ,(kva "token" (db-get username marmalade/users))))))))))

(defmacro marmalade/api (httpcon &rest body)
  "Abstract the common API tests."
  (declare (debug (sexp &rest form))
           (indent 1))
  `(noflet ((elnode-send-400 (httpcon &optional message)
             (elnode-http-start httpcon 400 '(content-type . "application/json"))
             (elnode-http-return httpcon (json-encode `((message . ,message))))))
    (elnode-method httpcon
      (POST
       (let* ((token (elnode-http-param httpcon "token"))
              (username (elnode-http-param httpcon "name")))
         (elnode/err-cond httpcon
             (((not token) "no token specified, use login to get a token")
              ((not (stringp token)) "only string tokens supported")
              ((equal token "")  "token is empty string")
              ((not username)  "no username specified")
              ((not (stringp username))  "only string names supported")
              ((equal username "")  "username is empty string"))
           ,@body))))))

(defun marmalade-api/merge-mv (src dest)
  "Merge mv the SRC file to the DEST.

If SRC is a file then it's just `rename-file' with
OK-IF-ALREADY-EXISTS.

If SRC is a directory then it is recursively descended until a
safe mv can be done.  The SRC is then `delete-directory'.
Because the contents have been moved away that should not fail
because of recursion."
  (let* ((src-base (file-name-base src))
         (dest-target (expand-file-name src-base dest)))
    (if (file-exists-p dest-target)
        (if (not (file-directory-p dest-target))
            (rename-file src dest t)
            ;; Else we need to merge it further
            (list
             (--map
              (marmalade-api/merge-mv it dest-target)
              (directory-files src t "^[^.]+"))
             (delete-directory src)))
        (rename-file src dest))))

(defun marmalade-api/package (httpcon)
  "Manage a package.

Either remove the package or add an owner to it."
  (marmalade/api httpcon
    (let ((package-name (elnode-http-mapping httpcon 1))
          (action (or (elnode-http-param httpcon "delete")
                      (elnode-http-param httpcon "addowner"))))
      (elnode/err-cond httpcon
          (((or (not package-name) (equal package-name ""))
            "you must specify a package-name to change")
           ((not action)
            "specify 'delete' to rm a package or 'addowner' to add an owner"))
        (cond
          ((equal action "delete")
           (let* ((filename (expand-file-name package-name marmalade-package-store-dir)))
             (elnode/err-cond httpcon
                 (((not (file-exists-p filename))
                   "package does not exist")
                  ((not (member package-name (marmalade-get-packages username)))
                   "you are not authorized to remove the package"))
               ;; Make the archive dir if it doesn't exist
               (unless (file-exists-p marmalade-package-archive-dir)
                 (make-directory (expand-file-name marmalade-package-archive-dir) t))
               (condition-case err
                   (progn
                     (marmalade-api/merge-mv filename marmalade-package-archive-dir)
                     (elnode-send-json httpcon (list (cons "removed" package-name)))
                     (elnode-proxy-post
                      httpcon "/packages/archive-contents/purge"
                      :data (list (cons "package" package-name))))
                 (error (elnode-send-400
                         httpcon
                         (format "failed because %S" err)))))))
          ((equal action "addowner")
           (let ((new-owner (elnode-http-param httpcon "new-owner"))
                 (my-packages (marmalade-get-packages username)))
             (elnode/err-cond httpcon
                 (((not (db-get new-owner marmalade/users))
                   "adding an owner requires a new-owner already on marmalade")
                  ((not (or (member package-name my-packages)
                            (member "marmalade-client" my-packages)))
                   "you must be the owner of the package to add an owner"))
               (marmalade-add-packages new-owner package-name)
               (elnode-send-json httpcon `(("new-owner" . ,new-owner)))))))))))

(defun marmalade-api/upload (httpcon)
  "Upload a package."
  (marmalade/api httpcon
    ;; Check auth
    (elnode/err-cond httpcon
        (((let ((user-record (db-get username marmalade/users)))
            (not (equal token (kva "token" user-record)))) "bad authentication"))
      (let* ((upload-file (elnode-http-param httpcon "package"))
             (upload-file-name
              (condition-case err
                  (get-text-property 0 :elnode-filename upload-file)
                (error nil))))
        (elnode/err-cond httpcon
            ;; Check the upload ...
            (((not upload-file) "no package uploaded")
             ((not (stringp upload-file)) "uploaded package is not a string")
             ((equal upload-file "") "uploaded package is blank")
             ;; ... and the upload file name
             ((not upload-file-name) "uploaded package has no filename")
             ((equal upload-file-name "") "uploaded package filename is blank"))
          (let ((base-file-name (file-name-nondirectory upload-file-name)))
            ;; This is ripped directly from marmalade/upload - we should abstract probably
            (condition-case err
                (destructuring-bind (&key info package-path temp-package)
                    (marmalade/save-package upload-file base-file-name)
                  (let* ((package-name (marmalade-pkname info))
                         (package-url (concat "/packages/" package-name))
                         ;; don't have the username here
                         (user-packages (marmalade-get-packages username)))
                    (if (and
                         (file-exists-p (expand-file-name "../.." package-path))
                         (not (member package-name user-packages)))
                        (elnode-send-400
                         httpcon (format "you aren't authorized to update %s" package-name))
                        ;; Else save the package in the store...
                        (marmalade/install-package
                         :info info
                         :package-path package-path
                         :temp-package temp-package
                         :username username)
                        ;; ... send the content of the package as json
                        (let ((json-to-send
                               `(("message" . "done")
                                 ("package" . ,package-name))))
                          (elnode-send-json httpcon json-to-send))
                        ;; ... and send the request to update the cache
                        (elnode-proxy-post
                         httpcon "/packages/archive-contents/update"
                         :data (list (cons "package-info" (format "%S" info)))))))
              (error
               (when (listp err)
                 (case (marmalade/err->sym err)
                   (:packages-lacks-a-file-header
                    (elnode-send-400 httpcon (format "bad file header")))
                   (:existing-package
                    (elnode-send-400 httpcon (format "%S already exists" (elt err 2))))))))))))))

(defun marmalade-api/add-user (httpcon)
  "Allow users to be added to Marmalade.

You have to be an administrator to do it."
  (marmalade/api httpcon
    (let ((new-username (elnode-http-param httpcon "new-username"))
          (new-email (elnode-http-param httpcon "new-email")))
      (elnode/err-cond httpcon
          (((or (not new-email)
                (not (string-match-p "[a-zA-Z0-9._-]+@[a-zA-Z0-9._-]+" new-email)))
            "the email must match \"[a-zA-Z0-9._-]+@[a-zA-Z0-9._-]+\"")
           ((not (member "marmalade-client" (marmalade-get-packages username)))
            "you are not an administrator"))
        ;; Add a user with a random password
        (marmalade-add-user
         new-username
         (format "%X%X" (random)(random))
         new-email)
        ;; Make a verification code and return it
        (let ((unverified-id 
               (marmalade/add-unverified new-username)))
          (elnode-send-json
           httpcon `(("message" . ,(format "added %s" new-username))
                     ("email" . ,new-email)
                     ("verified-code" . ,unverified-id))))))))

(provide 'marmalade-api)

;;; marmalade-api.el ends here
