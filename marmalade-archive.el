;;; make package archive files

;;; Commentary

;; Makes package archives from a source tree of marmalade packages.

;; The functions in marmalade-mongo define a way of turning the
;; marmalade v1 mongo-db into a list of files.

;; This is how marmalade manages it's packages, in a file system of
;; package files. We read the packages from the filesystem into a
;; hashtable and then present the hashtable.

;; The idea is that a web server could update the hashtable whenever a
;; user uploads.

;;; Notes

;; The code here isn't cleanly namespaced with marmalade/archive or
;; anything like that. But why should it? it's ridiculous to have to
;; do that all the time with a single package.

(require 'rx)
(require 'package)

(defgroup marmalade-archive nil
  "The marmalade package store. Elisp version."
  :group 'applications)

(defcustom marmalade-package-store-dir nil
  "The location of the package files."
  :group 'marmalade-archive
  :type '(choice
          (const :tag "Default" nil)
          (directory "~/marmalade-packages")))

(defcustom marmalade-archive-index-filename ""
  "The location of the package index file.

The package index file is a cached index of the state of the
package archive."
  :group 'marmalade-archive
  :type 'file)


;; Directory root mangling code

(defun marmalade/list-files-string (root)
  "Make the marmalade file list buffer for ROOT.

The file list buffer is a list of all files under the ROOT.  We
just use unix find for this right now.  But it could be done with
emacs-lisp as well of course.

The files are then filtered by `marmalade/list-files'."
  (let ((marmalade-list-buffer (get-buffer " *marmalade-list*")))
    (if (bufferp marmalade-list-buffer)
        (with-current-buffer marmalade-list-buffer
          (buffer-substring-no-properties (point-min)(point-max)))
        ;; Else really do it
        (with-current-buffer (get-buffer-create " *marmalade-list*")
          (erase-buffer)
          (shell-command
           (concat "find " root " -type f")
           (current-buffer))
          (buffer-substring (point-min)(point-max))))))

(defun marmalade/list-files (root)
  "Turn ROOT into a list of maramalade meta data."
  (loop for filename in (split-string
                         (marmalade/list-files-string root) "\n")
     if (string-match
         (concat
          "^.*/\\([A-Za-z0-9-]+\\)/"
          "\\([0-9.]+\\)/"
          "\\([A-Za-z0-9.-]+\\).\\(el\\|tar\\)$")
         filename)
     collect
       (list
        filename
        ;; (match-string 1 filename)
        ;; (match-string 2 filename)
        ;; (match-string 3 filename)
        ;; The type
        (match-string 4 filename))))

(defun marmalade/commentary-handle (buffer)
  "package.el does not handle bad commentary declarations.

People forget to add the ;;; Code marker ending the commentary.
This does a substitute."
  (with-current-buffer buffer
    (goto-char (point-min))
    ;; This is where we could remove the ;; from the start of the
    ;; commentary lines
    (let ((commentary-pos
           (re-search-forward "^;;; Commentary" nil t)))
      (if commentary-pos
          (buffer-substring-no-properties
           (+ commentary-pos 3)
           (- (re-search-forward "^;+ .*\n[ \n]+(" nil t) 2))
          "No commentary."))))

(defun marmalade/package-buffer-info (buffer)
  "Do `package-buffer-info' but with fixes."
  (with-current-buffer buffer
    (let ((pkg-info (package-buffer-info)))
      (unless (save-excursion (re-search-forward "^;;; Code:"  nil t))
        (aset pkg-info 4 (marmalade/commentary-handle (current-buffer))))
      pkg-info)))

(defun marmalade/package-stuff (filename type)
  "Make the FILENAME a package of TYPE.

This reads in the FILENAME.  But it does it safely and it also
kills it."
  (let ((ptype
         (case (intern type)
           (el 'single)
           (tar 'multi))))
    (cons
     ptype
     (case ptype
       (single
        (let ((buffer (let ((enable-local-variables nil))
                        (find-file-noselect filename))))
          (unwind-protect
               (marmalade/package-buffer-info buffer)
            ;; We should probably only kill it if we didn't have it before
            (kill-buffer buffer))))
       (multi
        (package-tar-file-info filename))))))

(defun marmalade/root->archive (root)
  "For ROOT make an archive list."
  (loop for (filename type) in (marmalade/list-files root)
     with package-stuff
     do
       (setq package-stuff
             (condition-case err
                 (marmalade/package-stuff filename type)
               (error nil)))
     if package-stuff
     collect package-stuff))

(defun marmalade/packages-list->archive-list (packages-list)
  "Turn the list of packages into an archive list."
  ;; elpakit has a version of this
  (loop for (type . package) in packages-list
     collect
       (cons
        (intern (elt package 0)) ; name
        (vector (version-to-list (elt package 3)) ; version list
                (elt package 1) ; requirements
                (elt package 2) ; doc string
                type))))

;; Handle the cache

(defvar marmalade/archive-cache (make-hash-table :test 'equal)
  "The cache of all current packages.")

(defun marmalade/archive-cache-fill (root)
  "Fill the cache by reading the ROOT."
  (loop
     for (type . package) in (marmalade/root->archive root)
     do (let* ((package-name (elt package 0))
               (current-package
                (gethash package-name marmalade/archive-cache)))
          (if current-package
              ;; Put it only if it's a newer version
              (let ((current-version (elt (cdr current-package) 3))
                    (new-version (elt package 3)))
                (when (string< current-version new-version)
                  (puthash
                   package-name (cons type package)
                   marmalade/archive-cache)))
              ;; Else just put it
              (puthash
               package-name (cons type package)
               marmalade/archive-cache)))))

(defun marmalade/cache->package-archive ()
  "Turn the cache into the package-archive."
  (marmalade/packages-list->archive-list
   (kvalist->values
    (kvhash->alist marmalade/archive-cache))))

(defun marmalade/package-store-modtime ()
  (let ((modtime 5))
    (elt
     (file-attributes
      marmalade-package-store-dir) modtime)))

(defun marmalade/archive-index-modtime ()
  (let ((modtime 5))
    (elt
     (file-attributes
      marmalade-archive-index-filename) modtime)))

(defun marmalade/archive-index-exists-p ()
  (and
   (stringp marmalade-archive-index-filename)
   (file-exists-p
    marmalade-archive-index-filename)))

(defun marmalade-cache-test ()
  "The implementation of the cache test.

Return `t' if the `marmalade/archive-cache-fill' should be
executed on the `marmalade-package-store-dir'."
  (or
   (not (marmalade/archive-index-exists-p))
   (let* ((last-store-change (marmalade/package-store-modtime))
          (cached-change-time (marmalade/archive-index-modtime)))
     (time-less-p cached-change-time last-store-change))))

(defun marmalade/archive-load ()
  "Load the archive from `marmalade-archive-index-filename'."
  (setq marmalade/archive-cache
        (catch 'return
          (load-file
           (concat
            marmalade-archive-index-filename ".el")))))

(defun marmalade/archive-save ()
  "Save the archive to `marmalade-archive-index-filename'."
  (let ((archive-file
         (concat marmalade-archive-index-filename ".el")))
    (when (and
           (stringp marmalade-archive-index-filename)
           (not (equal marmalade-archive-index-filename ""))
           (file-writable-p archive-file))
      (with-temp-buffer
        (insert
         (format "(throw 'return %S)" marmalade/archive-cache))
        (write-file archive-file)))))

(defun marmalade/package-archive ()
  "Make the package archive from package cache.

Re-caches the package cache from the files on disc if the call to
`marmalade-cache-test' returns `t'."
  (interactive)
  ;; Possibly rebuild the cache file
  (when (< (hash-table-size marmalade/archive-cache) 1)
    (if (not (marmalade-cache-test))
        (marmalade/archive-load)
        ;; Else rebuild the cache
        (marmalade/archive-cache-fill marmalade-package-store-dir)
        (marmalade/archive-save)))
  ;; Return the archive
  (cons 1 (marmalade/cache->package-archive)))

;; FIXME - should we make this conditional on elnode somehow?
(defun marmalade-archive-handler (httpcon)
  "Send the archive to the HTTP connection."
  ;; FIXME - What's the right mimetype here?
  (elnode-http-start httpcon 200 '("Content-type" . "text/plain"))
  (elnode-http-return
   httpcon
   (format "%S" (marmalade/package-archive))))

(provide 'marmalade-archive)

;;; marmalade-archive.el ends here
