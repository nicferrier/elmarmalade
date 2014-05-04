;;; tests for marmalade  -*- lexical-binding: t -*-

(require 'ert)
(require 'marmalade-service)
(require 'fakir)
(require 's)
(require 'noflet)

(ert-deftest marmalade-package-explode ()
  "Make sure the regex works."
  (let ((file "ascii-3.1.el"))
    (destructuring-bind (name version type)
        (marmalade/explode-package-string file)
      (should (equal name "ascii"))
      (should (equal version "3.1"))
      (should (equal type "el")))))

(ert-deftest marmalade/package-name->filename ()
  "Convert package name to a filename."
  (let ((file "ascii-3.1.el")
        (marmalade-package-store-dir "/packages"))
    (should
     (equal
      (marmalade/package-name->filename file)
      "ascii/3.1/ascii-3.1.el"))))

(defun marmalade/fakir-file (file-name &optional mod-time)
  "Make FILE-NAME be fake."
  (fakir-file
   :directory (file-name-directory file-name)
   :filename (file-name-nondirectory file-name)
   :mtime (current-time-string (or mod-time (current-time)))))


(ert-deftest marmalade/root->archive ()
  "Test making an archive list from the file list."
  (let ((files-alist
         `(("/r/m/p/nic-p/0.1.1/nic-p.el"
            (single . ["nic-p" () "nic's package summary" "0.1.1" "Description."]))
           ("/r/m/p/bob-p/0.1.1/bob-p.el"
            (single . ["bob-p" () "bob's package summary" "0.1.1" "Description."]))
           ("/r/m/p/tar-p/0.5.1/tar-p.tar"
            (tar . ["tar-p" () "tar package summary" "0.5.1" "Description."])))))
    (noflet
        ;; This is the function that runs find to get it's stuff
        ((marmalade/list-dir (root :package-names-list package-names-list)
           (mapcar (lambda (d)
                     (string-match "/r/m\\(.*\\)" d)
                     (match-string 1 d))
                   (kvalist->keys files-alist)))
         (marmalade/package-stuff (filename type)
           (cadr (assoc filename files-alist))))
      (should
       (equal
        (marmalade/root->archive "/r/m/p")
        (-map 'car (kvalist->values files-alist)))))))

(ert-deftest marmalade/package-list->archive-list ()
  "Test making an archive list from the file list."
  (let ((files-alist
         '(("/r/m/p/nic-p/0.1.1/nic-p.el"
            (single . ["nic-p" () "nic's package summary" "0.1.1" "Description."]))
           ("/r/m/p/bob-p/0.1.1/bob-p.el"
            (single . ["bob-p" () "bob's package summary" "0.1.1" "Description."]))
           ("/r/m/p/tar-p/0.5.1/tar-p.tar"
            (tar . ["tar-p" () "tar package summary" "0.5.1" "Description."])))))
    (noflet
        ;; This is the function that runs find to get it's stuff
        ((marmalade/list-dir (root :package-names-list package-names-list)
           (mapcar (lambda (d)
                     (string-match "/r/m\\(.*\\)" d)
                     (match-string 1 d))
                   (kvalist->keys files-alist)))
         (marmalade/package-stuff (filename type)
           (cadr (assoc filename files-alist))))
      (should
       (equal
        (marmalade/packages-list->archive-list
         (marmalade/root->archive "/r/m/p"))
        '((nic-p . [(0 1 1) () "nic's package summary" single])
          (bob-p . [(0 1 1) () "bob's package summary" single])
          (tar-p . [(0 5 1) () "tar package summary" tar])))))))

(defconst marmalade/test-package-files
  '("/packages/elixir-mix/0.0.1/elixir-mix-0.0.1.el"
    "/packages/elixir-mix/0.0.2/elixir-mix-0.0.2.el"
    "/packages/sawfish/1.32/sawfish-1.32.el"
    "/packages/less-css-mode/0.9/less-css-mode-0.9.el"
    "/packages/less-css-mode/0.8/less-css-mode-0.8.el"
    "/packages/less-css-mode/0.7/less-css-mode-0.7.el"
    "/packages/less-css-mode/0.10/less-css-mode-0.10.el"
    "/packages/less-css-mode/0.6/less-css-mode-0.6.el"
    "/packages/less-css-mode/0.4/less-css-mode-0.4.el"
    "/packages/less-css-mode/0.11/less-css-mode-0.11.el"
    "/packages/less-css-mode/0.3/less-css-mode-0.3.el"
    "/packages/less-css-mode/0.14/less-css-mode-0.14.el"
    "/packages/less-css-mode/0.2/less-css-mode-0.2.el"
    "/packages/less-css-mode/0.1/less-css-mode-0.1.el"
    "/packages/less-css-mode/0.12/less-css-mode-0.12.el"
    "/packages/less-css-mode/0.15/less-css-mode-0.15.el"
    "/packages/less-css-mode/0.5/less-css-mode-0.5.el"
    "/packages/less-css-mode/0.13/less-css-mode-0.13.el"
    "/packages/flymake-easy/0.9/flymake-easy-0.9.el"
    "/packages/flymake-easy/0.8/flymake-easy-0.8.el"
    "/packages/flymake-easy/0.7/flymake-easy-0.7.el"
    "/packages/flymake-easy/0.6/flymake-easy-0.6.el"
    "/packages/flymake-easy/0.4/flymake-easy-0.4.el"
    "/packages/flymake-easy/0.3/flymake-easy-0.3.el"
    "/packages/flymake-easy/0.2/flymake-easy-0.2.el"
    "/packages/flymake-easy/0.1/flymake-easy-0.1.el"
    "/packages/flymake-easy/0.5/flymake-easy-0.5.el")
  ;; FIXME - derive this list from the recipe? or from the packaged directory?
  "List of packaged files to test with.

Real files like these are held in the source code repository for
elmarmalade.")

(ert-deftest marmalade/list-files-dir ()
  "Test that we can read in the package store directory."
  (let ((marmalade-package-store-dir
         (concat marmalade-dir "marmalade-repo-test/packages")))
    (should
     (equal
      (sort 
       (marmalade/list-dir
        marmalade-package-store-dir)
       'string-lessp)
      (sort marmalade/test-package-files 'string-lessp)))
    ;; Now try with a filter
    (should
     (equal
      (sort 
       (marmalade/list-dir
        marmalade-package-store-dir
        :package-names-list '("elixir-mix" "sawfish"))
       'string-lessp)
      (sort '("/packages/elixir-mix/0.0.1/elixir-mix-0.0.1.el"
              "/packages/elixir-mix/0.0.2/elixir-mix-0.0.2.el"
              "/packages/sawfish/1.32/sawfish-1.32.el")
            'string-lessp)))))

(defconst marmalade/test-packages
  '((elixir-mix . [(0 0 2) nil "Emacs integration for Elixir's elixir-mix" single]) ; "marmalade-repo-test/packages/elixir-mix/0.0.2/elixir-mix-0.0.2.el"
    (sawfish . [(1 32) () "Sawfish mode." single]) ; "marmalade-repo-test/packages/sawfish/1.32/sawfish-1.32.el"
    (less-css-mode . [(0 15) () "Major mode for editing LESS CSS files (lesscss.org)" single]) ; "marmalade-repo-test/packages/less-css-mode/0.15/less-css-mode-0.15.el"
    (flymake-easy . [(0 9) () "Helpers for easily building flymake checkers" single])) ; "marmalade-repo-test/packages/flymake-easy/0.9/flymake-easy-0.9.el"
  "List of packages from the `marmalade/test-packages'.

A static representation of what the dummy package tree should
look like.  It is based on `marmalade/test-package-files'.")

(ert-deftest marmalade/package-archive ()
  "Make the package cache and test it.

This is a full test of the cache, it generates the internal hash
from the fake directory structure and then caches it and then
reads the cache back in and checks it against
`marmalade/test-packages'."
  (let* ((marmalade-archive-dir
          (concat marmalade-dir "marmalade-repo-test/archives"))
         (marmalade-package-store-dir
          (concat marmalade-dir "marmalade-repo-test/packages"))
         (newhash (make-hash-table :test 'equal))
         ;; The newest archive
         (newest (progn
                   (marmalade-archive-make-cache)
                   (marmalade/archive-newest))))
    (noflet ((symbol-lessp (a b)
               (string-lessp
                (symbol-name (car a))
                (symbol-name (car b))))
             (marmalade/archive-newest () newest))
      (should
       (equal
        ;; Read it in and sort it.
        (sort
         (kvhash->alist (marmalade/archive-cache->hash newhash))
         'symbol-lessp)
        (sort marmalade/test-packages 'symbol-lessp))))))

(ert-deftest marmalade/archive-list-files ()
  "Test we can list the files in the archive."
  (let* ((marmalade-archive-dir
          (concat marmalade-dir "marmalade-repo-test/archives"))
         (marmalade-package-store-dir
          (concat marmalade-dir "marmalade-repo-test/packages"))
         (sawfish (concat marmalade-package-store-dir
                          "/sawfish/1.32/sawfish-1.32.el")))
    (should
     (equal
      (marmalade/list-files
       marmalade-package-store-dir
       :package-names-list '("sawfish"))
      (list (list sawfish "el"))))))

(ert-deftest marmalade/archive-cache-fill ()
  "Test we can fill the cache with just one package."
  (let* ((marmalade-archive-dir
          (concat marmalade-dir "marmalade-repo-test/archives"))
         (marmalade-package-store-dir
          (concat marmalade-dir "marmalade-repo-test/packages"))
         (marmalade/archive-cache (make-hash-table :test 'equal)))
      (marmalade/archive-cache-fill
       marmalade-package-store-dir
       :package-names-list '("sawfish"))
      (should
       (equal
        (kvalist->keys (kvhash->alist marmalade/archive-cache))
        '("sawfish")))))

(defun marmalade/make-requires (depends)
  "Make a requires string."
  (if depends
      (format ";; Package-Requires: %S" depends)
      ""))

(defun marmalade/make-header (depends version)
  "Make a package header."
  ;; Expects the lex-val and lisp-val functions to have been fletted.
  (let ((requires (marmalade/make-requires depends)))
    (s-lex-format ";; Author: Some Person <person@example.com>
;; Maintainer: Other Person <other@example.com>
;; Version: ${version}
;; URL: http://git.example.com/place
${requires}
;; Keywords: lisp, tools
")))

(defun marmalade/make-test-pkg (name depends desc version commentary)
  "Make contents of a test pakage."
  (let* ((decl (marmalade/make-header depends version))
         (copy ";; Copyright (C) 2013 Some Person")
         (defn
          '(defun dummy-package ()
            (interactive)
            (message "ha")))
         (defn-code (s-lex-format "${defn}\n\n"))
         (prvide '(provide (quote dummy-package)))
         (prvide-code (s-lex-format "${prvide}\n\n")))
    (s-lex-format ";;; ${name}.el --- ${desc}

${copy}

${decl}

;;; Commentary:

${commentary}

;;; Code:

${defn-code}

${prvide-code}
;;; ${name}.el ends here
")))

(defun marmalade/package-requirify (src-list)
  "Transform package depends.

From: ((package-name \"0.1\"))
To ((package-name (0 1))).

This is code that is in `package-buffer-info'."
  (mapcar
   (lambda (elt)
     (list (car elt)
           (version-to-list (car (cdr elt)))))
   src-list))

(defmacro* marmalade/package-file ((&key (pkg-name "dummy-package")
                                         pkg-file-name ; can override the filename
                                         (pkg-desc "a fake package for the marmalade test suite")
                                         (pkg-depends '((timeclock "2.6.1")))
                                         (pkg-version "0.0.1")
                                         (pkg-commentary ";; This doesn't do anything.\n;; it's just a fake package for Marmalade."))
                                   &rest code)
  "Make a fake package file.

Everything is faked by default but can be over-ridden by using
the parameters.

Evaluates CODE with the package file made using
`fakir-mock-file'."
  (declare (debug (sexp &rest form))
           (indent 1))
  `(let* ((package-name ,pkg-name)
          (package-desc ,pkg-desc)
          (package-depends (quote ,pkg-depends))
          (package-version ,pkg-version)
          (package-commentary ,pkg-commentary)
          (package-file-name
           (or ,pkg-file-name
               (concat package-name ".el")))
          (package-content-string
           (marmalade/make-test-pkg
            package-name 
            package-depends
            package-desc
            package-version
            package-commentary))
          (package-file
           (fakir-file
            :filename package-file-name
            :directory "/tmp/"
            :content package-content-string))
          (fakir--home-root "/home/marmalade"))
     (fakir-mock-file package-file
       (progn ,@code))))

(ert-deftest marmalade/package-info ()
  "Tests for the file handling stuff.

Also shows some stuff about the `marmalade/package-file' macro."
  (marmalade/package-file ()
    (should
     (equal
      (marmalade/package-info "/tmp/dummy-package.el")
      (vector
       package-name
       (marmalade/package-requirify package-depends)
       package-desc
       package-version
       (concat ";;; Commentary:\n\n" package-commentary "\n\n")))))
  (marmalade/package-file (:pkg-name "nic-test")
    (should (equal package-name "nic-test"))
    (should
     (equal
      ;; the package name changes the file-name
      (marmalade/package-info "/tmp/nic-test.el") 
      (vector
       package-name
       (marmalade/package-requirify package-depends)
       package-desc
       package-version
       (concat ";;; Commentary:\n\n" package-commentary "\n\n")))))
  ;; A tar package
  (should
   (equal
    (marmalade/package-info
     (expand-file-name
      (concat marmalade-dir "elnode-0.9.9.6.9.tar")))
    ["elnode"
     ((web (0 1 4))
      (creole (0 8 14))
      (fakir (0 0 14))
      (db (0 0 5))
      (kv (0 0 15)))
     "The Emacs webserver."
     "0.9.9.6.9" nil])))

(ert-deftest marmalade/package-path ()
  (marmalade/package-file (:pkg-file-name "test546.el")
    (let* ((marmalade-package-store-dir "/tmp")
           (pkg (marmalade/package-path "/tmp/test546.el"))
           (pkg-path (plist-get pkg :package-path)))
     (should
      (equal
       pkg-path
       "/tmp/dummy-package/0.0.1/dummy-package-0.0.1.el")))))

(ert-deftest marmalade/temp-file ()
  "Test that we make the temp file in the right way."
  (unwind-protect 
       (flet ((make-temp-name (prefix)
                (concat prefix "2345")))
         (should
          (equal
           (marmalade/temp-file "blah.el")
           "/tmp/marmalade-upload2345.el")))
    (delete-file "/tmp/marmalade-upload2345.el")))
(defmacro comment (&rest code))

(ert-deftest marmalade/save-package ()
  "Test the save package stuff.

Probably the most complicated bit, it tests that an uploaded file
can be created from some content and a filename and then moved to
the package store."
  ;; Have to fake the temp file making stuff
  (noflet ((make-temp-name (prefix) (concat prefix "2345")))
    (let ((marmalade-package-store-dir "/tmp/test-marmalade-dir")
          (temp-file (fakir-file
                      :directory "/tmp/"
                      :filename "marmalade-upload2345.el")))
      (marmalade/package-file ()
        ;; Check that the saved file is in the package store
        (fakir-mock-file temp-file
          (let ((save-package-res (marmalade/save-package
                                   package-content-string "dummy-package.el")))
            (should
             (equal
              save-package-res
              (list :info ["dummy-package"
                           ((timeclock (2 6 1)))
                           "a fake package for the marmalade test suite"
                           "0.0.1"
                           ";;; Commentary:\n
;; This doesn't do anything.
;; it's just a fake package for Marmalade.\n\n"]
                    :package-path "/tmp/test-marmalade-dir/dummy-package/0.0.1/dummy-package-0.0.1.el"
                    :temp-package (fakir-file-path temp-file))))
            ;; Check that the temp file has been renamed
            (apply 'marmalade/install-package save-package-res)
            (should
             (equal
              "/tmp/test-marmalade-dir/dummy-package/0.0.1/dummy-package-0.0.1.el"
              (fakir-file-path temp-file)))))))))

(defun marmalade/file->string (filename)
  "Return the contents of FILENAME as a string."
  (with-temp-buffer
    (insert-file-contents-literally filename)
    (buffer-string)))

(ert-deftest marmalade/upload ()
  (let* ((package-file (concat marmalade-dir "dummy-package.el"))
         (package-content (marmalade/file->string package-file))
         (params `(("package-file" ,package-content :elnode-filename "dummy-package.el")))
         (dummy-package ["dummy-package"
                         ((timeclock (2 6 1)))
                         "a fake package for the marmalade test suite"
                         "0.0.1"
                         ";;; Commentary:\n\n;; This doesn't do anything.\n;; it's just a fake package for Marmalade.\n\n"])
         (marmalade/users (db-make `(db-hash :filename "/tmp/test-marmalade-users")))
         (elnode-loggedin-db (make-hash-table :test 'equal))
         (package-list (list (list 'package-list "dummy-package")))
         location package-data package-pushed)
    ;; Add the fake user to our fake databases
    (db-put "testuser" package-list marmalade/users)
    (puthash "testuser" (list :hash "faketoken") elnode-loggedin-db)
    (fakir-mock-proc-properties :httpcon
      (elnode-fake-params :httpcon params
        (noflet ((marmalade/save-package (upload-file base-file)
                   (list :info dummy-package
                         :package-path "/tmp/marmalade/package"
                         :temp-package "/tmp/package.el"))
                  (marmalade/install-package (:info info
                                                    :package-path package-path
                                                    :temp-package temp-package)
                    "Set `package-data' to show it's been uploaded."
                    (setq package-data info)
                    info)
                  (elnode-auth-get-cookie-value (httpcon :cookie-name cookie-name)
                    (cons "testuser" "faketoken"))
                  (elnode-send-redirect (httpcon loc &optional status)
                    (setq location loc))
                  (elnode-proxy-post (httpcon location :data data)
                    "Set the `package-pushed' to indicate data sent to proxy."
                    (setq package-pushed data)))
          (message "testuser is in the db? %s" (db-get "testuser" marmalade/users))
          (marmalade/upload :httpcon))
        ;; Is the uploaded package ok?
        (should (equal package-data dummy-package))
        ;; Check the one we sent to the proxy as well
        (should
         (equal
          (car (read-from-string (cdar package-pushed)))
          dummy-package))))))

(ert-deftest marmalade/relativize ()
  (should
   (equal
    (marmalade/relativize "/tmp/blah/blah" "/tmp/")
    "blah/blah"))
  (should
   (equal
    (marmalade/relativize "/tmp/blah/blah/more" "/tmp/")
    "blah/blah/more"))
  (should
   (equal
    (marmalade/relativize "/tmp/blah/blah/more" "/var/")
    nil)))

(ert-deftest marmalade/commentary->about ()
  (let ((about-result "this is a test of the function.\n\nIt should result in something without colons.\n\n"))
    ;; Test with a "Code:"" ending marker
    (should
     (equal
      (marmalade/commentary->about ";;; Commentary:

;; this is a test of the function.

;; It should result in something without colons.

;;; Code:")
      about-result))
    ;; Test without a Code: ending marker
    (should
     (equal
      (marmalade/commentary->about ";;; Commentary:

;; this is a test of the function.

;; It should result in something without colons.

(require 'something)")
      about-result))))

(ert-deftest marmalade/package-list ()
  (let ((files-list
         `(("/root/package-a" . 7)
           ("/root/package-b" . 2)
           ("/root/package-c" . 5))))
    ;; Lot's of specific fakery
    (noflet ((directory-files (dir full match)
               (kvalist->keys files-list))
             (file-attributes (a)
               (list
                0 1 2 3 4 ; make sure we have 5 elements
                (cdr (assoc a files-list))))
             (time-less-p (a b)
               (> a b)))
      (should
       (equal
        (marmalade/package-list :sorted 5 :take 3)
        '("package-b" "package-c" "package-a"))))))

(ert-deftest marmalade-archive-update ()
  "Test the archive update service handler."
  (let* ((marmalade-archive-dir
          (concat marmalade-dir "marmalade-repo-test/archives"))
         (marmalade-package-store-dir
          (concat marmalade-dir "marmalade-repo-test/packages"))
         (marmalade/archive-cache (make-hash-table :test 'equal))
         (info (format "%S" '("sawfish" nil "doc" "1.32"))))
    ;; Make it
    (marmalade-archive-make-cache)
    (noflet ((elnode-send-status (httpcon status &optional msg)
               (throw :done status))
              (marmalade/archive-cache-fill (root
                                             :package-names-list package-names-list)
                (if package-names-list
                    ;; if we have a name make sure we remove it - why
                    ;; do we need this? because archive-update handler
                    ;; does not actually delete the file, this is done
                    ;; by the main engine
                    (progn
                      (funcall this-fn root)
                      (remhash (car package-names-list)
                              marmalade/archive-cache))
                   ;; else just do it
                   (funcall this-fn root))))
      (elnode-fake-params :httpcon `(("package-info" . ,info))
        (equal
         (catch :done
           (noflet ((elnode-http-method (httpcon) "DELETE"))
             (marmalade-archive-update :httpcon)))
         202))
      ;; Check it's been removed
      (should-not (gethash "sawfish" marmalade/archive-cache)))))

(ert-deftest marmalade-user-packages ()
  "Test that we can add packages to a user."
  (let ((marmalade-users
         (db-make '(db-hash :filename "/tmp/marmalade-users-test"))))
    (marmalade-add-user "nic" "secret")
    (marmalade-add-packages "nic" "elnode")
    (should
     (equal (marmalade-get-packages "nic") '("elnode")))
    (marmalade-add-user "testuser" "secret")
    (marmalade-add-packages "testuser" "elnode" "marmalade")
    (should
     (equal (marmalade-get-packages "testuser") '("elnode" "marmalade")))
    (db-get "testuser" marmalade/users)
    (marmalade-rm-packages "testuser" "elnode")
    (should
     (equal (marmalade-get-packages "testuser")
            '("marmalade")))))

(provide 'marmalade-tests)

;;; marmalade-tests.el ends here
