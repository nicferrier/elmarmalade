;;; tests for marmalade

(require 'ert)
(require 'marmalade-s)
(require 'fakir)
(require 's)

(ert-deftest marmalade-package-explode ()
  "Make sure the regex works."
  (let ((file "ascii-3.1.el"))
    (destructuring-bind (name version type)
        (marmalade/explode-package-string file)
      (should (equal name "ascii"))
      (should (equal version "3.1"))
      (should (equal type "el")))))

(ert-deftest marmalade-package-name->filename ()
  "Convert package name to a filename."
  (let ((file "ascii-3.1.el")
        (marmalade-package-store-dir "/packages"))
    (should
     (equal
      (marmalade/package-name->filename file)
      "ascii/3.1/ascii-3.1.el"))))

(ert-deftest marmalade-cache-test ()
  "Test the cache test."
  ;; When the index is not specified
  (let ((marmalade-archive-index-filename nil))
    (flet ((marmalade/package-store-modtime () (current-time)))
      (should (marmalade-cache-test))))
  ;; When they are the same
  (let ((test-time (current-time)))
    (flet ((marmalade/archive-index-exists-p () t)
           (marmalade/archive-index-modtime () test-time)
           (marmalade/package-store-modtime () test-time))
      (should-not (marmalade-cache-test))))
  ;; Store time is earlier
  (let ((test-time (current-time)))
    (flet ((marmalade/archive-index-exists-p () t)
           (marmalade/archive-index-modtime () test-time)
           (marmalade/package-store-modtime ()
             (time-subtract
              test-time
              (seconds-to-time 60))))
      (should-not (marmalade-cache-test))))
  ;; Store time is more recent than archive
  (let ((test-time (current-time)))
    (flet ((marmalade/archive-index-exists-p () t)
           (marmalade/package-store-modtime () test-time)
           (marmalade/archive-index-modtime ()
             (time-subtract
              test-time
              (seconds-to-time 60))))
      (should (marmalade-cache-test)))))

(defmacro with-sformat-lex (&rest body)
  (declare (debug (&rest form))
           (indent 0))
  `(flet ((lex-val (var) (symbol-value (intern var)))
          (lisp-val (var) (format "%S" (lex-val var))))
     ,@body))

(defun marmalade/make-header (depends version)
  "Make a package header."
  ;; Expects the lex-val and lisp-val functions to have been fletted.
  (with-sformat-lex
    (let* ((requires
            (if (not depends)
                ""
                (s-format ";; Package-Requires: ${depends}" 'lisp-val)))
           (decl
            (s-format ";; Author: Some Person <person@example.com>
;; Maintainer: Other Person <other@example.com>
;; URL: http://git.example.com/place
;; Version: ${version}
${requires}
;; Keywords: lisp, tools
" 'lex-val)))
      decl)))

(defun marmalade/make-test-pkg (name depends desc version commentary)
  "Make contents of a test pakage."
  (with-sformat-lex
    (let* ((decl (marmalade/make-header depends version))
           (copy ";; Copyright (C) 2013 Some Person")
           (defn
            '(defun dummy-package ()
              (interactive)
              (message "ha")))
           (defn-code
            (s-format "${defn}\n\n" 'lisp-val))
           (prvide '(provide (quote dummy-package)))
           (prvide-code
            (s-format "${prvide}\n\n" 'lisp-val)))
      (s-format ";;; ${name}.el --- ${desc}

${copy}

${decl}

;;; Commentary:

${commentary}

;;; Code:

${defn-code}

${prvide-code}
;;; ${name}.el ends here\n" 'lex-val))))


(ert-deftest marmalade/package-handle ()
  "Tests for the file handling stuff."
  (let* ((pkg-commentary ";; This doesn't do anything.
;; it's just a fake package for Marmalade.")
         (pkg-name "dummy-package")
         (pkg-desc "a fake package for the marmalade test suite")
         (pkg-depends '((timeclock "2.6.1")))
         (pkg-version "0.0.1")
         (content (marmalade/make-test-pkg
                   pkg-name pkg-depends pkg-desc
                   pkg-version pkg-commentary))
         (file (make-fakir-file
                :filename (concat pkg-name ".el")
                :directory "/tmp/"
                :content content)))
    (fakir-mock-file file
        (should
         (equal
          (marmalade/package-handle "/tmp/dummy-package.el")
          (vector pkg-name
                  (mapcar
                   (lambda (elt)
                     (list (car elt)
                           (version-to-list (car (cdr elt)))))
                   pkg-depends)
                  pkg-desc pkg-version
                  (concat ";;; Commentary:\n\n" pkg-commentary "\n\n"))))))
  ;; A tar package
  (should
   (equal
    (marmalade/package-handle
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

(provide 'marmalade-tests)
;;; marmalade-tests.el ends here
