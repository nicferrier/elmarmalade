;;; tests for marmalade

(require 'ert)
(require 'marmalade-s)

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


;;; marmalade-tests.el ends here
