(require 'elmarmalade)

(ert-deftest elmarmalade--package-record ()
  "Test the package record construction."
  (let* ((json-key-type 'string)
         (data (json-read-file
                (expand-file-name
                 "~/work/marmalade/elmarmalade/testrecord.json"))))
    (should
     (equal
      (elmarmalade--package-record data)
      (cons
       'org-email
       [(0 6)
        nil
        "use org for an email database -*- lexical-binding: t -*-"
        single])))))

(ert-deftest elmarmalade--package-filename ()
  "Test the transformation of a filename."
  (should
   (equal
    "haml-mode.el/0.3.1"
    (elmarmalade--package-filename "haml-mode-0.3.1.el"))))

;; End
