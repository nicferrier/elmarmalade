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

;; End
