;;; file-format-tests.el --- tests for file-templates


(require 'fakir)
(require 'ert)
(require 'file-format)

(ert-deftest file-format-test ()
  "Fake a file and test that we can format it."
  (fakir-fake-file
      (fakir-file :filename "test-page" 
                  :directory "/root/pages"
                  :content "test that ${name}\nhas ${age} years\n")
    (equal
     (file-format
      "test-page" "/root/pages" 'aget
      '(("name" . "mr test")
        ("age" . "10")))
     "test that mr test\nhas 10 years\n")))

(ert-deftest file-format-html-test ()
  "Fake a file and test that we can HTML format it."
  (fakir-fake-file
      (fakir-file :filename "test-page" 
                  :directory "/root/pages"
                  :content "test that ${name}\nhas ${age} years\n")
    (equal
     (file-format-html
      "test-page" "/root/pages" 'aget
      '(("name" . "mr test")
        ("age" . "<10>")))
     "test that mr test\nhas &lt;10&gt; years\n")))

(provide 'file-format-tests)

;;; file-format-tests.el ends here
