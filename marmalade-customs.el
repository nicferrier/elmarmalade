;;; marmalade-customs.el -- defining the config vars

(defgroup marmalade-archive nil
  "The marmalade package store. Elisp version."
  :group 'applications)

(defcustom marmalade-package-store-dir nil
  "The location of the package files."
  :group 'marmalade-archive
  :type '(choice
          (const :tag "Default" nil)
          (directory "~/marmalade/packages")))

(defcustom marmalade-package-archive-dir nil
  "The location of old (removed) package files."
  :group 'marmalade-archive
  :type '(choice
          (const :tag "Default" nil)
          (directory "~/marmalade/old-packages")))

(defcustom marmalade-archive-dir nil
  "The location of the archive-contents files.

Each file is a version of the archive-contents."
  :group 'marmalade-archive
  :type '(choice
          (const :tag "Default" nil)
          (directory "~/marmalade/archive-contents")))

(defcustom marmalade-db-dir nil
  "The location of the databases."
  :group 'marmalade-archive
  :type '(choice
          (const :tag "Default" nil)
          (directory "~/marmalade/databases")))

(provide 'marmalade-customs)

;;; marmalade-customs.el ends here
