(setq deploy-dir (expand-file-name (car command-line-args-left) default-directory))
(make-directory (concat deploy-dir "/.emacs.d/elpa") t)
(setq package-user-dir (concat deploy-dir "/.emacs.d/elpa"))
(setq custom-file (setq user-init-file (concat deploy-dir "/.emacs.d/init.el")))
(customize-set-variable
 'package-archives
 '(("gnu" . "http://elpa.gnu.org/packages/")
   ("marmalade" . "http://marmalade-repo.org/packages/")))
(customize-set-variable 'elnode-init nil)
(customize-set-variable 'elnode-log-files-directory (expand-file-name "logs" deploy-dir))
(customize-set-variable 'lisp-indent-function 'common-lisp-indent-function)
(customize-set-variable 'revert-without-query '(".*"))
(customize-set-variable 'debug-on-error 'always) ; FIXME probably not?
(customize-set-variable 'marmalade-server-port 8005)
(customize-set-variable 'marmalade-db-dir (expand-file-name "~/marmalade/db"))
(customize-set-variable 'marmalade-package-store-dir (expand-file-name "~/marmalade/packages"))
(customize-set-variable 'marmalade-archive-dir (expand-file-name "~/marmalade/archives"))
(customize-set-variable 'marmalade-archive-port 8006)
(customize-set-variable 'marmalade-boot-onload t)
(customize-save-customized)
(toggle-debug-on-error)
(package-initialize)
(package-refresh-contents)
(package-install-file
 (expand-file-name
  "marmalade-service-2.0.10.tar"
  default-directory))
(with-temp-buffer
  (print
   '(add-hook
     'after-init-hook
     (lambda nil
       (marmalade-init)))
   (current-buffer))
  (append-to-file (point-min)(point-max) custom-file))
