(setq deploy-dir (expand-file-name (car command-line-args-left) default-directory))
(make-directory (concat deploy-dir "/.emacs.d/elpa") t)
(setq package-user-dir (concat deploy-dir "/.emacs.d/elpa"))
(setq custom-file (setq user-init-file (concat deploy-dir "/.emacs.d/init.el")))
(customize-set-variable
 'package-archives
 '(("gnu" . "http://elpa.gnu.org/packages/")
   ("marmalade" . "http://marmalade-repo.org/packages/")))
(customize-set-variable 'marmalade-server-port 8005)
(customize-set-variable 'marmalade-archive-port 8006)
(customize-set-variable 'marmalade-boot-onload t)
(customize-set-variable 'debug-on-error 'always)
(customize-save-customized)
(toggle-debug-on-error)
(package-initialize)
(package-refresh-contents)
(package-install-file
 (expand-file-name
  "marmalade-service-2.0.9.tar"
  default-directory))
(with-temp-buffer
  (print
   '(add-hook
     'after-init-hook
     (lambda nil
       (marmalade-init)))
   (current-buffer))
  (append-to-file (point-min)(point-max) custom-file))
(kill-emacs)
