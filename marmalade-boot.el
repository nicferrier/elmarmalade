;;; marmalade-boot.el ---  initialize marmalade

(require 'marmalade-service)

(defgroup marmalade nil
  "Marmalade, the ELPA repository."
  :group 'applications)

(defcustom marmalade-server-port nil
  "The TCP port used for the marmalade service."
  :group 'marmalade
  :type '(choice
          (const :tag "Default" nil)
          (integer :tag "Port" 8005)))

(defcustom marmalade-archive-port nil
  "The TCP port used for the marmalade service."
  :group 'marmalade
  ;; We could make these strings as well... for unix pipes that would
  ;; reduce the need to allocate ports which would be excellent
  :type '(choice
          (const :tag "Default" nil)
          (integer :tag "Port" 8006)))

(defcustom marmalade-boot-onload nil
  "Should marmalade start on load?"
  :group 'marmalade
  :type 'boolean)

;;;###autoload
(defun marmalade-init ()
  "Start the marmalade service."
  ;; this starts one... what about the archive service?
  (elnode-start
   'marmalade-router
   :port marmalade-server-port))

(defvar marmalade/inited nil
  "Autoload mechanics, when it's `t' we are started.")

;;;###autoload
(eval-after-load 'marmalade-boot
  (if (and (boundp 'marmalade-boot-onload)
           marmalade-boot-onload
	   (or (not (boundp 'marmalade/inited))
	       (not marmalade/inited)))
      (progn
        (marmalade-init)
        (setq marmalade/inited nil))))

(provide 'marmalade-boot)

;;; marmalade-boot.el ends here
