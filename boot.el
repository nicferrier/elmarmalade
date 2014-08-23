;;; boot.el ---  boot up marmalade

(defvar marmalade/inited nil
  "Autoload mechanics, when it's `t' we are started.")

(defun marmalade-init ()
  "Start the marmalade service."
  ;; this starts one... what about the archive service?
  (require 'marmalade-service)
  (elnode-start
   'marmalade-router
   :port marmalade-server-port
   :host "0.0.0.0")
  (setq marmalade/inited t))

(marmalade-init)

;;; boot.el ends here
