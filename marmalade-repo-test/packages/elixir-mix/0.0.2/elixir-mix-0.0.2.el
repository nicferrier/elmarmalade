;;; elixir-mix.el --- Emacs integration for Elixir's elixir-mix
;;
;; Filename: elixir-mix.el
;; Description: Integration of Elixir's building and deployment tool: mix into Emacs.
;; Author: Samuel Tonini
;; Maintainer: Samuel Tonini
;; Created: So Jun  9 10:01:02 2013 (+0200)
;; Version: 0.0.2
;; URL: http://github.com/tonini/elixir-mix.el
;; Keywords: elixir, mix, elixir-mix

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;;   Basic steps to setup:
;;
;;    (add-to-list 'load-path "~/path/to/elixir-mix.el/")
;;    (require 'elixir-mix)
;;    (global-elixir-mix-mode)
;;
;;   Interesting variables are:
;;
;;    `elixir-mix-command`
;;
;;    Path to the executable <mix> command
;;
;;
;;    `elixir-mix-buffer-name`
;;
;;    Name for the buffer used for mix shell output.
;;
;;    Major commands are:
;;
;;    M-x elixir-mix-new
;;
;;        Create a new Elixir application.
;;
;;    M-x elixir-mix-test
;;
;;        Run the whole Elixir application test suite.
;;
;;    M-x elixir-mix-test-this-buffer
;;
;;        Run the current buffer through <mix test> command.
;;
;;    M-x elixir-mix-compile
;;
;;        Compile the whole Elixir application.
;;
;;    M-x elixir-mix-run
;;
;;        Runs the given expression in the Elixir application context.
;;
;;    M-x elixir-mix-deps-with-prompt
;;
;;        Prompt for mix deps commands.
;;
;;    M-x elixir-mix-local-with-prompt
;;
;;        Prompt for mix local commands.
;;
;;    M-x elixir-mix-deps-install
;;
;;        Prompt for mix local.install <path> or <url>.
;;
;;    M-x elixir-mix-deps-install-with-path
;;
;;        Runs deps.install and prompt for a <path> as argument.
;;
;;    M-x elixir-mix-deps-install-with-url
;;
;;        Runs deps.install and prompt for a <url> as argument.
;;
;;    M-x elixir-mix-help
;;
;;        Show help output for a specific mix command.
;;
;;    M-x elixir-mix-execute
;;
;;        Run any command in the context of the application,
;;        except `help` and `new`.
;;        Just run any command as you like, including arguments
;;        for the specific command. (example: test --quick)
;;

;;; Code:

(defcustom elixir-mix-command "mix"
  "The shell command for mix"
  :type 'string
  :group 'elixir-mix)

(defvar elixir-mix-buffer-name "*MIX*"
  "Name of the mix output buffer.")

(defvar elixir-mix--elixir-project-root-indicators
  '("mix.exs" "mix.lock" ".git")
  "List of files and directories which indicate a elixir project root.")

(defvar elixir-mix--deps-commands
  '("deps" "deps.clean" "deps.compile" "deps.get" "deps.unlock" "deps.unlock")
  "List of all deps.* available commands.")

(defvar elixir-mix--local-commands
  '("local" "local.install" "local.rebar" "local.uninstall")
  "List of all local.* available commands.")

(defvar elixir-mix--local-install-option-types
  '("path" "url")
  "List of local.install option types.")

(defun elixir-mix--elixir-project-root-directory-p (a-directory)
  "Returns t if a-directory is the elixir project root"
  (equal a-directory (file-name-directory (directory-file-name a-directory))))

(defun elixir-mix--elixir-project-root (&optional directory)
  "Finds the root directory of the project by walking the
   directory tree until it finds a elixir project root indicator."
  (let* ((directory (file-name-as-directory (or directory (expand-file-name default-directory))))
         (present-files (directory-files directory)))
    (cond ((elixir-mix--elixir-project-root-directory-p directory) nil)
          ((> (length (intersection present-files elixir-mix--elixir-project-root-indicators :test 'string=)) 0) directory)
          (t (elixir-mix--elixir-project-root (file-name-directory (directory-file-name directory)))))))

(defun elixir-mix--get-buffer (name)
  "Get and kills a buffer if exists and returns a new one."
  (let ((buffer (get-buffer name)))
    (when buffer (kill-buffer buffer))
    (generate-new-buffer name)))

(defun elixir-mix--buffer-setup (buffer)
  "Setup the mix buffer before display."
  (display-buffer buffer)
  (with-current-buffer buffer
    (setq buffer-read-only nil)
    (local-set-key "q" 'quit-window)))

(defun elixir-mix--run-command-async (command)
  (let ((buffer (elixir-mix--get-buffer elixir-mix-buffer-name)))
    (async-shell-command (format "%s %s" elixir-mix-command command) buffer)
    (elixir-mix--buffer-setup buffer)))

(defun elixir-mix--completing-read (prompt command-list)
  (completing-read prompt command-list nil t nil nil (car command-list)))

(defun elixir-mix-new (name)
  "Create a new elixir project with mix."
  (interactive "Gmix new: ")
  (elixir-mix--run-command-async (format "new %s" name)))

(defun elixir-mix-test ()
  "Run the whole elixir test suite."
  (interactive)
  (elixir-mix-execute "test"))

(defun elixir-mix-test-this-buffer ()
  "Run the current buffer through mix test."
  (interactive)
  (elixir-mix-execute (format "test %s" buffer-file-name)))

(defun elixir-mix-compile ()
  "Compile the whole elixir project."
  (interactive)
  (elixir-mix-execute "compile"))

(defun elixir-mix-run (code)
  "Runs the given expression in the elixir application context."
  (interactive "Mmix run: ")
  (elixir-mix-execute (format "run '%s'" code)))

(defun elixir-mix-deps-with-prompt (command)
  "Prompt for mix deps commands."
  (interactive
   (list (elixir-mix--completing-read "mix deps: " elixir-mix--deps-commands)))
  (elixir-mix-execute command))

(defun elixir-mix-local-with-prompt (command)
  "Prompt for mix local commands."
  (interactive
   (list (elixir-mix--completing-read "mix local: " elixir-mix--local-commands)))
  (if (string= command "local.install")
      (call-interactively 'elixir-mix-deps-install)
    (elixir-mix-execute command)))

(defun elixir-mix-deps-install (path-or-url)
  "Prompt for mix local.install <path> or <url>."
  (interactive
   (list (completing-read "mix local.install FORMAT: "
                          elixir-mix--local-install-option-types
                          nil t nil nil (car elixir-mix--local-install-option-types))))
  (if (string= path-or-url (car elixir-mix--local-install-option-types))
      (call-interactively 'elixir-mix-deps-install-with-path)
    (call-interactively 'elixir-mix-deps-install-with-url)))

(defun elixir-mix-deps-install-with-path (path)
  "Runs deps.install and prompt for a <path> as argument."
  (interactive "fmix local.install PATH: ")
  (elixir-mix-execute (format "local.install %s" path)))

(defun elixir-mix-deps-install-with-url (url)
  "Runs deps.install and prompt for a <url> as argument."
  (interactive "Mmix local.install URL: ")
  (elixir-mix-execute (format "local.install %s" url)))

(defun elixir-mix-help (command)
  "Show help output for a specific mix command."
  (interactive "Mmix help: ")
  (elixir-mix--run-command-async (format "help %s" command)))

(defun elixir-mix-execute (command)
  "Run a mix command."
  (interactive "Mmix: ")
  (cond ((string= command "") (error "There is no such command."))
        ((string-match "^new" command)
         (error "Please use the `elixir-mix-new (name)` function to create a new elixir project."))
        ((string-match "^help" command)
         (error "Please use the `elixir-mix-help (command)` function to get a mix command specific help.")))
  (let ((project-root (elixir-mix--elixir-project-root)))
    (when (not project-root) (error "Couldn't find any elixir project root."))
    (setq default-directory (elixir-mix--elixir-project-root))
    (elixir-mix--run-command-async command)))

;;;###autoload
(define-minor-mode global-elixir-mix-mode
  "Toggle global-elixir-mix-mode to use elixir's mix build tool within emacs."
  :global t)

(provide 'elixir-mix)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; elixir-mix.el ends here
