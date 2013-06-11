
;;; less-css-mode.el --- Major mode for editing LESS CSS files (lesscss.org)
;;
;; Copyright 2011 Steve Purcell
;;
;; Author: Steve Purcell <steve@sanityinc.com>
;; URL: https://github.com/purcell/less-css-mode
;; Keywords: less css mode
;; Version: 0.6
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;;; Commentary:
;;
;; This mode provides syntax highlighting for LESS CSS files, plus
;; optional support for `flymake-mode' and compilation of .less files
;; to .css files at the time they are saved: use
;; `less-css-compile-at-save' to enable the latter.
;;
;; Command line utility "lessc" is required if enabling flymake or
;; setting `less-css-compile-at-save' to t.  To install "lessc" using
;; the Node.js package manager, run "npm install less"
;;
;; Also make sure the "lessc" executable is in emacs' PATH, example:
;; (setq exec-path (cons (expand-file-name "~/.gem/ruby/1.8/bin") exec-path))
;; or customize `less-css-lessc-command' to point to your "lessc" executable.
;;
;; `less-css-mode' is derived from `css-mode', and indentation of
;; nested blocks may not work correctly with versions of `css-mode'
;; other than that bundled with recent Emacs.
;;
;; You can specify per-file values for `less-css-compile-at-save',
;; `less-css-output-file-name' or `less-css-output-directory' using a
;; variables header at the top of your .less file, e.g.:
;;
;; // -*- less-css-compile-at-save: t; less-css-output-directory: "../css" -*-
;;
;; If you don't need CSS output but would like to be warned of any
;; syntax errors in your .less source, enable `flymake-mode': support
;; is provided for .less files, but note that the less compiler is a
;; little slow, so there can be a delay of several seconds between
;; editing and receiving feedback on any error.
;;
;;; Credits
;;
;; The original code for this mode was, in large part, written using
;; Anton Johansson's scss-mode as a template -- thanks Anton!
;; https://github.com/antonj
;;
;;; Code:

(require 'derived)
(require 'compile)
(require 'flymake)

(defgroup less-css nil
  "Less-css mode"
  :prefix "less-css-"
  :group 'css)

(defcustom less-css-lessc-command "lessc"
  "Command used to compile LESS files, should be lessc or the
  complete path to your lessc executable, e.g.:
  \"~/.gem/ruby/1.8/bin/lessc\""
  :group 'less-css)

(defcustom less-css-compile-at-save nil
  "If non-nil, the LESS buffers will be compiled to CSS after each save"
  :type 'boolean
  :group 'less-css)

(defcustom less-css-lessc-options '()
  "Command line options for less executable.

Use \"-x\" to minify output."
  :type '(repeat string)
  :group 'less-css)

(defvar less-css-output-directory nil
  "Directory in which to save CSS, or nil to use the LESS file's directory.

This path is expanded relative to the directory of the LESS file
using `expand-file-name', so both relative and absolute paths
will work as expected.")

(make-variable-buffer-local 'less-css-output-directory)

(defvar less-css-output-file-name nil
  "File name in which to save CSS, or nil to use <name>.css for <name>.less.

This can be also be set to a full path, or a relative path.  If
the path is relative, it will be relative to the value of
`less-css-output-dir', if set, or the current directory by
default.")

(make-variable-buffer-local 'less-css-output-file-name)

(defconst less-css-default-error-regex "Syntax Error on line \\([0-9]+\\)\e\\[39m\e\\[31m in \e\\[39m\\([^ ]+\\)$")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compilation to CSS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'compilation-error-regexp-alist-alist
             (list 'less-css (concat "\\(" less-css-default-error-regex "\\)") 3 2 nil nil 1))
(add-to-list 'compilation-error-regexp-alist 'less-css)


(defun less-css-compile-maybe ()
  "Runs `less-css-compile' on if `less-css-compile-at-save' is t"
  (if less-css-compile-at-save
      (less-css-compile)))

(defun less-css--output-path ()
  "Calculate the path for the compiled CSS file created by `less-css-compile'."
  (expand-file-name (or less-css-output-file-name
                        (concat (file-name-nondirectory (file-name-sans-extension buffer-file-name)) ".css"))
                    (or less-css-output-directory default-directory)))

;;;###autoload
(defun less-css-compile ()
  "Compiles the current buffer to css using `less-css-lessc-command'."
  (interactive)
  (message "Compiling less to css")
  (compile
   (mapconcat 'shell-quote-argument
              (append (list less-css-lessc-command)
                      less-css-lessc-options
                      (list buffer-file-name (less-css--output-path)))
              " ")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Minor mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: interpolation ("@{val}"), escaped values (~"..."), JS eval (~`...`), custom faces
(defconst less-css-font-lock-keywords
  '(;; Variables
    ("@[a-z_-][a-z-_0-9]*" . font-lock-constant-face)
    ("&" . font-lock-preprocessor-face)
    ;; Mixins
    ("\\(?:[ \t{;]\\|^\\)\\(\\.[a-z_-][a-z-_0-9]*\\)[ \t]*;" . (1 font-lock-keyword-face)))
  )

;;;###autoload
(define-derived-mode less-css-mode css-mode "LESS"
  "Major mode for editing LESS files, http://lesscss.org/
Special commands:
\\{less-css-mode-map}"
  (font-lock-add-keywords nil less-css-font-lock-keywords)
  ;; cpp-style comments
  (modify-syntax-entry ?/ "< 124b" less-css-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" less-css-mode-syntax-table)
  (set (make-local-variable 'comment-start) "//")
  (set (make-local-variable 'comment-end) "")

  (add-hook 'after-save-hook 'less-css-compile-maybe nil t))

(define-key less-css-mode-map "\C-c\C-c" 'less-css-compile)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.less" . less-css-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wiring for `flymake-mode'
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun flymake-less-css-init ()
  "Flymake support for LESS files"
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list less-css-lessc-command (append less-css-lessc-options (list local-file)))))

(push '(".+\\.less$" flymake-less-css-init) flymake-allowed-file-name-masks)

(push (list less-css-default-error-regex 2 1 nil 2) flymake-err-line-patterns)


(provide 'less-css-mode)
;;; less-css-mode.el ends here
