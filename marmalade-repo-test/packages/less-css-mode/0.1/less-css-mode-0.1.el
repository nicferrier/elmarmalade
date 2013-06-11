;;; less-css-mode.el --- Major mode for editing LESS CSS files (lesscss.org)
;;
;; Copyright 2011 Steve Purcell
;;
;; Author: Steve Purcell <steve@sanityinc.com>
;; URL: https://github.com/purcell/less-css-mode
;; Keywords: less css mode
;; Version: 0.1
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
;; optional support for flymake and compilation of .less files to .css
;; files at the time they are saved: use `less-css-compile-at-save' to
;; enable the latter.
;;
;; Command line utility "lessc" is required if enabling flymake or
;; setting `less-css-compile-at-save' to t.  To install "lessc" using
;; the Node.js package manager, run "npm install less"
;;
;; Also make sure the "lessc" executable is in emacs' PATH, example:
;; (setq exec-path (cons (expand-file-name "~/.gem/ruby/1.8/bin") exec-path))
;; or customize `less-css-lessc-command' to point to your "lessc" executable.
;;
;;; Credits
;;
;; This mode was, in large part, built using Anton Johansson's
;; scss-mode as a template -- thanks Anton! https://github.com/antonj
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
  complete path to your lessc runnable example:
  \"~/.gem/ruby/1.8/bin/lessc\""
  :group 'less-css)

(defcustom less-css-compile-at-save nil
  "If not nil the LESS buffers will be compiled after each save"
  :type 'boolean
  :group 'less-css)

(defcustom less-css-lessc-options '()
  "Command line Options for less executable."
  :group 'less-css)

(defconst less-css-default-error-regex "Syntax Error on line \\([0-9]+\\)\e\\[39m\e\\[31m in \e\\[39m\\([^ ]+\\)$")

(defcustom less-css-compile-error-regex (list (concat "\\(" less-css-default-error-regex "\\)") 3 2 nil nil 1)
  "Regex for finding line number file and error message in
compilation buffers, syntax from
`compilation-error-regexp-alist' (REGEXP FILE LINE COLUMN TYPE
HYPERLINK HIGHLIGHT)"
  :group 'less-css)


;; TODO: '&', interpolation, escaped values (~"..."), JS eval (~`...`), custom faces
(defconst less-css-font-lock-keywords
  '(;; Variables
    ("@[a-z_-][a-z-_0-9]*" . font-lock-constant-face)
    ;; Extended single-line comment syntax
    ("//.*$" . font-lock-comment-face)
    ;; Mixins
    ("\\(?:[ \t{;]\\|^\\)\\(\\.[a-z_-][a-z-_0-9]*\\)[ \t]*;" . (1 font-lock-keyword-face)))
  )

(defun less-css-compile-maybe()
  "Runs `less-css-compile' on if `less-css-compile-at-save' is t"
  (if less-css-compile-at-save
      (less-css-compile)))

(defun less-css-compile()
  "Compiles the current buffer, lessc filename.less"
  (interactive)
  (compile (concat less-css-lessc-command " " (mapconcat 'identity less-css-lessc-options " ") " "
                   "'" buffer-file-name "' '" (file-name-sans-extension buffer-file-name) ".css'")))

;;;###autoload
(define-derived-mode less-css-mode css-mode "LESS"
  "Major mode for editing LESS files, http://lesscss.org/
Special commands:
\\{less-css-mode-map}"
  (font-lock-add-keywords nil less-css-font-lock-keywords)
  (add-to-list 'compilation-error-regexp-alist less-css-compile-error-regex)
  (add-hook 'after-save-hook 'less-css-compile-maybe nil t))

(define-key less-css-mode-map "\C-c\C-c" 'less-css-compile)

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

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.less" . less-css-mode))

(provide 'less-css-mode)
;;; less-css-mode.el ends here
