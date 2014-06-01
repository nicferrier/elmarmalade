;;; marmalade-users.el --- user handling in marmalade

;; Copyright (C) 2014  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Keywords: processes

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'elnode)
(require 'kv)
(require 'base64)
(require 'file-format)

(defvar marmalade/users
  (db-make `(db-hash
             :filename
             ,(expand-file-name
               "marmalade-user-db"
               (or marmalade-db-dir marmalade-dir))))
  "The user database.")

(defun marmalade/user-hash (password salt)
  "Make the hash for the user.

This is stored in the user record as \"digest\"."
  (base64-encode-string (sha1 (concat password salt) nil nil t)))


(defconst marmalade/add-user-version 2
  "Version variable controlling marmalade user db format.")

(defun marmalade-add-user (username password email &rest packages)
  "Add USERNAME to the database.

Default their PACKAGES to the list."
  (case marmalade/add-user-version
    (1
     (db-put username 
             `(("username" . ,username)
               ("token" . ,(elnode-auth-make-hash username password))
               ("package-list" . ,packages))
             marmalade/users))
    (2-test ; same as 2 but no package list
     (db-put username
             ;; How to work out the salt?
             (let ((salt (shell-command-to-string "openssl rand -base64 32"))
                   (token (replace-regexp-in-string
                           "\n$" ""
                           (shell-command-to-string "openssl rand -base64 32"))))
               `(("digest" . ,(marmalade/user-hash password salt))
                 ("email" . ,email)
                 ("name"  . ,username)
                 ("salt"  . ,salt)
                 ("token" . ,token)))
             marmalade/users))
    (2
     (db-put username
             ;; How to work out the salt?
             (let ((salt (shell-command-to-string "openssl rand -base64 32"))
                   (token (replace-regexp-in-string
                           "\n$" ""
                           (shell-command-to-string "openssl rand -base64 32"))))
               `(("digest" . ,(marmalade/user-hash password salt))
                 ("email" . ,email)
                 ("name"  . ,username)
                 ("salt"  . ,salt)
                 ("package-list" . ,packages)
                 ("token" . ,token)))
             marmalade/users))))

(defun marmalade-add-packages (username &rest packages)
  "Add PACKAGES to USERNAME in the user database."
  (let* ((record (db-get username marmalade/users))
         (rec-packages (assoc "package-list" record)))
    (cond
      ((not rec-packages)
       (setcdr (last record) (list (cons "package-list" packages))))
      ((not (cdr rec-packages))
       (setcdr rec-packages packages))
      (t
       (setcdr rec-packages (-uniq (append (cdr rec-packages) packages)))))
    (db-hash/save marmalade/users)
    record))

(defun marmalade-rm-packages (username &rest packages)
  "Remove PACKAGES from USERNAME in the user database."
  (let* ((record (db-get username marmalade/users))
         (rec-packages (assoc "package-list" record)))
    ;; MUTATIVE - meh
    (when (cdr rec-packages)
      (setcdr rec-packages
              (--filter (not (member it packages))
                        (cdr rec-packages))))
    (db-hash/save marmalade/users)
    record))

(defun marmalade-get-packages (username)
  "Return the list of packages editable by USERNAME."
  (kva "package-list" (db-get username marmalade/users)))

(defun marmalade-user-profile (httpcon)
  "Elnode handler for users.

`elnode-http-mapping' 1 should be the username."
  (let ((username (elnode-http-mapping httpcon 1)))
    (elnode-http-start httpcon 200 '(Content-type . "text/html"))
    (elnode-http-return
     httpcon 
     (file-format-html
      "profile-page.html" marmalade-dir
      'aget `(("username" . ,username)
              ("package-list" . ,(s-join " " (marmalade-get-packages username))))))))

(provide 'marmalade-users)

;;; marmalade-users.el ends here
