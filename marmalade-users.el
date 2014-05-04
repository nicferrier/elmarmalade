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

(defvar marmalade/users 
  (db-make `(db-hash
             :filename
             ,(concat (file-name-as-directory
                       (or marmalade-db-dir marmalade-dir))
                      "/marmalade-user-db")))
  "The user database.")

(defun marmalade/user-hash (password salt)
  "Make the hash for the user.

This is stored in the user record as \"digest\"."
  (base64-encode-string (sha1 (concat password salt) nil nil t)))

(defun marmalade-add-user (username password email &rest packages)
  "Add USERNAME to the database.

Default their PACKAGES to the list."
  (let ((version 2))
    (case version
      (1
       (db-put username 
               `(("username" . ,username)
                 ("token" . ,(elnode-auth-make-hash username password))
                 ("package-list" . ,packages))
               marmalade/users))
      (2
       (db-put username
               ;; How to work out the salt?
               (let ((salt "n7/IDsBf7FkJDKvqiIG8k+WyM+SLGyf7LbJSHVGEUak=\n"))
                 `(("digest" . ,(marmalade/user-hash password salt))
                   ("email" . ,email)
                   ("name"  . ,username)
                   ("salt"  . ,salt)
                   ("package-list" . ,packages)
                   ;; What's the token?
                   ("token" . "KQCJnaLT6YySdmM6kddB8HRJMB0aR6ZHNFogH7vuZL0=")))
               marmalade/users)))))

(defun marmalade-add-packages (username &rest packages)
  "Add PACKAGES to USERNAME in the user database."
  (let* ((record (db-get username marmalade/users))
         (rec-packages (assoc "package-list" record)))
    (if (not (cdr rec-packages))
        (setcdr rec-packages packages)
        (push packages rec-packages))
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

(provide 'marmalade-users)

;;; marmalade-users.el ends here
