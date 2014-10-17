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
(require 'dash)

;;; UnVerifieds - stuff about users who haven't verified a password yet

(defvar marmalade/unverifieds
  (db-make `(db-hash
             :filename
             ,(expand-file-name
               "marmalade-verifies-db"
               (or marmalade-db-dir marmalade-dir))))
  "The database of verifications.

Actually there should only be one key in this but we persist it
over and over.")

(defun marmalade/list-unverifieds ()
  (db-get "unverified" marmalade/unverifieds))

(defun marmalade/add-unverified (username)
  (let ((code (format "%X" (random)))
        (existing (db-get "unverified" marmalade/unverifieds)))
    (db-put
     "unverified"
     (cons (cons code username) existing)
     marmalade/unverifieds)
    code))

(defun marmalade/remove-verified (verified)
  "Remove the specified VERIFIED code from the database."
  (db-put
   "unverified"
   (--filter
    (not (equal verified (car it)))
    (db-get "unverified" marmalade/unverifieds))
   marmalade/unverifieds)
  (db-hash/save marmalade/unverifieds))


;;; Users proper

(defvar marmalade/users
  (db-make `(db-hash
             :filename
             ,(expand-file-name
               "marmalade-user-db"
               (or marmalade-db-dir marmalade-dir))))
  "The user database.

Keys in the records are:

  \"digest\"
  \"email\"
  \"name\"
  \"salt\"
  \"package-list\"
  \"token\"

And they are stored in an alist.")

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
  (-sort
   'string<
   (kva "package-list" (db-get username marmalade/users))))

(provide 'marmalade-users)

;;; marmalade-users.el ends here
