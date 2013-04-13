;;; marmalade-mongo.el --- convert the marmalade db

;; Copyright (C) 2013  Nic Ferrier

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

;; The marmalade database is a mongo db thing.  This converts it to
;; emacs-db where we can put it into files or postgres or whatever we
;; like.

;;; Code:

(require 'dash)

(defun marmalade-mongo/target-file (target-root package-name version type)
  (format
   "%s/%s/%s/%s"
   target-root package-name version
   (concat package-name "-" version "." type)))

(defun marmalade-mongo/make-files (files target-root)
  (when files
    (destructuring-bind (file-entry &rest files) files
      (destructuring-bind (filename package-name type version) file-entry
        (let ((temp-file (make-temp-name
                          (format "mongofile-%s-%s" package-name version)))
              (target-file (marmalade-mongo/target-file
                            target-root package-name version type)))
          (if (file-exists-p target-file)
              (marmalade-mongo/make-files files target-root)
              ;; Else:
              (make-directory (file-name-directory target-file) t)
              (setq proc
                    (start-process
                     (concat "mongo-file-make-" filename)
                     " *mongofilemake*"
                     "mongofiles" "-d" "marmalade"
                     "-l" temp-file "get" filename))
              ;; Use the sentinel to move the file when it's been done
              (set-process-sentinel
               proc
               (lambda (proc status)
                 (when (equal status "finished\n")
                   (rename-file temp-file target-file)
                   (marmalade-mongo/make-files files target-root))))))))))

(defun marmalade-mongo/buf->list (buffer)
  "Converts the buffer listing of the files in mongo to a proper list."
  (with-current-buffer buffer
    (goto-char (point-min))
    (let (results)
      (while (re-search-forward
              (concat
               "^\\(\\([A-Za-z0-9-]+\\)\\.\\(el\\|tar\\)/\\([0-9.]+\\)\\)"
               "[ \t]+[0-9]")
              nil
              t)
        (setq results
              (append (list
                       (list
                        (match-string 1)
                        (match-string 2)
                        (match-string 3)
                        (match-string 4))) results)))
      results)))

(defun marmalade-mongo/make-filelist (target-root)
  "Get the list of files."
  (let ((mongo-buf (get-buffer-create "*mongofiles*")))
    (with-current-buffer mongo-buf (erase-buffer))
    (let ((proc (start-process
                 "mongo-list" mongo-buf
                 "mongofiles" "-d" "marmalade" "list")))
      (set-process-sentinel
       proc
       (lambda (proc stat)
         (when (string-match ".*finished\n" stat)
           (marmalade-mongo/make-files
            (-filter
             (lambda (entry)
               (destructuring-bind
                     (filename package-name type version) entry
                 (let ((target-file
                        (marmalade-mongo/target-file
                         target-root package-name version type)))
                   (unless (file-exists-p target-file)
                     (list filename target-file)))))
             (marmalade-mongo/buf->list mongo-buf))
            target-root)))))))

;;;###autoload
(defun marmalade-mongo-main ()
  "Main function for calling directly."
  (interactive)
  (destructuring-bind (&optional directory)
      command-line-args-left
    (let ((dir (or directory
                   marmalade-package-store-dir
                   "~/marmalade/packages")))
      (marmalade-mongo/make-filelist dir))))

(provide 'marmalade-mongo)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; marmalade-mongo.el ends here
