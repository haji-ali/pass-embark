;;; pass-embark.el --- Embark menu for password-store.el -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2019  Nicolas Petton & Damien Cassou

;; Author: Nicolas Petton <petton.nicolas@gmail.com>
;;         Damien Cassou <damien@cassou.me>
;; Version: 2.0
;; URL: https://github.com/NicolasPetton/pass
;; Package-Requires: ((emacs "25.1") (password-store "2.1.0") (password-store-otp "0.1.5") (f "0.17"))
;; Created: 18 August 2022
;; Keywords: password-store, password, keychain

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

;; Embark menu for password-store.el

;;; Code:
(require 'password-store)
(require 'async-completing-read)
(require 'embark)
(require 'f)
(require 'subr-x)

(defgroup pass-embark '()
  "Major mode for password-store."
  :group 'password-store)

(defcustom pass-embark-default-action 'pass-embark-view
  "The default action to run on `pass-embark-jump'.")

(defun pass-embark-view (entry)
  "Visit the ENTRY at point."
  (interactive (list (pass-embark--default-entry)))
  (find-file (concat (f-join (password-store-dir) entry) ".gpg")))

(defun pass-embark--copy-field (entry field)
  "Add FIELD of ENTRY at point to kill ring."
  (let* ((inhibit-message t)
         (parsed-entries (password-store-parse-entry entry)))
    (unless (assoc field parsed-entries)
      (user-error "Field `%s' not in  %s" field entry))
    (password-store-copy-field entry field)))

(defun pass-embark-copy-field (entry field)
  "Add FIELD of entry at point to kill ring.

When called interactively, prompt users for field with completion
using all fields in the entry."
  (interactive
   (list
    (pass-embark--default-entry)
    (password-store-read-field entry)))
  (if (equal field "secret")
      (pass-embark-copy entry)
    (pass-embark--copy-field entry field)))

(defun pass-embark-copy-username (entry)
  "Add username of entry at point to kill ring.

If the entry does not have a username field/value within the entry, and if
`pass-username-fallback-on-filename' is non-nil, copy the entry name instead."
  (interactive
   (list
    (password-store--completing-read t)))
  (condition-case err
      (pass-embark--copy-field pass-username-field)
    (user-error
     (if (not pass-username-fallback-on-filename)
         (signal (car err) (cdr err))) ;; rethrow
     (let ((entry-name (file-name-nondirectory entry)))
       (password-store--save-field-in-kill-ring entry entry-name "username")))))

(defun pass-emark-copy-url (entry)
  "Add url of entry at point to kill ring."
  (interactive (list
                (password-store--completing-read t)))
  (pass--copy-field entry password-store-url-field))

(defun pass-embark-copy-url (entry)
  "Add url of entry at point to kill ring."
  (interactive (list
                (password-store--completing-read t)))
  (pass--copy-field password-store-url-field))

(defvar pass-embark-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "k") #'password-store-remove)
    (define-key map (kbd "w") #'password-store-copy)
    (define-key map (kbd "r") #'password-store-rename)
    (define-key map (kbd "e") #'password-store-edit)
    (define-key map (kbd "U") #'password-store-url)

    (define-key map (kbd "b") #'pass-embark-copy-username)
    (define-key map (kbd "f") #'pass-embark-copy-field)
    (define-key map (kbd "u") #'pass-embark-copy-url)
    (define-key map (kbd "v") #'pass-embark-view)
    map)
  "Keymap for `pass' in `embark'.")

(add-to-list 'embark-keymap-alist
             '(pass . pass-embark-map))

(defun pass-embark-jump (entry)
  (interactive
   (list
    (let ((subdir (file-name-as-directory
                   (expand-file-name (password-store-dir)))))
      (async-completing-read
       "Pick password: "
       (acr-preprocess-lines-from-process
        'pass
        (lambda (x)
          ;; Accept only the first word in each line
          (mapcar
           (lambda (y)
             (let* ((entry (string-trim-left y (regexp-quote subdir)))
                    (dir (file-name-directory entry))
                    (file (file-name-base entry)))
               (concat
                (when dir
                  (propertize dir
                              'face 'pass-mode-directory-face))
                (propertize file
                            'face 'pass-mode-entry-face))))
           x))
        "find" subdir "-type" "f" "-name" "*.gpg" "-print")
       nil nil "" nil))))
  (funcall pass-embark-default-action entry))

(provide 'pass-embark)

;;; pass-embark.el ends here
