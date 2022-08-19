;;; pass-view.el --- Pass view major mode for password-store.el -*- lexical-binding: t; -*-

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

;; Extracted from pass.el

(require 'password-store)
(require 'f)
(require 'subr-x)

(defvar pass-view-mask "·············"
  "Mask used to hide passwords.")

(defvar pass-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'pass-view-toggle-password)
    (define-key map (kbd "C-c C-w") #'pass-view-copy-password)
    map))
(make-variable-buffer-local 'pass-view-mode-map)

(defun pass-view-entry-name (&optional buffer)
  "Return the entry name for BUFFER.
This function only works when `pass-view-mode' is enabled."
  (with-current-buffer (or buffer (current-buffer))
    (when (eq major-mode 'pass-view-mode)
      (f-no-ext (replace-regexp-in-string
                 (format "^%s/" (f-expand (password-store-dir)))
                 ""
                 buffer-file-name)))))

(defun pass-view-toggle-password ()
  "Enable or disable password hiding."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((buf-modified (buffer-modified-p)))
      (if (string= (get-text-property (point) 'display)
                   pass-view-mask)
          (pass-view-unmask-password)
        (pass-view-mask-password))
      (set-buffer-modified-p buf-modified))))

(defun pass-view-copy-password ()
  "Copy the password of the entry in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (copy-region-as-kill (point) (line-end-position))
    (message "Password copied to kill ring.")))

(defun pass-view-mask-password ()
  "Mask the password of the current buffer."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (set-text-properties (point-min) (line-end-position)
                           `(display ,pass-view-mask)))))

(defun pass-view-unmask-password ()
  "Show the password in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (remove-text-properties (point-min) (line-end-position)
                            '(display nil))))

(defun pass-view-copy-token ()
  "Copy current `pass-view' buffer's OTP token into clipboard."
  (interactive)
  (when-let (entry-name (pass-view-entry-name))
    (password-store-otp-token-copy entry-name)))

(defun pass-view-qrcode ()
  "Open a new buffer that displays a QR Code for the current entry."
  (interactive)
  (when-let (entry-name (pass-view-entry-name))
    (let ((qrcode-buffer (get-buffer-create "*pass-view-qrcode*")))
      (with-current-buffer qrcode-buffer
        (fundamental-mode)  ;; Return buffer *back* to fundamental, in case it isn't already.
        (erase-buffer)
        (insert (password-store-otp-qrcode entry-name "SVG"))
        (image-mode)
        (local-set-key (kbd "q") 'kill-this-buffer))
      (switch-to-buffer-other-window qrcode-buffer))))

(defun pass-view--otp-remaining-secs ()
  "Return a string with the remaining countdown base 30."
  (let* ((base 30)
         (remaining (- base (% (truncate (time-to-seconds (current-time)))
                               base)))
         (remaining-str (number-to-string remaining)))
    (if (< remaining 10)  ;; leftpad-ing
        (concat "0" remaining-str)
      remaining-str)))

(defun pass-view--set-otp-header (token remaining-secs)
  "Display OTP TOKEN and REMAINING-SECS in Header Line."
  (let ((otp-data (concat (propertize " " 'display '((space :align-to 0)))
                          (propertize "OTP: " 'face 'pass-mode-header-face)
                          token " - " remaining-secs "s remaining"))
        (key-binding (concat (propertize (substitute-command-keys
                                          (format "<\\[%s]>" "pass-view-copy-token"))
                                         'face 'font-lock-constant-face)
                             " Copy token")))
    (setq header-line-format (concat otp-data "    " key-binding))
    (force-mode-line-update)))

(defun pass-view--has-otp-p ()
  "Return t-ish value if there's an OTP URI in the current buffer.
nil otherwise."
  (save-excursion
    (goto-char (point-min))
    (search-forward "otpauth://" nil t)))

(defun pass-view--otp-counter (buffer &optional last-token force-create)
  "Reload BUFFER's OTP token and countdown, using LAST-TOKEN if any, and if FORCE-CREATE, build Header Line from scratch."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (or header-line-format force-create)
        (let* ((remaining-secs (pass-view--otp-remaining-secs))
               (token (if (or (not last-token)
                              (string= remaining-secs "30"))
                          (password-store-otp-token (pass-view-entry-name buffer))
                        last-token)))
          (pass-view--set-otp-header token remaining-secs)
          (run-at-time 1 nil #'pass-view--otp-counter buffer token))))))

(defun pass-view--prepare-otp ()
  "Start an OTP token/remaining secs counter in current buffer.
This function also binds a couple of handy OTP related key-bindings to
`pass-mode-map'."
  (when (and (require 'password-store-otp nil t)
             (pass-view--has-otp-p))
    ;; Build OTP counter
    (pass-view--otp-counter (current-buffer) nil t)
    ;; Rebuild header after saving.
    (add-hook 'after-save-hook
              #'(lambda ()
                  (if (pass-view--has-otp-p)
                      (pass-view--otp-counter (current-buffer) nil t)
                    ;; Remove header line
                    (setq header-line-format nil)))
              t t)
    ;; Define a couple of OTP helper shortcuts
    (define-key pass-view-mode-map (kbd "C-c C-o") #'pass-view-copy-token)
    (define-key pass-view-mode-map (kbd "C-c C-q") #'pass-view-qrcode)))

(defvar pass-view-font-lock-keywords '("\\(^[^:\t\n]+:\\) " 1 'font-lock-keyword-face)
  "Font lock keywords for ‘pass-view-mode’.")

(define-derived-mode pass-view-mode nil "Pass-View"
  "Major mode for viewing password-store entries.

\\{pass-view-mode-map}"
  (pass-view-toggle-password)
  (pass-view--prepare-otp)
  (setq-local font-lock-defaults '(pass-view-font-lock-keywords t))
  (font-lock-mode 1)
  (message
   (substitute-command-keys
    "Press <\\[pass-view-toggle-password]> to display & edit the password")))

(add-to-list 'auto-mode-alist
             (cons
              (format "%s/.*\\.gpg\\'"
                      (expand-file-name (password-store-dir)))
              'pass-view-mode))

(provide 'pass-view)

;;; pass-view.el ends here
