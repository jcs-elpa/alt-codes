;;; alt-codes.el --- Insert alt codes using meta key.  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Shen, Jen-Chieh
;; Created date 2019-06-23 00:27:18

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Insert alt codes using meta key.
;; Keyword: alt codes insertion meta
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3"))
;; URL: https://github.com/jcs090218/alt-codes

;; This file is NOT part of GNU Emacs.

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
;; Insert alt codes using meta key.
;;

;;; Code:


(defgroup alt-codes nil
  "Insert alt codes using meta key."
  :prefix "alt-codes-"
  :group 'tools
  :link '(url-link :tag "Repository" "https://github.com/jcs090218/alt-codes"))


(defvar-local alt-codes--code ""
  "Recording current key code.")


(defun alt-codes--pre-command-hook ()
  "Hook runs before every command."
  (when (symbolp last-input-event)
    (let* ((lie-str (symbol-name last-input-event))
           (key-id "")
           (insert-it nil))
      (if (and (stringp lie-str)
               (string-match-p "M-kp-" lie-str))
          (progn
            (setq key-id (substring lie-str (1- (length lie-str)) (length lie-str)))
            (if (string-match-p "[0-9]*" key-id)
                (setq-local alt-codes--code (concat alt-codes--code key-id))
              (setq insert-it t)))
        (setq insert-it t))

      (when (and insert-it
                 (not (string= alt-codes--code "")))
        (message "alt-codes--code: %s" alt-codes--code)
        (message "s: %s" (string-to-number alt-codes--code))
        (setq-local alt-codes--code ""))
      )))

(defun alt-codes--post-command-hook ()
  "Hook runs after every command."
  )


(defun alt-codes--enable ()
  "Enable `alt-codes-mode'."
  (add-hook 'pre-command-hook #'alt-codes--pre-command-hook nil t)
  (add-hook 'post-command-hook #'alt-codes--post-command-hook nil t))

(defun alt-codes--disable ()
  "Disable `alt-codes-mode'."
  (remove-hook 'pre-command-hook #'alt-codes--pre-command-hook t)
  (remove-hook 'post-command-hook #'alt-codes--post-command-hook t))

;;;###autoload
(define-minor-mode alt-codes-mode
  "Minor mode for inserting `alt-codes'."
  :lighter " alt-codes"
  :group alt-codes
  (if alt-codes-mode
      (alt-codes--enable)
    (alt-codes--disable)))

(defun alt-codes-turn-on-alt-codes-mode ()
  "Turn on the 'alt-codes-mode'."
  (alt-codes-mode 1))

;;;###autoload
(define-globalized-minor-mode global-alt-codes-mode
  alt-codes-mode alt-codes-turn-on-alt-codes-mode
  :require 'alt-codes)


(provide 'alt-codes)
;;; alt-codes.el ends here
