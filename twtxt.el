;;; twtxt.el --- A twtxt client for Emacs

;; SPDX-License-Identifier: GPL-3.0

;; Author: DEADBLACKCLOVER <deadblackclover@protonmail.com>
;; Colaborator: Andros - https://andros.dev
;; Version: 0.2
;; URL: https://codeberg.org/deadblackclover/twtxt-el
;; Package-Requires: ((emacs "25.1") (request "0.2.0"))

;; Copyright (c) 2020, DEADBLACKCLOVER.

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; twtxt is a decentralised, minimalist microblogging service for hackers.

;; You want to get some thoughts out on the internet in a convenient and
;; slick way while also following the gibberish of others? Instead of
;; signing up at a closed and/or regulated microblogging platform, getting
;; your status updates out with twtxt is as easy as putting them in a
;; publicly accessible text file.  The URL pointing to this file is your
;; identity, your account.  twtxt then tracks these text files, like a
;; feedreader, and builds your unique timeline out of them, depending on
;; which files you track.  The format is simple, human readable, and
;; integrates well with UNIX command line utilities.

;; Usage:
;; View your timeline `M-x twtxt-timeline`
;; Post a status update `M-x twtxt-post`

;;; Code:
(require 'cl-lib)
(add-to-list 'load-path ".")

(defgroup twtxt nil
  "A twtxt client for Emacs."
  :group 'twtxt)

(defcustom twtxt-file "~/twtxt"
  "Path to twtxt file."
  :type 'file
  :group 'twtxt)


;; Multiline Extension: https://twtxt.dev/exts/multiline.html
(defconst twtxt--char-newline (char-to-string #x2028))


(defvar twtxt-post-tweet-hook nil)

(defun twtxt-get-datetime ()
  "Getting date and time according to RFC 3339 standard."
  (concat (format-time-string "%Y-%m-%dT%T")

	  ((lambda (x)
	     (concat (substring x 0 3) ":" (substring x 3 5)))
	   (format-time-string "%z"))))


(defun twtxt-replace-tab (str)
  "Replacing tabs with line breaks in STR."
  (replace-regexp-in-string "\t" "\n" str))


(defun twtxt-replace-newlines (str)
  "Replace newline characters in STR with the Unicode character \\u2028. Source: https://twtxt.dev/exts/multiline.html"
  (replace-regexp-in-string "\n" (char-to-string #x2028) str))


(defun twtxt-timeline ()
  "View your timeline."
  (interactive)
  (twtxt--timeline-layout))

(defun twtxt-timeline--previous ()
  "Move to the previous post."
  (interactive)
  ;; Search for the previous twtxt-line
  (search-backward (concat twtxt-line "\n\n")))

(defun twtxt-timeline--next ()
  "Move to the next post."
  (interactive)
  ;; Search for the next twtxt-line
  (search-forward (concat twtxt-line "\n\n")))


(defun twtxt-post--mention ()
  "Insert a mention in the format '@<nick url>' into the post buffer. Source: https://twtxt.dev/exts/twt-subject.html"
  (interactive)
  (when (not twtxt-following)
    (message "No users in the following list."))
  (let* ((user-options (mapcar (lambda (item)
                                 (concat (car item) " " (cadr item)))
                               twtxt-following))
         (selected-user (completing-read "Mention: " user-options nil t)))
    (when selected-user
      (insert "@<" selected-user "> "))))


(defun twtxt-post-buffer ()
  "Open a temporary buffer for writing and posting a new status update."
  (let ((buffer-name "*Twtxt New Post*"))
    (switch-to-buffer (get-buffer-create buffer-name))
    (erase-buffer)
    (insert "Write your post below:\n\n")
    (insert (propertize "C-c C-c" 'face 'bold))
    (insert " to post\n")
    (insert (propertize "C-c C-m" 'face 'bold))
    (insert " to mention\n")
    (insert (propertize "C-c C-k" 'face 'bold))
    (insert " to cancel.\n\n")

    (use-local-map (let ((map (make-sparse-keymap)))
                     (set-keymap-parent map text-mode-map)
                     (define-key map (kbd "C-c C-c") 'twtxt-post--confirm)
		     (define-key map (kbd "C-c C-m") 'twtxt-post--mention)
		     (define-key map (kbd "C-c C-k") 'twtxt-post--cancel)
                     map))
    (goto-char (point-max))
    (message "Write your post and press C-c C-c to send or C-c C-k to cancel.")))

(defun twtxt-post--confirm ()
  "Post the content of the buffer as a new status update."
  (interactive)
  (let ((post (buffer-substring-no-properties
               (save-excursion
                 (goto-char (point-min))
                 (forward-paragraph)
                 (forward-line 1)
                 (point))
               (point-max))))
    (when (and post (not (string-blank-p post)))
      (append-to-file
       (concat (twtxt-get-datetime) "\t" (twtxt-replace-newlines post) "\n")
       nil
       twtxt-file)
      (message "Posted: %s" post)
      (run-hooks 'twtxt-post-tweet-hook))
    (kill-buffer)))

(defun twtxt-post--cancel ()
  "Cancel and close the new post buffer without posting."
  (interactive)
  (when (yes-or-no-p "Are you sure you want to cancel this post?")
    (kill-buffer)
    (message "Post canceled.")))

(defun twtxt-post ()
  "POST a status update."
  (interactive)
  (twtxt-post-buffer))

(provide 'twtxt)
;;; twtxt.el ends here
