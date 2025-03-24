;;; twtxt-post.el --- A twtxt client for Emacs -*- lexical-binding: t -*- -*- coding: utf-8 -*-

;; SPDX-License-Identifier: GPL-3.0

;; Author: Andros <https://andros.dev>
;; Version: 1.0
;; URL: https://codeberg.org/deadblackclover/twtxt-el
;; Package-Requires: ((emacs "25.1") (request "0.2.0") (visual-fill-column "2.4"))

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

;;; Code:

(require 'twtxt-variables)
(require 'request)

(defconst twtxt--post-name-buffer "*New post | twtxt*")
(defconst twtxt--post-help-lines 6)
(defvar twtxt-post-tweet-hook nil)
(defvar twtxt--mentions nil)

(defun twtxt--get-datetime ()
  "Getting date and time according to RFC 3339 standard."
  (concat (format-time-string "%Y-%m-%dT%T")

	  ((lambda (x)
	     (concat (substring x 0 3) ":" (substring x 3 5)))
	   (format-time-string "%z"))))

(defun twtxt--replace-tab (str)
  "Replacing tabs with line breaks in STR."
  (replace-regexp-in-string "\t" "\n" str))

(defun twtxt--replace-newlines (str)
  "Replace newline characters in STR with the Unicode character \\u2028. Source: https://twtxt.dev/exts/multiline.html"
  (replace-regexp-in-string "\n" (char-to-string #x2028) (string-trim str)))


(defun twtxt--insert-mention ()
  "Insert a mention in the format '@<nick url>' into the post buffer. Source: https://twtxt.dev/exts/twt-subject.html"
  (interactive)
  (let ((following (cdr (assoc 'follow twtxt--my-profile))))
    (if (not following)
      (message "No users in the following list.")
      (let* ((user-options (mapcar (lambda (user)
                                  (concat (cdr (assoc 'name user)) " " (cdr (assoc 'url user))))
				following))
          (selected-user (completing-read "Mention: " user-options nil t)))
     (when selected-user
       (setq twtxt--mentions (cons (car (cdr (split-string selected-user " "))) twtxt--mentions))
       (insert "@<" selected-user "> "))))))


(defun twtxt--post-buffer (&optional hash)
  "Open a temporary buffer for writing and posting a new status update."
  (setq twtxt--mentions nil)
  (let ((buffer-name twtxt--post-name-buffer))
    (switch-to-buffer (get-buffer-create buffer-name))
    (erase-buffer)
    (twtxt--insert-logo)
    (twtxt--insert-formatted-text (propertize "C-c C-c" 'face 'bold))
    (twtxt--insert-formatted-text " to post\n")
    (twtxt--insert-formatted-text (propertize "C-c C-m" 'face 'bold))
    (twtxt--insert-formatted-text " to mention\n")
    (twtxt--insert-formatted-text (propertize "C-c C-k" 'face 'bold))
    (twtxt--insert-formatted-text " to cancel.")
    (twtxt--insert-separator)
    (use-local-map (let ((map (make-sparse-keymap)))
                     (set-keymap-parent map text-mode-map)
                     (define-key map (kbd "C-c C-c") 'twtxt--post-confirm)
		     (define-key map (kbd "C-c C-m") 'twtxt--insert-mention)
		     (define-key map (kbd "C-c C-k") 'twtxt--post-cancel)
                     map))
    (when hash
      (twtxt--insert-formatted-text (format "(#%s) " hash)))
    (twtxt-mode 1)
    ;; Set the 6 first lines as read-only
    (goto-char (point-max))))

(defun twtxt--post-confirm ()
  "Post the content of the buffer as a new status update."
  (interactive)
  (let* ((post-start (save-excursion
                       (goto-char (point-min))
                       (forward-line twtxt--post-help-lines) ;; Jump over help lines.
                       (point)))
         (post (buffer-substring-no-properties post-start (point-max))))
    (when (and post (not (string-blank-p post)))
      ;; Add post to twtxt file
      (append-to-file
       (concat (twtxt--get-datetime) "\t" (twtxt--replace-newlines post) "\n")
       nil
       twtxt-file)
      ;; Multi-User User-Agent Extension: https://twtxt.dev/exts/multiuser-user-agent.html
      (dolist (mention-url twtxt--mentions)
	(request
	  mention-url
	  :type "GET"
	  :headers `(("User-Agent" . ,(format "twtxt-el/%s (+%s; @%s)" twtxt--version (cdr (assoc 'url twtxt--my-profile)) (cdr (assoc 'nick twtxt--my-profile))))
		     ("Content-Type" . "text/plain; charset=utf-8"))))
      ;; Run hook
      (run-hooks 'twtxt-post-tweet-hook)
      ;; Update timeline
      (twtxt--update-my-profile-on-feeds)
      ;; Feedback
      (message "New post published!"))
    (kill-buffer)))


(defun twtxt--post-cancel ()
  "Cancel and close the new post buffer without posting."
  (interactive)
  (when (yes-or-no-p "Are you sure you want to cancel this post?")
    (kill-buffer)
    (message "Post canceled.")))



(provide 'twtxt-post)
;;; twtxt-post.el ends here
