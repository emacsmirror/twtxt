;;; twtxt-profile.el --- A twtxt client for Emacs -*- lexical-binding: t -*- -*- coding: utf-8 -*-

;; SPDX-License-Identifier: GPL-3.0

;; Author: Andros <https://andros.dev>
;; Version: 1.0
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

(require 'twtxt-string)
(require 'twtxt-feed)
(require 'twtxt-image)

(defconst twtxt--profile-buffer "*Profile | twtxt*")

(defun twtxt--insert-section (title)
  "Insert a section TITLE formatted with separators."
  (twtxt--insert-formatted-text (concat "\n━━━━━━━━━━━━━━━━━━━\n  " title "\n━━━━━━━━━━━━━━━━━━━\n")))

(defun twtxt---profile-layout (author-id)
  "Open the twtxt profile buffer."
  (interactive)
  (switch-to-buffer twtxt--profile-buffer)
  (let* ((profile (twtxt--profile-by-id author-id))
	 (avatar (cdr (assoc 'avatar profile)))
	 (nick (cdr (assoc 'nick profile)))
	 (url (cdr (assoc 'url profile)))
	 (description (cdr (assoc 'description profile)))
	 (links (cdr (assoc 'link profile)))
	 (follows (cdr (assoc 'follow profile)))
	 (public-key (cdr (assoc 'public-key profile))))
    ;; Avatar
    (when avatar
      (twtxt--insert-formatted-text "\n ")
      (twtxt--put-image-from-cache avatar (line-number-at-pos) 200))
    ;; Nick
    (when nick
      (twtxt--insert-formatted-text "\n\n")
      (twtxt--insert-formatted-text (format " 👤 Nick: ") nil "yellow")
      (twtxt--insert-formatted-text nick))
    ;; URL
    (when url
      (twtxt--insert-formatted-text "\n\n")
      (if (stringp url)
	  (progn
	    ;; Only one URL
	    (twtxt--insert-formatted-text (format " 🔗 URL: ") nil "yellow")
	    (twtxt--insert-formatted-text url))
	(progn
	  ;; Multiple URLs
	  (twtxt--insert-formatted-text (format " 🔗 URLs:\n") nil "yellow")
	  (dolist (item url)
	    (twtxt--insert-formatted-text (format "      - %s\n" item))))))
    (when description
      (twtxt--insert-formatted-text "\n\n")
      (twtxt--insert-formatted-text (format " 📖 Description: ") nil "yellow")
      (twtxt--insert-formatted-text description)
      (twtxt--insert-formatted-text "\n"))
    ;; Links
    (when links
      (twtxt--insert-section "📌 LINKS")
      (dolist (link links)
	;; When name is present
	(when (cdr (assoc 'name link)) (twtxt--insert-formatted-text (concat (cdr (assoc 'name link)) " → ") nil "yellow"))
	(twtxt--insert-formatted-text (cdr (assoc 'url link)))
	(insert "\n")))
    ;; Follows
    (when follows
      (twtxt--insert-section (concat "👥 FOLLOWING " (format "%s" (length follows))))
      (dolist (follow follows)
	(twtxt--insert-formatted-text (concat (cdr (assoc 'name follow)) " → ") nil "yellow")
	(twtxt--insert-formatted-text (cdr (assoc 'url follow)))
	(insert "\n")))
    ;; Direct Messages
    (twtxt--insert-section " 🗨 DIRECT MESSAGE")
    (twtxt--insert-formatted-text " ")
    (twtxt--insert-formatted-text (if public-key "🟢" "🔴"))
    (twtxt--insert-formatted-text (if public-key " Active" " Inactive") nil "yellow")
    (twtxt--insert-formatted-text "\n")
    (when public-key
      (twtxt--insert-formatted-text " 🔑 Public key: " nil "yellow")
      (twtxt--insert-formatted-text public-key)))

  (local-set-key (kbd "q") (lambda () (interactive) (kill-buffer twtxt--profile-buffer)))
  (goto-char (point-min))
  (read-only-mode))

(provide 'twtxt-profile)
;;; twtxt-profile.el ends here
