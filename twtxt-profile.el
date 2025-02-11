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
  (insert-formatted-text (concat "\n━━━━━━━━━━━━━━━━━\n  " title "\n━━━━━━━━━━━━━━━━━\n")))

(defun twtxt---profile-layout (author-id)
  "Open the twtxt profile buffer."
  (interactive)
  (switch-to-buffer twtxt--profile-buffer)
  (let ((profile (twtxt--profile-by-id author-id)))
    ;; Avatar
    (insert-formatted-text "\n ")
    (twtxt--put-image-from-cache (cdr (assoc 'avatar profile)) (line-number-at-pos) 200)
    ;; Nick
    (insert-formatted-text "\n\n")
    (insert-formatted-text (format " 👤 Nick: ") nil "yellow")
    (insert-formatted-text (cdr (assoc 'nick profile)))
    ;; URL
    (insert-formatted-text "\n\n")
    (insert-formatted-text (format " 🔗 URL: ") nil "yellow")
    (insert-formatted-text (cdr (assoc 'url profile)))
    ;; Description
    (insert-formatted-text "\n\n")
    (insert-formatted-text (format " 📖 Description: ") nil "yellow")
    (insert-formatted-text (cdr (assoc 'description profile)))
    (insert-formatted-text "\n\n")
    ;; Relevant links
    (twtxt--insert-section "📌 LINKS")
    (dolist (link (cdr (assoc 'link profile)))
      (insert-formatted-text (car link) (cdr link)))

    ;; Following
    (twtxt--insert-section "👥 FOLLOWING")
    (dolist (follow (cdr (assoc 'following profile)))
      (insert-formatted-text (car follow) (cdr follow)))

    ;; Direct Messages
    (twtxt--insert-section "🗨 DIRECT MESSAGE")
    (let ((dm (cdr (assoc 'direct-message profile))))
      (insert-formatted-text "Enable:" (if (cdr (assoc 'enable dm)) "🟢" "🔴"))
      (insert "  Public key:\n" (cdr (assoc 'public-key dm)) "\n")))

  (local-set-key (kbd "q") (lambda () (interactive) (kill-buffer twtxt-profile-buffer)))
  (goto-char (point-min))
  (read-only-mode))

(provide 'twtxt-profile)
;;; twtxt-profile.el ends here
