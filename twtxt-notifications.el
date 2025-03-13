;;; twtxt-notifications.el --- A twtxt client for Emacs -*- lexical-binding: t -*- -*- coding: utf-8 -*-
;;;

;; SPDX-License-Identifier: GPL-3.0

;; Author: Andros - https://andros.dev
;; Version: 0.2
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
(require 'seq)
(require 'twtxt-string)
(require 'twtxt-feed)
(require 'twtxt-image)
(require 'twtxt-post)
(require 'twtxt-profile)
(require 'widget)
(require 'wid-edit)
(require 'url)
(require 'cl-lib)

;; Variables
(defconst twtxt--notifications-name-buffer "*Notifications | twtxt*")
(defvar twtxt--notifications-widget-loading-more nil)
(defvar twtxt--notifications-per-page 10)
(defvar twtxt--notifications-page 1)
(defvar twtxt--notifications-current-list nil)
(defvar twtxt--notifications-twts nil)

;; Functions
(defun twtxt--quit-notifications ()
  "Quit the notifications buffer."
  (interactive)
  (kill-buffer twtxt--notifications-name-buffer)
  (switch-to-buffer twtxt--timeline-name-buffer))

(defun twtxt--notifications-next-page ()
  "Go to the next page of notifications."
  (when (and (string= (buffer-name) twtxt--notifications-name-buffer)
             (< (* twtxt--notifications-page twtxt--notifications-per-page) (length twtxt--notifications-twts)))
    (setq twtxt--notifications-page (1+ twtxt--notifications-page))
    (let ((inhibit-read-only t))  ;; Allow editing
      (widget-delete twtxt--notifications-widget-loading-more)
      (twtxt--insert-notifications)
      (twtxt--notifications-insert-loading))))

(defun twtxt--notifications-insert-loading ()
  "Redraw the navigator."
  (setq twtxt--notifications-widget-loading-more (widget-create 'push-button
                                                  :notify (lambda (&rest ignore)
                                                            (twtxt--notifications-next-page))
                                                  " ↓ Show more ↓ ")))

(defun twtxt--insert-notifications-header ()
  "Redraw the header."
  (twtxt--insert-formatted-text "\n")
  ;; Logo
  (twtxt--insert-logo)
  (twtxt--insert-formatted-text "(n) Next | (p) Previous | (r) Reply | (t) Thread | (b) Back")
  (twtxt--insert-separator))

(defun twtxt--insert-notifications ()
  "Draw the notifications list."
  (dolist (twt (cl-subseq
                twtxt--notifications-twts
                (* (- twtxt--notifications-page 1) twtxt--notifications-per-page)
                (* twtxt--notifications-page twtxt--notifications-per-page)))
    (let* ((author-id (cdr (assoc 'author-id twt)))
	   (profile (twtxt--profile-by-id author-id))
	   (nick (cdr (assoc 'nick profile)))
	   (avatar-url (cdr (assoc 'avatar profile)))
	   (hash (cdr (assoc 'hash twt)))
	   (thread (cdr (assoc 'thread twt)))
	   (date (format-time-string "%Y-%m-%d %H:%M" (float-time (cdr (assoc 'date twt)))))
	   (text (cdr (assoc 'text twt)))
	   (type (alist-get 'type twt)))
      (twtxt--twt-component author-id text nick date avatar-url hash thread twtxt--notifications-current-list type))))


(defun twtxt--notifications-layout (current-list)
  "Create the main layout for notifications from CURRENT-LIST."
  (switch-to-buffer twtxt--notifications-name-buffer)
  (kill-all-local-variables)
  (setq twtxt--notifications-current-list current-list)
  (setq twtxt--notifications-twts (twtxt--list-notifications twtxt--notifications-current-list))
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  ;; Layouts
  (when twtxt--pandoc-p (org-mode))
  (twtxt--insert-notifications-header)
  (twtxt--insert-notifications)
  (twtxt--notifications-insert-loading)
  (use-local-map widget-keymap)
  (display-line-numbers-mode 0)
  ;; Keybindings
  (local-set-key (kbd "b") (lambda () (interactive) (twtxt--quit-notifications)))
  (twtxt--org-mode-visual-fill)
  (widget-setup)
  (widget-forward 1))

(add-hook 'twtxt--last-twt-hook (lambda () (twtxt--notifications-next-page)))

(provide 'twtxt-notifications)
;;; twtxt-notifications.el ends here
