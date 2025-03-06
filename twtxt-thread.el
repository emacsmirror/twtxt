;;; twtxt-thread.el --- A twtxt client for Emacs -*- lexical-binding: t -*- -*- coding: utf-8 -*-
;;;

;; SPDX-License-Identifier: GPL-3.0

;; Author: Andros - https://andros.dev
;; Version: 0.2
;; URL: https://codeberg.org/deadblackclover/twtxt-el
;; Package-Requires: ((emacs "25.1") (request "0.2.0") (visual-fill-column "1.12"))

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
(defconst twtxt--thread-name-buffer "*Thread | twtxt*")

;; Functions
(defun twtxt--quit-thread ()
  "Quit the thread buffer."
  (interactive)
  (kill-buffer twtxt--thread-name-buffer)
  (switch-to-buffer twtxt--timeline-name-buffer))

(defun twtxt--list-thread (thread-id current-list)
  "List all twts in CURRENT-LIST with THREAD-ID."
  (let* ((thread-list (seq-filter (lambda (twt)
				    (or
				     (equal thread-id (cdr (assoc 'hash twt)))
				     (equal thread-id (cdr (assoc 'thread twt)))))
				  current-list))
	 (sorted-list (seq-sort (lambda (a b)
				  (< (cdr (assoc 'date a))
				     (cdr (assoc 'date b)))) thread-list)))
    sorted-list))

(defun twtxt--insert-thread-header ()
  "Redraw the header."
  (twtxt--insert-formatted-text "\n")
  ;; Logo
  (twtxt--insert-logo)
  (twtxt--insert-formatted-text "(n) Next | (p) Previous | (r) Reply | (t) Thread | (b) Back")
  (twtxt--insert-separator))

(defun twtxt--insert-thread (thread-id current-list)
  "Draw the txt's thread. THREAD-ID is the id of the thread. CURRENT-LIST is the list of twts."
  (let ((twts-thread (twtxt--list-thread thread-id current-list)))
    (dolist (twt twts-thread)
      (let* ((author-id (cdr (assoc 'author-id twt)))
	     (profile (twtxt--profile-by-id author-id))
	     (nick (cdr (assoc 'nick profile)))
	     (avatar-url (cdr (assoc 'avatar profile)))
	     (hash (cdr (assoc 'hash twt)))
	     (thread (cdr (assoc 'thread twt)))
	     (date (format-time-string "%Y-%m-%d %H:%M" (cdr (assoc 'date twt))))
	     (text (cdr (assoc 'text twt))))
	(twtxt--twt-component author-id text nick date avatar-url hash (unless (equal thread thread-id) thread) twts-thread nil)))
    (twtxt--insert-formatted-text "\n\n")
    (widget-create 'push-button
		   :notify (lambda (&rest ignore)
			     (twtxt--post-buffer thread-id))
		   :help-echo "Add reply to thread"
		   " ＋ Add reply to thread ")
    (twtxt--insert-formatted-text "\n\n")
    (widget-create 'push-button
		   :notify (lambda (&rest ignore)
			     (twtxt--quit-thread))
		   :help-echo "Close the thread buffer."
		   " ← Back ")))


(defun twtxt--thread-layout (thread-id current-list)
  "Create the main layout for thread."
  (switch-to-buffer twtxt--thread-name-buffer)
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  ;; Layouts
  (when twtxt--pandoc-p (org-mode))
  (twtxt--insert-thread-header)
  (twtxt--insert-thread thread-id current-list)
  (use-local-map widget-keymap)
  (display-line-numbers-mode 0)
  ;; Keybindings
  (local-set-key (kbd "P") (lambda () (interactive) (twtxt---profile-layout (cdr (assoc 'id twtxt--my-profile)))))
  (local-set-key (kbd "b") (lambda () (interactive) (twtxt--quit-thread)))
  (twtxt--twt-component-keybindings)
  (widget-setup)
  (widget-forward 1))

(provide 'twtxt-thread)
;;; twtxt-timeline.el ends here
