;;; twtxt-timeline.el --- A twtxt client for Emacs -*- lexical-binding: t -*- -*- coding: utf-8 -*-
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
(require 'twtxt-string)
(require 'twtxt-feed)
(require 'twtxt-image)
(require 'twtxt-post)
(require 'twtxt-profile)
(require 'twtxt-ui)
(require 'widget)
(require 'wid-edit)
(require 'url)
(require 'cl-lib)

;; Variables
(defvar twtxt--widget-loading-more nil)
(defvar twtxt--twtxts-per-page 10)
(defvar twtxt--twtxts-page 1)
(defconst twtxt--timeline-name-buffer "*Timeline | twtxt*")


;; Functions
(defun twtxt--next-page ()
  "Go to the next page of twtxts."
  (when (and (string= (buffer-name) twtxt--timeline-name-buffer)
	 (< (* twtxt--twtxts-page twtxt--twtxts-per-page) (length (twtxt--list-timeline))))
    (setq twtxt--twtxts-page (1+ twtxt--twtxts-page))
    (let ((inhibit-read-only t))  ;; Allow editing
      (widget-delete twtxt--widget-loading-more)
      (twtxt--insert-timeline)
      (twtxt--insert-loading))))

(defun twtxt--timeline-refresh ()
  "Refresh the timeline."
  (interactive)
  (setq twtxt--twtxts-page 1)
  (twtxt-timeline))


(defun twtxt--insert-timeline-header ()
  "Redraw the header."
  (twtxt--insert-formatted-text "\n")
  ;; Logo
  (twtxt--insert-logo)
  ;; Buttons
  (widget-create 'push-button
		 :notify (lambda (&rest ignore)
			   (twtxt--post-buffer))
		 :help-echo "Publish a new twtxt post."
		 "＋ New post ")
  (twtxt--insert-formatted-text " ")
  (widget-create 'push-button
		 :notify (lambda (&rest ignore)
			   (twtxt--timeline-refresh))
		 " ↺ Refresh ")
  (twtxt--insert-formatted-text " ")
  (widget-create 'push-button
		 :notify (lambda (&rest ignore)
			   (twtxt---profile-layout (cdr (assoc 'id twtxt--my-profile))))
		 " 🖼 My profile ")
  (twtxt--insert-formatted-text "\n\n")
  (twtxt--insert-formatted-text "(n) Next | (p) Previous | (c) Create | (r) Reply | (t) Thread | (q) Quit")
  (twtxt--insert-separator))

(defun twtxt--insert-loading ()
  "Redraw the navigator."
  (setq twtxt--widget-loading-more (widget-create 'push-button
						  :notify (lambda (&rest ignore)
							    (twtxt--next-page))
						  " ↓ Show more ↓ ")))

(defun twtxt--insert-timeline ()
  "Redraw the timeline."
  ;; List twtxts
  (let ((current-list (twtxt--list-timeline)))
    (dolist (twt (cl-subseq
		  current-list
		  (* (- twtxt--twtxts-page 1) twtxt--twtxts-per-page)
		  (* twtxt--twtxts-page twtxt--twtxts-per-page)))
      (let* ((author-id (cdr (assoc 'author-id twt)))
	     (profile (twtxt--profile-by-id author-id))
	     (nick (cdr (assoc 'nick profile)))
	     (avatar-url (cdr (assoc 'avatar profile)))
	     (hash (cdr (assoc 'hash twt)))
	     (thread (cdr (assoc 'thread twt)))
	     (date (format-time-string "%Y-%m-%d %H:%M" (cdr (assoc 'date twt))))
	     (text (cdr (assoc 'text twt))))
	(twtxt--twt-component author-id text nick date avatar-url hash thread current-list)))))


(defun twtxt--timeline-layout ()
  "Create the main layout for the welcome screen."
  (switch-to-buffer twtxt--timeline-name-buffer)
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  ;; Layouts
  (when twtxt--pandoc-p (org-mode))
  (twtxt--insert-timeline-header)
  (twtxt--insert-timeline)
  (twtxt--insert-loading)
  (use-local-map widget-keymap)
  (display-line-numbers-mode 0)
  ;; Keybindings
  (local-set-key (kbd "c") (lambda () (interactive) (twtxt--post-buffer)))
  (local-set-key (kbd "g") (lambda () (interactive) (twtxt--timeline-refresh)))
  (local-set-key (kbd "P") (lambda () (interactive) (twtxt---profile-layout (cdr (assoc 'id twtxt--my-profile)))))
  (local-set-key (kbd "q") (lambda () (interactive) (kill-buffer twtxt--timeline-name-buffer)))
  (twtxt--twt-component-keybindings)
  (widget-setup)
  (widget-forward 1))

(add-hook 'twtxt--last-twt-hook (lambda () (twtxt--next-page)))

(provide 'twtxt-timeline)
;;; twtxt-timeline.el ends here
