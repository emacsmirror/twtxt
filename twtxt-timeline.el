;;; twtxt-timeline.el --- A twtxt client for Emacs -*- lexical-binding: t -*- -*- coding: utf-8 -*-
;;;

;; SPDX-License-Identifier: GPL-3.0

;; Author: Andros - https://andros.dev
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

;;; Code:
(require 'twtxt-feed)
(require 'twtxt-image)
(require 'widget)
(require 'url)
(require 'cl-lib)

(eval-when-compile
  (require 'wid-edit))

;; Variables
(defvar twtxt--widgets '())
(defvar twtxt--twtxts-per-page 5)
(defvar twtxt--twtxts-page 1)
(defconst twtxt--timeline-name-buffer "*twtxt - Timeline*")
(defvar twtxt--timeline-separator
  (make-string
   (window-width) ?\u2500))

;; Functions
(defun twtxt-recalculate-timeline-separator ()
  "Recalculate the timeline separator."
  (setq twtxt--timeline-separator
	(make-string
	 (window-width) ?\u2500)))

(defun twtxt--clear-txts ()
  "Clear the list of twtxts."
  (let ((line-from-twtxt 5))
    (when (>= (line-number-at-pos (point-max)) line-from-twtxt)
      (let ((inhibit-read-only t))
	;; Delete inserts: From line line-from-twtxt to the end
	(goto-line line-from-twtxt)
	(delete-region (point) (point-max))
	;; Delete old widgets
	(dolist (widget-item twtxt--widgets)
	  (widget-delete widget-item))
	(setq twtxt--widgets '())))))

(defun twtxt--previous-page ()
  "Go to the previous page of twtxts."
  (setq twtxt--twtxts-page (1- twtxt--twtxts-page))
  (twtxt--redraw-timeline))

(defun twtxt--next-page ()
  "Go to the next page of twtxts."
  (setq twtxt--twtxts-page (1+ twtxt--twtxts-page))
  (twtxt--redraw-timeline))

(defun twtxt--redraw-timeline ()
  "Redraw the timeline."
  (twtxt--clear-txts)
  ;; twtxts
  (dolist (twts (cl-subseq (twtxt--timeline)
			   (* (- twtxt--twtxts-page 1) twtxt--twtxts-per-page)
			   (* twtxt--twtxts-page twtxt--twtxts-per-page)))
    (let* ((profile (twtxt--profile-by-id (cdr (assoc 'author-id twts))))
	   (nick (cdr (assoc 'nick profile)))
	   (avatar-url (cdr (assoc 'avatar profile)))
	   (thead (cdr (assoc 'thread twts)))
	   (date (format-time-string "%Y-%m-%d %H:%M" (encode-time (cdr (assoc 'date twts)))))
	   (text (cdr (assoc 'text twts))))
      ;; text
      (widget-insert text)
      (when (image-p text) (progn
			     (widget-insert "\n\n")
			     (dolist (url (get-images-urls text))
			       (progn
				 (put-image-from-cache url (line-number-at-pos) 200)
				 (widget-insert "  ")))))
      (widget-insert "\n\n")
      ;; avatar
      (when avatar-url (put-image-from-cache avatar-url (line-number-at-pos) 50))
      ;; nick + date
      (widget-insert (concat "  " nick " - " date " "))
      (if thead (widget-create-and-add 'push-button
				       :notify (lambda (&rest ignore) (message "Feature not yet implemented."))
				       "Go to thread") (widget-create-and-add 'push-button " ↳ Reply "))
      ;; Separator
      (widget-insert "\n")
      (widget-insert twtxt--timeline-separator)
      (widget-insert "\n")))
  ;; Navigation
  (widget-insert "\n")
  (widget-create-and-add 'push-button
		 :notify (lambda (&rest ignore)
			     (twtxt--next-page))
		 " Next page → ")
  (widget-insert "\n\n")
  (when (> twtxt--twtxts-page 1)
    (widget-create-and-add 'push-button
		   :notify (lambda (&rest ignore)
			     (twtxt--previous-page))
		   " ← Previous page "))

  )

;; Macros
(defmacro widget-create-and-add (&rest body)
  "Create a widget and add it to the list of widgets."
  `(add-to-list 'twtxt--widgets (widget-create ,@body)))

;; Layout
(defun twtxt--timeline-layout ()
  "Create the main layout for the welcome screen."
  (when (get-buffer twtxt--timeline-name-buffer)
    (kill-buffer twtxt--timeline-name-buffer))
  (switch-to-buffer twtxt--timeline-name-buffer)
  (kill-all-local-variables)
  ;; Delete old widgets
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  ;; Controls
  (twtxt-recalculate-timeline-separator)
  (widget-insert "\n")
  (widget-create 'push-button
		 :notify (lambda (&rest ignore)
			   (twtxt--post-buffer))
		 :help-echo "Publish a new twtxt post."
		 " ＋ New post ")
  (widget-insert " ")
  (widget-create 'push-button
		 :notify (lambda (&rest ignore)
			   (twtxt-timeline))
		 " ↺ Refresh timeline ")
  (widget-insert " ")
  (widget-create 'push-button
		 :notify (lambda (&rest ignore)
			   (twtxt-timeline))
		 " 🖼 Show profile ")
  (widget-insert "\n\n")
  (widget-insert twtxt--timeline-separator)
  (widget-insert "\n\n")
  (twtxt--redraw-timeline)
  (use-local-map widget-keymap)
  (widget-setup)
  (display-line-numbers-mode 0)

  ;; Go to the top of the buffer
  (goto-char (point-min))
  (read-only-mode 1))

(provide 'twtxt-timeline)
;;; twtxt-timeline.el ends here
