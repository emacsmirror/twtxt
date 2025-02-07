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
(require 'wid-edit)
(require 'url)
(require 'cl-lib)

;; Variables
(defvar twtxt--widget-loading-more nil)
(defvar twtxt--twtxts-per-page 5)
(defvar twtxt--twtxts-page 1)
(defconst twtxt--timeline-name-buffer "*twtxt - Timeline*")
(defvar twtxt--timeline-separator
  (make-string
   (window-width) ?\u2500))

;; Macros
(defmacro widget-insert-text (text)
  "Insert text into the widget."
  `(widget-create 'item ,text))

;; Functions

(defun insert-formatted-text (text &optional size font-color background-color)
  "Inserts TEXT into the buffer with optional font SIZE, FONT-COLOR, and BACKGROUND-COLOR."
  (let ((start (point)))
    (insert text)
    (let ((end (point))
          (props (list)))
      (when size
        (push `(:height ,size) props))
      (when font-color
        (push `(:foreground ,font-color) props))
      (when background-color
        (push `(:background ,background-color) props))
      (when props
        (put-text-property start end 'face (apply #'append props))))))


(defun twtxt--goto-next-separator ()
  "Go to the next separator in the buffer."
  (interactive)
  (let ((separator (concat "^" (regexp-quote twtxt--timeline-separator) "$")))
    (if (search-forward-regexp separator nil t)
      (beginning-of-line) ;; Go to the matching line
      (twtxt--next-page) ;; Load more twtxts
      ))
  (forward-line 1)
  (recenter 0))

(defun twtxt--goto-previous-separator ()
  "Go to the previous separator in the buffer."
  (interactive)
  (let ((separator (concat "^" (regexp-quote twtxt--timeline-separator) "$")))
    (search-backward-regexp separator nil t)
    (when (search-backward-regexp separator nil t)
      (beginning-of-line)))
  (forward-line 1)
  (recenter 0))

(defun twtxt-recalculate-timeline-separator ()
  "Recalculate the timeline separator."
  (setq twtxt--timeline-separator
	(make-string
	 (window-width) ?\u2500)))

(defun twtxt--next-page ()
  "Go to the next page of twtxts."
  (when (< (* twtxt--twtxts-page twtxt--twtxts-per-page) (length (twtxt--timeline)))
      (setq twtxt--twtxts-page (1+ twtxt--twtxts-page))
      (widget-delete twtxt--widget-loading-more)
      (twtxt--insert-timeline)
      (twtxt--insert-loading)))

(defun twtxt--insert-header ()
  "Redraw the header."
  (insert-formatted-text "\n")
  (widget-create 'push-button
		 :notify (lambda (&rest ignore)
			   (twtxt--post-buffer))
		 :help-echo "Publish a new twtxt post."
		 " ＋ New post ")
  (insert-formatted-text " ")
  (widget-create 'push-button
		 :notify (lambda (&rest ignore)
			   (twtxt-timeline))
		 " ↺ Refresh timeline ")
  (insert-formatted-text " ")
  (widget-create 'push-button
		 :notify (lambda (&rest ignore)
			   (twtxt-timeline))
		 " 🖼 Show profile ")
  (insert-formatted-text "\n\n")
  (widget-insert-text twtxt--timeline-separator)
  (insert-formatted-text "\n\n"))

(defun twtxt--insert-loading ()
  "Redraw the navigator."
  (setq twtxt--widget-loading-more (widget-create 'item "\n\n ↓ Loading more ↓ ")))

(defun twtxt--insert-timeline ()
  "Redraw the timeline."
  (twtxt-recalculate-timeline-separator)
  ;; List twtxts
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
      (insert-formatted-text "  ")
      (insert-formatted-text text)
      ;; images
      (when (twtxt--image-p text)
	(progn
	  (insert-formatted-text "\n\n")
	  (dolist (url (get-images-urls text))
	    (progn
	      (twtxt--put-image-from-cache url (line-number-at-pos) 200)
	      (insert-formatted-text "  ")))))
      (insert-formatted-text "\n\n")
      ;; avatar
      (insert-formatted-text "  ")
      (if avatar-url
	  (twtxt--put-image-from-cache avatar-url (line-number-at-pos) 50)
	  (insert-formatted-text twtxt--anonymous-avatar 200 nil nil))

      ;; nick + date
      (insert-formatted-text "  ")
      (insert-formatted-text nick nil "green")
      (insert-formatted-text "  ")
      (insert-formatted-text date nil "gray")
      (insert-formatted-text "  ")
      (widget-create 'push-button
		     :notify (lambda (&rest ignore) (message "Feature not yet implemented."))
		     " ⎆ Go to thread")
      (insert-formatted-text "  ")
      (widget-create 'push-button
		     :notify (lambda (&rest ignore) (message "Feature not yet implemented."))
		     " ↳ Reply ")
      ;; Separator
      (widget-insert-text "\n")
      (widget-insert-text twtxt--timeline-separator)
      (widget-insert-text "\n"))))


(defun twtxt--timeline-layout ()
  "Create the main layout for the welcome screen."
  (switch-to-buffer twtxt--timeline-name-buffer)
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  ;; Layouts
  (twtxt-recalculate-timeline-separator)
  (twtxt--insert-header)
  (twtxt--insert-timeline)
  (twtxt--insert-loading)
  (use-local-map widget-keymap)
  (display-line-numbers-mode 0)
  ;; Keybindings
  (local-set-key (kbd "c") (lambda () (interactive) (twtxt--post-buffer)))
  (local-set-key (kbd "n") (lambda () (interactive) (twtxt--goto-next-separator)))
  (local-set-key (kbd "p") (lambda () (interactive) (twtxt--goto-previous-separator)))
  (local-set-key (kbd "g") (lambda () (interactive) (twtxt-timeline)))
  (local-set-key (kbd "q") (lambda () (interactive) (kill-buffer twtxt--timeline-name-buffer)))
  ;; Go to the top of the buffer
  (widget-setup)
  (widget-forward 1))

(provide 'twtxt-timeline)
;;; twtxt-timeline.el ends here
