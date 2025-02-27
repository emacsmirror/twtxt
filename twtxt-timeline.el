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
(defvar twtxt--widget-loading-more nil)
(defvar twtxt--twtxts-per-page 10)
(defvar twtxt--twtxts-page 1)
(defconst twtxt--text-button-reply-thread " ↳ Reply in thread ")
(defconst twtxt--text-button-reply-twt " ↳ Reply to twt ")
(defconst twtxt--timeline-name-buffer "*Timeline | twtxt*")
(defvar twtxt--timeline-separator
  (make-string
   (window-width) ?\u2500))

;; Macros
(defmacro widget-insert-text (text)
  "Insert text into the widget."
  `(widget-create 'item ,text))

;; Functions
(defun twtxt--goto-reply-twt ()
  "Reply to a twtxt."
  (search-backward-regexp twtxt--timeline-separator)
  (search-forward-regexp twtxt--text-button-reply-twt)
  (widget-button-press (point)))

(defun twtxt--goto-reply-thread ()
  "Reply to a thread."
  (search-backward-regexp twtxt--timeline-separator)
  (search-forward-regexp twtxt--text-button-reply-thread)
  (widget-button-press (point)))

(defun twtxt--goto-thread ()
  "Go to the thread of a twtxt."
  (interactive)
  (message "Feature not yet implemented."))

(defun twtxt--last-separator-p ()
  "Return t if only one `twtxt--timeline-separator` remains from point to the end."
  (save-excursion
    (<= (count-matches (concat "^" (regexp-quote twtxt--timeline-separator) "$")) 1)))

(defun twtxt--goto-next-separator ()
  "Go to the next separator in the buffer."
  (interactive)
  (let ((separator (concat "^" (regexp-quote twtxt--timeline-separator) "$")))
    (if (search-forward-regexp separator nil t)
      (beginning-of-line) ;; Go to the matching line
      (twtxt--next-page) ;; Load more twtxts
      ))
  (forward-line 1)
  (recenter 0)
  ;; Load more twtxts if we are at the end of the twtxts
  (when (twtxt--last-separator-p)
    (let ((current-point (point)))
      (goto-char (point-max))
      (twtxt--next-page)
      (goto-char current-point))))

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
  (when (< (* twtxt--twtxts-page twtxt--twtxts-per-page) (length (twtxt--list-timeline)))
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


(defun twtxt--insert-header ()
  "Redraw the header."
  (insert-formatted-text "\n")
  (widget-create 'push-button
		 :notify (lambda (&rest ignore)
			   (twtxt--post-buffer))
		 :help-echo "Publish a new twtxt post."
		 "＋ New post ")
  (insert-formatted-text " ")
  (widget-create 'push-button
		 :notify (lambda (&rest ignore)
			   (twtxt--timeline-refresh))
		 " ↺ Refresh timeline ")
  (insert-formatted-text " ")
  (widget-create 'push-button
		 :notify (lambda (&rest ignore)
			   (twtxt---profile-layout (cdr (assoc 'id twtxt--my-profile))))
		 " 🖼 Show profile ")
  (insert-formatted-text "\n\n")
  (insert-formatted-text twtxt--timeline-separator)
  (insert-formatted-text "\n\n"))

(defun twtxt--insert-loading ()
  "Redraw the navigator."
  (setq twtxt--widget-loading-more (widget-create 'push-button
						  :notify (lambda (&rest ignore)
							    (twtxt--next-page))
						  " ↓ Show more ↓ ")))

(defun twtxt--insert-timeline ()
  "Redraw the timeline."
  (twtxt-recalculate-timeline-separator)
  ;; List twtxts
  (dolist (twt (cl-subseq (twtxt--list-timeline)
			  (* (- twtxt--twtxts-page 1) twtxt--twtxts-per-page)
			  (* twtxt--twtxts-page twtxt--twtxts-per-page)))
    (let* ((profile (twtxt--profile-by-id (cdr (assoc 'author-id twt))))
	   (nick (cdr (assoc 'nick profile)))
	   (avatar-url (cdr (assoc 'avatar profile)))
	   (hash (cdr (assoc 'hash twt)))
	   (thread (cdr (assoc 'thread twt)))
	   (date (format-time-string "%Y-%m-%d %H:%M" (encode-time (cdr (assoc 'date twt)))))
	   (text (cdr (assoc 'text twt))))
      ;; text
      (insert-formatted-text "\n  ")
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
      (insert-formatted-text nick nil "yellow")
      (insert-formatted-text "  ")
      (insert-formatted-text date nil "#FF5733")
      (insert-formatted-text "  ")
      (when thread
	(widget-create 'push-button
		       :notify (lambda (&rest ignore) (message "Feature not yet implemented."))
		       " ⎆ Open thread "))
      (insert-formatted-text "  ")
      (when thread
	(widget-create 'push-button
		       :notify (lambda (&rest ignore) (twtxt--post-buffer thread))
		       twtxt--text-button-reply-thread))
      (insert-formatted-text "  ")
      (widget-create 'push-button
		     :notify (lambda (&rest ignore) (twtxt--post-buffer hash))
		     twtxt--text-button-reply-twt)
      (insert-formatted-text "  ")
      (widget-create 'push-button
		     :notify (lambda (&rest ignore) (twtxt---profile-layout (cdr (assoc 'author-id twt))))
		     " Show profile ")
      ;; Separator
      (insert-formatted-text "\n")
      (insert-formatted-text twtxt--timeline-separator)
      (insert-formatted-text "\n"))))


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
  (local-set-key (kbd "g") (lambda () (interactive) (twtxt--timeline-refresh)))
  (local-set-key (kbd "t") (lambda () (interactive) (twtxt--goto-thread)))
  (local-set-key (kbd "r") (lambda () (interactive) (twtxt--goto-reply-twt)))
  (local-set-key (kbd "R") (lambda () (interactive) (twtxt--goto-reply-thread)))
  (local-set-key (kbd "P") (lambda () (interactive) (twtxt-my-profile)))
  (local-set-key (kbd "q") (lambda () (interactive) (kill-buffer twtxt--timeline-name-buffer)))
  ;; Go to the top of the buffer
  (widget-setup)
  (widget-forward 1))

(provide 'twtxt-timeline)
;;; twtxt-timeline.el ends here
