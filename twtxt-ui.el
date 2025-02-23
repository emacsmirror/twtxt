;;; twtxt-ui.el --- A twtxt client for Emacs -*- lexical-binding: t -*- -*- coding: utf-8 -*-
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
(require 'twtxt-image)
(require 'widget)
(require 'wid-edit)
(require 'url)
(require 'cl-lib)

;; Variables
(defvar twtxt--last-twt-hook nil)
(defconst twtxt--text-button-reply-thread " ↳ Reply thread ")
(defconst twtxt--text-button-reply-twt " ↳ Reply twt ")
;; (defconst twtxt--char-separator ?\u2501)
(defconst twtxt--char-separator ?-)

;; Functions

(defun twtxt--string-separator ()
  "Return a string with the separator character."
  (make-string (window-body-width) twtxt--char-separator))

(defun twtxt--insert-separator ()
  "Insert a horizontal line in the buffer with full width of the window."
  (twtxt--insert-formatted-text "\n")
  (twtxt--insert-formatted-text (twtxt--string-separator))
  (twtxt--insert-formatted-text "\n"))

(defun twtxt--goto-reply-twt ()
  "Reply to a twtxt."
  (search-backward-regexp twtxt--char-separator)
  (search-forward-regexp twtxt--text-button-reply-twt)
  (widget-button-press (point)))

(defun twtxt--goto-reply-thread ()
  "Reply to a thread."
  (search-backward-regexp twtxt--char-separator)
  (search-forward-regexp twtxt--text-button-reply-thread)
  (widget-button-press (point)))

(defun twtxt--goto-thread ()
  "Go to the thread of a twtxt."
  (interactive)
  (message "Feature not yet implemented."))

(defun twtxt--regexp-separator ()
  "Return the regular expression for the separator."
  (concat "^" (regexp-quote (twtxt--string-separator)) "$"))

(defun twtxt--last-separator-p ()
  "Return t if only one `twtxt--timeline-separator` remains from point to the end."
  (save-excursion
    (<= (count-matches (twtxt--regexp-separator)) 1)))

(defun twtxt--goto-next-separator ()
  "Go to the next separator in the buffer."
  (interactive)
  (if (search-forward-regexp (twtxt--regexp-separator) nil t)
      (beginning-of-line))
  (forward-line 1)
  (recenter 0)
  ;; Send a hook when the last twt is reached
  (when (twtxt--last-separator-p)
    (let ((current-point (point)))
      (goto-char (point-max))
      (run-hooks 'twtxt--last-twt-hook)
      (goto-char current-point))))

(defun twtxt--goto-previous-separator ()
  "Go to the previous separator in the buffer."
  (interactive)
  (let ((separator (twtxt--regexp-separator)))
    (search-backward-regexp separator nil t)
    (when (search-backward-regexp separator nil t)
      (beginning-of-line)))
  (forward-line 1)
  (recenter 0))


(defun twtxt--twt-component (text nick date avatar-url hash thread twts-list &optional look-and-feel)
  "Insert a twt component in the buffer. TEXT is the twt text, NICK is the author's nickname, DATE is the date of the twt, AVATAR-URL is the URL of the author's avatar, HASH is the hash of the twt, THREAD is the hash of the thread, TWTS-LIST is the list of twts. LOOK-AND-FEEL is the look and feel of the twt: nil is a simple item, 'reply is a reply to a twt and 'direct-message is a direct message."
  (let ((prefix (if (eq look-and-feel 'reply) "  |  " "  " )))
    ;; direct message
    (when (eq look-and-feel 'direct-message)
      (twtxt--insert-formatted-text prefix)
      (twtxt--insert-formatted-text "🔒 Direct message from " nil "yellow")
      (twtxt--insert-formatted-text "\n"))
    ;; text
    (twtxt--insert-formatted-text "\n")
    (twtxt--insert-formatted-text prefix)
    (twtxt--insert-formatted-text (twtxt--markdown-to-org-string text))
   ;; images
   (when (twtxt--image-p text)
     (progn
       (twtxt--insert-formatted-text "\n\n")
       (dolist (url (get-images-urls text))
	 (progn
	   (twtxt--put-image-from-cache url (line-number-at-pos) 200)
	   (twtxt--insert-formatted-text "  ")))))
   (twtxt--insert-formatted-text "\n\n")
   ;; avatar
   (twtxt--insert-formatted-text prefix)
   (if avatar-url
       (twtxt--put-image-from-cache avatar-url (line-number-at-pos) 50)
     (twtxt--insert-formatted-text twtxt--anonymous-avatar 200 nil nil))

   ;; nick + date
   (twtxt--insert-formatted-text prefix)
   (twtxt--insert-formatted-text nick nil "yellow")
   (twtxt--insert-formatted-text prefix)
   (twtxt--insert-formatted-text date nil "#FF5733")
   (twtxt--insert-formatted-text prefix)
   (when (or (twtxt--replies-p hash twts-list) thread)
     (widget-create 'push-button
		    :notify (lambda (&rest ignore)
			      (setq twtxt--timeline-thread thread)
			      (twtxt--timeline-layout))
		    " ⎆ Go thread ")
     (twtxt--insert-formatted-text prefix)
     (widget-create 'push-button
		    :notify (lambda (&rest ignore) (twtxt--post-buffer thread))
		    twtxt--text-button-reply-thread))
   (twtxt--insert-formatted-text prefix)
   (widget-create 'push-button
		  :notify (lambda (&rest ignore) (twtxt--post-buffer hash))
		  twtxt--text-button-reply-twt)
   (twtxt--insert-formatted-text prefix)
   (widget-create 'push-button
		  :notify (lambda (&rest ignore) (twtxt---profile-layout (cdr (assoc 'author-id twt))))
		  " Profile ")
   ;; End of twt
   (twtxt--insert-separator)))

(defun twtxt--twt-component-keybindings ()
  "Set the keybindings for the twt component."
  (local-set-key (kbd "n") (lambda () (interactive) (twtxt--goto-next-separator)))
  (local-set-key (kbd "p") (lambda () (interactive) (twtxt--goto-previous-separator)))
  (local-set-key (kbd "t") (lambda () (interactive) (twtxt--goto-thread)))
  (local-set-key (kbd "r") (lambda () (interactive) (twtxt--goto-reply-twt)))
  (local-set-key (kbd "T") (lambda () (interactive) (twtxt--goto-reply-thread))))


(provide 'twtxt-ui)
;;; twtxt-ui.el ends here
