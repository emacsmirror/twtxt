;;; twtxt-feed.el --- A twtxt client for Emacs -*- lexical-binding: t -*- -*- coding: utf-8 -*-

;; SPDX-License-Identifier: GPL-3.0

;; Author: Andros <https://andros.dev>
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
(require 'twtxt-variables)
(require 'request)
(require 'async)
(require 'seq)

;; Hooks
(defvar twtxt-after-fetch-posts-hook nil)

;; Variables
(defvar twtxt--my-profile nil)
(defvar twtxt--feeds nil)

;; Example of structure with Metadata Extension: https://twtxt.dev/exts/metadata.html
;; '((id . 1) ;; The id of the user, unique for each user
;; (nick . "Foo") ;; The nick of the user
;; (url . ("http://example.com/twtxt.txt")) ;; urls of the user
;; (avatar . "http://example.com/avatar.png") ;; The avatar of the user
;; (description . "A description of the user") ;; The description of the user)
;; (follow . (((name . "Bar") (url . ("http://example.com/twtxt.txt"))))) ;; The users that the user follows
;; (link . (((name . "Blog") (url . "http://example.com/blog")) (name . "GitHub") (url . "https://github.com/username"))) ;; The links of the user
;; (twts . ;; The twts of the user
;;                (((id . 1) ;; The id of the twt, unique for each post
;; 		    (date . date) ;; The date of the twt
;; 		    (thread . "ohmmloa") ;; The thread id of the twt.
;;                  (hash . "ohmmloa") ;; The hash of the twt.)))
;; 		    (text . "Hello, world!"))))) ;; The text of the twt


(defun calculate-twtxt-hash (feed-url timestamp message)
  "Calculate the twtxt hash using FEED-URL, TIMESTAMP, and MESSAGE.
Returns the resulting 8-character hash as a string. This Emacs Lisp
implementation replicates the shell command:
  printf '%s\\n%s\\n%s' URL TIMESTAMP MESSAGE | b2sum -l 256 | awk '{ print $1 }' | xxd -r -p | base32 | tr -d '=' | tr 'A-Z' 'a-z' | tail -c 8."
  (let* ((input (format "%s\n%s\n%s" feed-url timestamp message))
         (hash ""))
    ;; Use a temporary buffer to pass input to the shell pipeline.
    (with-temp-buffer
      (insert input)
      (call-process-region (point-min) (point-max) "sh" t t nil "-c"
                           "b2sum -l 256 | awk '{ print $1 }' | xxd -r -p | base32 | tr -d '=' | tr 'A-Z' 'a-z' | tail -c 8")
      (setq hash (buffer-string)))
    ;; Trim and return the resulting hash.
    (string-trim hash)))


(defun twtxt--get-value (feed key)
  "Extract a single or multiple values from a twtxt feed based on a KEY.
Parameters:
  FEED (string) - The complete twtxt feed as a string.
  KEY (string) - The key to search for.
Returns:
  Either a single value (string) if only one match exists,
  or a list of values (strings) if multiple matches are found.
  If no match exists, returns nil."
  (when (and (stringp feed) (stringp key))
    (let* ((lines (split-string feed "\n"))
	   (regex (concat "^#\s*" (regexp-quote key) "\s*=\s*\\(.+\\)$")) ;; # key = value
	   values)
      ;; Loop through each line and find matches
      (dolist (line lines)
	(when (string-match regex line)
	  (let ((value (string-trim (match-string 1 line))))
	    (setq values (cons value values))))) ;; Extract and clean matches
      (if values
	  (if (= (length values) 1)
	      (car values) ;; Return single value if there's one
	    (reverse values)) ;; Return a list of values if there are multiple
	nil)))) ;; Return nil if no match found


(defun twtxt--split-link (raw-text)
  "Split RAW-TEXT into a link with a name and a URL.
Return nil if it doesn't contain a valid name and URL. For example: My blog https://example.com -> ((name . \"My blog\") (url . \"https://example.com\")). If the text doesn't contain a valid URL, return nil. Extension: https://twtxt.dev/exts/metadata.html"
  (when raw-text (let ((split-text (split-string raw-text " "))
		       ;; Source: https://stackoverflow.com/questions/3809401/what-is-a-good-regular-expression-to-match-a-url
		       (url-regex "\\([-a-zA-Z0-9+.]++://[a-zA-Z0-9.-]+\\.[a-zA-Z]+\\(?:/[a-zA-Z0-9._~:/?#@!$&'()*+,;=%-]*\\)?\\)"))
		   (if (and (> (length split-text) 1)
			    (string-match-p url-regex (car (last split-text))))
		       (list (cons 'name (mapconcat #'identity (butlast split-text) " "))
			     (cons 'url (car (last split-text))))
		     nil))))

(defun twtxt--get-thread-id (text)
  "Get the thread id from TEXT. Hash extension according to: https://twtxt.dev/exts/twthashextension.html. For example: '2024-09-29T13:40:00Z   (#ohmmloa) Is anyone alive? 🤔' returns 'ohmmloa'."
  (when (string-match "#\\([a-zA-Z0-9]\\{7\\}\\)" text)
    (match-string 1 text)))

(defun twtxt--clean-thread-id (text)
  "Clean the thread id from TEXT. For example: '2024-09-29T13:40:00Z   (#ohmmloa) Is anyone alive?' return '2024-09-29T13:40:00Z   Is anyone alive?'."
  (replace-regexp-in-string "(#\\w+) *" "" text))

(defun twtxt--get-feed (url)
  "Get the feed, text, from URL."
  (let ((feed nil))
    (request url
      :sync t
      :timeout 5
      :success (cl-function
		(lambda (&key data &allow-other-keys)
		  (setq feed data)))
      :error (cl-function
	      (lambda (&key error-thrown &allow-other-keys))))
    feed))

(defun twtxt--get-my-profile (&optional custom-twtxt-file)
  "Get the profile of the user from the feed (path: twtxt-file). Parameters: CUSTOM-TWTXT-FILE (string) Optional. Return: A list with the profile of the user."
  (let ((feed nil))
    (with-temp-buffer
      (insert-file-contents (or custom-twtxt-file twtxt-file))
      (setq feed (buffer-string)))
    (let* ((profile (twtxt--get-profile-from-feed feed))
	   (twts (twtxt--get-twts-from-feed feed)))
      (append profile (list (cons 'twts twts))))))

(defun twtxt--get-profile-from-feed (feed)
  "Get the profile of the user from the feed. Parameters: FEED (text). Return: A list with the profile of the user."
  (let* ((feed-without-twts (replace-regexp-in-string "^[^#].*\n?" "" feed))
	 (links (twtxt--get-value feed-without-twts "link"))
	 (links-list (if (listp links) links (list links))) ; Transform into a list if it's a single value
	 (follows (twtxt--get-value feed-without-twts "follow"))
	 (follows-list (if (listp follows) follows (list follows)))) ; Transform into a list if it's a single value
    (list
     (cons 'id (gensym))
     (cons 'nick (twtxt--get-value feed-without-twts "nick"))
     (cons 'url (twtxt--get-value feed-without-twts "url"))
     (cons 'link (mapcar #'twtxt--split-link (delq nil links-list)))
     (cons 'follow (mapcar #'twtxt--split-link (delq nil follows-list)))
     (cons 'avatar (twtxt--get-value feed-without-twts "avatar"))
     (cons 'description (twtxt--get-value feed-without-twts "description")))))

(defun twtxt--get-twts-from-feed (feed)
  "Get the twts from a feed. Parameters: FEED (text). Return: A list with the twts from the feed: date and text."
  (let* ((url (cdr (assoc 'url (twtxt--get-profile-from-feed feed))))
	 (feed-without-comments (replace-regexp-in-string "^#.*\n" "" feed))
	 (feed-without-empty-lines (replace-regexp-in-string "^\n" "" feed-without-comments))
	 (list-of-lines (split-string feed-without-empty-lines "\n"))
	 (twts nil))
    (dolist (line list-of-lines)
      (let ((columns (split-string line "\t")))
	(when (length= columns 2)
	  (let ((date (parse-time-string (car columns)))
		(text (replace-regexp-in-string twtxt--char-newline "\n" (cadr columns))))
	    (when (and date text)
	      (setq twts
		    (cons (list
			   (cons 'id (gensym))
			   (cons 'thread (twtxt--get-thread-id text))
			   (cons 'hash (calculate-twtxt-hash url (car columns) (cadr columns)))
			   (cons 'date date)
			   (cons 'text (twtxt--clean-thread-id text))) twts)))))))
    twts))

(defun twtxt--get-twts-from-all-feeds ()
  "Get the twts from all feeds. Return: A list with the twts from all feeds."
  (setq twtxt--my-profile (twtxt--get-my-profile)) ;; Refresh my twtxts
  (let ((twtxt--feeds nil))
    (dolist (follow (cdr (assoc 'follow twtxt--my-profile)))
      (let* ((feed (twtxt--get-feed (cdr (assoc 'url follow))))
	     (profile (twtxt--get-profile-from-feed feed))
	     (twts (twtxt--get-twts-from-feed feed))
	     (user (append profile (list (cons 'twts twts)))))
	(setq twtxt--feeds (cons user twtxt--feeds))
	(message "Got twts from %s" (cdr (assoc 'nick profile)))))
    ;; Add my profile to the list of feeds
    (setq twtxt--feeds (cons twtxt--my-profile twtxt--feeds))
    (message "Finished!")
    (run-hooks 'twtxt-after-fetch-posts-hook)
    twtxt--feeds))


(defun twtxt--normalize-date (date)
  "Normalize DATE by replacing `nil` values in `parse-time-string` with defaults.
DATE is a list like (SEC MIN HOUR DAY MON YEAR DOW DST TZ)."
  (mapcar (lambda (el) (or el 0)) date))

(defun twtxt--timeline ()
  "Get the timeline of the user. RETURN: A list with the twts from all feeds sorted by date with the structure: (id author-id date text)."
  (let* ((timeline (mapcan (lambda (feed)
                             (let ((author-id (cdr (assoc 'id feed))) ;; Get author ID once
                                   (twts (cdr (assoc 'twts feed)))) ;; Get twts
                               (mapcar (lambda (twt)
                                         (list
                                          (cons 'id (cdr (assoc 'id twt)))
                                          (cons 'author-id author-id)
                                          (cons 'date (cdr (assoc 'date twt)))
                                          (cons 'text (cdr (assoc 'text twt)))))
                                       twts)))
                           twtxt--feeds))
	 (timeline-sorted
	  (sort timeline
		(lambda (a b)
		  (time-less-p
		   (apply #'encode-time (twtxt--normalize-date (cdr (assoc 'date b))))
		   (apply #'encode-time (twtxt--normalize-date (cdr (assoc 'date a)))))))))
    timeline))

(defun twtxt--profile-by-id (id)
  "Get the profile of the user by ID. Parameters: ID (string). Return: A list with the profile of the user."
  (car (seq-filter (lambda (feed) (string= id (cdr (assoc 'id feed)))) twtxt--feeds)))


;; Initialize
(setq twtxt--my-profile (twtxt--get-my-profile))

(provide 'twtxt-feed)
;;; twtxt-feed.el ends here
