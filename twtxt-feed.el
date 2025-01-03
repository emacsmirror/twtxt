;;; twtxt-feed.el --- A twtxt client for Emacs

;; SPDX-License-Identifier: GPL-3.0

;; Author: DEADBLACKCLOVER <deadblackclover@protonmail.com>
;; Colaborator: Andros - https://andros.dev
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
(require 'request)
(require 'async)
(require 'twtxt)

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
;;                (((id . 1) ;; The id of the tweet, unique for each post
;; 		    (date . date) ;; The date of the tweet
;; 		    (text . "Hello, world!"))))) ;; The text of the tweet

(defun twtxt--get-a-single-value (feed key)
  "Extract a single or multiple values from a twtxt feed based on a KEY.
Parameters:
  FEED (string) - The complete twtxt feed as a string.
  KEY (string) - The key to search for.
Returns:
  Either a single value (string) if only one match exists,
  or a list of values (strings) if multiple matches are found.
  If no match exists, returns nil."
  (let* ((lines (split-string feed "\n"))
         (regex (format "^#\\s-*%s\\s-*=?\\s-*\\(.+\\)$" (regexp-quote key))) ;; # key = value
         values)
    ;; Loop through each line and find matches
    (dolist (line lines)
      (when (string-match regex line)
	(let ((value (match-string 1 line)))
	  (setq values (cons value values))))) ;; Extract and clean matches
    (if values
        (if (= (length values) 1)
            (car values) ;; Return single value if there's one
          (reverse values)) ;; Return a list of values if there are multiple
      nil))) ;; Return nil if no match found

(defun twtxt--split-link (raw-text)
  "Split RAW-TEXT a link into a name and URL. For example: 'My blog http://example.com/blog' -> '(name . 'My blog') (url . 'http://example.com/blog')."
  (let ((split-text (split-string raw-text " ")))
    (if (length> split-text 1)
	(list (cons 'name (butlast split-text)) (cons 'url (last split-text)))
      nil)))

(defun twtxt--get-thread-id (text)
  "Get the thread id from TEXT. Hash extension: https://twtxt.dev/exts/twthashextension.html. For example: '2024-09-29T13:40:00Z   (#ohmmloa) Is anyone alive? 🤔' is 'ohmmloa'."
  (let ((thread-id nil)
	   (regex (format "^\(#\(\w+\)\)")))
    (when (string-match regex text)
      (setq thread-id (match-string 1 text)))
    thread-id))

(defun twtxt--clean-thread-id (text)
  "Clean the thread id from TEXT. For example: '2024-09-29T13:40:00Z   (#ohmmloa) Is anyone alive?' is 'ohmmloa'. Hash extension: https://twtxt.dev/exts/twthashextension.html."
  (let ((cleaned-text text)
	(regex (format "^\(#\(\w+\)\)")))
    (when (string-match regex text)
      (setq cleaned-text (replace-regexp-in-string regex "" text)))
    cleaned-text))

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

(defun twtxt--get-profile-from-feed (feed)
  "Get the profile of the user from the feed. Parameters: FEED (text). Return: A list with the profile of the user."
  (list
   (cons 'id (gensym))
   (cons 'nick (twtxt--get-a-single-value feed "nick"))
   (cons 'url (twtxt--get-a-single-value feed "url"))
   (cons 'link (mapcar twtxt--split-link (twtxt--get-a-single-value feed "link")))
   (cons 'follow (twtxt--get-a-single-value feed "follow"))
   (cons 'avatar (twtxt--get-a-single-value feed "avatar"))
   (cons 'description (twtxt--get-a-single-value feed "description"))))


(defun twtxt--get-twts-from-feed (feed)
  "Get the twts from a feed. Parameters: FEED (text). Return: A list with the twts from the feed: date and text."
  (let* ((feed-without-comments (replace-regexp-in-string "^#.*\n" "" feed))
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
			   (cons 'date date)
			   (cons 'text (twtxt--clean-thread-id text))) twts)))))))
    twts))

;; (defun twtxt--get-twts-from-all-feeds ()
;;   "Get the twts from all feeds. Return: A list with the twts from all feeds."
;;   (setq twtxt--feeds nil) ;; Clear the feeds
;;   (dolist (user twtxt-following)
;;     (let* ((feed (twtxt--get-feed (car (cdr user))))
;; 	   (profile (twtxt--get-profile-from-feed feed))
;; 	   (twts (twtxt--get-twts-from-feed feed))
;; 	   (user (append profile (list (cons 'twts twts)))))
;;       (setq twtxt--feeds (cons user twtxt--feeds))
;;       (message "Got twts from %s" (cdr (assoc 'nick profile)))
;;       ))
;;   ;; (run-hooks 'twtxt-after-fetch-posts-hook)
;;   twtxt--feeds
;;   )

(provide 'twtxt-feed)
;;; twtxt-feed.el ends here
