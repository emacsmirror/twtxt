;;; twtxt-feed-test.el -*- lexical-binding: t -*-

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


(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'twtxt-feed)
(require 'ert)

(defconst twtxt-feed-example-file "twtxt-debug.txt")
(defconst twtxt-feed-example (with-temp-buffer
			       (insert-file-contents twtxt-feed-example-file)
			       (buffer-string)))

(ert-deftest test-twtxt--calculate-hash ()
  (should (string= (twtxt--calculate-hash "https://example.com/twtxt.txt" "2024-09-29T13:30:00Z" "Hello World!") "ohmmloa")))

(ert-deftest test-twtxt--get-value ()
  (should (string= "foo" (twtxt--get-value twtxt-feed-example "nick")))
  (let ((urls (twtxt--get-value twtxt-feed-example "url"))
	(follows (twtxt--get-value twtxt-feed-example "follow"))
	(links (twtxt--get-value twtxt-feed-example "link")))
    (should (string= "https://foo.com" (car urls)))
    (should (string= "http://blog.bar.com" (cadr urls)))
    (should (string= "gemini://baz.com" (caddr urls)))
    (should (string= "andros https://twtxt.andros.dev" (car follows)))
    (should (string= "prologic https://twtxt.net/user/prologic/twtxt.txt" (cadr follows)))
    (should (string= "Website https://my-website.com" (car links)))
    (should (string= "My blog http://blog.mi-website.com" (cadr links))))
  (should (string= "https://foo.com/avatar.jpg" (twtxt--get-value twtxt-feed-example "avatar")))
  (should (string= " Full-Stack developer Emacs addicted ðŸ± Cat food opening" (twtxt--get-value twtxt-feed-example "description"))))

(ert-deftest test-twtxt--split-link ()
  (let* ((text-1 "My blog http://example.com/blog")
         (link-1 (twtxt--split-link text-1))
         (text-2 "Website gemini://my.example.com/")
         (link-2 (twtxt--split-link text-2))
         (text-3 "My blog")
         (link-3 (twtxt--split-link text-3))
         (text-4 "https://example.com/blog")
         (link-4 (twtxt--split-link text-4))
         (text-5 "Website")
         (link-5 (twtxt--split-link text-5))
         (text-6 "")
         (link-6 (twtxt--split-link text-6))
	 (text-7 nil)
	 (link-7 (twtxt--split-link text-7)))
    ;; Tests for text-1
    (should (string= "My blog" (cdr (assoc 'name link-1)))) ; Get only the value of 'name))
    (should (string= "http://example.com/blog" (cdr (assoc 'url link-1)))) ; Get only the value of 'url
    ;; Tests for text-2
    (should (string= "Website" (cdr (assoc 'name link-2)))) ; Get only the value of 'name
    (should (string= "gemini://my.example.com/" (cdr (assoc 'url link-2)))) ; Get only the value of 'url
    ;; Tests for invalid inputs
    (should (null link-3)) ; No URL
    (should (null link-4)) ; No name
    (should (null link-5)) ; No URL, single word
    (should (null link-6))
    (should (null link-7)))) ; Empty string

(ert-deftest test-twtxt--get-thread-id ()
  (should (string= "ohmmloa" (twtxt--get-thread-id "2024-09-29T13:40:00Z   (#ohmmloa) Is anyone alive? 🤔")))
  (should (not (twtxt--get-thread-id "2024-09-29T13:40:00Z   Hello world #twtxt #emacs")))
  (should (string= "asdfgha" (twtxt--get-thread-id "2024-09-29T13:40:00Z   (#asdfgha) Hello world #twtxt #emacs"))))

(ert-deftest test-twtxt--clean-thread-id ()
  (should (string= "2024-09-29T13:40:00Z   Is anyone alive?" (twtxt--clean-thread-id "2024-09-29T13:40:00Z   (#ohmmloa) Is anyone alive?")))
  (should (string= "2024-09-29T13:40:00Z   Is anyone alive?" (twtxt--clean-thread-id "2024-09-29T13:40:00Z   (#ohmmloa)Is anyone alive?")))
  (should (string= "2024-09-29T13:40:00Z   Is anyone alive?" (twtxt--clean-thread-id "2024-09-29T13:40:00Z   Is anyone alive?"))))

(ert-deftest test-twtxt--get-feed ()
  (let* ((url "https://twtxt.andros.dev/")
	 (feed (twtxt--get-feed url)))
    (should (string= "andros" (twtxt--get-value feed "nick")))))

(ert-deftest test-twtxt--twtxt--get-my-profile ()
  (should (string= "foo" (cdr (assoc 'nick (twtxt--get-my-profile twtxt-feed-example-file))))))

(ert-deftest test-twtxt--get-profile-from-feed ()
  (let ((profile (twtxt--get-profile-from-feed twtxt-feed-example)))
    (should (string= "foo" (cdr (assoc 'nick profile))))
    (let ((urls (cdr (assoc 'url profile))))
      (should (string= "https://foo.com" (car urls)))
      (should (string= "http://blog.bar.com" (cadr urls)))
      (should (string= "gemini://baz.com" (caddr urls))))
    (let ((follows (cdr (assoc 'follow profile))))
      (should (string= "andros" (cdr (assoc 'name (nth 0 follows)))))
      (should (string= "https://twtxt.andros.dev" (cdr (assoc 'url (nth 0 follows)))))
      (should (string= "prologic" (cdr (assoc 'name (nth 1 follows)))))
      (should (string= "https://twtxt.net/user/prologic/twtxt.txt" (cdr (assoc 'url (nth 1 follows))))))
    (let ((links (cdr (assoc 'link profile))))
      (should (string= "Website" (cdr (assoc 'name (nth 0 links)))))
      (should (string= "https://my-website.com" (cdr (assoc 'url (nth 0 links)))))
      (should (string= "My blog" (cdr (assoc 'name (nth 1 links)))))
      (should (string= "http://blog.mi-website.com" (cdr (assoc 'url (nth 1 links))))))
    (should (string= "https://foo.com/avatar.jpg" (cdr (assoc 'avatar profile))))
    (should (string= " Full-Stack developer Emacs addicted ðŸ± Cat food opening"
                     (cdr (assoc 'description profile))))))

(ert-deftest test-twtxt--get-twts-from-feed ()
  (let ((test-posts '("Thanks @<prologic https://twtxt.net/user/prologic/twtxt.txt> !"
		      "Hello everyone! ðŸ˜"
		      "Thanks @<bender https://twtxt.net/user/bender/twtxt.txt> for the feedback."
		      "I like it"
		      "Hi Twtxt"))
	(posts (twtxt--get-twts-from-feed twtxt-feed-example)))
    (should (equal (length test-posts) (length posts)))
    (dotimes (i (length test-posts))
      (should (string= (cdr (assoc 'text (nth i posts))) (nth i test-posts))))))

(provide 'twtxt-feed-test)
