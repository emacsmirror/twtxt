;;; twtxt-dm-test.el -*- lexical-binding: t -*-

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
(require 'twtxt-dm)
(require 'ert)

(ert-deftest test-twtxt--dm-twt-p ()
  (should (twtxt--dm-twt-p "!<foo https://example.com/> test"))
  (should (twtxt--dm-twt-p "!<foo http://example.com/> test"))
  (should (twtxt--dm-twt-p "!<foo https://example.com> test"))
  (should (twtxt--dm-twt-p "!<foo https://example.com/twtxt.txt> test"))
  (should (not (twtxt--dm-twt-p "!<foo https://example.com/twtxt.txt>")))
  (should (not (twtxt--dm-twt-p "!<https://example.com/twtxt.txt> test")))
  (should (not (twtxt--dm-twt-p "!<foo> test")))
  (should (not (twtxt--dm-twt-p "test !<foo https://example.com/> test"))))

(ert-deftest test-twtxt--dm-get-url ()
  )

(ert-deftest test-twtxt--dm-is-for-me-p ()
  )

(ert-deftest test-twtxt--dm-send-p ()
  )

(ert-deftest test-twtxt--dm-receive-p ()
  )

(ert-deftest test-twtxt--dm-comunicate-p ()
  )

(ert-deftest test-twtxt--dm-make-shared-key ()
  )

(ert-deftest test-twtxt--dm-read ()
  )

(provide 'twtxt-dm-test)
