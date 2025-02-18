;;; twtxt.el --- A twtxt client for Emacs -*- lexical-binding: t -*- -*- coding: utf-8 -*-

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

;; Usage:
;; View your timeline `M-x twtxt-timeline`
;; Post a status update `M-x twtxt-post`

;;; Code:

;; Autoload
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'twtxt-variables)
(require 'twtxt-post)
(require 'twtxt-feed)
(require 'twtxt-timeline)
(require 'cl-lib)

(defgroup twtxt nil
  "A twtxt client for Emacs."
  :group 'twtxt)

(define-minor-mode twtxt-mode
  "Minor mode for enhancing the twtxt experience."
  :lighter " twtxt")

(defun twtxt-open-file ()
  "Open twtxt file."
  (interactive)
  (find-file twtxt-file))


(defun twtxt-timeline ()
  "View your timeline."
  (interactive)
  (twtxt-mode 1)
  (twtxt--fetch-all-feeds-async)
  (add-hook 'twtxt-after-fetch-posts-hook (lambda ()
					    (setq twtxt--twtxts-page 1)
					    (setq twtxt--timeline-thread nil)
					    (twtxt--timeline-layout)) nil t))


(defun twtxt-post ()
  "POST a status update."
  (interactive)
  (twtxt-mode 1)
  (twtxt--post-buffer))

(provide 'twtxt)
;;; twtxt.el ends here
