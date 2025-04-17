;;; twtxt-dm.el --- A twtxt client for Emacs -*- lexical-binding: t -*- -*- coding: utf-8 -*-

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

;; Specification: https://twtxt.dev/exts/direct-message.html
(defvar twtxt-dm-private-key-file nil)

(defun twtxt--dm-twt-p (twt)
  "Return t if the twt is a DM."
  )

(defun twtxt--dm-for-me (twt)
  "Return t if the twt is a DM and it is for the user."
  (when (twtxt--dm-twt-p twt) t)
  )

(defun twtxt--dm-send-p ()
  "Return t if the user can send DMs."
  )

(defun twtxt--dm-receive-p (user-id)
  "Return t if the USER-ID can receive DMs."
  )

(defun twtxt--dm-comunicate-p (receive-id)
  "Returns t if the user RECEIVE-ID can send and receive DMs."
  (and (twtxt--dm-send-p)
       (twtxt--dm-receive-p receive-id)))

(defun twtxt--dm-make-shared-key (receive-id)
  "Make a shared key for the user RECEIVE-ID using the private key."
  )

(defun twtxt--dm-make (twt receive-id)
  "Generate a DM from the twt and the user RECEIVE-ID with the shared key using the patter '!<nick url> encrypted_message'."
  )

(defun twtxt--dm-read (twt receive-id)
  "Return the decrypted DM from the twt and the user RECEIVE-ID using the shared key."
  )

(provide 'twtxt-dm)
;;; twtxt-dm.el ends here
