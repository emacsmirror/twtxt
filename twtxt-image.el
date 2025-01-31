;;; twtxt-image.el --- A twtxt client for Emacs -*- lexical-binding: t -*- -*- coding: utf-8 -*-

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

(require 'twtxt-variables)

(defconst twtxt--regex-image "http[^ ]*\\(png\\|jpg\\|jpeg\\|gif\\)")

(defun twtxt--cache-image-p (url)
  "Check if an image from URL is already cached. It is cached if it is in `twtxt-cache-image-directory' as a base64 encoded string of the URL."
  (file-exists-p (expand-file-name (base64-encode-string url :no-line-break) twtxt-cache-image-directory)))

(defun twtxt--cache-image (url)
  "Download an image from URL to cache (file system)."
  (unless (file-exists-p twtxt-cache-image-directory)
    (make-directory twtxt-cache-image-directory t))
  (request
    url
    :type "GET"
    :sync t
    :parser 'buffer-string
    :success (cl-function
	      (lambda (&key data &allow-other-keys)
		(let ((filename-image (base64-encode-string url :no-line-break)))
		  (with-temp-file (expand-file-name filename-image twtxt-cache-image-directory)
		    (set-buffer-file-coding-system 'binary)
		    (insert data)))))))

(defun twtxt--image-p (text)
  "Check if TXT contains an image."
  (string-match-p twtxt--regex-image text))

(defun get-images-urls (text)
  "Get all image URLs from TEXT."
  (let ((urls '())
        (pos 0))
    (while (string-match twtxt--regex-image text pos)
      (push (match-string 0 text) urls)
      (setq pos (match-end 0))) ;; Avanza el cursor para evitar procesar el mismo texto
    urls))

(defun twtxt--put-image-from-cache (url pos &optional width)
  "Put an image from cache at URL at POS."
  (unless (twtxt--cache-image-p url)
    (twtxt--cache-image url))
  (let ((image (base64-encode-string url)))
    (insert-image (create-image (expand-file-name image twtxt-cache-image-directory) nil nil :width width) pos)))

(defun twtxt--clean-images ()
  "Remove all images from the buffer."
  (remove-images (point-min) (point-max))

)

(provide 'twtxt-image)
;;; twtxt-variables.el ends here
