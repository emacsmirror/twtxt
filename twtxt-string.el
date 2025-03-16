;;; twtxt-string.el --- A twtxt client for Emacs -*- lexical-binding: t -*- -*- coding: utf-8 -*-

;; SPDX-License-Identifier: GPL-3.0

;; Author: Andros <https://andros.dev>
;; Version: 1.0
;; URL: https://codeberg.org/deadblackclover/twtxt-el
;; Package-Requires: ((emacs "25.1") (request "0.2.0") (visual-fill-column "2.4"))

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

(defconst twtxt--pandoc-p (executable-find "pandoc"))

(defun twtxt--insert-formatted-text (text &optional size font-color background-color)
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

(defun twtxt--replace-markdown-links (text)
       "Replace @<name url> mentions in TEXT with Markdown links.
Example: @<my alias http://example.com> → [my alias](http://example.com)."
       (let ((regex "@<\\([^>]+\\)>"))
	 (if (string-match regex text)
	   (let* ((source (match-string 0 text))
		  (items (split-string (match-string 1 text) " "))
		  (alias (mapconcat #'identity (butlast items) " "))
		  (url (car (last items)))) ;; `car` para extraer el string de la lista
	     (twtxt--replace-markdown-links (replace-regexp-in-string (regexp-quote source)
								      (format "[%s](%s)" alias url)
								      text)))
	   text)))

(defun twtxt--add-label-to-markdown-images (text)
  "Add a incremental label to each Markdown image in TEXT. For example 'Boo ![](https://example.com/image.png) foo ![](https://example.com/moo.jpg)' becomes 'Boo ![image 1](https://example.com/image.png) foo ![image 2](https://example.com/moo.jpg)'."
  (let ((regex "!\\[\\([^]]+\\)\\]\\(([^)]+)\\)"))
    (if (string-match regex text)
	(let* ((source (match-string 0 text))
	       (items (split-string (match-string 1 text) " "))
	       (label (format "image %d" (1+ (length (split-string text regex)))))
	       (url (car (last items))))))))

(defun twtxt--markdown-to-org-string (md-text)
  "Convert the given MD-TEXT (Markdown format) to Org-mode using Pandoc.
Returns the converted text as a string."
  (if twtxt--pandoc-p
      (with-temp-buffer
	(insert (twtxt--replace-markdown-links md-text))
	(shell-command-on-region (point-min) (point-max) "pandoc -f markdown -t org" t t)
	(buffer-string)) md-text))

(defun convert-region-to-org-mode (start end)
  "Enable Org-mode from the cursor to the end of the buffer."
  (interactive "r")
  (let* ((text (buffer-substring-no-properties start end))
         (org-text (twtxt--markdown-to-org-string text)))
    (goto-char start)
    (delete-region start end)
    (insert org-text)
    (org-mode)
    (goto-char (point-max))
    (org-mode)))

(provide 'twtxt-string)
;;; twtxt-string.el ends here
