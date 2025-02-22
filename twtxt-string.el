;;; twtxt-string.el --- A twtxt client for Emacs -*- lexical-binding: t -*- -*- coding: utf-8 -*-

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
;;; Code:

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
  "Replace mention format in TEXT to Markdown format."
  (let ((output text))
    (while (string-match "@<\\([^ ]+\\) \\([^ ]+\\)>" output)
      (setq output
            (replace-match (format "[%s](%s)" (match-string 1 output) (match-string 2 output))
                           t t output)))
    output))

(defun twtxt--markdown-to-org-string (md-text)
  "Convert the given MD-TEXT (Markdown format) to Org-mode using Pandoc.
Returns the converted text as a string."
  (if (executable-find "pandoc")
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
