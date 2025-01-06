;;; twtxt.el --- A twtxt client for Emacs

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
(require 'cl-lib)
(add-to-list 'load-path ".")

(defgroup twtxt nil
  "A twtxt client for Emacs."
  :group 'twtxt)

(defcustom twtxt-file "~/twtxt"
  "Path to twtxt file."
  :type 'file
  :group 'twtxt)

(defcustom twtxt-following nil
  "Following list."
  :type '(repeat (list (string :tag "Name")
		       (string :tag "URL")))
  :group 'twtxt)

;; Multiline Extension: https://twtxt.dev/exts/multiline.html
(defconst twtxt--char-newline (char-to-string #x2028))

(defconst twtxt-line "\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500")

(defvar twtxt-timeline-list nil
  "Timeline list.")

(defvar twtxt-username ""
  "Temporary storage of username.")

(defvar twtxt-post-tweet-hook nil)

(defun twtxt-get-datetime ()
  "Getting date and time according to RFC 3339 standard."
  (concat (format-time-string "%Y-%m-%dT%T")

	  ((lambda (x)
	     (concat (substring x 0 3) ":" (substring x 3 5)))
	   (format-time-string "%z"))))

(defun twtxt-parse-text (text)
  "Convert TEXT to list post."
  (split-string text "\n"))

(defun twtxt-replace-tab (str)
  "Replacing tabs with line breaks in STR."
  (replace-regexp-in-string "\t" "\n" str))

(defun twtxt-sort-posts (posts)
  "Sort the list of twtxt POSTS by RFC 3339 date, newest first."
  (let ((is-valid-post (lambda (line)
                         (string-match-p rfc3339-regex
                                         (car (split-string line "\t")))))
        (extract-date (lambda (line)
                        (date-to-time (car (split-string line "\t"))))))
    (let ((valid-posts (cl-remove-if-not is-valid-post posts)))
      (if valid-posts
          (sort valid-posts
                (lambda (a b)
                  (time-less-p (funcall extract-date b)
                               (funcall extract-date a))))
        (progn
          (message "No valid posts found in the provided list.")
          nil)))))

(defun twtxt-append-username (text)
  "Append username in TEXT."
  (mapcar (lambda (item)
	    (concat twtxt-username "\t" item))
	  (twtxt-parse-text text)))
(defun twtxt-replace-newlines (str)
  "Replace newline characters in STR with the Unicode character \\u2028. Source: https://twtxt.dev/exts/multiline.html"
  (replace-regexp-in-string "\n" (char-to-string #x2028) str))

(defun twtxt-push-text (text)
  "Concatenate the resulting TEXT with the current list."
  (setq twtxt-timeline-list (append twtxt-timeline-list (twtxt-append-username text))))

(defun twtxt-fetch (url)
  "Getting text by URL."
  (twtxt--get-tweets-from-all-feeds))

(defun twtxt-fetch-list ()
  "Getting a list of texts."
  (mapc (lambda (item)
	  (setq twtxt-username (concat "[[" (car (cdr item)) "][" (car item) "]]"))
	  (twtxt-fetch (car (cdr item)))) twtxt-following))

(defun twtxt-timeline--previous ()
  "Move to the previous post."
  (interactive)
  ;; Search for the previous twtxt-line
  (search-backward (concat twtxt-line "\n\n")))

(defun twtxt-timeline--next ()
  "Move to the next post."
  (interactive)
  ;; Search for the next twtxt-line
  (search-forward (concat twtxt-line "\n\n")))


(defun twtxt-timeline-buffer (data)
  "Create buffer and DATA recording."
  (switch-to-buffer (get-buffer-create "*twtxt-timeline*"))
  (mapc (lambda (item)
	  ;; Line
	  (insert twtxt-line)
	  (insert "\n\n")
          (insert (twtxt-replace-tab item))
          (insert "\n\n")) data)
  (org-mode)
  (use-local-map (let ((map (make-sparse-keymap)))
                   (set-keymap-parent map text-mode-map)
		   (define-key map (kbd "p") 'twtxt-timeline--previous)
                   (define-key map (kbd "n") 'twtxt-timeline--next)
		   map))
  (goto-char (point-min)))


(defun twtxt-post--mention ()
  "Insert a mention in the format '@<nick url>' into the post buffer. Source: https://twtxt.dev/exts/twt-subject.html"
  (interactive)
  (when (not twtxt-following)
    (message "No users in the following list."))
  (let* ((user-options (mapcar (lambda (item)
                                 (concat (car item) " " (cadr item)))
                               twtxt-following))
         (selected-user (completing-read "Mention: " user-options nil t)))
    (when selected-user
      (insert "@<" selected-user "> "))))


(defun twtxt-timeline ()
  "View your timeline."
  (interactive)
  ;; Fetch texts
  (twtxt-fetch-list)
  ;; Sort posts
  (let ((sorted-list (twtxt-sort-posts twtxt-timeline-list)))
    (if sorted-list
        (twtxt-timeline-buffer sorted-list)
      (message "No valid posts found in your timeline.")))
  ;; Reset temporal data
  (setq twtxt-timeline-list nil))


(defun twtxt-post-buffer ()
  "Open a temporary buffer for writing and posting a new status update."
  (let ((buffer-name "*Twtxt New Post*"))
    (switch-to-buffer (get-buffer-create buffer-name))
    (erase-buffer)
    (insert "Write your post below:\n\n")
    (insert (propertize "C-c C-c" 'face 'bold))
    (insert " to post\n")
    (insert (propertize "C-c C-m" 'face 'bold))
    (insert " to mention\n")
    (insert (propertize "C-c C-k" 'face 'bold))
    (insert " to cancel.\n\n")

    (use-local-map (let ((map (make-sparse-keymap)))
                     (set-keymap-parent map text-mode-map)
                     (define-key map (kbd "C-c C-c") 'twtxt-post--confirm)
		     (define-key map (kbd "C-c C-m") 'twtxt-post--mention)
		     (define-key map (kbd "C-c C-k") 'twtxt-post--cancel)
                     map))
    (goto-char (point-max))
    (message "Write your post and press C-c C-c to send or C-c C-k to cancel.")))

(defun twtxt-post--confirm ()
  "Post the content of the buffer as a new status update."
  (interactive)
  (let ((post (buffer-substring-no-properties
               (save-excursion
                 (goto-char (point-min))
                 (forward-paragraph)
                 (forward-line 1)
                 (point))
               (point-max))))
    (when (and post (not (string-blank-p post)))
      (append-to-file
       (concat (twtxt-get-datetime) "\t" (twtxt-replace-newlines post) "\n")
       nil
       twtxt-file)
      (message "Posted: %s" post)
      (run-hooks 'twtxt-post-tweet-hook))
    (kill-buffer)))

(defun twtxt-post--cancel ()
  "Cancel and close the new post buffer without posting."
  (interactive)
  (when (yes-or-no-p "Are you sure you want to cancel this post?")
    (kill-buffer)
    (message "Post canceled.")))

(defun twtxt-post ()
  "POST a status update."
  (interactive)
  (twtxt-post-buffer))

(provide 'twtxt)
;;; twtxt.el ends here
