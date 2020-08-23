;;; twtxt.el - client twtxt
;;; Copyright (c) 2020, DEADBLACKCLOVER. This file is
;;; licensed under the GNU General Public License version 3 or later. See
;;; the LICENSE file.
;;;
;;; twtxt is a decentralised, minimalist microblogging service for hackers.

(require 'cl-lib)
(require 'request)

(defvar twtxt-file "~/twtxt")
(defvar twtxt-following nil)

(defvar twtxt-timeline-list nil)
(defvar twtxt-username "")

(defun twtxt-get-datetime () 
  "Getting date and time according to RFC 3339 standard"
  (concat (format-time-string "%Y-%m-%dT%T") 
	  ((lambda (x) 
	     (concat (substring x 0 3) ":" (substring x 3 5))) 
	   (format-time-string "%z"))))

(defun twtxt-parse-text (text) 
  "Convert text to list post"
  (split-string text "\n"))

(defun twtxt-replace-tab (str) 
  "Replacing tabs with line breaks"
  (replace-regexp-in-string "\t" "\n" str))

(defun twtxt-sort-post (list) 
  "Sorting posts"
  (sort list #'string>))

(defun twtxt-append-username (text) 
  "Append username in posts"
  (mapcar (lambda (item) 
	    (concat twtxt-username "\t" item)) 
	  (twtxt-parse-text text)))

(defun twtxt-push-text (text) 
  "Concatenate the resulting list with the current list" 
  (setq twtxt-timeline-list (append twtxt-timeline-list (twtxt-append-username text))))

(defun twtxt-fetch (url) 
  "Get text"
  (progn (request url 
	   :parser 'buffer-string 
	   :success (cl-function (lambda 
				   (&key 
				    data
				    &allow-other-keys) 
				   (twtxt-push-text data))))))

(defun twtxt-fetch-list () 
  "Getting a list of texts"
  (mapcar (lambda (item) 
	    (setq twtxt-username (concat "[[" (car (cdr item)) "][" (car item) "]]")) 
	    (twtxt-fetch (car (cdr item)))) twtxt-following))

(defun twtxt-timeline-buffer (data) 
  "Create buffer"
  (switch-to-buffer (get-buffer-create "*twtxt-timeline*")) 
  (mapcar (lambda (item) 
	    (insert (twtxt-replace-tab item)) 
	    (insert "\n\n")) data) 
  (org-mode) 
  (beginning-of-buffer))

(defun twtxt-timeline () 
  "View your timeline" 
  (interactive) 
  (twtxt-fetch-list) 
  (twtxt-timeline-buffer (twtxt-sort-post twtxt-timeline-list)) 
  (setq twtxt-timeline-list nil))

(defun twtxt-post (post) 
  "Post a status update" 
  (interactive "sPost:") 
  (append-to-file (concat (twtxt-get-datetime) "\t" post "\n") nil twtxt-file))

(provide 'twtxt)
