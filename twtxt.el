;;; twtxt.el - client twtxt
;;; Copyright (c) 2020, DEADBLACKCLOVER. This file is
;;; licensed under the GNU General Public License version 3 or later. See
;;; the LICENSE file.
;;;
;;; twtxt is a decentralised, minimalist microblogging service for hackers.

(require 'cl-lib)
(require 'request)

(defvar twtxt-timeline-list nil)

(defvar twtxt-following nil)

(defun twtxt-parse-text (text) 
  "Convert text to list post"
  (split-string text "\n"))

(defun twtxt-push-text (text) 
  "Concatenate the resulting list with the current list" 
  (setq twtxt-timeline-list (append twtxt-timeline-list (twtxt-parse-text text))))

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
  (mapcar 'twtxt-fetch twtxt-following))

(defun twtxt-timeline-buffer (data) 
  "Create buffer"
  (switch-to-buffer (get-buffer-create "*twtxt-timeline*")) 
  (mapcar (lambda (item) 
	    (insert item) 
	    (insert "\n\n")) data) 
  (org-mode) 
  (beginning-of-buffer))

(defun twtxt-timeline () 
  "View your timeline" 
  (interactive) 
  (twtxt-fetch-list) 
  (twtxt-timeline-buffer twtxt-timeline-list))

(provide 'twtxt)
