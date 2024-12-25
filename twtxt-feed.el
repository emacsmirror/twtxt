;;; twtxt-feed.el --- A twtxt client for Emacs
;; twtxt-following
;; (mapcar 'get-tweets-from-feed twtxt-feeds)

;;; Code:
(require 'request)
(require 'async)

(defvar twtxt--my-profile nil)
(defvar twtxt--feeds '((nick . "Foo")
		       (urls . ("http://example.com/twtxt.txt"))
		       (avatar . "http://example.com/avatar.png")
		       (tweets . (((date . "2018-01-01T00:00:00Z")
				   (text . "Hello, world!"))))))

(defun twtxt--get-a-single-value (feed key)
  (let ((regex (format "#\\s-*%s\\s-*=?\\s-*\\([a-zA-Z0-9_-]+\\)" (regexp-quote key))))
    (if (string-match regex feed)
        (match-string 1 feed)
      nil)))


(defun twtxt--get-profile-from-feed (url)
  "Get the profile of the user at URL."
  (let ((profile nil))
    (request url
      :sync t
      :timeout 5
      :success (cl-function
		(lambda (&key data &allow-other-keys)
		  (setq profile '((nick . (twtxt--get-a-single-value data "nick"))
				  (urls . ("http://example.com/twtxt.txt"))
				  (avatar . "http://example.com/avatar.png")
				  (description . "A description of the user.")))))
      :error (cl-function
	      (lambda (&key error-thrown &allow-other-keys)
		(message "Error getting profile from feed: %S" error-thrown)
		nil))) profile))


(defun twtxt--get-tweets-from-feed (url))


(defun twtxt--get-tweets-from-all-feeds ()
  (async-start
   (lambda ()
     (message "Getting tweets from all feeds")
     (setq twtxt--feeds nil) ;; Clear the feeds
     (dolist (user twtxt-following)
       (setq twtxt--feeds (cons (twtxt--get-tweets-from-feed (cdr (assoc 'url user))) twtxt--feeds))))
   (lambda (result)
     (message "Done getting tweets from all feeds")
     )))

(provide 'twtxt-feed)
