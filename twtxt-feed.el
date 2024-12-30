;;; twtxt-feed.el --- A twtxt client for Emacs

;;; Code:
(require 'request)
(require 'async)
(require 'twtxt)

(defvar twtxt--my-profile nil)
(defvar twtxt--feeds '((nick . "Foo")
		       (urls . ("http://example.com/twtxt.txt"))
		       (avatar . "http://example.com/avatar.png")
		       (tweets . (((date . "2018-01-01T00:00:00Z")
				   (text . "Hello, world!"))))))

(defun twtxt--get-a-single-value (feed key)
  "Get a single value or multiple values from a twtxt feed based on a KEY.
Parameters:
  FEED (text) - Twtxt feed content.
  KEY (string) - The key to search for.
Return:
  A single value (string) or a list (if multiple values are found)."
  (let* ((regex (format "#\\s-*%s\\s-*=?\\s-*\\(.+\\)" (regexp-quote key)))
         (pos 0)
         values)
    (while (string-match regex feed pos)
      (push (string-trim (match-string 1 feed)) values)
      (setq pos (match-end 0)))
    (if values
        (if (= (length values) 1)
            (car values) ;; Return a single value if there is only one
          (reverse values)) ;; Return a list if there are multiple values
      nil)))

(defun twtxt--get-feed (url)
  "Get the feed, text, from URL."
  (let ((feed nil))
    (request url
      :sync t
      :timeout 5
      :success (cl-function
		(lambda (&key data &allow-other-keys)
		  (setq feed data)))
      :error (cl-function
	      (lambda (&key error-thrown &allow-other-keys))))
    feed))

(defun twtxt--get-profile-from-feed (feed)
  "Get the profile of the user from the feed. Parameters: FEED (text). Return: A list with the profile of the user."
  '((nick . (twtxt--get-a-single-value feed "nick"))
    (urls . (twtxt--get-a-single-value feed "urls"))
    (avatar . (twtxt--get-a-single-value feed "avatar"))
    (description . (twtxt--get-a-single-value feed "description"))))


(defun twtxt--get-tweets-from-feed (feed)
  "Get the tweets from a feed. Parameters: FEED (text). Return: A list with the tweets from the feed: date and text."
  (let ((feed-without-comments (replace-regexp-in-string "^#.*\n" "" feed))
	(feed-without-empty-lines (replace-regexp-in-string "^\n" "" feed))
	(list-of-lines (split-string feed-without-empty-lines "\n"))
	(tweets nil))
    (dolist (line list-of-lines)
      (let* ((date (parse-time-string (car (split-string line "\t"))))
	     (text (replace-string twtxt--char-newline "\n" (cadr (split-string line "\t")))))
	(setq tweets (cons (list (cons 'date date) (cons 'text text)) tweets))))
    tweets))


(defun twtxt--get-tweets-from-all-feeds ()
  "Get the tweets from all feeds. Return: A list with the tweets from all feeds."
  (async-start
   (lambda ()
     (message "Getting tweets from all feeds")
     (setq twtxt--feeds nil) ;; Clear the feeds
     (dolist (user twtxt-following)
       (let* ((feed (twtxt--get-feed (cdr (assoc 'urls user))))
	      (profile (twtxt--get-profile-from-feed feed))
	      (tweets (twtxt--get-tweets-from-feed feed))
	      (user '('profile . profile 'tweets . tweets)))
	 (setq twtxt--feeds (cons user twtxt--feeds))
	 (run-hooks 'twtxt-after-fetch-posts-hook))))
   (lambda (result)
     (message "Done getting tweets from all feeds"))))

(provide 'twtxt-feed)
