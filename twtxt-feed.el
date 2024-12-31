;;; twtxt-feed.el --- A twtxt client for Emacs

;;; Code:
(require 'request)
(require 'async)
(require 'twtxt)

(defvar twtxt--my-profile nil)
(defvar twtxt--feeds '((id . 1)
		       (nick . "Foo")
		       (urls . ("http://example.com/twtxt.txt"))
		       (avatar . "http://example.com/avatar.png")

		       g(tweets . (((id . 1)
				    (date . "2018-01-01T00:00:00Z")
				   (text . "Hello, world!"))))))

(defun twtxt--get-a-single-value (feed key)
  "Extract a single or multiple values from a twtxt feed based on a KEY.
Parameters:
  FEED (string) - The complete twtxt feed as a string.
  KEY (string) - The key to search for.
Returns:
  Either a single value (string) if only one match exists,
  or a list of values (strings) if multiple matches are found.
  If no match exists, returns nil."
  (let* ((lines (split-string feed "\n"))
         (regex (format "^#\\s-*%s\\s-*=?\\s-*\\(.+\\)$" (regexp-quote key))) ;; # key = value
         values)
    ;; Loop through each line and find matches
    (dolist (line lines)
      (when (string-match regex line)
	(let ((value (match-string 1 line)))
	  (setq values (cons value values))))) ;; Extract and clean matches
    (if values
        (if (= (length values) 1)
            (car values) ;; Return single value if there's one
          (reverse values)) ;; Return a list of values if there are multiple
      nil))) ;; Return nil if no match found

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
  (list (cons 'nick (twtxt--get-a-single-value feed "nick"))
	  (cons 'urls (twtxt--get-a-single-value feed "url"))
	  (cons 'avatar (twtxt--get-a-single-value feed "avatar"))
	  (cons 'description (twtxt--get-a-single-value feed "description"))))


(defun twtxt--get-tweets-from-feed (feed)
  "Get the tweets from a feed. Parameters: FEED (text). Return: A list with the tweets from the feed: date and text."
  (let* ((feed-without-comments (replace-regexp-in-string "^#.*\n" "" feed))
	(feed-with-good-pattern (replace-regexp-in-string "" "" feed-without-comments))
	(feed-without-empty-lines (replace-regexp-in-string "^\n" "" feed-with-good-pattern))
	(list-of-lines (split-string feed-with-good-pattern "\n"))
	(tweets nil))
    (dolist (line list-of-lines)
      (let* ((date (parse-time-string (car (split-string line "\t"))))
	     (text (replace-string twtxt--char-newline "\n" (cadr (split-string line "\t")))))
	(setq tweets (cons (list (cons 'date date) (cons 'text text)) tweets))))
    tweets))


;; (defun twtxt--get-tweets-from-all-feeds ()
;;   "Get the tweets from all feeds. Return: A list with the tweets from all feeds."
;;   (async-start
;;    (lambda ()
;;      (message "Getting tweets from all feeds")
;;      (setq twtxt--feeds nil) ;; Clear the feeds
;;      (dolist (user twtxt-following)
;;        (let* ((feed (twtxt--get-feed (cdr (assoc 'urls user))))
;; 	      (profile (twtxt--get-profile-from-feed feed))
;; 	      (tweets (twtxt--get-tweets-from-feed feed))
;; 	      (user '('profile . profile 'tweets . tweets)))
;; 	 (setq twtxt--feeds (cons user twtxt--feeds))
;; 	 (run-hooks 'twtxt-after-fetch-posts-hook))))
;;    (lambda (result)
;;      (message "Done getting tweets from all feeds"))))

(provide 'twtxt-feed)
