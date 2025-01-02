;;; twtxt-feed.el --- A twtxt client for Emacs

;;; Code:
(require 'request)
(require 'async)
(require 'twtxt)

(defvar twtxt--feeds nil)
;; Example of structure with Metadata Extension: https://twtxt.dev/exts/metadata.html
;; '((id . 1) ;; The id of the user, unique for each user
;; (nick . "Foo") ;; The nick of the user
;; (url . ("http://example.com/twtxt.txt")) ;; urls of the user
;; (avatar . "http://example.com/avatar.png") ;; The avatar of the user
;; (description . "A description of the user") ;; The description of the user)
;; (follow . (((name . "Bar") (url . ("http://example.com/twtxt.txt"))))) ;; The users that the user follows
;; (link . (((name . "Blog") (url . "http://example.com/blog")) (name . "GitHub") (url . "https://github.com/username"))) ;; The links of the user
;; (tweets . ;; The tweets of the user
;;                (((id . 1) ;; The id of the tweet, unique for each post
;; 		    (date . date) ;; The date of the tweet
;; 		    (text . "Hello, world!"))))) ;; The text of the tweet

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

(defun twtxt--split-link (raw-text)
  "Split a link into a name and URL. For example: 'My blog http://example.com/blog' -> '(name . 'My blog') (url . 'http://example.com/blog')."
  (let ((split-text (split-string raw-text " ")))
    (if (length> split-text 1)
	(list (cons 'name (butlast split-text)) (cons 'url (last split-text)))
      nil)))

(defun twtxt--get-thread-id (text)
  "Get the thread id from the text. Hash extension: https://twtxt.dev/exts/twthashextension.html. For example: '2024-09-29T13:40:00Z   (#ohmmloa) Is anyone alive? 🤔' is 'ohmmloa'"
  (let ((thread-id nil)
	   (regex (format "^\(#\(\w+\)\)")))
    (when (string-match regex text)
      (setq thread-id (match-string 1 text)))
    thread-id))

(defun twtxt--clean-thread-id (text)
  "Clean the thread id from the text. Hash extension: https://twtxt.dev/exts/twthashextension.html. For example: '2024-09-29T13:40:00Z   (#ohmmloa) Is anyone alive? 🤔 is 'ohmmloa'��'"
  (let ((cleaned-text text)
	(regex (format "^\(#\(\w+\)\)")))
    (when (string-match regex text)
      (setq cleaned-text (replace-regexp-in-string regex "" text)))
    cleaned-text))

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
  (list
   (cons 'id (gensym))
   (cons 'nick (twtxt--get-a-single-value feed "nick"))
   (cons 'url (twtxt--get-a-single-value feed "url"))
   (cons 'link (mapcar twtxt--split-link (twtxt--get-a-single-value feed "link")))
   (cons 'follow (twtxt--get-a-single-value feed "follow"))
   (cons 'avatar (twtxt--get-a-single-value feed "avatar"))
   (cons 'description (twtxt--get-a-single-value feed "description"))))


(defun twtxt--get-tweets-from-feed (feed)
  "Get the tweets from a feed. Parameters: FEED (text). Return: A list with the tweets from the feed: date and text."
  (let* ((feed-without-comments (replace-regexp-in-string "^#.*\n" "" feed))
	(feed-without-empty-lines (replace-regexp-in-string "^\n" "" feed-without-comments))
	(list-of-lines (split-string feed-without-empty-lines "\n"))
	(tweets nil))
    (dolist (line list-of-lines)
      (let ((columns (split-string line "\t")))
	(when (length= columns 2)
	  (let ((date (parse-time-string (car columns)))
		(text (replace-regexp-in-string twtxt--char-newline "\n" (cadr columns))))
	    (when (and date text)
	      (setq tweets
		    (cons (list
			   (cons 'id (gensym))
			   (cons 'thread (twtxt--get-thread-id text))
			   (cons 'date date)
			   (cons 'text (twtxt--clean-thread-id text))) tweets)))))))
    tweets))

;; (defun twtxt--get-tweets-from-all-feeds ()
;;   "Get the tweets from all feeds. Return: A list with the tweets from all feeds."
;;   (setq twtxt--feeds nil) ;; Clear the feeds
;;   (dolist (user twtxt-following)
;;     (let* ((feed (twtxt--get-feed (car (cdr user))))
;; 	   (profile (twtxt--get-profile-from-feed feed))
;; 	   (tweets (twtxt--get-tweets-from-feed feed))
;; 	   (user (append profile (list (cons 'tweets tweets)))))
;;       (setq twtxt--feeds (cons user twtxt--feeds))
;;       (message "Got tweets from %s" (cdr (assoc 'nick profile)))
;;       ))
;;   ;; (run-hooks 'twtxt-after-fetch-posts-hook)
;;   twtxt--feeds
;;   )

(provide 'twtxt-feed)
