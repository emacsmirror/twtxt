;;; twtxt-feed-test.el

(require 'twtxt-feed)
(require 'ert)

(defconst twtxt-feed-example-file "twtxt-debug.txt")
(defconst twtxt-feed-example (with-temp-buffer
			       (insert-file-contents twtxt-feed-example-file)
			       (buffer-string)))

(ert-deftest test-twtxt--get-value ()
  (should (string= "foo" (twtxt--get-value twtxt-feed-example "nick")))
  (let ((urls (twtxt--get-value twtxt-feed-example "url"))
	(follows (twtxt--get-value twtxt-feed-example "follow"))
	(links (twtxt--get-value twtxt-feed-example "link")))
    (should (string= "https://foo.com" (car urls)))
    (should (string= "http://blog.bar.com" (cadr urls)))
    (should (string= "gemini://baz.com" (caddr urls)))
    (should (string= "andros https://twtxt.andros.dev" (car follows)))
    (should (string= "prologic https://twtxt.net/user/prologic/twtxt.txt" (cadr follows)))
    (should (string= "Website https://my-website.com" (car links)))
    (should (string= "My blog http://blog.mi-website.com" (cadr links))))
  (should (string= "https://foo.com/avatar.jpg" (twtxt--get-value twtxt-feed-example "avatar")))
  (should (string= " Full-Stack developer Emacs addicted ðŸ± Cat food opening" (twtxt--get-value twtxt-feed-example "description"))))

(ert-deftest test-twtxt--split-link ()
  (let* ((text-1 "My blog http://example.com/blog")
         (link-1 (twtxt--split-link text-1))
         (text-2 "Website gemini://my.example.com/")
         (link-2 (twtxt--split-link text-2))
         (text-3 "My blog")
         (link-3 (twtxt--split-link text-3))
         (text-4 "https://example.com/blog")
         (link-4 (twtxt--split-link text-4))
         (text-5 "Website")
         (link-5 (twtxt--split-link text-5))
         (text-6 "")
         (link-6 (twtxt--split-link text-6))
	 (text-7 nil)
	 (link-7 (twtxt--split-link text-7)))
    ;; Tests for text-1
    (should (string= "My blog" (cdr (assoc 'name link-1)))) ; Get only the value of 'name))
    (should (string= "http://example.com/blog" (cdr (assoc 'url link-1)))) ; Get only the value of 'url
    ;; Tests for text-2
    (should (string= "Website" (cdr (assoc 'name link-2)))) ; Get only the value of 'name
    (should (string= "gemini://my.example.com/" (cdr (assoc 'url link-2)))) ; Get only the value of 'url
    ;; Tests for invalid inputs
    (should (null link-3)) ; No URL
    (should (null link-4)) ; No name
    (should (null link-5)) ; No URL, single word
    (should (null link-6))
    (should (null link-7)))) ; Empty string

(ert-deftest test-twtxt--get-thread-id ()
  (should (string= "ohmmloa" (twtxt--get-thread-id "2024-09-29T13:40:00Z   (#ohmmloa) Is anyone alive? 🤔"))))

(ert-deftest test-twtxt--clean-thread-id ()
  (should (string= "2024-09-29T13:40:00Z   Is anyone alive?" (twtxt--clean-thread-id "2024-09-29T13:40:00Z   (#ohmmloa) Is anyone alive?")))
  (should (string= "2024-09-29T13:40:00Z   Is anyone alive?" (twtxt--clean-thread-id "2024-09-29T13:40:00Z   (#ohmmloa)Is anyone alive?")))
  (should (string= "2024-09-29T13:40:00Z   Is anyone alive?" (twtxt--clean-thread-id "2024-09-29T13:40:00Z   Is anyone alive?"))))

(ert-deftest test-twtxt--get-feed ()
  (let* ((url "https://twtxt.andros.dev/")
	 (feed (twtxt--get-feed url)))
    (should (string= "andros" (twtxt--get-value feed "nick")))))

(ert-deftest test-twtxt--twtxt--get-my-profile ()
  (should (string= "foo" (cdr (assoc 'nick (twtxt--get-my-profile twtxt-feed-example-file))))))

(ert-deftest test-twtxt--get-profile-from-feed ()
  (let ((profile (twtxt--get-profile-from-feed twtxt-feed-example)))
    (should (string= "foo" (cdr (assoc 'nick profile))))
    (let ((urls (cdr (assoc 'url profile))))
      (should (string= "https://foo.com" (car urls)))
      (should (string= "http://blog.bar.com" (cadr urls)))
      (should (string= "gemini://baz.com" (caddr urls))))
    (let ((follows (cdr (assoc 'follow profile))))
      (should (string= "andros" (cdr (assoc 'name (nth 0 follows)))))
      (should (string= "https://twtxt.andros.dev" (cdr (assoc 'url (nth 0 follows)))))
      (should (string= "prologic" (cdr (assoc 'name (nth 1 follows)))))
      (should (string= "https://twtxt.net/user/prologic/twtxt.txt" (cdr (assoc 'url (nth 1 follows))))))
    (let ((links (cdr (assoc 'link profile))))
      (should (string= "Website" (cdr (assoc 'name (nth 0 links)))))
      (should (string= "https://my-website.com" (cdr (assoc 'url (nth 0 links)))))
      (should (string= "My blog" (cdr (assoc 'name (nth 1 links)))))
      (should (string= "http://blog.mi-website.com" (cdr (assoc 'url (nth 1 links))))))
    (should (string= "https://foo.com/avatar.jpg" (cdr (assoc 'avatar profile))))
    (should (string= " Full-Stack developer Emacs addicted ðŸ± Cat food opening"
                     (cdr (assoc 'description profile))))))

(ert-deftest test-twtxt--get-twts-from-feed ()
  (let ((test-posts '("Thanks @<prologic https://twtxt.net/user/prologic/twtxt.txt> !"
		      "Hello everyone! ðŸ˜"
		      "Thanks @<bender https://twtxt.net/user/bender/twtxt.txt> for the feedback."
		      "I like it"
		      "Hi Twtxt"))
	(posts (twtxt--get-twts-from-feed twtxt-feed-example)))
    (should (equal (length test-posts) (length posts)))
    (dotimes (i (length test-posts))
      (should (string= (cdr (assoc 'text (nth i posts))) (nth i test-posts))))))

(ert-deftest test-twtxt--get-twts-from-all-feeds ()
  (let ((profiles (twtxt--get-twts-from-all-feeds))))
  (should (equal 2 (length profiles)))
  (should (string= "andros" (cdr (assoc 'nick (nth 0 profiles)))))
  (should (string= "prologic" (cdr (assoc 'nick (nth 1 profiles))))))


(provide 'twtxt-feed-test)
