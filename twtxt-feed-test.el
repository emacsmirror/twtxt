;;; twtxt-feed-test.el

(add-to-list 'load-path "twtxt-feed.el")
(require 'ert)

(defvar twtxt-feed-example "# Learn more about twtxt:
#     https://twtxt.readthedocs.io/en/stable/
#
# nick = foo
#url = https://foo.com
# url =http://bar.com
#   url= gemini://baz.com
#avatar=https://foo.com/avatar.jpg
#      DEScripTION= Full-Stack developer Emacs addicted ðŸ± Cat food opening

2024-12-18T14:18:26+01:00	Hi Twtxt
2024-12-18T14:54:56+01:00	I like it
2024-12-23T08:33:19+01:00	Thanks @<bender https://twtxt.net/user/bender/twtxt.txt> for the feedback.

2024-12-23T08:49:02+01:00	(#hsyv65q) Hello everyone! ðŸ˜
2024-12-23T10:00:59+01:00	Thanks @<prologic https://twtxt.net/user/prologic/twtxt.txt> !

")

(ert-deftest test-twtxt--get-a-single-value ()
  (should (string= "foo" (twtxt--get-a-single-value twtxt-feed-example "nick")))
  (let ((urls (twtxt--get-a-single-value twtxt-feed-example "url")))
    (should (string= "https://foo.com" (car urls)))
    (should (string= "http://bar.com" (cadr urls)))
    (should (string= "gemini://baz.com" (caddr urls))))
  (should (string= "https://foo.com/avatar.jpg" (twtxt--get-a-single-value twtxt-feed-example "avatar")))
  (should (string= " Full-Stack developer Emacs addicted ðŸ± Cat food opening" (twtxt--get-a-single-value twtxt-feed-example "description"))))

(ert-deftest test-twtxt--split-link ()
  (let* ((text-1 "My blog http://example.com/blog")
         (link-1 (twtxt--split-link text-1))
         (text-2 "Website gemini://example.com/")
         (link-2 (twtxt--split-link text-2))
         (text-3 "My blog")
         (link-3 (twtxt--split-link text-3))
         (text-4 "http://example.com/blog")
         (link-4 (twtxt--split-link text-4))
         (text-5 "Website")
         (link-5 (twtxt--split-link text-5))
         (text-6 "")
         (link-6 (twtxt--split-link text-6)))
    ;; Tests for text-1
    (should (string= "My blog" (cdr (assoc 'name link-1)))) ; Get only the value of 'name))
    (should (string= "http://example.com/blog" (cdr (assoc 'url link-1)))) ; Get only the value of 'url
    ;; Tests for text-2
    (should (string= "Website" (cdr (assoc 'name link-2)))) ; Get only the value of 'name
    (should (string= "gemini://example.com/" (cdr (assoc 'url link-2)))) ; Get only the value of 'url
    ;; Tests for invalid inputs
    (should (null link-3)) ; No URL
    (should (null link-4)) ; No name
    (should (null link-5)) ; No URL, single word
    (should (null link-6)))) ; Empty string

(ert-deftest test-twtxt--get-feed ()
  (let* ((url "https://twtxt.andros.dev/")
	 (feed (twtxt--get-feed url)))
    (should (string= "andros" (twtxt--get-a-single-value feed "nick")))))

(ert-deftest test-twtxt--get-profile-from-feed ()
  (let ((profile (twtxt--get-profile-from-feed twtxt-feed-example)))
    (should (string= "foo" (cdr (assoc 'nick profile))))
    (let ((urls (cdr (assoc 'urls profile))))
      (should (string= "https://foo.com" (car urls)))
      (should (string= "http://bar.com" (cadr urls)))
      (should (string= "gemini://baz.com" (caddr urls))))
    (should (string= "https://foo.com/avatar.jpg" (cdr (assoc 'avatar profile))))
    (should (string= " Full-Stack developer Emacs addicted ðŸ± Cat food opening"
                     (cdr (assoc 'description profile))))))

(ert-deftest test-twtxt--get-tweets-from-feed ()
  (let ((test-posts '("Thanks @<prologic https://twtxt.net/user/prologic/twtxt.txt> !"
		     "(#hsyv65q) Hello everyone! ðŸ˜"
		     "Thanks @<bender https://twtxt.net/user/bender/twtxt.txt> for the feedback."
		     "I like it"
		      "Hi Twtxt"))
	(posts (twtxt--get-tweets-from-feed twtxt-feed-example)))
    (should (equal (length test-posts) (length posts)))
    (dotimes (i (length test-posts))
      (should (string= (cdr (assoc 'text (nth i posts))) (nth i test-posts))))))


(provide 'twtxt-feed-test)
