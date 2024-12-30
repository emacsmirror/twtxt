;;; twtxt-feed-test.el

(add-to-list 'load-path "twtxt-feed.el")
(require 'ert)

(defvar twtxt-feed-example "# Learn more about twtxt:
#     https://twtxt.readthedocs.io/en/stable/
#
# nick = foo
# url = https://foo.com
# url = http://bar.com
# url = gemini://baz.com
# avatar = https://foo.com/avatar.jpg
# description =  Full-Stack developer Emacs addicted ðŸ± Cat food opening

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


(provide 'twtxt-feed-test)
