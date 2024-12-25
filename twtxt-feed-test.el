;;; twtxt-feed-test.el

(add-to-list 'load-path "twtxt-feed.el")
(require 'ert)


(ert-deftest test-twtxt--get-profile-from-feed ()
  (let* ((url-feed "https://twtxt.andros.dev/")
	 (nick-test "andros")
	 (urls-test '("https://twtxt.andros.dev" "https://activity.andros.dev" "https://andros.dev" "gemini://andros.dev"))
	 (avatar-test "https://andros.dev/img/avatar.jpg")
	 (profile (twtxt--get-profile-from-feed url-feed)))
    (should (string= nick-test (alist-get 'nick profile)))
    (should (equal urls-test (alist-get 'urls profile)))
    (should (string= avatar-test (alist-get 'avatar profile)))
    (should (not (string-empty-p (alist-get 'description profile))))))

(provide 'twtxt-feed-test)
