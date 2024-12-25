(add-to-list 'load-path "twtxt-feed.el")
(require 'ert)


(ert-deftest test-twtxt--get-profile-from-feed ()
  (let* ((url-feed "https://twtxt.andros.dev/")
	 (nick-test "andros")
	 (urls-test '("https://twtxt.andros.dev" "https://activity.andros.dev" "https://andros.dev" "gemini://andros.dev"))
	 (avatar-test "https://andros.dev/img/avatar.jpg")
	 (profile (twtxt--get-profile-from-feed url-feed)))
    (message profile)
    (should (string= nick-test (profile 'nick)))
    (should (equal urls-test (profile 'urls)))
    (should (string= avatar-test (profile 'avatar)))
    (should (not (string-empty-p (profile 'description))))))

(provide 'twtxt-feed-test)
