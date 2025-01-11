;; Imports
(require 'widget)
(require 'url)
(require 'twtxt)
(require 'twtxt-feed)
(require 'cl-lib)

(eval-when-compile
  (require 'wid-edit))

;; Variables
(defconst twtxt--timeline-name-buffer "*twtxt - Timeline*")
(defconst twtxt--timeline-margin 20)
(defvar twtxt--timeline-separator
  (make-string
   (- (window-width) (* 2 twtxt--timeline-margin)) ?\u2500))

;; Functions
(defun put-image-from-url (url pos &optional width)
  "Put an image from an URL in the buffer at position."
  (unless url (setq url (url-get-url-at-point)))
  (unless url
    (error "Couldn't find URL."))
  (let ((buffer (url-retrieve-synchronously url)))
    (unwind-protect
        (let ((data (with-current-buffer buffer
                      (goto-char (point-min))
                      (search-forward "\n\n")
                      (buffer-substring (point) (point-max)))))
	  (save-excursion
            (goto-char (point-min))
            (forward-line (1- pos)) ; Go to the beginning of the specified line
            (setq pos (line-beginning-position)))
	  (put-image (create-image data nil t :width width) pos))
      (kill-buffer buffer))))

;; Layout
(defun twtxt--timeline-layout ()
  "Create the main layout for the welcome screen."
  (switch-to-buffer twtxt--timeline-name-buffer)
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (erase-buffer)
  ;; Controls
  (widget-insert "\n")
  (widget-create 'push-button
		 :button-face '(:background "green" :foreground "white")
		 :notify (lambda (&rest ignore)
			   (twtxt--timeline-layout))
		 "New post")
  (widget-create 'push-button
		 :notify (lambda (&rest ignore)
			   (twtxt--timeline-layout))
		 "Refresh")
  (widget-insert "\n\n")
  (widget-insert twtxt--timeline-separator)
  (widget-insert "\n\n")
  ;; twtxts
  (dolist (twts (cl-subseq (twtxt--timeline) 0 5))
    (let* ((profile (twtxt--profile-by-id (cdr (assoc 'author-id twts))))
	   (nick (cdr (assoc 'nick profile)))
	   (avatar-url (cdr (assoc 'avatar profile)))
	   (thead (cdr (assoc 'thread twts)))
	   (date (format-time-string "%Y-%m-%d %H:%M" (encode-time (cdr (assoc 'date twts)))))
	   (text (cdr (assoc 'text twts))))
      ;; text
      (widget-insert text)
      (widget-insert "\n\n")
      ;; avatar
      (when avatar-url (put-image-from-url avatar-url (line-number-at-pos) 50))
      ;; nick + date
      (widget-insert (concat "  " nick " - " date " "))
      (if thead (widget-create 'push-button "Go to thread") (widget-create 'push-button "Reply"))
      ;; Separator
      (widget-insert "\n")
      (widget-insert twtxt--timeline-separator)
      (widget-insert "\n")
      ))

  (use-local-map widget-keymap)
  (widget-setup)
  (display-line-numbers-mode 0)
  (set-window-margins nil twtxt--timeline-margin twtxt--timeline-margin)
  ;; Go to the top of the buffer
  (goto-char (point-min))
  (read-only-mode 1))

;; Init
(twtxt--timeline-layout)
