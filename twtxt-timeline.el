;; Imports
(require 'widget)
(require 'url)
(require 'relative-date)
(require 'twtxt)
(require 'twtxt-feed)

(eval-when-compile
  (require 'wid-edit))

;; Variables
(defconst twtxt--timeline-name-buffer "*twtxt - Timeline*")
(defconst twtxt--separator "\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500")

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

  (dolist (twts (twtxt--timeline))
    (let* ((profile (twtxt--profile-by-id (cdr (assoc 'author-id twts))))
	   (nick (cdr (assoc 'nick profile)))
	   (avatar-url (cdr (assoc 'avatar profile)))
	   (date (encode-time (cdr (assoc 'date twts))))
	   (text (cdr (assoc 'text twts))))
      (widget-insert "\n")
      ;; avatar
      (when avatar-url (put-image-from-url avatar-url (line-number-at-pos) 50))
      (widget-insert "\n")
      ;; text
      (widget-insert (concat " " text))
      ;; nick + date
      (widget-insert (concat " " nick " - " date))
      (widget-insert "\n")
      (widget-insert twtxt--separator)))

  (use-local-map widget-keymap)
  (widget-setup)
  (display-line-numbers-mode 0))

;; Init
(twtxt--timeline-layout)
