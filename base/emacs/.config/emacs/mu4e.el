;;; mu4e.el --- Configurations for the mu4e mail client

;;; Commentary:
;; This depends on the external programs
;; - mu (to index mail,
;; - mbsync/isync (to download mail from the server),
;; - pass (for password information),
;; - gnupg (for encryption), and
;; - protonmail-bridge to download protonmail emails.
;;

;;; Code:

;;;;;;;;
;; Fluff
(setq mu4e-compose-signature "Thalia Wright\n<https://www.lagrangian.space>"
      message-default-headers (concat
                               "\nX-Clacks-Overhead: GNU Terry Pratchet\n"))

;;;;;;;;
;; Basic settings
(setq message-send-mail-function 'smtpmail-send-it
      mu4e-get-mail-command "mbsync -c ~/.config/isync/mbsyncrc -a"
      ;; Don't leave a bunch of useless buffers
      message-kill-buffer-on-exit t
      ;; Prevent Maildir error: duplicate UID error messages
      mu4e-change-filenames-when-moving t)

;;;;;;;;
;; Helper functions
(defun revert-if-mu4e-main ()
  "Revert/update buffer if it's mu4e-main."
  (when (string-match-p (buffer-name (current-buffer)) "mu4e-main")
    (revert-buffer)))

(defun my-msg-match (msg arg address)
  "Match message on headers."
  (mu4e-message-contact-field-matches msg arg address))

;;;;;;;;
;; Use password-store for authentication
;; TODO: Figure out how to use pinentry-(tty|emacs) in terminal frames
(setq auth-sources '(password-store))
(auth-source-pass-enable)

;;;;;;;;
;; Message composition settings
;; Write messages with format=flowed (RFC 3676) instead of hard 72 columns.
(setq mu4e-compose-format-flowed t)
(add-hook 'mu4e-compose-mode-hook
          (defun local/mu4e-compose-settings ()
            "Email writing: identify hard newlines and soft-wrap at 72 cols"
            (save-excursion
              (whitespace-newline-mode)
              (visual-fill-column-mode))))

;;;;;;;;
;; Message view settings
(add-hook 'mu4e-view-mode-hook
          (defun local/mu4e-view-settings ()
            "Soft-wrap plain text emails"
            (when (not mu4e~message-body-html)
              (save-excursion
                (setq visual-fill-column-width 80)
                (visual-fill-column-mode)
                (visual-line-mode)))))

(setq mu4e-html2text-command
      (lambda ()
        "Use EWW and SHR to view HTML messages."
        (let ((dom (libxml-parse-html-region (point-min) (point-max))))
          (erase-buffer)
          (shr-insert-document dom)
          (goto-char (point-min)))))

(add-to-list 'mu4e-view-actions
             '("ViewInBrowser" . mu4e-action-view-in-browser) t)


;;;;;;;;
;; Account-specific stuff
(setq mu4e-contexts
      `( ,(make-mu4e-context
           :name "personal"
           :enter-func (lambda ()
                         (mu4e-message "Entering personal context")
                         (revert-if-mu4e-main))
           :leave-func (lambda ()
                         (mu4e-message "Leaving personal context")
                         (revert-if-mu4e-main))
           :match-func (lambda (msg)
                         (when msg
                           (or
                            (my-msg-match msg :to "vesperous@protonmail.com")
                            (my-msg-match msg :from "vesperous@protonmail.com")
                            (my-msg-match msg :cc "vesperous@protonmail.com")
                            (my-msg-match msg :bcc "vesperous@protonmail.com")
                            (my-msg-match msg :to "thalia@lagrangian.space")
                            (my-msg-match msg :from "thalia@lagrangian.space")
                            (my-msg-match msg :cc "thalia@lagrangian.space")
                            (my-msg-match msg :bcc "thalia@lagrangian.space")
                            (string-match-p "^/vesperous/INBOX"
					    (mu4e-message-field msg
                                                                :maildir)))))
           :vars '((user-mail-address      . "thalia@lagrangian.space")
                   (smtpmail-smtp-user     . "vesperous@protonmail.com")
                   (smtpmail-smtp-server   . "localhost")
                   (smtpmail-smtp-service  . 1025)
		   (mu4e-refile-folder     . "/vesperous/Archive")
		   (mu4e-sent-folder       . "/vesperous/Sent")
		   (mu4e-drafts-folder     . "/vesperous/Drafts")
		   (mu4e-trash-folder      . "/vesperous/Trash")
                   (mu4e-maildir-shortcuts
                    .
                    ((:maildir "/vesperous/INBOX" :key ?i)
                     (:maildir "/vesperous/Archive" :key ?a)
                     (:maildir "/vesperous/Folders.consoom" :key ?c)
                     (:maildir "/vesperous/Folders.vesperous" :key ?v)
                     (:maildir "/vesperous/Folders.gnwrighter" :key ?g)))
                   (mu4e-bookmarks
		    .
		    ((:name "Unread messages"
			    :query (concat
                                    "maildir:/vesperous/INBOX "
				    "OR maildir:/vesperous/Folders.consoom "
				    "OR maildir:/vesperous/Folders.vesperous "
				    "OR maildir:/vesperous/Folders.gnwrighter "
				    "AND flag:unread "
				    "AND NOT flag:trashed")
			    :key ?u)
                     (:name "Drafts"
                            :query "maildir:/vesperous/Drafts"
                            :key ?d)
                     (:name "Sent"
                            :query "maildir:/vesperous/Sent"
                            :key ?s)
		     (:name "Trashed"
			    :query "flag:trashed"
			    :key ?t)))))

         ,(make-mu4e-context
           :name "wrightng"
           :enter-func (lambda ()
                         (mu4e-message "Entering wrightng context")
                         (revert-if-mu4e-main))
           :leave-func (lambda ()
                         (mu4e-message "Leaving wrightng context")
                         (revert-if-mu4e-main))
           :match-func (lambda (msg)
                         (when msg
                           (or (my-msg-match msg :to "wrightng@reed.edu")
                               (my-msg-match msg :from "wrightng@reed.edu")
                               (my-msg-match msg :cc "wrightng@reed.edu")
                               (my-msg-match msg :bcc "wrightng@reed.edu"))))

           :vars '((user-mail-address      . "wrightng@reed.edu")
		   (smtpmail-smtp-user     . "wrightng@reed.edu")
                   (smtpmail-smtp-server   . "smtp.gmail.com")
                   (smtpmail-smtp-service  . 587)
		   (mu4e-refile-folder     . "/wrightng/Archive-y2")
		   (mu4e-sent-folder       . "/wrightng/[Gmail].Sent Mail")
		   (mu4e-drafts-folder     . "/wrightng/[Gmail].Drafts")
		   (mu4e-trash-folder      . "/wrightng/[Gmail].Trash")
                   (mu4e-maildir-shortcuts
                    .
                    ((:maildir "/wrightng/INBOX" :key ?i)
                     (:maildir "/wrightng/Archive-y2" :key ?a)
                     (:maildir "/wrightng/News" :key ?n)
                     (:maildir "/wrightng/[Gmail.All Mail" :key ?p)))
                   (mu4e-bookmarks
                    .
                    ((:name  "Unread school messages"
                             :query (concat "maildir:/wrightng/INBOX "
                                            "OR maildir:/wrightng/News "
                                            "AND flag:unread "
                                            "AND NOT flag:trashed")
                             :key ?u)
                     (:name "Drafts"
                            :query "maildir:/wrightng/[Gmail].Drafts"
                            :key ?d)
                     (:name "Sent"
                            :query "maildir:/vesperous/[Gmail].Sent Mail"
                            :key ?s)
		     (:name "Trashed"
			    :query "flag:trashed"
			    :key ?t)))))))

(provide 'mu4e)
;;; mu4e.el end here
