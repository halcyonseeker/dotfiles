;;;;;;;
;; Mu4e
;;;;;;;
(require 'mu4e)
(require 'smtpmail)
(setq mu4e-compose-signature
      "\nThalia Wright\nhttps://www.lagrangian.space\n")
(setq message-default-headers (concat "\nX-Clacks-Overhead: GNU Terry Pratchet\n"))
(setq message-send-mail-function 'smtpmail-send-it)
(setq mu4e-get-mail-command "mbsync -c ~/.config/isync/mbsyncrc -a")
;; Don't leave a bunch of useless buffers
(setq message-kill-buffer-on-exit t)
;; Prevent Maildir error: duplicate UID error messages
(setq mu4e-change-filenames-when-moving t) 
;; Render html Messages and Open in Browser
(setq mu4e-html2text-command 'my-html2text)
(defun my-html2text ()
  (let ((dom (libxml-parse-html-region (point-min) (point-max))))
    (erase-buffer)
    (shr-insert-document dom)
    (goto-char (point-min))))
(add-to-list 'mu4e-view-actions '("ViewInBrowser" . mu4e-action-view-in-browser) t)
;; Helper function
(defun revert-if-mu4e-main ()
  "Revert/update buffer if it's mu4e-main."
  (when (string-match-p (buffer-name (current-buffer)) "mu4e-main")
    (revert-buffer)))
(defun my-msg-match (msg arg address)
  "Match message on headers."
  (mu4e-message-contact-field-matches msg arg address))

(setq auth-sources '(password-store))
(auth-source-pass-enable)

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
                           (or (my-msg-match msg :to "vesperous@protonmail.com")
                               (my-msg-match msg :from "vesperous@protonmail.com")
                               (my-msg-match msg :cc "vesperous@protonmail.com")
                               (my-msg-match msg :bcc "vesperous@protonmail.com")
                               (my-msg-match msg :to "thalia@lagrangian.space")
                               (my-msg-match msg :from "thalia@lagrangian.space")
                               (my-msg-match msg :cc "thalia@lagrangian.space")
                               (my-msg-match msg :bcc "thalia@lagrangian.space")
                               (string-match-p "^/vesperous/INBOX"
					       (mu4e-message-field msg :maildir)))))
           :vars '((user-mail-address      . "thalia@lagrangian.space")
                   (smtpmail-smtp-user     . "vesperous@protonmail.com")
                   (mu4e-compose-signature . "Thalia Wright")
                   (smtpmail-smtp-server   . "localhost")
                   (smtpmail-smtp-service  . 1025)
		   (mu4e-refile-folder     . "/vesperous/Archive")
		   (mu4e-sent-folder       . "/vesperous/Sent")
		   (mu4e-drafts-folder     . "/vesperous/Drafts")
		   (mu4e-trash-folder      . "/vesperous/Trash")
                   (mu4e-maildir-shortcuts . ((:maildir "/vesperous/INBOX" :key ?i)
						      (:maildir "/vesperous/Archive" :key ?a)
						      (:maildir "/vesperous/Folders.consoom" :key ?c)
						      (:maildir "/vesperous/Folders.vesperous" :key ?v)
						      (:maildir "/vesperous/Folders.gnwrighter" :key ?g)))
                   (mu4e-bookmarks
		    .
		    ((:name "Unread messages"
			    :query (concat "maildir:/vesperous/INBOX "
					   "OR maildir:/vesperous/Folders.consoom "
					   "OR maildir:/vesperous/Folders.vesperous "
					   "OR maildir:/vesperous/Folders.gnwrighter "
					   "AND flag:unread "
					   "AND NOT flag:trashed")
			    :key ?u)
		     (:name "Deleted"
			    :query "flag:trashed"
			    :key ?d)))))

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
                   (mu4e-compose-signature . "Thalia Wright")
		   (mu4e-refile-folder     . "/wrightng/Archive-y2")
		   (mu4e-sent-folder       . "/wrightng/[Gmail].Sent Mail")
		   (mu4e-drafts-folder     . "/wrightng/[Gmail].Drafts")
		   (mu4e-trash-folder      . "/wrightng/[Gmail].Trash")
                   (mu4e-maildir-shortcuts . ((:maildir "/wrightng/INBOX" :key ?i)
						 (:maildir "/wrightng/Archive-y2" :key ?a)
						 (:maildir "/wrightng/News" :key ?n)
						 (:maildir "/wrightng/[Gmail.All Mail" :key ?p)))
                    ( mu4e-bookmarks . ((:name  "Unread school messages"
						:query (concat "maildir:/wrightng/INBOX "
							       "OR maildir:/wrightng/News "
							       "AND flag:unread "
							       "AND NOT flag:trashed")
						:key ?u)))))))

