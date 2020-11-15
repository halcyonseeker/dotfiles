;;; init.el --- Thalia Wright's startup file for GNU Emacs
;;; Commentary:
;;; Code:
;;;;;;;;;;;;;;;;
;; Personal Info
;;;;;;;;;;;;;;;;
(setq mail-user-agent 'mu4e-user-agent
      mu4e-attachment-dir "~/temporary"
      user-full-name "Thalia Wright"
      user-mail-address "vesperous@protonmail.com"
      org-directory "~/org")

;;;;;;;;;;;;;;;;;;;
;; Aesthetic Tweaks
;;;;;;;;;;;;;;;;;;;
;; Fix emacs under X
(when (window-system)
  (tool-bar-mode 0)
  (menu-bar-mode 0)
  (scroll-bar-mode 0))

;; Fix emacs in a terminal
(when (not window-system)
  (menu-bar-mode 0)
  (xterm-mouse-mode))

;; Improve tab behavior
(setq-default indent-tabs-mode nil)
(setq c-default-style "k&r"
      c-basic-offset 4)

;; Make scroll behavior less jarring
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq scroll-step 1)

;; Add nice things
(display-battery-mode 1)
(display-time-mode 1)
(column-number-mode 1)
(line-number-mode 1)
(save-place-mode 1)
;(global-visual-line-mode 1)

;; Suppress annoying things
(setq inhibit-startup-message t)
(setq vc-follow-symlinks t)
(defalias 'yes-or-no-p 'y-or-n-p) 

;; Use melpa repositories
(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; Put all backup files in a single place
(setq backup-by-copying t
      backup-directory-alist '((".*" . "~/.config/emacs/backup")))

;;;;;;;;;;;;;;;;;;;;;
;; Packages and Modes
;;;;;;;;;;;;;;;;;;;;;
;; Mu4e Mail Client
(load "~/.config/emacs/mu4e.el")
;; use mu4e as a default mail client
;; https://etienne.depar.is/emacs.d/mu4e.html
(defun ed/parse-mailto-string (mailto-string)
  (let ((data (split-string mailto-string "[ :?&=]"))
        to is-subject subject)
    (setq to (pop data))
    (when (string= to "mailto")
      (setq to (pop data)))
    (dolist (item data)
      (cond ((string= "subject" item)
             (setq is-subject t))
            (is-subject
             (setq is-subject nil)
             (setq subject item))))
    (list to subject)))

(defun ed/quick-mu4e-pong-handler (data)
  "Handle 'pong' responses from the mu server."
  (setq mu4e~server-props (plist-get data :props)) ;; save info from the server
  (let ((doccount (plist-get mu4e~server-props :doccount)))
    (mu4e~check-requirements)))

(defun ed/quick-mu4e-start ()
  "Quickly start mu4e, avoiding as trouble as possible."
  ;; Load a context as soon as possible to avoid error messages about
  ;; missing folders
  (mu4e~context-autoswitch nil mu4e-context-policy)
  (setq mu4e-pong-func #'(lambda (info) (ed/quick-mu4e-pong-handler info)))
  (mu4e~proc-ping))

(defun ed/compose-new-mail (mailto-string)
  "Compose a new mail with metadata extracted from MAILTO-STRING."
  (ed/quick-mu4e-start)
  (let* ((mailto (ed/parse-mailto-string mailto-string))
         (to (car mailto))
         (subject (cadr mailto)))
    (mu4e~compose-mail to subject)))

;; Evil Mode
(require 'evil)
(evil-mode 1)
(setq evil-want-C-i-jump nil) ; Fix TAB with org-mode in tty
(add-to-list 'evil-emacs-state-modes ; Use emacs-state in various modes
	     'eww-mode)
(evil-set-toggle-key "C-M-z") ; Don't break suspend-frame bindings
;(evil-define-key 'normal global-map " " 'evil-scroll-page-down)
;(evil-define-key 'normal global-map (kbd "DEL") 'evil-scroll-page-up)
;(evil-define-key 'normal global-map (kbd "S-SPC") 'evil-scroll-page-up)

;; AuTeX and DocView Modes
(add-hook 'TeX-mode-hook 'auto-fill-mode)
(setq doc-view-continuous t)

;; Ivy and Counsel Mode
(ivy-mode 1)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)

;; Dired and Peep Mode
(global-set-key (kbd "C-x i") 'peep-dired)
(evil-define-key 'normal peep-dired-mode-map
  (kbd "j") 'peep-dired-next-file
  (kbd "k") 'peep-dired-prev-file)
(add-hook 'peep-dired-hook 'evil-normalize-keymaps)

;; Org Mode
;(add-hook 'after-save-hook 'org-html-export-to-html nil t)
(add-hook 'org-mode-hook 'auto-fill-mode)
(setq org-publish-timestamp-directory "~/.config/emacs/org-timestamps/")

(global-set-key "\C-ca" 'org-agenda)
(define-key global-map "\C-cc"
  (lambda () (interactive) (org-capture nil "c")))
(setq org-capture-templates
      '(("c" "Add agenda event" entry
	 (file+headline "~/org/agenda.org" "Captured Events")
	 "* %?\n  ")))
(add-hook 'org-capture-mode-hook 'auto-fill-mode)

;; Dictionary Mode
;(setq dictionary-server "localhost")

;; Load parchment theme
(load-theme 'parchment t)

;; Emacs Web Wowser
(setq shr-use-colors nil
      eww-download-directory "~/temporary")

;; Magit
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)
(setq evil-magit-state 'motion)
(require 'evil-magit)

;; Weechat
;(require 'weechat)
;;;;;;;;;;;;;;;;;;;
;; Suspend behavior
;;;;;;;;;;;;;;;;;;;
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z C-z") ; Prevent confilct in stumpwm
		(lambda () ; Only suspend frame if running from terminal
		   (interactive)
		   (if (display-graphic-p)
		       (message "suspend-frame disabled for graphical displays.")
		     (suspend-frame))))

;;;;;;;;;;;;;;;;;;;
;; Set Window Title
;;;;;;;;;;;;;;;;;;;
(setq-default frame-title-format
              '(:eval
                (format "%s %s [%s@%s]"
                        (buffer-name)
			(cond
			 (buffer-file-truename
                          (concat "(" buffer-file-truename ")"))
                         (dired-directory
                          (concat "{" dired-directory "}"))
                         (t ""))
                        (or (file-remote-p default-directory 'user)
                            user-real-login-name)
                        (or (file-remote-p default-directory 'host)
                            system-name))))

;;;;;;;;;;;;;;;
;; Transparency
;;;;;;;;;;;;;;;
;; From https://gitlab.com/mvanorder1390/my-emacs-config/blob/master/init.el
(defun toggle-transparency ()
  "Toggle the transparency of an Emacs frame."
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; Also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         '(85 . 50) '(100 . 100)))))
;; Set transparency of emacs
(defun transparency (value)
  "Set the transparency of the frame window. 0=transparent/100=opaque."
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))
;(transparency 85)

;;;;;;;;;;;;
;; Emojis 🤣
;;;;;;;;;;;;
;; From http://ergoemacs.org/emacs/emacs_list_and_set_font.html
(set-fontset-font
 t
 '(#x1f300 . #x1fad0)
 (cond
  ((member "Noto Color Emoji" (font-family-list)) "Noto Color Emoji")
  ((member "Noto Emoji" (font-family-list)) "Noto Emoji")
  ((member "Segoe UI Emoji" (font-family-list)) "Segoe UI Emoji")
  ((member "Symbola" (font-family-list)) "Symbola")
  ((member "Apple Color Emoji" (font-family-list)) "Apple Color Emoji"))
 ;; Apple Color Emoji should be before Symbola, but Richard Stallman disabled it.
 ;; GNU Emacs Removes Color Emoji Support on the Mac
 ;; http://ergoemacs.org/misc/emacs_macos_emoji.html
 ;;
 )

;;;;;;;
;; Mu4e
;;;;;;;
;(require 'mu4e)
;(require 'smtpmail)
;(setq mu4e-maildir "~/.local/share/mail/vesperous")
;(setq mu4e-refile-folder "/Archive"
      ;mu4e-sent-folder "/Sent"
      ;mu4e-drafts-folder "/Drafts"
      ;mu4e-trash-folder "/Trash")
;(setq mu4e-maildir-shortcuts
      ;'((:maildir "/INBOX"    :key ?i)
	;(:maildir "/Archive"  :key ?a)
	;(:maildir "/Folders.consoom"    :key ?c)
	;(:maildir "/Folders.vesperous"  :key ?v)
	;(:maildir "/Folders.gnwrighter" :key ?g)))
;(setq mu4e-get-mail-command "mbsync -c ~/.config/isync/mbsyncrc vesperous")
;(setq mu4e-compose-signature
      ;"\nThalia Wright\nhttps://www.lagrangian.space\n")
;(setq message-send-mail-function 'smtpmail-send-it
      ;smtpmail-default-smtp-server "localhost"
      ;smtpmail-local-domain "localhost"
      ;smtpmail-smtp-server "localhost"
      ;smtpmail-smtp-service 1025
      ;smtpmail-smtp-user "vesperous@protonmail.com")
;;; Don't leave a bunch of useless buffers
;(setq message-kill-buffer-on-exit t)
;;; Prevent Maildir error: duplicate UID error messages
;(setq mu4e-change-filenames-when-moving t) 
;;; Render html Messages and Open in Browser
;(setq mu4e-html2text-command 'my-html2text)
;(defun my-html2text ()
  ;(let ((dom (libxml-parse-html-region (point-min) (point-max))))
    ;(erase-buffer)
    ;(shr-insert-document dom)
    ;(goto-char (point-min))))
;(add-to-list 'mu4e-view-actions '("ViewInBrowser" . mu4e-action-view-in-browser) t)

;;;;;;;;;;;;;
;; Begin Auto
;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-view-program-selection
   '(((output-dvi has-no-display-manager)
      "dvi2tty")
     ((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "Zathura")
     (output-html "lynx")))
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(auth-source-save-behavior nil)
 '(custom-enabled-themes nil)
 '(custom-safe-themes
   '("79b6be0f84d3beb977d67ed477b6f876799bdf928370ce2d45d5eb87e9666097" default))
 '(default-input-method "russian-computer")
 '(org-agenda-files '("~/org/agenda.org"))
 '(org-drill-done-count-color "#663311")
 '(org-drill-failed-count-color "#880000")
 '(org-drill-mature-count-color "#005500")
 '(org-drill-new-count-color "#004488")
 '(package-selected-packages
   '(evil-collection mastodon ereader evil-magit magit dired-git-info persist slack weechat cider clojure-mode elpher parchment-theme evil-mu4e peep-dired rust-mode haskell-mode counsel ivy nov evil markdown-mode dictionary latex-math-preview auctex writeroom-mode htmlize)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;; init.el ends here
