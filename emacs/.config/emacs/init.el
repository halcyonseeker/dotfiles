;;; init.el --- Thalia Wright's startup file for GNU Emacs
;;; Commentary:
;; TODO:
;; - Fix TeX-view-program-selection to use xdg-open when running with -nw
;;   in a graphical environemnt
;; - Use pinentry-emacs or pinentry-tty in non-graphical frames
;; - Resolve "{add} Access Denied" issue with emms and mpd
;; - Would it be good to byte-compile init.el?

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Out of the Box Functionality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Startup optimizations
(setq gc-cons-threshold (* 50 1000 1000))
(setq gc-cons-percentage 0.6)

;; Fix general appearance and behavior
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(xterm-mouse-mode 1)

;; Prefer BSD style
(setq c-default-style "bsd")
(setq-default c-basic-offset 'set-from-style
              tab-width 8
              indent-tabs-mode t)
(setq sh-basic-offset tab-width)

;; Break lines at 72 columns when writing prose and code comments
(add-hook 'TeX-mode-hook 'auto-fill-mode)
(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'org-mode-hook 'auto-fill-mode)
(add-hook 'prog-mode-hook
	  (lambda ()
	    (setq-local comment-auto-fill-only-comments t)
	    (auto-fill-mode t)))

;; Make scroll behavior less jarring
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq scroll-step 1)

;; Add nice things
(display-battery-mode 1)
(display-time-mode 1)
(column-number-mode 1)
(line-number-mode 1)
(save-place-mode 1)
(goto-address-mode 1)

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

;; Move custom stuff into a seperate file
(setq custom-file "~/.config/emacs/custom.el")
(when (file-exists-p "~/.config/emacs/custom.el")
  (load-file custom-file))

;; Don't suspend graphical frames
(global-set-key (kbd "C-z")
		(lambda ()
		  (interactive)
		  (when (not (display-graphic-p)) (suspend-frame))))

;; Set the frame title to something more meaningful
(setq-default frame-title-format
              '(:eval
                (cond (dired-directory
                       (format "%s" (abbreviate-file-name
				     (expand-file-name dired-directory))))
                      ((and buffer-file-truename
                            (not (string= major-mode "mu4e-compose-mode")))
                       (format "%s %s" (buffer-name) (file-name-directory
						      buffer-file-truename)))
                      (t (format "%s" (buffer-name))))))

;; Use C-\ to toggle between US qwerty and Russian Typewriter
(setq default-input-method "russian-computer")

;; I want this to work in the mini-buffer
(global-set-key (kbd "C-w") 'backward-kill-word)

;; Use C-l to clear scrollback in eshell
(bind-keys
 ("C-l" . (lambda ()
            (interactive)
            (when (string= major-mode "eshell-mode")
              (goto-char (point-max))
              (eshell-kill-input)
              (insert "clear-scrollback")
              (eshell-send-input)
              (yank)))))

;; Org Mode
(add-hook 'org-mode-hook 'org-indent-mode)
(setq org-publish-timestamp-directory "~/.config/emacs/org-timestamps/"
      org-directory "~/org")
(setq org-startup-folded t)

;; Emacs Web Wowser
(setq browse-url-browser-function 'eww-browse-url
      shr-use-colors nil
      eww-download-directory "~/temporary"
      eww-browse-secondary-browser-function '(shell-command "xdg-open"))
(add-hook 'eww-after-render-hook #'local-eww--rename-buffer)

;; Doc View
(setq doc-view-continuous t)
(add-hook 'doc-view-mode-hook 'auto-revert-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Install and Configure Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize use-package
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-always-ensure t)

;; Mu4e Mail Client
(when (and (require 'mu4e nil 'noerror)
           (require 'smtpmail nil 'noerror))
  (setq mu4e-attachment-dir "~/temporary"
        mail-user-agent 'mu4e-user-agent
        message-send-mail-function 'smtpmail-send-it
        mu4e-completing-read-function 'ivy-completing-read
        mu4e-get-mail-command "mbsync -c ~/.local/mail/mbsyncrc -a"
        message-kill-buffer-on-exit t
        mu4e-headers-skip-duplicates t
        mu4e-change-filenames-when-moving t
        mu4e-view-show-images nil
        auth-sources '(password-store))
  (auth-source-pass-enable)
  (add-to-list 'mu4e-view-actions
               '("View In Browser" . mu4e-action-view-in-browser) t)
  (when (file-exists-p "~/.local/mail/accounts.el")
    (load "~/.local/mail/accounts.el")))

;; Evil Mode - A better text editor for Emacs
(use-package evil
  :demand t
  :init
  (setq evil-want-C-i-jump nil          ; Fix TAB in -nw frames
        evil-want-keybinding nil
        evil-disable-insert-state-bindings t)
  :config
  (evil-mode 1)
  (evil-set-toggle-key "C-M-z")		; Let me suspend the frame
  (add-hook 'org-mode-hook
	    (lambda ()	; Fix paragraph motions in org mode
	      (defalias 'evil-forward-paragraph 'org-forward-paragraph)
	      (defalias 'evil-backward-paragraph 'org-backward-paragraph))))
(use-package evil-collection
  :after evil
  :init (setq evil-want-keybinding nil)
  :config (evil-collection-init))
(use-package evil-terminal-cursor-changer
  :config (unless (display-graphic-p)
	    (evil-terminal-cursor-changer-activate)))
(use-package undo-tree
  :after evil
  :diminish
  :config
  (evil-set-undo-system 'undo-tree)
  (global-undo-tree-mode 1))

;; AucTeX for a better TeX and LaTeX experience
(use-package tex
  :ensure auctex
  :config
  (setq TeX-view-program-selection
	'(((output-dvi has-no-display-manager) "dvi2tty")
          ((output-pdf has-no-display-manager) "fbpdf")
          ((output-html has-no-display-manager) "lynx")
          (output-dvi "xdg-open")
          (output-pdf "xdg-open")
          (output-html "xdg-open"))))

;; Markdown editing mode
(use-package markdown-mode
  :config
  (add-hook 'markdown-mode-hook 'auto-fill-mode))

;; Ivy and counsel for nicer minibuffer behavior
(use-package ivy
  :config
  (ivy-mode 1)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file))
(use-package counsel :after ivy)

;; Writeroom mode for a distraction-free environment
(use-package writeroom-mode
  :config
  (global-set-key (kbd "C-x w") 'writeroom-mode))

;; Magit -- Make Git way faster and easier to use
(use-package magit
  :bind ("C-x g" . magit-status))

(use-package slime
  :config
  (setq inferior-lisp-program "sbcl")
  (setq slime-repl-history-file "~/.config/emacs/slime/slime-history.eld"))

;; Miscellaneous Useful Packages
(use-package fvwm-mode)
(use-package go-mode)
(use-package htmlize)
(use-package ereader)
(use-package elpher)
(use-package nov :after ereader)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Some Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(defun local-eww--rename-buffer ()
  "Rename EWW buffer using page title or URL. This makes it easy to have
lots of EWW buffers open at one time. Used by `eww-after-render-hook'"
  (let ((name (if (eq "" (plist-get eww-data :title))
                  (plist-get eww-data :url)
                (plist-get eww-data :title))))
    (rename-buffer (format "*eww: %s*" name) t)))
;;; init.el ends here
