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

;; Change some irritating defaults
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(xterm-mouse-mode 1)
(setq frame-resize-pixelwise t)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq scroll-step 1)
(setq dired-listing-switches "-alh")
(setq inhibit-startup-message t)
(setq vc-follow-symlinks t)
(setq c-default-style "bsd")
(defalias 'yes-or-no-p 'y-or-n-p)

;; Put all backup files in a single place
(setq backup-by-copying t)
(setq backup-directory-alist '((".*" . "~/.config/emacs/backup")))

;; Move custom stuff into a seperate file
(setq custom-file "~/.config/emacs/custom.el")
(when (file-exists-p "~/.config/emacs/custom.el") (load-file custom-file))

;; Selectively enable tab indentation only where it makes sense
(setq-default indent-tabs-mode nil)
(defun indent-with-tabs () (setq indent-tabs-mode t))
(add-hook 'c-mode-hook #'indent-with-tabs)
(add-hook 'c++-mode-hook #'indent-with-tabs)
(add-hook 'awk-mode-hook #'indent-with-tabs)
(add-hook 'sh-mode-hook #'indent-with-tabs)
(setq sh-basic-offset tab-width)

;; Prefer C-style comments in JS
(add-hook 'js-mode-hook
          (lambda ()
            (setq comment-start "/* ")
            (setq comment-end " */")))

;; Break lines at 72 columns when writing prose and code comments
(add-hook 'TeX-mode-hook 'auto-fill-mode)
(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'org-mode-hook 'auto-fill-mode)
(add-hook 'prog-mode-hook
          (lambda ()
            (setq-local comment-auto-fill-only-comments t)
            (auto-fill-mode t)))

;; Add nice things
(display-battery-mode 1)
(setq display-time-default-load-average nil)
(setq display-time-format "%H:%M")
(display-time-mode 1)
(column-number-mode 1)
(line-number-mode 1)
(save-place-mode 1)
(goto-address-mode 1)

;; Enable tab completion in editing buffers, mostly for elisp and sly
(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)

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

;; Only show the stuff I really need in the mode line
(setq-default mode-line-format '("%e" mode-line-front-space mode-line-mule-info
                                 mode-line-client mode-line-modified
                                 mode-line-remote mode-line-frame-identification
                                 mode-line-buffer-identification "   "
                                 mode-line-position evil-mode-line-tag
                                 (vc-mode vc-mode) "  " mode-line-misc-info
                                 mode-line-end-spaces "%-"))

;; Use C-\ to toggle between US qwerty and Russian Typewriter,
;; switching the ispell dictionary as well.
(setq default-input-method "russian-computer")
(add-hook 'input-method-activate-hook
          (lambda () (ispell-change-dictionary "ru")))
(add-hook 'input-method-deactivate-hook
          (lambda () (ispell-change-dictionary "english")))

;; I want this to work in the mini-buffer
(global-set-key (kbd "C-w") 'backward-kill-word)

;; Use C-c M-o to clear scrollback in eshell, just like in Lisp repls
(eval-after-load 'esh-mode
  '(define-key eshell-mode-map (kbd "C-c M-o")
     (lambda ()
       (interactive)
       (goto-char (point-max))
       (eshell-kill-input)
       (insert "clear-scrollback")
       (eshell-send-input)
       (yank))))

;; Org Mode
(add-hook 'org-mode-hook 'org-indent-mode)
(setq org-publish-timestamp-directory "~/.config/emacs/org-timestamps/"
      org-directory "~/org")
(setq org-startup-folded t)
(global-set-key (kbd "C-c a") 'org-agenda)

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
(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-always-ensure t)

;; Mu4e Mail Client
(add-hook 'message-header-setup-hook 'message-add-openpgp-header)
(setq message-openpgp-header
      (list "0870AC115CE741A8DCD93F4D11C2EE8C6A3E3E78"
            (concat "https://keys.openpgp.org/vks/v1/by-fingerprint/"
                    "0870AC115CE741A8DCD93F4D11C2EE8C6A3E3E78")
            "signencrypt"))
(setq mml-secure-openpgp-encrypt-to-self t)
(when (and (require 'mu4e nil 'noerror)
           (require 'smtpmail nil 'noerror))
  (setq mu4e-attachment-dir "~/temporary"
        mail-user-agent 'mu4e-user-agent
        mu4e-completing-read-function 'ivy-completing-read
        mu4e-get-mail-command "mbsync -c ~/.local/mail/mbsyncrc -a"
        message-kill-buffer-on-exit t
        mu4e-headers-skip-duplicates t
        mu4e-change-filenames-when-moving t
        mu4e-view-show-images nil
        sendmail-program (executable-find "msmtp")
        message-send-mail-function 'message-send-mail-with-sendmail)
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
  (evil-set-toggle-key "C-M-z")         ; Let me suspend the frame
  (add-hook 'org-mode-hook
            (lambda ()  ; Fix paragraph motions in org mode
              (defalias 'evil-forward-paragraph 'org-forward-paragraph)
              (defalias 'evil-backward-paragraph 'org-backward-paragraph)))
  ;; Emacs' default paragraph motions are nicer
  (defalias 'evil-forward-paragraph 'forward-paragraph)
  (defalias 'evil-backward-paragraph 'backward-paragraph))
(use-package evil-collection
  :after evil
  :init (setq evil-want-keybinding nil)
  :config (evil-collection-init))
(use-package evil-terminal-cursor-changer
  :config (unless (display-graphic-p) (evil-terminal-cursor-changer-activate)))
(use-package undo-tree
  :after evil
  :diminish
  :config
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist
        '(("." . "~/.config/emacs/undo-tree/")))
  (evil-set-undo-system 'undo-tree)
  (global-undo-tree-mode 1))

;; AucTeX for a better TeX and LaTeX experience
(use-package tex
  :ensure auctex
  :config
  (setq TeX-view-program-selection
        '((output-pdf "xdg-open")
          (output-html "xdg-open"))))

;; Ivy and counsel for nicer minibuffer behavior
(use-package ivy
  :config (ivy-mode 1)
  :bind ("C-x C-f" . 'counsel-find-file))
(use-package counsel :after ivy)

;; A fantastic IDE for the best programming language
(use-package sly
  :config
  (setq inferior-lisp-program "sbcl")
  (setq sly-mrepl-history-file-name "~/.config/emacs/sly-mrepl-history"))

;; Play multimedia with emms
(use-package emms
  :config
  (require 'emms-setup)
  (emms-all)
  (emms-default-players)
  (setq emms-player-list (list 'emms-player-mpv))
  (setq emms-source-file-default-directory "~/media/music")
  (add-to-list 'emms-player-mpv-parameters "--vo=null"))

;; Add modes for languages and formats not supported OoB
(use-package haskell-mode)
(use-package bison-mode)
(use-package fvwm-mode)
(use-package rust-mode)
(use-package go-mode :hook (before-save . gofmt-before-save))
(use-package markdown-mode :hook (markdown-mode . auto-fill-mode))
(use-package alchemist)
(use-package erlang)

;; Miscellaneous Useful Packages
(use-package htmlize)
(use-package ereader)
(use-package telega)
(use-package elpher)
(use-package nov :after ereader)
(use-package rainbow-delimiters :hook (prog-mode . rainbow-delimiters-mode))
(use-package writeroom-mode :bind ("C-x w" . 'writeroom-mode))
(use-package magit :bind ("C-x g" . magit-status))
(use-package marginalia :init (marginalia-mode))

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

