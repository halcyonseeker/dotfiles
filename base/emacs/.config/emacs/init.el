;;; init.el --- Thalia Wright's startup file for GNU Emacs
;;; Commentary:
;; TODO:
;; - Fix TeX-view-program-selection to use xdg-open when running with
;;   -nw in a graphical environemnt
;; - Evil Mode
;;   - Fix redo behavior
;;   - Fix elfeed bindings
;; - Use pinentry-emacs or pinentry-tty in non-graphical frames
;; - Make document viewing nicer
;; - Clean up init file and look into use-package
;; - Resolve "{add} Access Denied" issue with emms and mpd

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
;; Fix general appearance and behavior
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(xterm-mouse-mode 1)

;; Improve tab behavior
(setq-default indent-tabs-mode nil)
(setq default-tab-width 4)
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

;; Decrypt GPG key with pinentry-emacs when running in tty/terminal
;; (when (not (window-system))
;;   (setenv "INSIDE_EMACS"(format "%s,comint" emacs-version))
;;   (pinentry-start))

;; Don't suspend graphical frames
(global-set-key (kbd "C-z")
		(lambda ()
		  (interactive)
		  (when (not (display-graphic-p)) (suspend-frame))))

;;;;;;;;;;;;;;;;;;;;;
;; Packages and Modes
;;;;;;;;;;;;;;;;;;;;;
;; Mu4e Mail Client
(when (and (require 'mu4e nil 'noerror)
           (require 'smtpmail nil 'noerror))
  (load "~/.config/emacs/mu4e.el"))

;; Elfeed RSS/Atom Reader
(global-set-key (kbd "C-x n") 'elfeed)
(when (file-exists-p "~/documents/elfeed.el")
  (load "~/documents/elfeed.el"))

;; Evil Mode
(setq evil-want-C-i-jump nil)          ; Fix tab in tty. BEFORE require 'evil
(when (require 'evil nil 'noerror)
  (evil-mode 1)
  (add-to-list 'evil-emacs-state-modes ; Use emacs-state in various modes
               'elfeed-search-mode
	       'eww-mode)
  (evil-set-toggle-key "C-M-z")        ; Don't break suspend-frame bindings
  ;; Use emacs like bindings in insert-state
  (define-key evil-insert-state-map "\C-n" 'evil-next-line)
  (define-key evil-insert-state-map "\C-p" 'evil-previous-line)
  (define-key evil-insert-state-map "\C-a" 'evil-beginning-of-line)
  (define-key evil-insert-state-map "\C-e" 'bevil-end-of-line)
  ;; Page through buffer with space and shift+space/backspace in normal-state
  (define-key evil-normal-state-map (kbd "SPC") 'scroll-up-command)
  (define-key evil-normal-state-map (kbd "DEL") 'scroll-down-command)
  (define-key evil-normal-state-map (kbd "S-SPC") 'scroll-down-command))

;; AuTeX and DocView Modes
(add-hook 'TeX-mode-hook 'auto-fill-mode)
(setq doc-view-continuous t
      TeX-view-program-selection
      '(((output-dvi has-no-display-manager) "dvi2tty")
        ((output-pdf has-no-display-manager) "fbpdf")
        ((output-html has-no-display-manager) "lynx")
        (output-dvi "xdg-open")
        (output-pdf "xdg-open")
        (output-html "xdg-open")))

;; Use Ido if Ivy is not present
(if (boundp 'ivy-mode)
    (progn
      (ivy-mode 1)
      (global-set-key (kbd "C-x C-f") 'counsel-find-file))
  (progn
    (ido-mode 1)
    (setq ido-default-file-method 'selected-window
          ido-default-buffer-method 'selected-window)
    (make-local-variable 'ido-decorations)
    (setf (nth 2 ido-decorations) "\n")))

;; Dired and Peep Mode
(global-set-key (kbd "C-x i") 'peep-dired)
(when (boundp 'evil-define-key)
  (evil-define-key 'normal peep-dired-mode-map
                   (kbd "j") 'peep-dired-next-file
                   (kbd "k") 'peep-dired-prev-file)
  (add-hook 'peep-dired-hook 'evil-normalize-keymaps))

;; Org Mode
(add-hook 'org-mode-hook 'auto-fill-mode)
(setq org-publish-timestamp-directory "~/.config/emacs/org-timestamps/")
(global-set-key "\C-ca" 'org-agenda)

;; Dictionary Mode
;(setq dictionary-server "localhost")

;; Load parchment theme
(when (boundp 'parchment)
  (load-theme 'parchment t))

;; Writeroom mode
(global-set-key (kbd "C-x w") 'writeroom-mode)

;; Emacs Multi Media System
(when (and (require 'emms-setup nil 'noerror)
           (require 'emms-player-mpd nil 'noerror))
  (emms-all)
  (emms-default-players)
  (setq emms-player-mpd-server-name "localhost"
        emms-player-mpd-server-port "6600")
  (add-to-list 'emms-info-functions 'emms-info-mpd)
  (add-to-list 'emms-player-list 'emms-player-mpd)
  (global-set-key (kbd "C-x p") 'emms-play-file))

;; Emacs Web Wowser
(setq shr-use-colors nil
      eww-download-directory "~/temporary")

;; Magit
(when (and (require 'magit nil 'noerror)
           (require 'evil-magit nil 'noerror))
  (global-set-key (kbd "C-x g") 'magit-status)
  (setq evil-magit-state 'motion))

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
  "Set transparency of the frame window to VALUE.  0=transparent/100=opaque."
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))
;(transparency 85)

;;;;;;;;;;;;
;; Emojis 🤣
;;;;;;;;;;;;
;; From http://ergoemacs.org/emacs/emacs_list_and_set_font.html
(set-fontset-font
 t '(#x1f300 . #x1fad0)
 (cond
  ((member "Noto Color Emoji" (font-family-list)) "Noto Color Emoji")
  ((member "Noto Emoji" (font-family-list)) "Noto Emoji")
  ((member "Segoe UI Emoji" (font-family-list)) "Segoe UI Emoji")
  ((member "Symbola" (font-family-list)) "Symbola")
  ((member "Apple Color Emoji" (font-family-list)) "Apple Color Emoji")))
;;; init.el ends here
