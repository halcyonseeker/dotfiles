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
  (xterm-mouse-mode 1))

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
;; (global-visual-line-mode 1)

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

;; Decrypt GPG key with pinentry-emacs when running in tty/terminal
;; (when (not (window-system))
;;   (setenv "INSIDE_EMACS"(format "%s,comint" emacs-version))
;;   (pinentry-start))

;;;;;;;;;;;;;;;;;;;;;
;; Packages and Modes
;;;;;;;;;;;;;;;;;;;;;
;; Mu4e Mail Client
(when (and (require 'mu4e nil 'noerror)
           (require 'smtpmail nil 'noerror))
  (load "~/.config/emacs/mu4e.el"))

;; Evil Mode
(setq evil-want-C-i-jump nil)          ; Fix tab in tty. BEFORE require 'evil
(when (require 'evil nil 'noerror)
  (evil-mode 1)
  (add-to-list 'evil-emacs-state-modes ; Use emacs-state in various modes
	       'eww-mode)
  (evil-set-toggle-key "C-M-z"))       ; Don't break suspend-frame bindings

;; AuTeX and DocView Modes
(add-hook 'TeX-mode-hook 'auto-fill-mode)
(setq doc-view-continuous t)
(setq TeX-view-program-selection
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

;; Dictionary Mode
;(setq dictionary-server "localhost")

;; Load parchment theme
(when (boundp 'parhment)
  (load-theme 'parchment t))

;; Emacs Web Wowser
(setq shr-use-colors nil
      eww-download-directory "~/temporary")

;; Magit
(when (and (require 'magit nil 'noerror)
           (require 'evil-magit nil 'noerror))
  (global-set-key (kbd "C-x g") 'magit-status)
  (setq evil-magit-state 'motion))

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

;;;;;;;;;;;;;
;; Begin Auto
;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes nil)
 '(default-input-method "russian-computer")
 '(org-drill-done-count-color "#663311")
 '(org-drill-failed-count-color "#880000")
 '(org-drill-mature-count-color "#005500")
 '(org-drill-new-count-color "#004488")
 '(package-selected-packages
   '(gemini-mode evil-collection ereader evil-magit magit dired-git-info persist cider clojure-mode elpher parchment-theme evil-mu4e peep-dired rust-mode haskell-mode counsel ivy nov evil markdown-mode dictionary latex-math-preview auctex writeroom-mode htmlize)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;; init.el ends here
