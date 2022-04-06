;; INIT STUFF
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;;;; MODES SETUP

;; misc setup
(setq inhibit-startup-screen t)
(menu-bar-mode 0)
(tool-bar-mode 0)
(toggle-scroll-bar -1)
(set-default-font "Jetbrains Mono-10")

;; enable keycast in modeline
(keycast-mode 1)

;; ivy stuff
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(counsel-mode 1)

;; which key mode
(which-key-mode 1)
(which-key-setup-side-window-right)

;; relative numbers
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

;; recent files
(recentf-mode 1)

;; plant uml mode
(setq plantuml-jar-path "~/plantuml.jar")
(setq plantuml-default-exec-mode 'jar)
(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))

;; discord rich presence
(elcord-mode)

;; refresh buffer when file changed
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

;; remove last position in file
(save-place-mode 1)

;;;; QOL STUFF

;; registers
(set-register ?d (cons 'file "~/source/portfolio/src/diary/index.md"))
(set-register ?e (cons 'file "~/.emacs"))

;; sync emacs clipboard with systems
(setq x-select-enable-clipboard t)

;;;; EXWM STUFF

;; exwm config
(require 'exwm)
(require 'exwm-config)
(exwm-config-default)
(setq exwm-workspace-number 10)

;; startup scripts
(call-process "/bin/bash" "~/scripts/capsescape.sh")

;;;; KEYBINDS

;; quick window switching
(global-set-key (kbd "C-.") #'other-window)
(global-set-key (kbd "C-,") #'prev-window)

;; keybind C-c m to compile
(global-set-key (kbd "C-c m") 'recompile)

;; Counsel switch buffer
(global-set-key (kbd "C-x b") #'counsel-switch-buffer)
(global-set-key (kbd "C-x d") #'counsel-dired)

;;;; MISC

;; set theme
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes (quote (tango-dark)))
 '(package-selected-packages
   (quote
    (counsel ivy zygospore exwm quelpa zzz-to-char elisp-format rjsx-mode json-mode which-key plantuml-mode elcord yaml-mode use-package ob-php markdown-mode magit transpose-frame keycast smex avy))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; autosave dir
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

(defun prev-window ()
  (interactive)
  (other-window -1))

;; set tab width
(setq tab-stop-list '(4 8 12 16))

;;;; CUSTOM FUNCTIONS/COMMANDS
(defun insert-current-date () 
  (interactive) 
  (insert (shell-command-to-string "echo -n $(date +%F)")))
