;; melpa stuff
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)      

;; misc setup
(setq inhibit-startup-screen t)
(menu-bar-mode 0)
(tool-bar-mode 0)
(toggle-scroll-bar -1)
(set-default-font "Jetbrains Mono-16")

;; enable keycase in modeline
(keycast-mode 1)

;; ivy stuff
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)

;; relative numbers
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

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
    (elcord yaml-mode use-package ob-php markdown-mode magit ivy-rich transpose-frame keycast smex avy))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; autosave dir
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

;; quick window switching
(global-set-key (kbd "C-.") #'other-window)
(global-set-key (kbd "C-,") #'prev-window)

(defun prev-window ()
  (interactive)
  (other-window -1))

;; keybind C-c m to compile
(global-set-key (kbd "C-c m") 'recompile)

;; set tab width
(setq tab-stop-list '(4 8 12 16))

;; discord rich presence
(elcord-mode)

;;;; QOL STUFF

;; registers
(set-register ?d (cons 'file "~/source/portfolio/src/diary/index.md"))

;; recent files
(recentf-mode 1)

;; sync emacs clipboard with systems
(setq x-select-enable-clipboard t)

;; remove last position in file
(save-place-mode 1)

;; refresh buffer when file changed
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

