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
(set-default-font "Ubuntu Mono-16")

;; enable keycase in modeline
(keycast-mode 1)

;; sync emacs clipboard with systems
(setq x-select-enable-clipboard t)

;; ivy stuff
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)

;; auto save stuff
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs-saves/" t)))

;; relative numbers
(display-line-numbers-mode 1)
(setq display-line-numbers 'relative)

;; set theme
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes (quote (tango-dark)))
 '(package-selected-packages (quote (transpose-frame keycast smex avy))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; autosave dir
;; (setq backup-directory-alist '(("." . "~/.emacs-saves"))
(setq backup-directory-alist
      `(("~/.emacs-saves")))
