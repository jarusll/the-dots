; misc setup
(setq inhibit-startup-screen t)
(menu-bar-mode 0)
(tool-bar-mode 0)
(set-default-font "Ubuntu Mono-16")

; enable keycase in modeline
(setq keycast-mode t)

; auto save stuff
(setq auto-save-file-name-transforms
  `((".*" "~/.emacs-saves/" t)))

; auto completions for C-x C-f
(ido-mode 1)

; remap M-x to smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

; good old M-x
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

; relative numbers
(display-line-numbers-mode 1)
(setq display-line-numbers 'relative)

; set theme
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes (quote (tango-dark)))
 '(package-selected-packages (quote (keycast smex avy))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

; autosave dir
; (setq backup-directory-alist '(("." . "~/.emacs-saves"))
(setq backup-directory-alist
      `(("~/.emacs-saves")))

; melpa stuff
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)      
