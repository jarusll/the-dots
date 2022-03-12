(setq inhibit-startup-screen t)
(menu-bar-mode 0)
; (tool-bar-mode 0)
(set-default-font "Ubuntu Mono-18")
; auto completions for C-x C-f
(ido-mode 1)
; remap M-x to smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
; good old M-x
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
; relative numbers
(display-line-numbers-mode)
(setq display-line-numbers 'relative)

; set theme
(custom-set-variables
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes (quote (tango-dark))))
(custom-set-faces)

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
