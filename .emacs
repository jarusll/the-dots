;; INIT STUFF
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(package-initialize)

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
    (exec-path-from-shell keycast general evil htmlize 0blayout treemacs cfrs hydra pfuture ace-window bui haskell-mode flycheck typescript-mode company rust-mode yasnippet exec-path-from-shell ansible elixir-mode ivy-rich helpful dockerfile-mode desktop-environment sx golden-ratio counsel ivy zygospore quelpa zzz-to-char elisp-format rjsx-mode json-mode which-key plantuml-mode elcord yaml-mode use-package markdown-mode magit transpose-frame keycast smex avy)))
 '(which-key-allow-evil-operators t)
 '(which-key-allow-imprecise-window-fit t)
 '(yas-global-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages
(dolist (package package-selected-packages)
  (unless (package-installed-p package)
    (package-install package)))

;;;; CONSTANTS
(defconst user-full-name "Suraj Yadav")

;; import shell vars
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package org
  :config
  ;; load languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (python . t)
     (shell . t)
     (sql . t)
     ))

  ;; Syntax highlight in #+BEGIN_SRC blocks
  (setq org-src-fontify-natively t)

  ;; Don't prompt before running code in org
  (setq org-confirm-babel-evaluate nil)
  )

;; enable keycast in modeline
(use-package keycast
  :ensure t
  :config
  (keycast-mode 1))

;; enable company-mode for autocompletion in buffer
(use-package company
  :ensure t
  :config
  (global-company-mode 1))

;; snippets
(use-package yasnippet
  :ensure t
  :config (setq yas-snippets-dirs '("~/.emacs/snippets"))
  (yas-global-mode 1))

;; ivy stuff
(use-package counsel
  :ensure t
  :after ivy
  :config (counsel-mode))

(use-package ivy
  :ensure t
  :config
  (ivy-mode)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t))

(use-package ivy-rich
  :ensure t
  :after ivy
  :config
  (ivy-rich-mode 1))

;; which key mode
(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom)
  (setq which-key-idle-delay 0.10)
  (setq which-key-idle-secondary-delay 0.05)
  (setq which-key-popup-type 'side-window))

;; relative numbers
(use-package emacs
  :bind (
	 ("C-M-j" . counsel-switch-buffer)
	 )
  :init
  ;; relative line numbers
  (global-display-line-numbers-mode 1)
  (setq display-line-numbers-type 'relative)

  ;; refresh buffer when file changed
  (global-auto-revert-mode 1)
  (setq global-auto-revert-non-file-buffers t)
  ;; remove last position in file
  (save-place-mode 1)
  ;; autosave dir
  (setq backup-directory-alist
	`(("." . ,(concat user-emacs-directory "backups"))))

  ;; set tab width
  (setq tab-stop-list '(4 8 12 16))

  ;;;; QOL STUFF

  ;; registers
  (set-register ?d (cons 'file "~/source/jarusll.github.io/src/diary/index.md"))
  (set-register ?e (cons 'file "~/.emacs"))

  ;; sync emacs clipboard with systems
  (setq x-select-enable-clipboard t)

  ;; disable menu bar
  (menu-bar-mode 0)
  ;; disable menu bar
  (tool-bar-mode 0)
  ;; disable menu bar
  (toggle-scroll-bar -1)
  ;; set font to jetbrains mono
  (set-face-attribute 'default nil :font "Jetbrains Mono" :height 160)
  ;; always follow symlinks without confirmation
  (setq vc-follow-symlinks t)
  )

(use-package recentf
  :config
  (recentf-mode 1))

;; plant uml mode
(use-package plantuml-mode
  :config
  (setq plantuml-jar-path "~/plantuml.jar")
  (setq plantuml-default-exec-mode 'jar)
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode)))

;; discord rich presence
;; (elcord-mode)

;; golden ratio mode
(use-package golden-ratio
  :init
  (setq golden-ratio-auto-scale t)
  (setq golden-ratio-adjust-factor .9
	golden-ratio-wide-adjust-factor .9)
  :config
  (golden-ratio-mode 1)
  (setq golden-ratio-extra-commands
	'(evil-window-next
	  evil-window-prev
	  evil-window-left
	  evil-window-right
	  evil-window-top
	  evil-window-bottom)))

;;;; evil stuff
(use-package evil
  :init
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1)
  ;; set leader to Space
  (evil-set-leader 'normal (kbd "SPC"))
  (define-key evil-normal-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  (evil-set-initial-state 'dired-mode 'emacs)
  )

(defconst general-leader "SPC")
(use-package general
  :config
  (general-evil-setup)
  (general-create-definer general-leader
    :prefix "SPC")

  ;; ** Global Keybindings
  (general-leader
    :states 'normal
    :keymaps 'override
    ;; files
    "f" '(:ignore t :which-key "files")
    "ff" 'find-file
    "fw" 'save-buffer
    "fd" 'counsel-dired
    "e" '(:ignore t :which-key "eval")
    "eb" 'eval-buffer
    "er" 'eval-region
    "ed" 'eval-defun
    "b" '(:ignore t :which-key "buffer")
    "bs" 'save-buffer
    "bc" 'buffer/close
    "n" '(:ignore t :which-key "new")
    "ns" 'yas-new-snippet
    "nf" 'find-file
    "o" '(:ignore t :which-key "open")
    "od" 'open/diary
    "oe" 'open/init-el
    ":" 'eval-expression					;
    "!" 'shell-command
    "\\" 'transpose-frame
    ))

(defun buffer/close()
  (interactive)
  (kill-buffer (current-buffer)))

(defun open/diary()
  (interactive)
  (jump-to-register ?d))

(defun open/init-el()
  (interactive)
  (jump-to-register ?e))

(add-hook 'rust-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c C-c C-c") #'compile-rust)))

(add-hook 'haskell-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c C-c C-c") #'compile-haskell)))

;;;; CUSTOM FUNCTIONS/COMMANDS
(defun insert-current-date () 
  (interactive) 
  (insert (shell-command-to-string "echo -n $(date +%F)")))

(defun org-babel-execute:python (body params)
  (let ((filepath (concat (temporary-file-directory) (number-to-string (random)))))
    (with-temp-file filepath)
    (append-to-file body nil filepath)
    (shell-command-to-string (concat "python3 " filepath))))

(defun compile-cpp()
  (interactive)
  (let ((filename (buffer-name))
	(execname (file-name-base)))
    (shell-command (concat "g++ " filename " -o " execname " && " "./" execname))))

(defun compile-ts()
  (interactive)
  (let ((filename (buffer-name))
	(execname (file-name-base)))
    (shell-command (concat "tsc " filename " && node " execname ".js"))))

(defun compile-rust()
  (interactive)
  (let ((filename (buffer-name))
	(execname (file-name-base)))
    (shell-command (concat "rustc " filename " && ./" execname))))

(defun compile-haskell()
  (interactive)
  (let ((filename (buffer-name))
	(execname (file-name-base)))
    (shell-command (concat "ghc --make " filename " && ./" execname))))


(defun compile-js()
  (interactive)
  (let ((filename (buffer-name))
	(execname (file-name-base)))
    (shell-command (concat "node " filename))))

(defun docker-compose-up()
  (interactive)
  (shell-command "docker-compose up"))

;; variables
(set-variable (quote scheme-program-name) "chezscheme")
