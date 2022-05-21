;; INIT STUFF
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(package-initialize)

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
    (exec-path-from-shell keycast general evil htmlize 0blayout lsp-treemacs treemacs cfrs hydra pfuture ace-window bui lsp-mode dap-mode haskell-mode flycheck typescript-mode company rust-mode yasnippet exec-path-from-shell ansible elixir-mode ivy-rich helpful dockerfile-mode desktop-environment sx golden-ratio counsel ivy zygospore quelpa zzz-to-char elisp-format rjsx-mode json-mode which-key plantuml-mode elcord yaml-mode use-package markdown-mode magit transpose-frame keycast smex avy)))
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

;; import shell vars
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

;;;; MODES SETUP

;; ORG SETUP

;; Run/highlight code using babel in org-mode
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

;;;; MISC SETUP
(menu-bar-mode 0)
(tool-bar-mode 0)
(toggle-scroll-bar -1)
(set-face-attribute 'default nil :font "Jetbrains Mono" :height 160)

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
  (setq which-key-idle-delay 0.05)
  (setq which-key-idle-secondary-delay 0.05)
  (setq which-key-popup-type 'side-window))

;; relative numbers
(use-package emacs
  :init
  (global-display-line-numbers-mode 1)
  (setq display-line-numbers-type 'relative)
  ;; refresh buffer when file changed
  (global-auto-revert-mode 1)
  (setq global-auto-revert-non-file-buffers t)
  ;; remove last position in file
  (save-place-mode 1)
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
  :config
  (golden-ratio-mode 1)
  (setq golden-ratio-auto-scale t)
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
  (evil-mode 1) ; evil global 
  ;; (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
  (evil-set-initial-state 'dired-mode 'emacs))

;;;; QOL STUFF

;; registers
(set-register ?d (cons 'file "~/source/jarusll.github.io/src/diary/index.md"))
(set-register ?e (cons 'file "~/.emacs"))

;; sync emacs clipboard with systems
(setq x-select-enable-clipboard t)

;;;; KEYBINDS
;; quick window switching

(defun prev-window ()
  (interactive)
  (other-window -1))

;; keybind C-c m to compile
(global-set-key (kbd "C-c C-c c") 'compile)
(global-set-key (kbd "C-c m") 'recompile)

;; Counsel switch buffer
(global-set-key (kbd "C-x b") #'counsel-switch-buffer)
(global-set-key (kbd "C-x d") #'counsel-dired)
(global-set-key (kbd "C-x C-f") #'counsel-find-file)

(global-set-key (kbd "C-x \\") #'transpose-frame)

;; company mode keybinds
(global-set-key (kbd "C-c C-SPC") #'company-complete-common)

(add-hook 'rust-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c C-c C-c") #'compile-rust)))

(add-hook 'haskell-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c C-c C-c") #'compile-haskell)))


;; autosave dir
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

;; set tab width
(setq tab-stop-list '(4 8 12 16))

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
