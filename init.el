;; fichier config de base, transféré dans init.el pour pouvoir charger ce fichier automatiquement à chaque démarrage d'emacs

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ivy-rich-mode t)
 '(package-selected-packages
   '(emojify general python-mode dap-mode counsel-projectile typescript-mode desktop-environment evil-magit projectile evil-collection evil helpful counsel ivy-rich which-key rainbow-delimiters doom-modeline swiper ivy use-package magit))
 '(set-input-method "us")
 '(set-language-environment "English")
 '(set-language-environment-hook nil)
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 25))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load "server")
(unless (server-running-p) (server-start))


(setq inhibit-startup-message t) ;; supprime le message d'accueil

(scroll-bar-mode -1) ;; supprime la barre de scroll
(tool-bar-mode -1) ;; supprime la toolbar
(tooltip-mode -1) ;; supprime les tooltips
(set-fringe-mode 10) ;; donne plus de place

(menu-bar-mode -1) ;; supprime la barre de menu

(setq visible-bell t) ;; remplace le son de cloche par un flash moins désagréable

;; active le theme darcula de jetbrains que j'aime bien
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")(load-theme 'tango-dark)
;; (add-to-list 'default-frame-alist '(font . "JetBrains Mono-14"))
;; (add-to-list 'default-frame-alist '(line-spacing . 0.2))
(load-theme 'jetbrains-darcula t)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit) ;; relie la touche escape à l'action quit

;; package qui permet une assignation plus facile des keybindings
(use-package general
  :config
  (general-create-definer rune/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (rune/leader-keys
    "t"  '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")))

;; empêche le comportement agaçant par défaut qui ferme tous les autres buffer ouvert sauf celui sur lequel on est, quand on appuie sur espace
(defadvice keyboard-escape-quit
  (around keyboard-escape-quit-dont-close-windows activate)
  (let ((buffer-quit-function (lambda () ())))
    ad-do-it))

;; Initialise les packages
(require 'package)

;; instancie les repos de packages pour bénéficier d'une auto-update
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; Initialise use-package sur les plateformes non-linux
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

;; always-ensure vérifie que les packages sont à jour
(require 'use-package)
(setq use-package-always-ensure t)

;; active le numéros des ligne à droite + exceptions dans certains modes comme le eshell ou ça sert à rien
(column-number-mode)
(global-display-line-numbers-mode t)

(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda() (display-line-numbers-mode 0))))

;; installe swiper (outil puissant pour chercher dans un document) au cas où ivy ne l'installe pas par défaut via elpa ou melpa
(use-package swiper :ensure t)

(use-package emojify
  :hook (after-init . global-emojify-mode))

;; installe ivy: https://github.com/abo-abo/swiper
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

;; permet d'avoir les icones des packages
(use-package all-the-icons)

;; installe doom-modeline qui rend le footer plus jolia barre inférieure plus jolie
(use-package doom-modeline
  :init (doom-modeline-mode 1))


;; donne des infos supplémentaires en mode M-x
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

;; active le M-x en mode counsel pour bénéficier des infos de ivy-rich qui donne une description de la commande
(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))

;; meilleur aide avec C-h
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; text-scaling plus facile
(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(rune/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

;; Ctrl + Shift + N pour changer de buffer un peu comme dans les IDE de jetbrains quand on cherche des fichiers dans le projet
(global-set-key (kbd "C-M-n") 'counsel-switch-buffer)


(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

;; IDE dans emacs
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . efs/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
  :after lsp)

(use-package lsp-ivy)

(use-package dap-mode
  ;; Uncomment the config below if you want all UI panes to be hidden by default!
  ;; :custom
  ;; (lsp-enable-dap-auto-configure nil)
  ;; :config
  ;; (dap-ui-mode 1)

  :config
  (require 'dap-node)
  (dap-node-setup)
  (general-define-key
    :keymaps 'lsp-mode-map
    :prefix lsp-keymap-prefix
    "d" '(dap-hydra t :wk "debugger")))

;; contenu additionnel si on veut utiliser lsp avec typescript ou python
(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(use-package python-mode
  :ensure t
  :hook (python-mode . lsp-deferred)
  :custom
  (python-shell-interpreter "python3")
  (dap-python-executable "python3")
  (dap-python-debugger 'debugpy)
  :config
  (require 'dap-python))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

;; installe which-key qui pop une aide quand on commence à taper une commande
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.5))


;; naviguer facilement dans les projets ayant un repo git, en utilisant la navigation de ivy
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Projets")
    (setq projectile-project-search-path '("~/Projets")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))


;; gestionnaire de versioning
(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; hook qui lance des applications en fond
(defun efs/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

;; hooks au démarrage de exwm
(defun efs/exwm-init-hook ()
  (exwm-workspace-switch-create 1)
  ;; Montre l'heure est la date en modeline
  (setq display-time-day-and-date t)
  (setq display-time-format "%H:%M:%S %a %d %b %Y")
  (display-time-mode 1)

  ;; icône en bas à droite du bluetooth, du son et de la connection internet
  (efs/run-in-background "nm-applet")
  (efs/run-in-background "pasystray")
  (efs/run-in-background "blueman-applet")) 

;; formattage du nom des buffers dans exwm pour mieux s'y retrouver
(defun efs/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

(defun efs/exwm-update-title ()
  (pcase exwm-class-name
    ("Chromium" (exwm-workspace-rename-buffer (format "Chromium: %s" exwm-title)))))

;; message qui pop dans le mini-bufffer quand un nouveau buffer est ouvert
(defun efs/configure-window-by-class ()
  (interactive)
  (message "Windows '%s' appeared" exwm-class-name)
  (pcase exwm-class-name
    ("Firefox" (exwm-workspace-move-window 2))
    ("Chromium" (exwm-workspace-move-window 1))
    ("mpv" (exwm-floating-toggle-floating)
           (exwm-layout-toggle-mode-line))))

;; exwm: emacs en tant qu'environnement desktop
(use-package exwm
  :config
  ;; 5 workspaces par defaut
  (setq exwm-workspace-number 5)
  
  (add-hook 'exwm-update-class-hook #'efs/exwm-update-class)

  ;; Quand le nom de la fenetre update on en profite pour renommer le buffer, utile pour la navigation entre les onglets du mavigateur
  (add-hook 'exwm-update-title-hook #'efs/exwm-update-title)
  (add-hook 'exwm-manage-finish-hook #'efs/configure-window-by-class)


  (require 'exwm-systemtray)
  (setq exwm-systemtray-height 17)
  (exwm-systemtray-enable)

  ;; on définit la résolution pour éviter tout problème
  (require 'exwm-randr)
  (exwm-randr-enable)
  (start-process-shell-command "xrandr" nil "xrandr --output Virtual-1 --primary --mode 1920x1080 --pos 0x0 --rotate normal")
  
  (add-hook 'exwm-update-class-hook #'efs/exwm-update-class)
  (add-hook 'exwm-init-hook #'efs/exwm-init-hook)
  (add-hook 'after-init-hook #'global-emojify-mode)


  ;; Cles pour lesquelles emacs sera prioritaire sur exwm
  (setq exwm-input-prefix-keys
    '(?\C-x
      ?\C-u
      ?\C-\M-n
      ?\C-h
      ?\M-x
      ?\M-`
      ?\M-&
      ?\M-:
      ?\C-\M-j
      ?\C-\ ))

  ;; Permet d'echapper le prochain keybinding de exwm, utilise pour copier / coller
  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

  ;; Implemente les keybindings globales
  (setq exwm-input-global-keys
        `(
          ([?\s-r] . exwm-reset)

          ;; Bouger entre les fenetres
	  ([s-left] . windmove-left)
          ([s-right] . windmove-right)
          ([s-up] . windmove-up)
          ([s-down] . windmove-down)

          ;; Lance une application avec une commande shell
          ([?\s-&] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))

          ;; Switch de workspace
          ([?\s-w] . exwm-workspace-switch)

          ;; 's-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))))

  (exwm-input-set-key (kbd "s-n") 'counsel-linux-app)

  (exwm-enable))

;; permet d'utiliser les touches clavier pour modifier le volume ou la luminosité
(use-package desktop-environment
  :after exwm
  :config (desktop-environment-mode)
  :custom
  (desktop-environment-brightness-small-increment "2%+")
  (desktop-environment-brightness-small-decrement "2%-")
  (desktop-environment-brightness-normal-increment "5%+")
  (desktop-environment-brightness-normal-decrement "5%-"))

;; boite mail dans emacs
(use-package mu4e
  :ensure nil
  :load-path "/usr/share/emacs/site-lisp/mu4e/"
  :defer 20 ; Wait until 20 seconds after startup
  :config
  (setq mu4e-change-filenames-when-moving t)

  

  ;; Rafraichi les mail toutes les 3 minutes
  (setq mu4e-update-interval (* 3 60))
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-maildir "~/Mail")
  (setq message-send-mail-function 'smtpmail-send-it)
  
  ;; ameliore l'affichage dans certains cas
  (setq mu4e-compose-format-flowed t)

  (setq mu4e-drafts-folder "/Gmail/[Gmail]/Drafts")
  (setq mu4e-sent-folder   "/Gmail/[Gmail]/Sent Mail")
  (setq mu4e-refile-folder "/Gmail/[Gmail]/All Mail")
  (setq mu4e-trash-folder  "/Gmail/[Gmail]/Trash")(setq mu4e-contexts
        (list
         ;; Adresse Gmail Perso
         (make-mu4e-context
          :name "Perso"
          :match-func
            (lambda (msg)
              (when msg
                (string-prefix-p "/Gmail" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "vicmartindev@gmail.com")
                  (user-full-name    . "Victor Martin")
		  (smtpmail-smtp-server  . "smtp.gmail.com")
                  (smtpmail-smtp-service . 465)
                  (smtpmail-stream-type  . ssl)
                  (mu4e-drafts-folder  . "/Gmail/[Gmail]/Drafts")
                  (mu4e-sent-folder  . "/Gmail/[Gmail]/Sent Mail")
                  (mu4e-refile-folder  . "/Gmail/[Gmail]/All Mail")
                  (mu4e-trash-folder  . "/Gmail/[Gmail]/Trash")))))

  ;; Choisis par defaut le premier contexte qu'il trouve
  (setq mu4e-context-policy 'pick-first)

  (setq mu4e-maildir-shortcuts
      '(("/Gmail/Inbox"             . ?i)
        ("/Gmail/[Gmail]/Sent Mail" . ?s)
        ("/Gmail/[Gmail]/Trash"     . ?t)
        ("/Gmail/[Gmail]/Drafts"    . ?d)
        ("/Gmail/[Gmail]/All Mail"  . ?a)))
  (mu4e t))

;; permet aux programmes externes d'aller chercher les mots de passe cryptes dans .authinfo.gpg
(defun efs/lookup-password (&rest keys)
  (let ((result (apply #'auth-source-search keys)))
    (if result
        (funcall (plist-get (car result) :secret))
        nil)))

;; quelques reglages de base pour ERC
(setq erc-server "irc.libera.chat"
      erc-nick "gabagool"
      erc-user-full-name "Victor Martin"
      erc-track-shorten-start 8
      erc-autojoin-channels-alist '(("irc.libera.chat" "#systemcrafters" "#emacs"))
      erc-kill-buffer-on-part t
      erc-auto-query 'bury)


;; bascule le clavier en qwerty
(shell-command "setxkbmap us")
