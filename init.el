;; fichier config de base, transféré dans init.el pour pouvoir charger ce fichier automatiquement à chaque démarrage d'emacs

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ivy-rich-mode t)
 '(package-selected-packages
   (quote
    (exwm ivy-prescient fish-completion eshell-syntax-highlighting emojify general python-mode dap-mode counsel-projectile typescript-mode desktop-environment evil-magit projectile evil-collection evil helpful counsel ivy-rich which-key rainbow-delimiters doom-modeline swiper ivy use-package magit)))
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

(setq inhibit-startup-message t) ;; supprime le message d'accueil

(scroll-bar-mode -1) ;; supprime la barre de scroll
(tool-bar-mode -1) ;; supprime la toolbar
(tooltip-mode -1) ;; supprime les tooltips
(set-fringe-mode 10) ;; donne plus de place

(menu-bar-mode -1) ;; supprime la barre de menu

(setq visible-bell t) ;; remplace le son de cloche par un flash moins désagréable

;; desactive le message d'avertissement quand on ouvre un fichier volumineux
(setq large-file-warning-threshold nil)

;; active le theme darcula de jetbrains, ainsi que la font que j'aime bien
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")(load-theme 'tango-dark)
(add-to-list 'default-frame-alist '(font . "JetBrains Mono-15"))
(load-theme 'jetbrains-darcula t)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit) ;; relie la touche escape à l'action quit

;; ameliore le scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; une ligne a la fois
(setq mouse-wheel-progressive-speed nil) ;; desactive l'acceleration pendant le scroll
(setq mouse-wheel-follow-mouse 't) ;; scroll de la fenetre
(setq scroll-step 1) ;; scroll du clavier une ligne a la fois
(setq use-dialog-box nil)

;; Initialise les packages
(require 'package)


;; empêche le comportement agaçant par défaut qui ferme tous les autres buffer ouvert sauf celui sur lequel on est, quand on appuie sur espace
(defadvice keyboard-escape-quit
  (around keyboard-escape-quit-dont-close-windows activate)
  (let ((buffer-quit-function (lambda () ())))
    ad-do-it))

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
  :hook (erc-mode . emojify-mode)
  :commands emojify-mode)

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


;; command highlighting pour eshell
(use-package eshell-syntax-highlighting
  :after esh-mode
  :config
  (eshell-syntax-highlighting-global-mode +1))

;; auto completion pour eshell
(use-package fish-completion
  :hook (eshell-mode . fish-completion-mode))

;; fonction qui lance des applications en fond
(defun efs/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

;; hooks au démarrage de exwm
(defun efs/exwm-init-hook ()
  
  ;; demarre exwm dans le workspace 1
  (exwm-workspace-switch-create 1)

  ;; demarre le serveur pour charger les mails et la polybar
  (load "server")
  (unless (server-running-p) (server-start))
  
  ;; lance la polybar
  (efs/start-panel)

  ;; lance redshift (filtre anti lumiere bleue) avec mes coordonnees GPS
  (efs/run-in-background "redshift -l 43.70313:7.26608")

  ;; lance le gestionnaire de notifications
  (efs/run-in-background "dunst")
  
  ;; icône en bas à droite du son et de la connection internet
  (efs/run-in-background "nm-applet")
  (efs/run-in-background "pasystray")) 

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
  ;; 10 workspaces par defaut
  (setq exwm-workspace-number 10)
  
  (add-hook 'exwm-update-class-hook #'efs/exwm-update-class)

  ;; Quand le nom de la fenetre update on en profite pour renommer le buffer, utile pour la navigation entre les onglets du mavigateur
  (add-hook 'exwm-update-title-hook #'efs/exwm-update-title)
  (add-hook 'exwm-manage-finish-hook #'efs/configure-window-by-class)

  ;; on définit la résolution des ecrans et leur disposition, sauvegardez une config sur xrandr pour obtenir le code
  (require 'exwm-randr)
  (exwm-randr-enable)
  (start-process-shell-command "xrandr" nil "xrandr --output DVI-D-0 --primary --mode 1920x1080 --pos 0x414 --rotate normal --output HDMI-0 --mode 1920x1080 --pos 1920x0 --rotate left")

  ;; fait la repartition de quel workspace appartient a quel ecran
  (setq exwm-randr-workspace-monitor-plist '(1 "DVI-D-0" 2 "DVI-D-0" 3 "DVI-D-0" 4 "DVI-D-0" 8 "HDMI-0" 9 "HDMI-0" 0 "HDMI-0"))

  ;; teleporte le curseur de la souris sur l'autre ecran quand il y a un changement
  (setq exwm-workspace-warp-cursor t)
  
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

          ([?\s-w] . exwm-workspace-switch)

          ;; 's-N': Switch de workspace
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
  :defer 1 ;; Attend 1 seconde apres le demarrage
  :config
  (setq mu4e-change-filenames-when-moving t)

  

  ;; Rafraichi les mails toutes les 5 minutes
  (setq mu4e-update-interval (* 5 60))
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-maildir "~/Mail")
  (setq message-send-mail-function 'smtpmail-send-it)
  
  ;; ameliore l'affichage des mails dans certains cas
  (setq mu4e-compose-format-flowed t)
  (add-hook 'mu4e-compose-mode-hook (lambda () (use-hard-newlines -1)))

  ;; enleve les messages d'indexation dans le mini-buffer
  (setf mu4e-display-index-messages nil) 

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

  ;; Choisis par defaut le premier contexte qu'il trouve au demarrage de emacs, dans mon cas, mon adresse perso
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

;; commande pour quitter / demarrer polybar facilement
(defvar efs/polybar-process nil
  "Holds the process of the running Polybar instance, if any")

(defun efs/kill-panel ()
  (interactive)
  (when efs/polybar-process
    (ignore-errors
      (kill-process efs/polybar-process)))
  (setq efs/polybar-process nil))

(defun efs/start-panel ()
  (interactive)
  (efs/kill-panel)
  (setq efs/polybar-process (start-process-shell-command "polybar" nil "polybar panel")))

;; remplace la modale par un message dans le minibuffer pour demander la phrase de passe de gpg
(require 'epg)
(setq epg-pinentry-mode 'loopback)

;; controle de l'historique des notifications de dunst en ligne de commande
(defun efs/dunstctl (command)
  (start-process-shell-command "dunstctl" nil (concat "dunstctl " command)))

(exwm-input-set-key (kbd "s-b") (lambda () (interactive) (efs/dunstctl "history-pop")))
(exwm-input-set-key (kbd "s-B") (lambda () (interactive) (efs/dunstctl "close-all")))


;; pause et resume des notifications de dunst
(defun efs/disable-desktop-notifications ()
  (interactive)
  (start-process-shell-command "notify-send" nil "notify-send \"DUNST_COMMAND_PAUSE\""))

(defun efs/enable-desktop-notifications ()
  (interactive)
  (start-process-shell-command "notify-send" nil "notify-send \"DUNST_COMMAND_RESUME\""))

(defun efs/toggle-desktop-notifications ()
  (interactive)
  (start-process-shell-command "notify-send" nil "notify-send \"DUNST_COMMAND_TOGGLE\""))

  (start-process-shell-command "notify-send" nil "notify-send \"Notifications!\"")
