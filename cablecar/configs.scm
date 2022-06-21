(define-module (cablecar configs)
  #:use-module (ice-9 match)
  #:use-module (rde features)
  #:use-module (rde features base)
  #:use-module (rde features gnupg)
  #:use-module (rde features keyboard)
  #:use-module (rde features system)
  #:use-module (rde features wm)
  #:use-module (rde features xdisorg)
  #:use-module (rde features xdg)
  #:use-module (rde features password-utils)
  #:use-module (rde features version-control)
  #:use-module (rde features fontutils)
  #:use-module (rde features terminals)
  #:use-module (rde features tmux)
  #:use-module (rde features shells)
  #:use-module (rde features shellutils)
  #:use-module (rde features ssh)
  #:use-module (rde features emacs)
  #:use-module (rde features linux)
  #:use-module (rde features bittorrent)
  #:use-module (rde features docker)
  #:use-module (rde features video)
  #:use-module (rde features finance)
  #:use-module (rde features markup)
  #:use-module (rde features mail)
  #:use-module (rde features networking)
  #:use-module (gnu services)
  #:use-module (gnu services desktop)
  #:use-module (gnu services sddm)
  #:use-module (gnu system keyboard)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system mapped-devices)
  #:use-module (gnu packages)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages fonts)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)
  #:use-module (guix gexp)
  #:use-module (guix inferior)
  #:use-module (guix channels)
  #:use-module (rde packages)
  #:use-module (rde packages emacs)
  #:use-module (rde packages emacs-xyz)
  #:use-module (cablecar utils)
  #:use-module (cablecar features state)
  #:export (
            %cablecar-base-system-packages
            %cablecar-base-home-packages
            %cablecar-base-features))

(define* (pkgs #:rest lst)
  (map specification->package+output lst))

(define %cablecar-base-system-packages
  (append (pkgs "git" "nss-certs")))

;; Move some of the packages to separate features?
(define %cablecar-base-home-packages
  (append
   (pkgs
    "icecat" "nyxt"
    "ungoogled-chromium-wayland" "ublock-origin-chromium"
    "emacs-exwm" "emacs-desktop-environment" "arandr"
    "alsa-utils" "youtube-dl" "imv"
    "obs" "obs-wlrobs"
    "pcmanfm"
    "recutils"
    "fheroes2"
    "hicolor-icon-theme" "adwaita-icon-theme" "gnome-themes-standard"
    "ripgrep" "curl" "ncurses" "make")))

(define %cablecar-base-home-services (list ))
(define %cablecar-base-system-services (list ))

(define %cablecar-base-features
  (list
   (feature-custom-services
    #:system-services %cablecar-base-system-services
    #:home-services %cablecar-base-home-services)

   (feature-base-services)
   (feature-desktop-services)
   (feature-docker)
   (feature-pipewire)

   (feature-fonts
    #:font-monospace (font "Iosevka" #:size 18 #:weight 'regular)
    #:font-packages (list font-iosevka font-fira-mono))

   (feature-alacritty
    #:config-file (local-file "./files/alacritty/alacritty.yml")
    #:default-terminal? #f
    #:backup-terminal? #t
    #:software-rendering? #f)
   (feature-vterm)
   (feature-tmux
    #:config-file (local-file "./files/tmux/tmux.conf"))
   (feature-zsh
    #:enable-zsh-autosuggestions? #t)
   (feature-bash)
   (feature-direnv)
   (feature-git)
   (feature-ssh)
   (feature-sway
    #:add-keyboard-layout-to-config? #f
    #:extra-config
    `((include ,(local-file "./files/sway/config"))))
   (feature-sway-run-on-tty
    #:sway-tty-number 2)
   (feature-sway-screenshot)
   ;; (feature-sway-statusbar
   ;;  #:use-global-fonts? #t)
   (feature-waybar
    #:waybar-modules
    (list
     (waybar-sway-workspaces)
     (waybar-sway-window)
     (waybar-tray)
     (waybar-idle-inhibitor)
     ;; (waybar-temperature)
     (waybar-sway-language)
     (waybar-battery #:intense? #f)
     (waybar-clock)))
   (feature-swayidle)
   (feature-swaylock
    #:swaylock (@ (gnu packages wm) swaylock-effects)
    ;; The blur on lock screen is not privacy-friendly.
    #:extra-config '( ;; (screenshots)
                     ;; (effect-blur . 7x5)
                     (clock)))
   (feature-rofi)

   (feature-emacs
    #:emacs
    (if (string=? (or (getenv "BUILD_SUBMITTER") "") "git.sr.ht")
        (@ (gnu packages emacs) emacs-next-pgtk)
        emacs-next-pgtk-latest)
    #:extra-init-el `()
    #:additional-elisp-packages
    (append
     (list emacs-consult-dir)
     (pkgs "emacs-elfeed" "emacs-hl-todo"
           "emacs-ytdl"
           "emacs-ement"
           "emacs-restart-emacs"
           "emacs-org-present")))

   (feature-xdg
    #:xdg-user-directories-configuration
    (home-xdg-user-directories-configuration
     (music "$HOME/music")
     (videos "$HOME/vids")
     (pictures "$HOME/pics")
     (documents "$HOME/docs")
     (download "$HOME/dl")
     (desktop "$HOME")
     (publicshare "$HOME")
     (templates "$HOME")))
   (feature-base-packages
    #:system-packages %cablecar-base-system-packages
    #:home-packages %cablecar-base-home-packages)
   (feature-emacs-appearance)
   (feature-emacs-faces)
   (feature-emacs-completion
    #:mini-frame? #t)
   (feature-emacs-vertico)
   (feature-emacs-project)
   (feature-emacs-perspective)
   (feature-emacs-input-methods)
   (feature-emacs-which-key)
   (feature-emacs-keycast #:turn-on? #t)

   (feature-emacs-dired)
   (feature-emacs-eshell)
   (feature-emacs-monocle)
   (feature-emacs-message)

   ;; (feature-dotfiles
   ;;  #:dotfiles
   ;;  `(("aliasrc" ,(local-file "files/aliasrc"))
   ;;    ("inputrc" ,(local-file "files/inputrc"))
   ;;    ("nix-channels" ,(local-file "files/nix-channels"))
   ;;    ("config/guix/channels.scm" ,(local-file "channels.scm"))
   ;;    ("config/guix/config.scm" ,(make-entrypoint))
   ;;    ("config/dunst/dunstrc" ,(local-file "files/config/dunst/dunstrc"))
   ;;    ("config/nvim/init.vim" ,(local-file "files/config/nvim/init.vim"))
   ;;    ("config/nvim/autoload/plug.vim" ,(local-file "files/config/nvim/autoload/plug.vim"))
   ;;    ("config/picom/picom.conf" ,(local-file "files/config/picom/picom.conf"))))
   ))
