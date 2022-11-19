(define-module (cablecar configs)
  #:use-module (gnu services)
  #:use-module (rde packages)
  #:use-module (rde features)
  #:use-module (rde features base)
  #:use-module (rde features wm)
  #:use-module (rde features xdisorg)
  #:use-module (rde features xdg)
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
  #:use-module (gnu system)
  #:use-module (gnu system keyboard)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system mapped-devices)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu packages)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (rde packages)
  #:use-module (rde packages emacs)
  #:use-module (rde packages emacs-xyz)
  #:use-module (gnu packages fonts)
  #:use-module (guix gexp)
  #:use-module (guix inferior)
  #:use-module (guix channels)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)
  #:use-module (ice-9 match)
  #:use-module (cablecar utils)
  #:use-module (cablecar configs)
  #:use-module (cablecar features state)
  #:use-module (cablecar features emacs)
  #:use-module (cablecar packages emacs-xyz)
  ;; #:use-module (engstrand features emacs)
  #:use-module (engstrand features audio)
  #:export (%base-features))


(define* %base-features
  (append
   (list
    (feature-custom-services
     #:system-services
     (list
      ;;(service mate-desktop-service-type)
      (service sddm-service-type)))

    (feature-base-services)
    (feature-desktop-services)
    (feature-docker)
    (feature-pulseaudio-control)
    (feature-pipewire)
    (feature-fonts
     #:font-monospace (font "Iosevka" #:size 18 #:weight 'regular)
     #:font-packages (list font-iosevka font-fira-mono))
    (feature-alacritty
     #:config-file (local-file "./files/alacritty/alacritty.yml")
     #:default-terminal? #f
     #:backup-terminal? #t
     #:software-rendering? #f)
    (feature-tmux
     #:config-file (local-file "./files/tmux/tmux.conf"))
    (feature-zsh
     #:enable-zsh-autosuggestions? #t)
    (feature-bash)
    (feature-direnv)
    (feature-git)
    (feature-ssh)
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
     #:system-packages
     (append
      (list cablecar-emacs-exwm)
      (pkgs "emacs-desktop-environment"))
     #:home-packages
     (append
      (pkgs-vanilla
       "icecat" "nyxt"
       "ungoogled-chromium" "ublock-origin-chromium")
      (pkgs
       "arandr"
       "alsa-utils" "youtube-dl" "imv"
       "obs" "obs-wlrobs"
       "recutils"
       "fheroes2"
       "feh"
       "hicolor-icon-theme" "adwaita-icon-theme" "gnome-themes-extra"
       "ripgrep" "curl" "make")))
    ;; (feature-dotfiles
    ;;  #:dotfiles
    ;;  `((".exwm" ,(local-file "files/emacs/exwm"))))
    )

   %cablecar-base-emacs-packages
   ))
