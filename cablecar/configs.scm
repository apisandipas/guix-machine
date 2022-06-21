(define-module (cablecar configs)
  #:use-module (rde features)
  #:use-module (rde features xdg)
  #:use-module (rde features ssh)
  #:use-module (rde features base)
  #:use-module (rde features linux)
  #:use-module (rde features fontutils)
  #:use-module (rde features version-control)
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
  #:use-module (rde packages)
  #:use-module (rde packages emacs)
  #:use-module (rde packages emacs-xyz)
  #:use-module (gnu packages fonts)
  #:use-module (guix gexp)
  #:use-module (cablecar utils)
  #:use-module (cablecar systems)
  #:export (
            %cablecar-base-system-packages
            %cablecar-base-home-packages
            %cablecar-base-features))

;; This module is responsible for creating the rde config.
;; It will define all the different base system services.
;;
;; Operating system configuration should be done in cablecar/systems.scm,
;; and computer specific settings in each corresponding file in cablecar/systems/.
;;
;; TODO: Add feature for setting custom groups (preferrably directly in features).
;;       This is required by certain services, e.g. virtualization.

(define* (pkgs #:rest lst)
  (map specification->package+output lst))

(define* (pkgs-vanilla #:rest lst)
  "Packages from guix channel."
  (define channel-guix
    (list (channel
           (name 'guix)
           (url "https://git.savannah.gnu.org/git/guix.git")
           (commit
            "2b6af630d61dd5b16424be55088de2b079e9fbaf"))))

  (define inferior (inferior-for-channels channel-guix))
  (define (get-inferior-pkg pkg-name)
    (car (lookup-inferior-packages inferior pkg-name)))

   (map get-inferior-pkg lst))

(define %cablecar-base-system-packages
  (pkgs '("git" "nss-certs" "emacs-exwm" "emacs-desktop-environment")))

;; Move some of the packages to separate features?
(define %cablecar-base-home-packages
    (append
     (pkgs-vanilla
      "icecat" "nyxt"
      "ungoogled-chromium-wayland" "ublock-origin-chromium")
     (pkgs
      "emacs-exwm" "emacs-desktop-environment" "arandr"
      "alsa-utils" "youtube-dl" "imv"
      "obs" "obs-wlrobs"
      "recutils"
      "fheroes2"
      ;; TODO: Enable pipewire support to chromium by default
      ;; chrome://flags/#enable-webrtc-pipewire-capturer
      "hicolor-icon-theme" "adwaita-icon-theme" "gnome-themes-standard"
      "ripgrep" "curl" "make")))

;; Dynamically create a configuration that can be reproduced
;; without having the correct environment variables set.
;; This is required for some commands to work, e.g. guix pull.
;; (define (make-entrypoint)
;;   (scheme-file "entrypoint.scm"
;;                #~(begin
;;                    (use-modules (cablecar reconfigure))
;;                    (make-config #:user #$(getenv "RDE_USER")
;;                                 #:system #$(gethostname)))))

(define %cablecar-base-features
  (list
   (feature-custom-services
    #:system-services
    (list
     ;; (service mate-desktop-service-type)
     ;; (service sddm-service-type)
     ;; (service nix-service-type)
     )
    #:home-services
    (list
     ;; ((@ (gnu services) simple-service)
     ;;  'extend-shell-profile
     ;;  (@ (gnu home-services shells) home-shell-profile-service-type)
     ;;  (list
     ;;   #~(string-append
     ;;      "alias superls="
     ;;      #$(file-append (@ (gnu packages base) coreutils) "/bin/ls"))))
     ))

   (feature-base-services
    #:guix-substitute-urls (list "https://substitutes.nonguix.org")
    #:guix-authorized-keys (list (local-file "files/nonguix-signing-key.pub")))
   (feature-desktop-services)
   (feature-docker)
   (feature-pipewire)
   (feature-fonts
    #:font-monospace (font "Iosevka" #:size 16 #:weight 'regular)
    #:font-packages (list font-iosevka font-fira-mono))

   ;; TODO: Consider making a `feature-kitty` if this does work ok enough.
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
   (feature-swayidle)
   (feature-swaylock
    #:swaylock (@ (gnu packages wm) swaylock-effects)
    ;; The blur on lock screen is not privacy-friendly.
    #:extra-config '( ;; (screenshots)
                     ;; (effect-blur . 7x5)
                     (clock)))
   (feature-rofi)

   (feature-emacs
    #:emacs emacs-next-pgtk-latest
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
    #:system-packages %arden-base-system-packages
    #:home-packages %arden-base-home-packages)
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
   ;;
   ))

;; ((   ;; (feature-dotfiles
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
;; ))
