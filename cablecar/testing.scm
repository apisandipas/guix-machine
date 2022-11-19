(define-module (cablecar testing)

  #:use-module (rde features)
  #:use-module (rde features base)
  #:use-module (rde features gnupg)
  #:use-module (rde features security-token)
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
  #:use-module (rde features emacs-xyz)
  #:use-module (rde features linux)
  #:use-module (rde features bittorrent)
  #:use-module (rde features mail)
  #:use-module (rde features docker)
  #:use-module (rde features virtualization)
  #:use-module (rde features video)
  #:use-module (rde features finance)
  #:use-module (rde features markup)
  ;; #:use-module (gnu services)
  #:use-module (rde features networking)
  #:use-module (gnu services)
  #:use-module (rde home services i2p)

  #:use-module (cablecar configs)
  #:use-module (cablecar systems)

  ;; #:use-module (gnu services nix)
  #:use-modules (gnu system)
  #:use-module (gnu system keyboard)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system mapped-devices)
  #:use-module (gnu packages)
  #:use-module (rde packages)
  #:use-module (rde packages emacs)
  #:use-module (rde packages emacs-xyz)
  #:use-module (gnu packages fonts)
  #:use-module (guix gexp)
  #:use-module (guix inferior)
  #:use-module (guix channels)
  #:use-module (ice-9 match))


;; Allows dynamic loading of configuration modules based on file name.
(define* (dynamic-load sub mod var-name #:key (throw? #t))
  (let ((var (module-variable
              (resolve-module `(cablecar ,sub ,(string->symbol mod))) var-name)))
    (if (or (not var) (not (variable-bound? var)))
        (when throw?
          (raise-exception
           (make-exception-with-message
            (string-append "reconfigure: could not load module '" mod "'"))))
        (variable-ref var))))


;;; User-specific features

;; Initial user's password hash will be available in store, so it's
;; use this feature with care
;; (display (crypt "hi" "$6$abc"))

(define* (mail-acc id user #:optional (type 'gmail))
  "Make a simple mail-account with gmail type by default."
  (mail-account
   (id   id)
   (fqda user)
   (type type)))

(define* (mail-lst id fqda urls)
  "Make a simple mailing-list."
  (mailing-list
   (id   id)
   (fqda fqda)
   (config (l2md-repo
            (name (symbol->string id))
            (urls urls)))))

(define %main-features
  (list


   (feature-base-services)
   (feature-desktop-services)
   (feature-docker)
   (feature-qemu)

   (feature-pipewire)
   (feature-backlight #:step 10)

   (feature-fonts
    #:font-monospace (font "Iosevka" #:size 11 #:weight 'regular)
    ;; #:font-monospace (font "Fira Mono" #:size 14 #:weight 'semi-light)
    ;; #:font-packages (list font-fira-mono)
    #:default-font-size 11)

   (feature-vterm)
   (feature-zsh
    #:enable-zsh-autosuggestions? #t)
   (feature-bash)
   (feature-direnv)
   (feature-git)


   ;; https://github.com/natpen/awesome-wayland
   (feature-sway
    ;; #:xwayland? #t
    #:extra-config
    `((output DP-2 scale 2)
      ;; (output eDP-1 disable)
      ,@(map (lambda (x) `(workspace ,x output DP-2)) (iota 8 1))

      ;; (workspace 9 output DP-2)
      ;; (workspace 10 output DP-2)

      ;; (bindswitch --reload --locked lid:on exec /run/setuid-programs/swaylock)

      ;; FIXME: Use absolute path, move to feature-network, fix permissions issue
      (exec nm-applet --indicator)
      (bindsym
       --locked $mod+Shift+t exec
       ,(file-append (@ (gnu packages music) playerctl) "/bin/playerctl")
       play-pause)

      (bindsym
       --locked $mod+Shift+n exec
       ,(file-append (@ (gnu packages music) playerctl) "/bin/playerctl")
       next)

      (bindsym $mod+Shift+o move workspace to output left)
      (bindsym $mod+Ctrl+o focus output left)
      (input type:touchpad
             ;; TODO: Move it to feature-sway or feature-mouse?
             (;; (natural_scroll enabled)
              (tap enabled)))
      (bindsym $mod+Shift+Return exec emacs)))
   (feature-sway-run-on-tty
    #:sway-tty-number 2)
   (feature-sway-screenshot)
   ;; (feature-sway-statusbar
   ;;  #:use-global-fonts? #f)
   (feature-waybar
    #:waybar-modules
    (list
     (waybar-sway-workspaces)
     ;; (waybar-sway-window)
     (waybar-tray)
     (waybar-idle-inhibitor)
     ;; (waybar-temperature)
     (waybar-sway-language)
     (waybar-microphone)
     (waybar-volume)
     (waybar-battery #:intense? #f)
     (waybar-clock)))
   (feature-swayidle)
   (feature-swaylock
    #:swaylock (@ (gnu packages wm) swaylock-effects)
    ;; The blur on lock screen is not privacy-friendly.
    #:extra-config '(;; (screenshots)
                     ;; (effect-blur . 7x5)
                     (clock)))
   (feature-kanshi
    #:extra-config
    `((profile laptop ((output eDP-1 enable)))
      (profile docked ((output eDP-1 enable)
                       (output DP-2 scale 2)))))
   ;; (feature-rofi)

   ;; TODO: Add an app for saving and reading articles and web pages
   ;; https://github.com/wallabag/wallabag
   ;; https://github.com/chenyanming/wallabag.el

   (feature-emacs-appearance
    #:extra-elisp
    `((setq modus-themes-syntax '(faint))
      ;; (setq modus-themes-region '(bg-only))
      ;; (setq modus-themes-paren-match '(underline))
      (setq modus-themes-org-blocks 'tinted-background)))
   (feature-emacs-faces)
   (feature-emacs-tramp)
   (feature-emacs-completion
    #:mini-frame? #f
    #:marginalia-align 'right)

   (feature-emacs-corfu
    #:corfu-doc-auto #f)
   (feature-emacs-vertico)
   (feature-emacs-project)
   (feature-emacs-perspective)
   (feature-emacs-input-methods)
   (feature-emacs-which-key)
   (feature-emacs-keycast #:turn-on? #f)

   (feature-emacs-dired)
   (feature-emacs-eshell)
   (feature-emacs-monocle)
   (feature-emacs-message)
   (feature-emacs-pdf-tools)
   (feature-emacs-nov-el)
   (feature-emacs-smartparens
    #:show-smartparens? #t)
   (feature-emacs-geiser)
   (feature-emacs-guix)
   (feature-emacs-tempel
    #:default-templates? #t
    #:templates `(fundamental-mode
                  ,#~""
                  (t (format-time-string "%Y-%m-%d"))))


   (feature-ledger)
   (feature-markdown)

   (feature-mpv
    #:extra-mpv-conf '((speed . 1.61)))
   (feature-isync #:isync-verbose #t)
   (feature-l2md)
   (feature-msmtp)
   (feature-notmuch
    #:extra-tag-updates-post
    '("notmuch tag +guix-home -- 'thread:\"\
{((subject:guix and subject:home) or (subject:service and subject:home) or \
subject:/home:/) and tag:new}\"'")
    #:notmuch-saved-searches
    (cons*
     ;; TODO: Add tag:unread to all inboxes.  Revisit archive workflow.
     '(:name "Work Inbox" :query "tag:work and tag:inbox and tag:unread" :key "W")
     '(:name "Personal Inbox" :query "tag:personal and tag:inbox" :key "P")
     '(:name "Guix Home Inbox" :key "H" :query "tag:guix-home and tag:unread")
     '(:name "RDE Inbox"       :key "R"
             :query "(to:/rde/ or cc:/rde/) and tag:unread")

     ;; '(:name "Watching" :query "thread:{tag:watch} and tag:unread" :key "tw")
     %rde-notmuch-saved-searches))

   (feature-transmission #:auto-start? #f)

   (feature-emacs
    #:default-application-launcher? #t
    #:additional-elisp-packages
    (append
     (list emacs-dirvish)
     (strings->packages
      "emacs-elfeed" "emacs-hl-todo"
      "emacs-yasnippet"
      ;; "emacs-company"
      "emacs-consult-dir"
      ;; "emacs-all-the-icons-completion" "emacs-all-the-icons-dired"
      "emacs-kind-icon"
      "emacs-nginx-mode" "emacs-yaml-mode"
      ;; "emacs-lispy"
      "emacs-ytdl"
      "emacs-multitran"
      "emacs-minimap"
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
    #:home-packages
    (append
     (strings->packages
      "figlet" ;; TODO: Move to emacs-artist-mode
      "calibre"
      "icecat" "nyxt"
      "ungoogled-chromium-wayland" "ublock-origin-chromium"

      "utox" "qtox" "jami"

      "alsa-utils" "youtube-dl" "imv" "cozy"
      "pavucontrol" "wev"
      "imagemagick"
      "obs" "obs-wlrobs"
      "recutils" "binutils"
      "fheroes2"
      ;; TODO: Enable pipewire support to chromium by default
      ;; chrome://flags/#enable-webrtc-pipewire-capturer
      "hicolor-icon-theme" "adwaita-icon-theme" "gnome-themes-extra"
      "papirus-icon-theme" "arc-theme"
      "thunar"
      ;; "glib:bin"

      "libreoffice"
      ;; TODO: Fix telega package!
      "ffmpeg"
      "ripgrep" "curl")))))



(define* (make-config
          #:key
          (user (getenv "RDE_USER"))
          (system (getenv "HOST"))
          (target (getenv "RDE_TARGET"))
          (initial-os %cablecar-initial-os))

  (define %user-features (dynamic-load 'configs user '%user-features))
  (define %system-features (dynamic-load 'systems system '%system-features))
  (define %system-swap (dynamic-load 'systems system '%system-swap #:throw? #f))

  (define %initial-os
    (if (or (unspecified? %system-swap) (null? %system-swap))
        initial-os
        (operating-system
          (inherit initial-os)
          (swap-devices
           (list %system-swap)))))

  (define %generated-config
    (rde-config
     (initial-os %initial-os)
     (features
      (append
       %user-features
       %base-features
       %system-features))))

  (define %cablecar-he
    (rde-config-home-environment %generated-config))

  (define %cablecar-os
    (operating-system
      (inherit (rde-config-operating-system %generated-config))
      (kernel-arguments
       (append
        (get-value
         'kernel-arguments %generated-config
         (operating-system-user-kernel-arguments %initial-os))
        (get-feature-kernel-arguments 'kernel-arguments-radios %generated-config)))
      (issue (operating-system-issue %initial-os))))


  (match target
    ("home" %cablecar-he)
    ("system" %cablecar-os)
    (_ %cablecar-he)))

(make-config)
