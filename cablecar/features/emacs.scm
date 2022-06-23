(define-module (cablecar features emacs)
  #:use-module (rde features)
  #:export (feature-emacs-exwm
            %cablecar-base-emacs-packages))

(define* (make-emacs-feature base-name
                             #:key
                             (home-services (const '()))
                             (system-services (const '())))
  "Creates a basic emacs feature configuration."
  (let ((f-name (symbol-append 'emacs- base-name)))
    (feature
     (name f-name)
     (values `((,f-name . #t)))
     (home-services-getter home-services)
     (system-services-getter system-services))))

(define* (feature-emacs-exwm
          ;; #:key
          ;; ()
          )
  "Add and configure Corfu completion for Emacs."
  (define emacs-f-name 'exwm)

  (define (get-home-services config)
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((require 'exwm)


        (defun bp/run-in-background (command)
          (let ((command-parts (split-string command "[ ]+")))
            (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

        (defun exwm-async-run (name)
          "Run a process asynchronously"
          (interactive)
          (start-process name nil name))

        (defun bp/exwm-init-hook ()

          (modify-all-frames-parameters
           '((right-divider-width . 24)
             (alpha . (100 . 100))
             (mouse-color . "white")
             (internal-border-width . 24)))

          ;; (doom-mark-buffer-as-real-h)

          ;; Make workspace 1 be the one where we land at startup
          (exwm-workspace-switch-create 0)

          ;; Useless gaps
          (exwm-outer-gaps-mode -1)

          ;; Start the Polybar panel
          ;; (bp/start-panel)

          ;; Launch apps that will run in the background
          ;; (bp/run-in-background "dropbox")
          ;; (bp/run-in-background "nm-applet")
          ;; (bp/run-in-background "pasystray")
          ;; ;; (bp/run-in-background "blueman-applet")
          ;; (bp/run-in-background "blueman-tray")
          ;; (bp/run-in-background "~/Apps/KeylightControl.AppImage")
          ;; (bp/run-in-background "dunst")

          ;; (bp/run-in-background "companion")
          ;; (bp/run-in-background "kitty")
          ;; (bp/run-in-background "obs")
          ;; (bp/run-in-background "spotify")
          )
          (exwm-enable)
          )
      #:elisp-packages (list
                        emacs-exwm emacs-desktop-environment))))

  (make-emacs-feature emacs-f-name
                      #:home-services get-home-services))



(define %cablecar-base-emacs-packages
  (list
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

   (feature-emacs-evil)
   (feature-emacs-appearance)
   (feature-emacs-faces)
   (feature-emacs-completion
    #:mini-frame? #t)
   (feature-vterm)
   (feature-emacs-vertico)
   (feature-emacs-project)
   (feature-emacs-perspective)
   (feature-emacs-git)
   (feature-emacs-input-methods)
   (feature-emacs-which-key)
   (feature-emacs-keycast
    #:turn-on? #t)
   (feature-emacs-dired)
   (feature-emacs-eshell)
   (feature-emacs-monocle)
   (feature-emacs-message)))
