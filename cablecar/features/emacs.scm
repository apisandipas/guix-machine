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


(define* (feature-emacs-winum)


  )

(define* (feature-emacs-winum)
  "Add and configure winum-node for Emacs. This enables switching of buffer windows by M-n"
  (define emacs-f-name 'winum)

  (define (get-home-services config)
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((require 'winum)
        (global-set-key (kbd "M-0") 'treemacs-select-window)
        (global-set-key (kbd "M-1") 'winum-select-window-1)
        (global-set-key (kbd "M-2") 'winum-select-window-2)
        (global-set-key (kbd "M-3") 'winum-select-window-3)
        (global-set-key (kbd "M-4") 'winum-select-window-4)
        (global-set-key (kbd "M-5") 'winum-select-window-5)
        (global-set-key (kbd "M-6") 'winum-select-window-6)
        (global-set-key (kbd "M-7") 'winum-select-window-7)
        (global-set-key (kbd "M-8") 'winum-select-window-8)
        (winum-mode 1))
      #:elisp-packages (list emacs-winum))))

  (make-emacs-feature emacs-f-name
                      #:home-services get-home-services))

(define* (feature-emacs-exwm
          ;; #:key
          ;; ()
          )
  "Add and configure EXWM for Emacs."
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
   (feature-emacs-exwm)
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
   (feature-emacs-message)
   (feature-emacs-winum)))
