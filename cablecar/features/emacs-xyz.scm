(define-module (cablecar features emacs-xyz)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (gnu home services)
  #:use-module (gnu home-services emacs)
  #:use-module (gnu home-services wm)
  #:use-module (gnu home services xdg)
  #:use-module (gnu services)
  #:use-module (rde home services emacs)

  #:use-module (rde packages)
  #:use-module (rde packages emacs)
  #:use-module (rde packages emacs-xyz)
  #:use-module (rde features emacs)
  #:use-module (rde features emacs-xyz)
  #:use-module (rde features terminals)

  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages mail)

  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix transformations)
  #:use-module (cablecar gexp)
  #:use-module (cablecar features emacs)
  #:use-module (cablecar packages emacs-xyz)
  #:export (feature-emacs-exwm))

(define* (feature-emacs-exwm)
  "Add and configure Emacs X Window Manager"
  (define emacs-f-name 'exwm)

  (define (get-home-services config)
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      '(
        (require 'exwm)
        (defun bp/run-in-background (command)
          (let ((command-parts (split-string command "[ ]+")))
            (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

        (defun exwm-async-run (name)
          "Run a process asynchronously"
          (interactive)
          (start-process name nil name))

        (defun bp/polybar-exwm-workspace ()
          (interactive)
          (pcase exwm-workspace-current-index
                 (0 "  Video")
                 (1 "  Term")
                 (2 "  Chat")
                 (3 "  Dev")
                 (4 "  Mail")
                 (5 "爵  Web")
                 (6 "  VCS")
                 (7 "  Music")
                 (8 "  Files")
                 (9 "  Streaming")))


        (defun bp/exwm-init-hook ()

          (modify-all-frames-parameters
           '((right-divider-width . 24)
             (alpha . (100 . 100))
             (mouse-color . "white")
             (internal-border-width . 24)))

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
          )

        (add-hook 'exwm-init-hook #'bp/exwm-init-hook)

        (setq exwm-input-prefix-keys
              '(?\C-x
                ?\C-u
                ?\C-h
                ?\M-x
                escape
                ?\M-`
                ?\M-&
                ?\M-:
                ?\s-o    ;;Allow org-capture to passthru in Xwindows
                ?\s-i    ;; Toggles char-mode/line-mode
                ?\C-\M-j ;; Buffer list
                ?\C-\ )) ;; Ctrl+Space

        (setq exwm-input-global-keys
              `(
                ;; Move between windows
                ([s-left] . windmove-left)
                ([s-right] . windmove-right)
                ([s-up] . windmove-up)
                ([s-down] . windmove-down)

                ;; ;; Launch applications via shell command
                ([?\s-\\] . (lambda (command)
                              (interactive (list (read-shell-command " ")))
                              (start-process-shell-command command nil command)))

                ;; Switch workspace
                ;; ([?\s-w] . exwm-workspace-switch)
                ;; ([?\s-w] . counsel-switch-buffer)
                ;;
                ;; move window workspace with SUPER+SHIFT+{0-9}
                ,@(cl-mapcar (lambda (c n)
                               `(,(kbd (format "s-%c" c)) .
                                 (lambda ()
                                   (interactive)
                                   (exwm-workspace-move-window ,n)
                                   ;; (exwm-workspace-switch ,n)
                                   )))
                             '(?! ?@ ?# ?$ ?% ?^ ?& ?* ?\( ?\))
                             ;; '(?\) ?! ?@ ?# ?$ ?% ?^ ?& ?* ?\()
                             (number-sequence 0 9))

                ;; Switch to window workspace with SUPER+{0-9}
                ([?\s-1] . (lambda () (interactive) (exwm-workspace-switch-create 0)))
                ([?\s-2] . (lambda () (interactive) (exwm-workspace-switch-create 1)))
                ([?\s-3] . (lambda () (interactive) (exwm-workspace-switch-create 2)))
                ([?\s-4] . (lambda () (interactive) (exwm-workspace-switch-create 3)))
                ([?\s-5] . (lambda () (interactive) (exwm-workspace-switch-create 4)))
                ([?\s-6] . (lambda () (interactive) (exwm-workspace-switch-create 5)))
                ([?\s-7] . (lambda () (interactive) (exwm-workspace-switch-create 6)))
                ([?\s-8] . (lambda () (interactive) (exwm-workspace-switch-create 7)))
                ([?\s-9] . (lambda () (interactive) (exwm-workspace-switch-create 8)))
                ([?\s-0] . (lambda () (interactive) (exwm-workspace-switch-create 9)))))

            (require 'desktop-environment)
            (desktop-environment-mode)
            (desktop-environment-brightness-small-increment "2%+")
            (desktop-environment-brightness-small-decrement "2%-")
            (desktop-environment-brightness-normal-increment "5%+")
            (desktop-environment-brightness-normal-decrement "5%-")


            (defun exwm-input-line-mode ()
              "Set exwm window to line-mode and show mode line"
              (call-interactively #'exwm-input-grab-keyboard))

            (defun exwm-input-char-mode ()
              "Set Exwm window to char-mode and hide mode line"
              (call-interactively #'exwm-input-release-keyboard))

            (defun exwm-input-toggle-mode ()
              "Toggle between line- and char-mode"
              (with-current-buffer (window-buffer)
                (when (eq major-mode 'exwm-mode)
                  (if (equal (nth 1 (nth 1 mode-line-process)) "line")
                      (exwm-input-char-mode)
                    (exwm-input-line-mode)))))

            (exwm-input-set-key (kbd "s-i")
                                (lambda () (interactive)
                                  (exwm-input-toggle-mode)))

            (exwm-input-set-key (kbd "s-o")
                                (lambda ()
                                  (interactive)
                                  (exwm-input-toggle-mode)
                                  (org-capture)))

        (exwm-enable)
        )
      #:elisp-packages (list
                        cablecar-emacs-exwm
                        emacs-desktop-environment))))

  (make-emacs-feature emacs-f-name
                      #:home-services get-home-services)
  )
