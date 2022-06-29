(define-module (cablecar features emacs)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features terminals)
  #:use-module (rde packages emacs)
  #:use-module (rde packages emacs-xyz)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (engstrand features emacs)
  #:use-module (cablecar utils)
  #:export (%cablecar-base-emacs-packages))

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


;; (define* (feature-emacs-exwm)
;;   "Add and configure EXWM for Emacs."
;;   (define emacs-f-name 'exwm)

;;   (define (get-home-services config)
;;     (list
;;      (rde-elisp-configuration-service
;;       emacs-f-name
;;       config
;;       `((require 'exwm)
;;         (defun bp/run-in-background (command)
;;           (let ((command-parts (split-string command "[ ]+")))
;;             (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

;;         (defun exwm-async-run (name)
;;           "Run a process asynchronously"
;;           (interactive)
;;           (start-process name nil name))

;;         (defun bp/exwm-init-hook ()

;;           (modify-all-frames-parameters
;;            '((right-divider-width . 24)
;;              (alpha . (100 . 100))
;;              (mouse-color . "white")
;;              (internal-border-width . 24)))
;;           )
;;         (exwm-enable)
;;         )
;;       #:elisp-packages (list
;;                         emacs-exwm emacs-desktop-environment))))

;;   (make-emacs-feature emacs-f-name
;;                       #:home-services get-home-services))



(define %cablecar-base-emacs-packages
  (list
   (feature-emacs
    #:emacs emacs-next-pgtk-latest
    #:extra-init-el '(;; no fringes
                      (fringe-mode 0)
                      ;; do not open the Emacs welcome screen when we pass an
                      ;; existing file as a command-line argument
                      (defun my-inhibit-startup-screen-file ()
                        (ignore
                         (setq inhibit-startup-screen
                               (file-exists-p
                                (expand-file-name argi command-line-default-directory)))))
                      (add-hook 'command-line-functions 'my-inhibit-startup-screen-file)
                      ;; ignore warnings from native-comp
                      ;; (setq native-comp-async-report-warnings-errors nil)
                      ;; Relative line numbers, but only when relevant
                      (setq-default display-line-numbers-type 'relative)
                      (add-hook 'prog-mode-hook 'display-line-numbers-mode)
                      ;; Olivetti mode when working with text
                      (add-hook 'text-mode-hook 'olivetti-mode)
                      ;; Nicer mouse scrolling
                      (setq mouse-wheel-progressive-speed nil)
                      (setq mouse-wheel-scroll-amount '(3))
                      ;; Configure the look of  tabs
                      (setq tab-bar-close-button-show nil
                            tab-bar-new-button-show nil
                            tab-bar-new-tab-choice "*scratch*")
                      ;; Move to future C feature
                      (setq c-default-style "linux")
                      (add-hook 'c-mode-common-hook '(lambda () (setq indent-tabs-mode t)))
                      ;; Delete whitespace from indentations immediately
                      (setq backward-delete-char-untabify-method 'hungry)
                      ;; Transparency - laggy when window large
                      ;; (add-to-list 'default-frame-alist '(alpha 93 . 93))
                      ;; Clean up white space
                      (add-hook 'before-save-hook 'whitespace-cleanup)
                      ;; Allow execution of src blocks without asking
                      (setq org-confirm-babel-evaluate nil))
    #:additional-elisp-packages
    (append
     (list emacs-consult-dir)
     (pkgs "emacs-elfeed" "emacs-hl-todo"
           "emacs-ytdl"
           "emacs-ement"
           "emacs-restart-emacs"
           "emacs-org-present")))
   ;;(feature-emacs-exwm)
   (feature-emacs-evil)
   (feature-emacs-appearance)
   (feature-emacs-faces)
   (feature-emacs-completion)
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
   (feature-emacs-org
    #:org-directory "~/docs/notes")
   (feature-emacs-org-agenda
    #:org-agenda-files '("~/docs/agenda/todo.org"))
   ;; (feature-emacs-smartparens
   ;;  #:show-smartparens? #t)
   (feature-emacs-monocle)))
