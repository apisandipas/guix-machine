(define-module (cablecar features emacs)
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
  #:use-module (cablecar utils)
  #:use-module (cablecar packages emacs-xyz)
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

(define* (rde-emacs-configuration-package
          name
          #:optional (elisp-expressions '())
          #:key
          summary authors url keywords commentary
          (elisp-packages '())
          (autoloads? #f))
  "Returns a package, which configures emacs.  Can be used as a
dependency for other packages."
  (let* ((configure-package
          (elisp-configuration-package
           (string-append "configure-" (symbol->string name))
           elisp-expressions
           #:elisp-packages elisp-packages
           #:autoloads? autoloads?
           #:summary summary
           #:commentary commentary
           #:keywords keywords
           #:url (or url "https://trop.in/rde")
           #:authors (or authors '("Andrew Tropin <andrew@trop.in>")))))
    configure-package))

(define* (rde-elisp-configuration-service
          name config
          #:optional (elisp-expressions '())
          #:key
          summary authors url keywords commentary
          (early-init '())
          (elisp-packages '()))
  "Adds a configure-NAME package to the profile and emacs load path and if
emacs-portable? rde value is present adds autoloads cookies to each expression
of it, otherwise adds a require to @file{init.el}."
  (let* ((pkg-name (symbol-append 'configure- name))
         (autoloads? (get-value 'emacs-portable? config))
         (require-in-init? (not autoloads?))
         (configure-package
          (rde-emacs-configuration-package
           name elisp-expressions
           #:summary summary
           #:commentary commentary
           #:keywords keywords
           #:url url
           #:authors authors
           #:elisp-packages elisp-packages
           #:autoloads? autoloads?)))
    (simple-service
     (symbol-append 'emacs- name '-configurations)
     home-emacs-service-type
     (home-emacs-extension
      (init-el (if require-in-init? `((require ',pkg-name)) '()))
      (early-init-el early-init)
      ;; It's necessary to explicitly add elisp-packages here, because
      ;; we want to overwrite builtin emacs packages.  Propagated
      ;; inputs have lowest priority on collisions, that's why we have
      ;; to list those package here in addition to propagated-inputs.
      (elisp-packages (append elisp-packages (list configure-package)))))))
;;;
;;; Emacs features.
;;;

(define emacs-configure-rde-keymaps
  (rde-emacs-configuration-package
   'rde-keymaps
   `((defvar rde-app-map nil "Prefix keymap for applications.")
     (define-prefix-command 'rde-app-map nil)
     (defvar rde-toggle-map nil "\
Prefix keymap for binding various minor modes for toggling functionalitty.")
     (define-prefix-command 'rde-toggle-map nil))
   #:summary "Keymaps inteded for reuse among configure-* packages"))

;; MAYBE: make handler to be actions instead of desktop entries?
(define* (emacs-xdg-service
          name xdg-name gexp
          #:key
          (default-for '())
          (exec-argument "%u"))
  (define file-name (string-append "emacs-" (symbol->string name)))
  (define file-file (file-append (program-file file-name gexp)
                                 (string-append " " exec-argument)))
  (define desktop-file (symbol-append 'emacs- name '.desktop))
  (simple-service
   (symbol-append 'emacs-xdg- name)
   home-xdg-mime-applications-service-type
   (home-xdg-mime-applications-configuration
    (default (map (lambda (m) (cons m desktop-file)) default-for))
    (desktop-entries
     (list
      (xdg-desktop-entry
       (file file-name)
       (name xdg-name)
       (config `((exec . ,file-file)
                 (icon . "emacs")))
       (type 'application)))))))

(define (expand-extra-elisp elisp config)
  "If ELISP is a list just return it, if it's a function call it with CONFIG
argument, throw an exception otherwise."
  (let ((res (if (procedure? elisp) (elisp config) elisp)))
    (ensure-pred list? res)
    res))

(define (feature-doom-modeline)
  "Adds and configures the Doom modeline"
  (define emacs-f-name 'doom-modeline)

  (define (get-home-services config)
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `(
        (require 'doom-modeline)
        (doom-modeline-mode)
        (with-eval-after-load 'doom-modeline
            (setq doom-modeline-height 36
                doom-modeline-bar-width 6
                doom-modeline-lsp t
                doom-modeline-github nil
                doom-modeline-mu4e t
                doom-modeline-irc t
                doom-modeline-minor-modes nil
                doom-modeline-persp-name t
                doom-modeline-buffer-file-name-style 'truncate-except-project
                doom-modeline-major-mode-icon t))
        )
      #:elisp-packages (list emacs-doom-modeline))))

  (make-emacs-feature emacs-f-name
                      #:home-services get-home-services))


(define* (feature-emacs-evil
          #:key
          (no-insert-state-message? #t)
          (leader? #t)
          (undo-fu? #t)
          (commentary? #t)
          (collection? #f)
          (surround? #t))
  "Add and configure evil-mode for Emacs."
  (ensure-pred boolean? no-insert-state-message?)
  (ensure-pred boolean? leader?)
  (ensure-pred boolean? undo-fu?)
  (ensure-pred boolean? collection?)
  (ensure-pred boolean? surround?)
  (define emacs-f-name 'evil)

  (define (get-home-services config)
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `( ;; Make the Escape key behave more nicely for evil-mode
        (global-set-key (kbd "<escape>") 'keyboard-quit)
        (define-key query-replace-map (kbd "<escape>") 'quit)
        ;; Hide ``-- INSERT --'' message
        ,@(if no-insert-state-message?
              `((setq evil-insert-state-message nil))
              '())
        ;; Required by the additional packages... should we toggle this?
        (setq evil-want-keybinding nil)
        ;; Use C-u to scroll up
        (setq evil-want-C-u-scroll t)
        ;; undo with higher granularity
        (setq evil-want-fine-undo t)
        ;; The packages below must be loaded and configured in a certain order
        (require 'evil)
        ,@(if leader?
              `((require 'evil-leader)
                (global-evil-leader-mode)
                (evil-leader/set-leader "<SPC>")
                (evil-leader/set-key
                 "<SPC>" 'find-file
                 "b" 'switch-to-buffer
                 "k" 'kill-buffer
                 "K" 'kill-this-buffer
                 "s" 'save-buffer
                 "S" 'evil-write-all
                 )
                '()))
        ,@(if undo-fu?
              `((eval-when-compile (require 'undo-fu))
                (setq evil-undo-system 'undo-fu)
                (define-key evil-normal-state-map (kbd "u") 'undo-fu-only-undo)
                (define-key evil-normal-state-map (kbd "C-r") 'undo-fu-only-redo))
              '())
        (evil-mode 1)
        ,@(if commentary?
              `((require 'evil-commentary)
                (evil-commentary-mode))
              '())
        ;; ,@(if collection?
        ;;       `((when (require 'evil-collection nil t)
        ;;           (evil-collection-init)))
        ;;       '())
        ,@(if surround?
              `((require 'evil-surround)
                (global-evil-surround-mode 1))
              '())
        )
      #:elisp-packages (list
                        emacs-evil
                        (if leader? emacs-evil-leader)
                        (if undo-fu? emacs-undo-fu)
                        (if commentary? emacs-evil-commentary)
                        ;; (if collection? emacs-evil-collection)
                        (if surround? emacs-evil-surround)))))

  (make-emacs-feature emacs-f-name
                      #:home-services get-home-services)
  )

(define %cablecar-base-emacs-packages
  (list
   (feature-emacs-evil)
   (feature-emacs-appearance
    #:header-line-as-mode-line? #t
    #:dark? #t)
   ;; (feature-doom-modeline) ;; Weird zoom level issues cause gaps to behave strangely
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
   (feature-emacs-monocle)
   (feature-emacs
    #:extra-init-el
    '(
      (setq inhibit-startup-screen t)
      (display-time)
      (display-battery-mode)

      (defun bp/make-frame-pretty ()
        "Set the initial look and feel of the frame"
        (modify-all-frames-parameters
         '((right-divider-width . 24)
           (alpha . (85 . 75))
           (mouse-color . "white")
           (internal-border-width . 24))))

      (add-hook 'before-make-frame-hook 'bp/make-frame-pretty)

      (setq initial-frame-alist
            '((right-divider-width . 24)
              (fullscreen . maximized)
              (alpha . (85 . 75))
              (internal-border-width. 24)))

      (add-to-list 'default-frame-alist '(internal-border-width . 24))
      (add-to-list 'default-frame-alist '(alpha . (85 . 75)))
      (add-to-list 'default-frame-alist '(right-divider-width . 24))
      (add-to-list 'default-frame-alist '(fullscreen . maximized))
        
      ;; alias y/n to yes/no
      (defalias 'yes-or-no-p 'y-or-n-p)

      ;; Default to insert mode in vterm
      (with-eval-after-load 'evil
                            (evil-set-initial-state 'vterm-mode 'insert))


      )
    #:additional-elisp-packages
    (append
     (list emacs-consult-dir emacs-exwm-outer-gaps emacs-exwm-modeline)
     (pkgs "emacs-elfeed" "emacs-hl-todo"
           "emacs-ytdl"
           "emacs-ement"
           ;; "emacs-vertico-posframe" ;; will require custom package input
           ;; "emacs-doom-modeline"
           "emacs-counsel"
           "emacs-restart-emacs"
           "emacs-org-present")))))

