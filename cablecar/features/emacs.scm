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
        (defun bp/s-truncate (len s &optional ellipsis)
            "Like `s-truncate' but
            - return S when LEN is nil
            - return empty string when len is shorter than ELLIPSIS"
            (declare (pure t) (side-effect-free t))
            (let ((ellipsis (or ellipsis "...")))
                (cond
                ((null len) s)
                ((< len (length ellipsis)) "")
                (t (s-truncate len s ellipsis)))))

        (defun fw/doom-modeline-segment--buffer-info (orig-fn &rest args)
            "`doom-modeline-segment--buffer-info' but truncate for EXWM buffers."
            (bp/s-truncate
                (and (derived-mode-p 'exwm-mode)
                        (max 15 (- (window-width) 45)))
                (format-mode-line (apply orig-fn args))
                "..."))
        (advice-add 'doom-modeline-segment--buffer-info :around 'fw/doom-modeline-segment--buffer-info)
        (with-eval-after-load 'doom-modeline
            (setq doom-modeline-height 18
                 doom-modeline-bar-width 6
                 doom-modeline-lsp t
                 doom-modeline-github nil
                 doom-modeline-mu4e t
                 doom-modeline-irc t
                 doom-modeline-minor-modes nil
                 doom-modeline-persp-name nil
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
              `((require 'general)
                
                (defun bp/evil-shift-right ()
                    (interactive)
                    (evil-shift-right evil-visual-beginning evil-visual-end)
                    (evil-normal-state)
                    (evil-visual-restore))

                (defun bp/evil-shift-left ()
                    (interactive)
                    (evil-shift-left evil-visual-beginning evil-visual-end)
                    (evil-normal-state)
                    (evil-visual-restore))

                (evil-define-key 'visual global-map (kbd ">") 'bp/evil-shift-right)
                (evil-define-key 'visual global-map (kbd "<") 'bp/evil-shift-left)

                (general-def :states '(normal motion emacs) "SPC" nil)

                (general-evil-setup)

                (general-create-definer bp/leader-def
		  :keymaps '(normal visual emacs)
                  :prefix "SPC")

                (bp/leader-def
		    "SPC" 'find-file
                    "b" '(:ignore t :which-key "Buffers")
                    ;; Buffer-related bindings
                    "bw"   '(counsel-switch-buffer :which-key "Switch Buffer")
                    "bc"   '(clone-indirect-buffer-other-window :which-key "Clone indirect buffer other window")
                    "bk"   '(kill-current-buffer :which-key "Kill current buffer")
                    "bn"   '(next-buffer :which-key "Next buffer")
                    "bp"   '(previous-buffer :which-key "Previous buffer")
                    "bs"   '(save-buffer :which-key "Save buffer")
                    "bB"   '(ibuffer-list-buffers :which-key "Ibuffer list buffers")
                    "bK"   '(kill-buffer :which-key "Kill buffer")
                    "w" '(:ignore t :which-key "Windows")
                    ;; Window-related bindings
                    "wc"   '(evil-window-delete :which-key "Close window")
                    "wn"   '(evil-window-new :which-key "New window")
                    "ws"   '(evil-window-split :which-key "Horizontal split window")
                    "wv"   '(evil-window-vsplit :which-key "Vertical split window")
                    ;; Window motions
                    "wh"   '(evil-window-left :which-key "Window left")
                    "wj"   '(evil-window-down :which-key "Window down")
                    "wk"   '(evil-window-up :which-key "Window up")
                    "wl"   '(evil-window-right :which-key "Window right")
                    "ww"   '(evil-window-next :which-key "Goto next window")
                   )
                '()
                ))
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
                        (if leader? emacs-general)
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
    #:header-line-as-mode-line? #f
    ;; #:dark? #t
    )
   (feature-doom-modeline) ;; Weird zoom level issues cause gaps to behave strangely
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
    #:turn-on? #f)
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
    #:extra-early-init-el
    '(
      (eval-when-compile (require 'use-package))
      )
    #:extra-init-el
    '(
      (use-package general)

        ;; Make Default face a consistent size...
        (set-face-attribute 'default nil :height 203)
        (setq display-line-numbers-type 'relative)
        (column-number-mode)
        (global-display-line-numbers-mode t)
        (setq standard-indent 2)
        (setq-default indent-tabs-mode nil)

        (setq inhibit-startup-screen t)

        (display-time)
        (display-battery-mode)

        (setq display-line-numbers-type 'relative)
        (setq standard-indent 2)
        (setq-default indent-tabs-mode nil)

        (setq window-divider-default-right-width 24)
        (setq window-divider-default-bottom-width 1)
        (setq window-divider-default-places 'right-only)
        (window-divider-mode t)
        ;; Make sure new frames use window-divider
        (add-hook 'before-make-frame-hook 'window-divider-mode)

        (set-face-foreground 'window-divider "#212337");; TODO Change this color

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

      (defun bp/show-rofi ()
        (interactive)
        (bp/run-in-background "rofi -show drun"))

      (global-set-key (kbd "s-SPC") 'bp/show-rofi)

      )
    #:additional-elisp-packages
    (append
     (list emacs-consult-dir emacs-exwm-outer-gaps emacs-exwm-modeline)
     (pkgs "emacs-elfeed" "emacs-hl-todo"
           "emacs-ytdl"
           "emacs-ement"
           "emacs-use-package"
;;           "emacs-vertico-posframe"     ;
           "emacs-rainbow-mode"
           "emacs-counsel"
           "emacs-restart-emacs"
           "emacs-org-present")))))

