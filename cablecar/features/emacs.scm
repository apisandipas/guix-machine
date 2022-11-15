(define-module (cablecar features emacs)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (gnu home services)
  #:use-module (gnu home-services emacs)
  #:use-module (gnu home-services wm)
  #:use-module (gnu home services xdg)
  #:use-module (gnu services)

  #:use-module (rde packages)
  #:use-module (rde packages emacs)
  #:use-module (rde packages emacs-xyz)
  #:use-module (rde features emacs)
  #:use-module (rde features terminals)

  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages mail)

  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix transformations)
  #:use-module (engstrand features emacs)
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


;; (define* (feature-emacs-exwm)
;;   "Add and configure EXWM for Emacs."
;;   (define emacs-f-name 'exwm)

;;   (define (get-home-services config)
;;     (list
;;      (rde-elisp-configuration-service
;;       emacs-f-name
;;       config
;;       `((require 'exwm)
;;         ;; (defun bp/run-in-background (command)
;;         ;;   (let ((command-parts (split-string command "[ ]+")))
;;         ;;     (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

;;         (defun exwm-async-run (name)
;;           "Run a process asynchronously"
;;           (interactive)
;;           (start-process name nil name))

;;         (defun bp/exwm-init-hook ()

;;           (modify-all-frames-parameters
;;            '((right-divider-width . 24)
;;              (alpha . (85 . 85))
;;              (mouse-color . "white")
;;              (internal-border-width . 24)))
;;           )
;;         (exwm-enable)
;;         )
;;       #:elisp-packages (list
;;                         cablecar-emacs-exwm emacs-desktop-environment))))

;;   (make-emacs-feature emacs-f-name
;;                       #:home-services get-home-services))


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

(define emacs-configure-rde-keymaps
  (rde-emacs-configuration-package
   'rde-keymaps
   `((defvar rde-app-map nil "Prefix keymap for applications.")
     (define-prefix-command 'rde-app-map nil)
     (defvar rde-toggle-map nil "\
Prefix keymap for binding various minor modes for toggling functionalitty.")
     (define-prefix-command 'rde-toggle-map nil))
   #:summary "Keymaps intended for reuse among configure-* packages"))

(define* (feature-emacs-cablecar
          #:key
          (emacs emacs-next)
          (emacs-server-mode? #t)
          (additional-elisp-packages '())
          (extra-init-el '())
          (extra-early-init-el '())
          (default-terminal? #t))
  "Setup and configure GNU Emacs."
  (ensure-pred boolean? emacs-server-mode?)
  (ensure-pred list-of-elisp-packages? additional-elisp-packages)
  (ensure-pred any-package? emacs)

  (define emacs-client (file-append emacs "/bin/emacsclient"))
  (define emacs-client-create-frame
    (program-file "emacs-client-create-frame"
                  #~(apply system*
                           #$(file-append emacs "/bin/emacsclient")
                           "--create-frame"
                           (cdr (command-line)))))
  (define emacs-client-no-wait
    (program-file "emacs-client-no-wait"
                  #~(apply system*
                           #$(file-append emacs "/bin/emacsclient")
                           "--no-wait"
                           (cdr (command-line)))))
  (define emacs-editor
    (program-file "emacs-editor"
                  #~(apply system*
                           #$(file-append emacs "/bin/emacs")
                           "--no-splash"
                           (cdr (command-line)))))

  (define (emacs-home-services config)
    "Returns home services related to GNU Emacs."
    (require-value 'full-name config)
    (require-value 'email config)
    (let* ((full-name (get-value 'full-name config))
           (email     (get-value 'email config)))
      (list
       (emacs-xdg-service 'emacs-q "Emacs (No init: -q)"
                          #~(system* "emacs" "-q"))
       (emacs-xdg-service 'emacs-Q "Emacs (No init, no site-lisp: -Q)"
                          #~(system* "emacs" "-Q"))

       (rde-elisp-configuration-service
        'rde-emacs
        config
        `((require 'configure-rde-keymaps)

          (setq user-full-name ,full-name)
          (setq user-mail-address ,email)

          ,#~""
          (setq custom-file
                (concat (or (getenv "XDG_CACHE_HOME") "~/.cache")
                        "/emacs/custom.el"))
          (load custom-file t)

          (customize-set-variable
           'backup-directory-alist
           `(,(cons "." (concat (or (getenv "XDG_CACHE_HOME") "~/.cache")
                                "/emacs/backup"))))

          (customize-set-variable
           'recentf-save-file
           (concat (or (getenv "XDG_CACHE_HOME") "~/.cache")
                   "/emacs/recentf"))

          ;; (add-hook 'after-init 'recentf-mode)
          (recentf-mode 1)
          (run-with-idle-timer 30 t 'recentf-save-list)

          ;; (customize-set-variable 'history-length 10000)
          (customize-set-variable
           'savehist-file (concat (or (getenv "XDG_CACHE_HOME") "~/.cache")
                                  "/emacs/history"))

          (savehist-mode 1)
          (run-with-idle-timer 30 t 'savehist-save)

          (customize-set-variable
           'bookmark-default-file
           (concat (or (getenv "XDG_CACHE_HOME") "~/.cache")
                   "/emacs/bookmarks"))

          ,#~""
          ;; (pixel-scroll-precision-mode 1)
          (column-number-mode 1)
          (save-place-mode 1)
          ;; MAYBE: Make it buffer local?
          (show-paren-mode 1)
          ,#~";; Treat camelCased parts as separate words."
          (subword-mode 1)
          ;; (add-hook 'prog-mode-hook 'subword-mode)

          (setq-default indent-tabs-mode nil)
          (setq save-interprogram-paste-before-kill t)
          (setq mouse-yank-at-point t)
          (setq require-final-newline t)

          (defun rde-whitespace-mode ()
            "Equivalent of `whitespace-mode', but highlights only tabs."
            (interactive)
            (if (and (featurep 'whitespace) whitespace-mode)
                (whitespace-mode 0)
                (let ((whitespace-style '(face tabs)))
                  (whitespace-mode 1))))
          (add-hook 'prog-mode-hook
                    (lambda ()
                      (rde-whitespace-mode)
                      (setq show-trailing-whitespace t)))

          ;; Highlight zero-width whitespaces and other glypless characters.
          (set-face-background 'glyphless-char "red")
          ,#~""
          (define-key global-map (kbd "C-=") 'er/expand-region)

          ,#~""
          (defun rde-display-load-time ()
            (interactive)
            (message "\
rde emacs loaded in %s, C-h r i for search in emacs manual by topic. \
C-h C-a to open About Emacs buffer."
                     (emacs-init-time)))

          (defun display-startup-echo-area-message ()
            (rde-display-load-time))

          ,#~""
          ;; TODO: Move it to feature-isearch
          (setq search-whitespace-regexp ".*?")

          ,#~""
          ;; TODO: Move it to feature-compile
          (defun rde-compilation-colorizer ()
            "Prevent color escape sequences to popup in compilation buffer."
            (ansi-color-apply-on-region (point-min) (point-max)))
          (add-hook 'compilation-filter-hook 'rde-compilation-colorizer)

          (dolist (mode-hook '(prog-mode-hook compilation-mode-hook))
                  (add-hook mode-hook (lambda () (setq truncate-lines t))))
          (setq compilation-scroll-output 'first-error)
          (define-key global-map (kbd "s-r") 'recompile)

          ,#~""
          (define-key global-map (kbd "s-b") 'switch-to-buffer)
          (define-key global-map (kbd "s-w") 'kill-current-buffer)
          (define-key global-map (kbd "s-W") 'kill-buffer-and-window)
          (define-key global-map (kbd "s-o") 'other-window)

          ,#~""
          ,@(if (get-value 'emacs-advanced-user? config)
                '((put 'narrow-to-page   'disabled nil)
                  (put 'narrow-to-region 'disabled nil))
                '())

          ,#~""
          ;; TODO: Extend info-lookup-alist with Guix Manual nodes to
          ;; make `C-h S' find guix services and other items.
          (eval-when-compile (require 'guix))
          (define-key rde-toggle-map (kbd "p") 'guix-prettify-mode)
          (define-key rde-toggle-map (kbd "P") 'global-guix-prettify-mode)
          (global-guix-prettify-mode 1)

          ,#~""
          (eval-when-compile
           (require 'time))
          (setq world-clock-list
                '(("America/Los_Angeles" "Los Angeles")
                  ("America/Boise" "Boise")
                  ("America/New_York" "New York")
                  ("UTC" "UTC")
                  ("Europe/London" "London")
                  ("Europe/Paris" "Paris")
                  ("Europe/Helsinki" "Helsinki")
                  ("Europe/Moscow" "Moscow")
                  ("Asia/Tokyo" "Tokyo")))
          (define-key rde-app-map (kbd "w") 'world-clock)

          ;; TODO: Move to feature-sane-bindings
          (let ((map goto-map))
            (define-key map "L" 'find-library)
            (define-key map "F" 'find-function)
            (define-key map "K" 'find-function-on-key)
            (define-key map "V" 'find-variable))

          (defun rde-kill-region-dwim (&optional count)
            "The function kills region if mark is active, otherwise kills a word.
Prefix argument can be used to kill a few words."
            (interactive "p")
            (if (use-region-p)
                (kill-region (region-beginning) (region-end) 'region)
                (backward-kill-word count)))

          ;; (define-key global-map (kbd "C-h") 'backward-delete-char-untabify)
          (define-key global-map (kbd "M-K") 'kill-whole-line)
          (define-key global-map (kbd "M-c") 'capitalize-dwim)
          (define-key global-map (kbd "M-l") 'downcase-dwim)
          (define-key global-map (kbd "M-u") 'upcase-dwim)
          (define-key global-map (kbd "C-w") 'rde-kill-region-dwim)

          (define-key mode-specific-map (kbd "a")
            '("rde applications" . rde-app-map))
          (define-key mode-specific-map (kbd "t")
            '("rde toggles" . rde-toggle-map)))
        #:summary "General settings, better defaults"
        #:commentary "\
It can contain settings not yet moved to separate features."
        #:keywords '(convenience)
        #:elisp-packages (list (get-value 'emacs-configure-rde-keymaps config)
                               emacs-expand-region emacs-guix))

       (service
        home-emacs-service-type
        (home-emacs-configuration
         (package emacs)
         (elisp-packages additional-elisp-packages)
         (server-mode? emacs-server-mode?)
         (xdg-flavor? #t)
         (init-el extra-init-el)
         (early-init-el
          `(,(slurp-file-like (local-file "./emacs/early-init.el"))
            ,@extra-early-init-el))
;;; TODO: Rebuilding packages with emacs will be useful for
;;; native-comp, but for some reason dash.el fails to build,
;;; need to investigate the issue.
         ;; (rebuild-elisp-packages? #t)
         ))

       (simple-service 'emacs-set-default-editor
                       home-environment-variables-service-type
                       `(("ALTERNATE_EDITOR" . ,emacs-editor)
                         ("VISUAL" . ,emacs-client-no-wait)))

       )))

  (feature
   (name 'emacs)
   (values (append
            (make-feature-values
             emacs
             emacs-editor emacs-client
             emacs-client-create-frame
             emacs-client-no-wait
             emacs-configure-rde-keymaps
             emacs-server-mode?)
            (if default-terminal?
                `((default-terminal . ,emacs-client-create-frame))
                '())))
   (home-services-getter emacs-home-services)))

(define %cablecar-base-emacs-packages
  (list
   (feature-emacs-cablecar
    #:extra-init-el '( ;; no fringes
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
                      ;; (setq mouse-wheel-progressive-speed nil)
                      ;; (setq mouse-wheel-scroll-amount '(3))
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
                      (setq org-confirm-babel-evaluate nil)

                      ;; wmctrl responds "Name:..." with name of running window manager
                      ;; or "Cannot get window manager..." if there isn't one
                      (when (and (equal window-system 'x)
                                 (string= (substring (shell-command-to-string "wmctrl -m")
                                                     0 1)
                                          "C"))
                        (require 'exwm)
                        (require 'exwm-config)

                        ;; delete the following line and replace with your exwm configuration
                        (exwm-config-example)
                        )
                      )
    #:additional-elisp-packages
    (append
     (list emacs-consult-dir)
     (pkgs "emacs-elfeed" "emacs-hl-todo"
           "emacs-ytdl"
           "emacs-ement"
           "emacs-restart-emacs"
           "emacs-org-present")))
   ;; (feature-emacs-exwm)
   (feature-emacs-evil)
   (feature-emacs-appearance
    #:dark? #t)
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
