(define-module (cablecar reconfigure)
  #:use-module (ice-9 match)
  #:use-module (gnu services)
  #:use-module (gnu services desktop)
  #:use-module (gnu services sddm)
  #:use-module (gnu system keyboard)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system mapped-devices)

  #:use-module (gnu packages)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages fonts)

  #:use-module (guix gexp)
  #:use-module (guix inferior)
  #:use-module (guix channels)

  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)

  #:use-module (rde features)
  #:use-module (rde features predicates)

  #:use-module (rde features base)
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

  #:use-module (gnu packages)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)

  #:use-module (rde packages)
  #:use-module (rde packages emacs)
  #:use-module (rde packages emacs-xyz)

  #:use-module (cablecar systems)
  #:export (make-config))

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

;; Finds a list of needed user supplementary groups for feature with
;; a value of name. Returns an empty list if no groups are found.
(define (get-feature-kernel-arguments name config)
  (let ((arguments (get-value name config)))
    (if arguments arguments '())))


(define* (make-config
          #:key
          (user (getenv "RDE_USER"))
          (system (gethostname))
          (target (getenv "RDE_TARGET")))

  ;; (ensure-pred string? user)
  ;; (ensure-pred string? system)
  ;; (ensure-pred operating-system? initial-os)

  (define %user-features (dynamic-load 'configs user '%user-features))
  (define %system-features (dynamic-load 'systems system '%system-features))

  (define-public %generated-config
    (rde-config
     (features
      (append
       %user-features
       ;; %cablecar-base-features
       %system-features))))


  (define %cablecar-home-environment
    (rde-config-home-environment %generated-config))

  (define %cablecar-system
    (rde-config-operating-system %generated-config))


  (match target
    ("home" %cablecar-home-environment)
    ("system" %cablecar-system)
    (_ %cablecar-home-environment)))


(make-config #:user "bryan")
