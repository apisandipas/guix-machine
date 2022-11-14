(define-module (cablecar reconfigure)
  #:use-module (rde features)
  #:use-module (gnu services)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (ice-9 match)
  #:use-module (ice-9 exceptions)
  #:use-module (cablecar configs)
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
