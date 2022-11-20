(define-module (cablecar packages emacs-xyz)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages texinfo)
  #:use-module ((guix licenses) #:prefix license:))

(define-public cablecar-emacs-exwm
  (package
    (inherit emacs-exwm)
    (name "cablecar-emacs-exwm")
    (arguments
     `(#:emacs ,emacs-next
       #:phases
       (modify-phases %standard-phases
         (add-after 'build 'install-xsession
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (xsessions (string-append out "/share/xsessions"))
                    (bin (string-append out "/bin"))
                    (exwm-executable (string-append bin "/exwm")))
               ;; Add a .desktop file to xsessions
               (mkdir-p xsessions)
               (mkdir-p bin)
               (make-desktop-entry-file
                (string-append xsessions "/exwm.desktop")
                #:name "Emacs EXWM"
                #:exec exwm-executable
                #:try-exec exwm-executable)
               ;; Add a shell wrapper to bin
               (with-output-to-file exwm-executable
                 (lambda _
                   (format
                    #t
                    "#!~a ~@
                     ~a +SI:localuser:$USER ~@
                     exec ~a --exit-with-session ~a \"$@\" -mm --debug-init -l ~/.exwm -fn iosevka-20 ~%"
                    (search-input-file inputs "/bin/sh")
                    (search-input-file inputs "/bin/xhost")
                    (search-input-file inputs "/bin/dbus-launch")
                    (search-input-file inputs "/bin/emacs")
                    '(cond
                      ((file-exists-p "~/.exwm")
                       (load-file "~/.exwm"))
                      ((not (feature 'exwm))
                       (require 'exwm)
                       (require 'exwm-config)
                       (exwm-config-default)
                       (message (concat "exwm config not found. "
                                        "Falling back to default config.."))))
                    )))
               (chmod exwm-executable #o555)
               #t))))))))

(define-public emacs-exwm-out-gaps
  (package))
