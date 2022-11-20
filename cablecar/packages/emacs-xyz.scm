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
                     exec ~a --exit-with-session ~a \"$@\" -mm --debug-init -l ~/.config/emacs/init.el -fn iosevka-20 --eval '~s' ~%"
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

(define-public emacs-exwm-outer-gaps
  (package
    (name "emacs-exwm-outer-gaps")
    (version "0.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lucasgruss/exwm-outer-gaps")
             (commit "e9026b1e627e0465e2f4d29ded5ef8cf4a2017b9")))
       (file-name (git-file-name name version))
       (sha256
        (base32 ""))))
    (build-system emacs-build-system)
    (propagated-inputs
     (list cablecar-emacs-exwm))
    (home-page
     "https://github.com/lucasgruss/exwm-outer-gaps")
    (synopsis "Adds (useless) outer gaps to EXWM")
    (description
     "@code{exwm-outer-gaps} A lot of window managers provide a feature
called “gaps”. It is useless, it is pretty but most importantly it
is a feature. And therefore Emacs must have it. Sorry, I don’t make the rules.")
    (license license:gpl3+)))
