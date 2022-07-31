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
                     exec ~a --exit-with-session ~a \"$@\" -mm --debug-init -l ~/.config/emacs/init.el -fn iosevka-20 ~%"
                    (string-append (assoc-ref inputs "bash") "/bin/sh")
                    (string-append (assoc-ref inputs "xhost") "/bin/xhost")
                    (string-append (assoc-ref inputs "dbus") "/bin/dbus-launch")
                    (string-append (assoc-ref inputs "emacs") "/bin/emacs"))))
               (chmod exwm-executable #o555)
               #t))))))))
