(define-module (cablecar systems)
  #:use-module (rde features)
  #:use-module (rde features base)
  #:use-module (rde features system)
  #:use-module (rde features keyboard)
  #:use-module (gnu system)
  #:use-module (gnu system keyboard)
  #:use-module (gnu system file-systems)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)
  #:export (
            %cablecar-timezone
            %cablecar-locale
            %cablecar-kernel-arguments
            %cablecar-keyboard-layout
            %cablecar-initial-os
            %cablecar-system-base-features
           ))


(define %cablecar-timezone "America/Chicago")
(define %cablecar-locale "en_US.utf8")

(define %cablecar-kernel-arguments
  (list "modprobe.blacklist=pcspkr,snd_pcsp"
        "quiet"))

(define %cablecar-keyboard-layout
  (keyboard-layout "us"))

;; (define %cablecar-initial-os
;;   (operating-system
;;    (host-name "cablecar")
;;    (locale  %cablecar-locale)
;;    (timezone  %cablecar-timezone)
;;    (kernel linux)
;;    (firmware (list linux-firmware))
;;    (initrd microcode-initrd)
;;    (kernel-arguments %cablecar-kernel-arguments)
;;    (keyboard-layout %cablecar-keyboard-layout)
;;    (bootloader (bootloader-configuration
;;                 (bootloader grub-efi-bootloader)
;;                 (targets '("/boot/efi"))))
;;    (services '())
;;    (file-systems %base-file-systems)
;;    (issue "This is the GNU/Linux+Cablecar system. Welcome.\n")))

;; (define %cablecar-system-base-features
;;   (list
;;    (feature-keyboard
;;     #:keyboard-layout %cablecar-keyboard-layout)))
