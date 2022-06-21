;; This module is responsible for configuring an operating system,
;; i.e. kernel, microcode, hostname, keyboard layout, etc.
;;
;; Base packages, services and other features should be defined in
;; cablecar/configs, or in one of the custom configs at cablecar/configs/.
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

            %cablecar-timezone
            %cablecar-locale
            %cablecar-kernel-arguments
            %cablecar-keyboard-layout
            %cablecar-initial-os
            %cablecar-system-base-features
            ))


(define-public %cablecar-timezone "America/Chicago")
(define-public %cablecar-locale "en_US.utf8")

(define-public %cablecar-kernel-arguments
  (list "modprobe.blacklist=pcspkr,snd_pcsp"
        "quiet"))

(define-public %cablecar-keyboard-layout
  (keyboard-layout "us" "qwerty"
                   #:options
                   '("ctrl:nocaps")))

(define-public %cablecar-initial-os
  (operating-system
   (host-name "cablecar")
   (locale  %cablecar-locale)
   (timezone  %cablecar-timezone)
   (kernel linux)
   (firmware (list linux-firmware))
   (initrd microcode-initrd)
   (kernel-arguments %cablecar-kernel-arguments)
   (keyboard-layout %cablecar-keyboard-layout)
   (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (targets '("/boot/efi"))))
   (services '())
   (file-systems %base-file-systems)
   (issue "This is the GNU/Linux+Cablecar system. Welcome.\n")))

(define-public %cablecar-system-base-features
  (list
   (feature-keyboard
    #:keyboard-layout %cablecar-keyboard-layout)))
