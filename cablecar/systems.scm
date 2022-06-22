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
  ;; #:export (
  ;;           %cablecar-timezone
  ;;           %cablecar-locale
  ;;           %cablecar-initial-os)
  )

(define-public %cablecar-timezone "America/Chicago")
(define-public %cablecar-locale "en_US.utf8")

(define-public %cablecar-initial-os
  (operating-system
    (host-name "cablecar")
    (kernel linux)
    (locale  %cablecar-locale)
    (timezone %cablecar-timezone)
    (firmware (list linux-firmware))
    (initrd microcode-initrd)
    (bootloader (bootloader-configuration
                 (bootloader grub-efi-bootloader)
                 (targets '("/boot/efi"))))
    (services '())
    (file-systems %base-file-systems)
    (issue "This is the GNU/Linux+Cablecar system. Welcome.\n")))
;;; System-specific configurations
