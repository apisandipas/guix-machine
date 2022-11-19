(define-module (cablecar systems)
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
            %cablecar-generic-file-systems
            %cablecar-initial-os))

(define %cablecar-timezone "America/Chicago")
(define %cablecar-locale "en_US.utf8")

(define %cablecar-generic-file-systems
  (list
   (file-system
     (mount-point "/boot/efi")
     (device (file-system-label "EFI_PART"))
     (type "vfat"))
   (file-system
     (mount-point "/")
     (device
      (file-system-label "system-root"))
     (type "ext4"))
   (file-system
     (mount-point "/home")
     (device
      (file-system-label "system-home"))
     (type "ext4"))))

(define %cablecar-initial-os
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
