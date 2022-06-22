(define-module (cablecar systems norrin)
  #:use-module (cablecar  utils)
  #:use-module (cablecar systems)
  #:use-module (rde features system)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu system file-systems))

;;; System-specific configurations

(define generic-file-systems
  (list (file-system
          (mount-point "/boot/efi")
          (device (file-system-label "EFI_PART"))
          (type "vfat"))
         (file-system
          (mount-point "/")
          (device
           (file-system-label "root_partition"))
          (type "ext4"))
         (file-system
          (mount-point "/home")
          (device
           (file-system-label "home_partition"))
          (type "ext4"))))

(define-public %system-features
  (list (feature-host-info
     #:host-name "norrin"
     #:timezone  "America/Chicago")
   (feature-kernel
    #:kernel linux
    #:initrd microcode-initrd
    #:firmware (list linux-firmware))
    (feature-file-systems
     #:file-systems generic-file-systems)))
