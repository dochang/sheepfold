(define-module (sheepfold packages wm)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages pkg-config)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (gnu packages xorg)
  #:use-module ((guix licenses) #:prefix license:))

(define-public wmdocker
  (package
    (name "wmdocker")
    (version "1.5.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mdomlop/wmdocker")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32 "1y51wpv46bsg0r10l62fvxsn8hpl89fdrfczvcxl439wa5y9m7gw"))))
    (build-system gnu-build-system)
    (inputs
     (list glib
           libx11))
    (native-inputs
     (list pkg-config))
    (arguments
     (list
      #:tests? #f
      #:make-flags
      #~(list (string-append "CC=" #$(cc-for-target))
              (string-append "PREFIX=" #$output))
      #:phases
      '(modify-phases %standard-phases
         (delete 'configure)
         (add-after 'install 'post-install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (simple-format #f "~A/share/doc/~A/"
                                        out (strip-store-file-name out))))
               (for-each (lambda (d)
                           (delete-file-recursively
                            (string-append %output "/share/" d)))
                         '("licenses" "doc"))
               (for-each (lambda (f)
                           (install-file f doc))
                         '("README.md" "THANKS" "ChangeLog" "AUTHORS"))))))))
    (home-page "https://github.com/mdomlop/wmdocker")
    (synopsis "Docking System Tray")
    (description "WMDocker is a docking application (WindowMaker dock app)
which acts as a system tray for KDE3 and GNOME2.  It can be used to replace
the panel in either environment, allowing you to have a system tray without
running the KDE/GNOME panel.")
    (license license:gpl2+)))
