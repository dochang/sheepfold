(define-module (sheepfold packages xdisorg)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages pkg-config)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module ((gnu packages xdisorg) #:prefix gnu:)
  #:use-module ((guix licenses) #:prefix license:))

;; (name "unclutter-xfixes")
(define-public unclutter-xfixes
  (package/inherit gnu:unclutter-xfixes
    (version "1.6")
    (source (origin
              (inherit (package-source gnu:unclutter-xfixes))
              (uri (git-reference
                    (inherit (origin-uri (package-source gnu:unclutter-xfixes)))
                    (commit (string-append "v" version))))
              (file-name (git-file-name (package-name gnu:unclutter-xfixes) version))
              (sha256
               (base32 "1mqir7imiiyl7vrnnnid80kb14fh78acrkffcm3z1l3ah9madqmj"))))))

;; (name "xss-lock")
(define-public xss-lock
  (let ((version "0.3.0")
        (revision "2")
        (commit "0c562bfa5d9073c6a47060ee06946aa39eff2fae"))
    (package/inherit gnu:xss-lock
      (version (git-version version revision commit))
      (source (origin
                (inherit (package-source gnu:xss-lock))
                (uri (git-reference
                      (url "https://github.com/wavexx/xss-lock")
                      (commit commit)))
                (file-name (git-file-name (package-name gnu:xss-lock) version))
                (sha256
                 (base32 "14mm6ra1hgwixvfzkd26r9flqp4nrx25fmwnm2qjpg7m9zhg6663"))))
      (home-page "https://github.com/wavexx/xss-lock"))))

(define-public warpd
  (let ((version "1.3.5")
        (revision "1")
        (commit "effea402ec8668bb2bc5c5117800d16d0d9d349a"))
    (package
      (name "warpd")
      (version (git-version version revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/rvaiya/warpd.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32 "1ynsssa5p3hchyj1vf280f88zckacd9zwckv9fvjcr8jvvjd0x30"))))
      (build-system gnu-build-system)
      (inputs
       (list libxi
             libxinerama
             libxft
             libxfixes
             libxtst
             libx11
             cairo
             libxkbcommon
             wayland))
      (native-inputs
       (list pkg-config))
      (arguments
       (list
        #:modules '((ice-9 popen)
                    (ice-9 rdelim)
                    (guix build utils)
                    (guix build gnu-build-system))
        #:make-flags
        #~(list (string-append "PREFIX=" %output)
                (string-append "CC=" #$(cc-for-target)))
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'setenv
              (lambda _
                (let ((ft-flags
                       (read-line
                        (open-pipe* OPEN_READ
                                    #$(pkg-config-for-target)
                                    "--cflags" "--libs" "freetype2"))))
                  (setenv "CFLAGS" (string-append ft-flags " -O2"))
                  ;; `-O2' is for the follwing warning:
                  ;;
                  ;; warning: #warning _FORTIFY_SOURCE requires compiling with optimization (-O) [-Wcpp]
                  )))
            (delete 'bootstrap)
            (delete 'configure)
            (delete 'check))))
      (home-page "https://github.com/rvaiya/warpd")
      (synopsis "A modal keyboard-driven virtual pointer")
      (description "A modal keyboard driven interface for mouse manipulation.")
      (license license:expat))))
