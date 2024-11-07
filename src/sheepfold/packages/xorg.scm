(define-module (sheepfold packages xorg)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages perl)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:))

(define-public keynav
  (package
    (name "keynav")
    (version "0.20220824.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/jordansissel/keynav")
                    (commit "28a1ba9a045c62a9d2bc5c3474a66d96c8bf5c32")))
              (file-name (git-file-name name version))
              (sha256
               (base32 "0imx64zzxsn61l33zzl481z5g54l6dpwzbq431amh163lymqv0yb"))))
    (build-system gnu-build-system)
    (inputs
     (list libx11
           libxext
           libxinerama
           libxrandr
           glib
           cairo
           xdotool))
    (native-inputs
     (list pkg-config
           perl
           xorg-server)) ;; For Xvfb in test.sh
    (arguments
     (list
      #:make-flags
      #~(list (string-append "PREFIX=" %output)
              (string-append "CC=" #$(cc-for-target)))
      #:modules `(,@%default-gnu-imported-modules
                  (ice-9 match))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'setenv
            (lambda _
              (match-let (((major release revision)
                           (string-split #$version #\.)))
                (setenv "MAJOR" major)
                (setenv "RELEASE" release)
                (setenv "REVISION" revision))))
          (delete 'bootstrap)
          (delete 'configure)
          (replace 'check
            (lambda* (#:key target (tests? (not target))
                      ;; Do not test on cross building.
                      #:allow-other-keys)
              (if tests?
                  (let ((script "./test.sh"))
                    (if (executable-file? script)
                        (invoke script)
                        (invoke "sh" script)))
                  (format #t "test suite not run~%"))))
          (add-after 'install 'install-example
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (doc (simple-format #f "~A/share/doc/~A/"
                                         out (strip-store-file-name out)))
                     (examples (string-append doc "examples")))
                (for-each
                 (lambda (fname)
                   (install-file fname doc))
                 '("README.md" "COPYRIGHT" "CHANGELIST" "TODO"))
                (copy-recursively "examples" examples)
                (install-file "keynavrc" examples)))))))
    (home-page "https://github.com/jordansissel/keynav")
    (synopsis "Keyboard-driven mouse cursor mover")
    (description "Keynav makes your keyboard a fast mouse cursor mover.  You
can move the cursor to any point on the screen with a few key strokes.  It
also simulates mouse click.  You can do everything mouse can do with a
keyboard.")
    (license license:bsd-3)))
