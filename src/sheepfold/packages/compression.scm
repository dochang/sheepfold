(define-module (sheepfold packages compression)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages version-control)
  #:use-module ((guix licenses) #:prefix license:))

(define-public bzip3
  (package
    (name "bzip3")
    (version "1.3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/kspalaiologos/bzip3")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32 "0knfmcs8x75h4j14wfiannb5q8zz8lzzhvdcnf6yjvp60rv8vbw3"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf
           automake
           libtool
           pkg-config
           git))
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-before 'bootstrap 'pre-bootstrap
                 (lambda _
                   (call-with-output-file ".tarball-version"
                     (lambda (port)
                       (simple-format port "~A\n" #$version)))))
               (add-after 'install 'post-install
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let* ((out (assoc-ref outputs "out"))
                          (doc (simple-format #f "~A/share/doc/~A/"
                                              out (strip-store-file-name out))))
                     (for-each
                      (lambda (subdir)
                        (copy-recursively subdir (string-append doc subdir)))
                      '("doc" "etc" "examples"))
                     (for-each
                      (lambda (f)
                        (install-file f doc))
                      '("libsais-LICENSE" "NEWS" "PORTING.md" "README.md"))))))))
    (home-page "https://github.com/kspalaiologos/bzip3")
    (synopsis "Better and stronger spiritual successor to BZip2")
    (description "A better, faster and stronger spiritual successor to BZip2.
Features higher compression ratios and better performance thanks to a order-0
context mixing entropy coder, a fast Burrows-Wheeler transform code making use
of suffix arrays and a RLE with Lempel Ziv+Prediction pass based on LZ77-style
string matching and PPM-style context modeling.

Like its ancestor, BZip3 excels at compressing text or code.")
    (license (list license:gpl3
                   license:asl2.0))))
