(define-module (sheepfold packages terminals)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages pkg-config)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (gnu packages gperf)
  #:use-module ((guix licenses) #:prefix license:))

(define-public pspg
  (package
    (name "pspg")
    (version "5.7.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/okbob/pspg")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32 "06nhsk4ydp24i52ma368vlpbpsy99iv14dxandfp8pjl1pazp7hx"))))
    (build-system gnu-build-system)
    (inputs
     (list ncurses
           postgresql
           readline))
    (arguments
     `(#:tests? #f)) ;; No target `check'.
    (home-page "https://github.com/okbob/pspg")
    (synopsis "Unix pager designed for work with tables")
    (description "Unix pager (with very rich functionality) designed for
work with tables.  Designed for PostgreSQL, but MySQL is supported too.  Works
well with @command{pgcli} too.  Can be used as CSV or TSV viewer too.  It
supports searching, selecting rows, columns, or block and export selected area
to clipboard.")
    (license license:bsd-2)))

(define-public mandown
  (package
    (name "mandown")
    (version "1.0.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Titor8115/mandown")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0izirfkffn7i5jiy3k9drab8gqn21fcsgfvci6drpygixz6l425g"))))
    (build-system gnu-build-system)
    (inputs
     (list ncurses
           libconfig
           libxml2))
    (native-inputs
     (list pkg-config
           gperf))
    (arguments
     (list
      #:tests? #f ;; No tests.
      #:make-flags
      #~(list (string-append "PKG_CONFIG=" #$(pkg-config-for-target))
              (string-append "PREFIX=" (assoc-ref %outputs "out"))
              (string-append "CONFIGDIR=" (mkdtemp
                                           (string-append
                                            (or (getenv "TMPDIR") "/tmp")
                                            "/guix-mandown-config-XXXXXX"))))
      #:phases
      `(modify-phases %standard-phases
         (delete 'bootstrap)
         (delete 'configure)
         (add-after 'install 'install-docs
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (package (strip-store-file-name out))
                    (doc (string-append out "/share/doc/" package)))
               (for-each (lambda (fname)
                           (install-file fname doc))
                         '("README.md" "sample.md"))))))))
    (home-page "https://github.com/Titor8115/mandown")
    (synopsis "Man-page inspired Markdown viewer")
    (description "A man-page inspired Markdown pager written in C.")
    (license license:gpl3)))
