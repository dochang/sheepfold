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
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
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

(define-public termrec
  (package
    (name "termrec")
    (version "0.19")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/kilobyte/termrec")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "08jh3xxalid5hak754w5gn2r72gdd0hzlqkybj8ghnh86py9b5m3"))))
    (build-system gnu-build-system)
    (inputs
     (list zlib
           bzip2
           xz
           `(,zstd "lib")
           curl))
    (native-inputs
     (list autoconf
           automake
           libtool
           which
           perl
           git))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'bootstrap 'pre-bootstrap
            (lambda _
              (call-with-output-file "VERSION"
                (lambda (port)
                  (simple-format port "~A\n" #$version)))
              (patch-shebang "get_version")))
          (add-after 'install 'post-install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (doc (simple-format #f "~A/share/doc/~A/"
                                         out (strip-store-file-name out))))
                (for-each
                 (lambda (f)
                   (install-file f doc))
                 '("README" "INSTALL" "BUGS" "ChangeLog"))))))))
    (home-page "https://angband.pl/termrec.html")
    (synopsis "TTY recorder/player")
    (description "Termrec is a tty recorder; it can record the output of any
text mode program which you can then replay with @command{termplay},
@command{ttyplay}, @command{ipbt}, @command{ttyplayer}, @command{nh-recorder},
@command{asciinema} or similar.")
    (license license:lgpl3)))
