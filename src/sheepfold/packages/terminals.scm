(define-module (sheepfold packages terminals)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages readline)
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
