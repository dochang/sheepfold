(define-module (sheepfold packages admin)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:))

(define-public limitcpu
  (package
    (name "limitcpu")
    (version "3.1")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "mirror://sourceforge/limitcpu/limitcpu/cpulimit-"
                              version ".tar.gz"))
              (sha256
               (base32 "0k13gfy05qwqjdk4axds5b41975qc3k79gc9bc4h77zkc3n98scl"))
              (modules '((guix build utils)))
              (snippet
               '(substitute* "test/Makefile"
                  (("(busy:[ \t]*busy.c).*[ \t]\\$\\(LIBS\\)" all head)
                   (string-append head "\n"))))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:test-target "tests"
      #:make-flags
      #~(list (string-append "PREFIX=" %output)
              (string-append "CC=" #$(cc-for-target)))
      #:phases
      '(modify-phases %standard-phases
         (delete 'configure))))
    (home-page "https://limitcpu.sourceforge.net/")
    (synopsis "Throttle the CPU cycles used by other applications")
    (description "LimitCPU is a program to throttle the CPU cycles used by
other applications.  LimitCPU will monitor a process and make sure its CPU
usage stays at or below a given percentage.  This can be used to make sure
your system has plenty of CPU cycles available for other tasks.  It can also
be used to keep laptops cool in the face of CPU-hungry processes and for
limiting virtual machines.

LimitCPU is the direct child of CPUlimit, a creation of Angelo Marletta,
which can be found at http://cpulimit.sourceforge.net.")
    (license license:gpl2)))
