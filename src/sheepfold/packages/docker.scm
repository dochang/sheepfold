(define-module (sheepfold packages docker)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module ((guix licenses) #:prefix license:))

(define-public multirun
  (package
    (name "multirun")
    (version "1.1.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/nicolas-van/multirun")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32 "1vrfc7rm0iv78pp05afvy729l37fhv0ww32flbid3rpsj32ngpi3"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f)) ;; No target `check'.
    (home-page "https://github.com/nicolas-van/multirun")
    (synopsis "Minimalist init process designed for Docker")
    (description "A simple Unix utility in C to run multiple commands concurrently.

@itemize
@item A very light alternative to classic init processes or supervisord to run
      multiple services in the same Docker container.
@item Is dead-simple to use.
@item Can be run without root permissions.
@item Cleanly kills all the processes it starts, including their subprocesses.
@item Delegates the restart duty to the upper level.
@item Forwards stdout and stderr for proper logging with Docker or systemd.
@end itemize")
    (license license:expat)))
