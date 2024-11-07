(define-module (sheepfold packages file-systems)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages base)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages file-systems)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:))

(define-public format-udf
  (package
    (name "format-udf")
    (version "1.8.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/JElchison/format-udf")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32 "0ki3d8k4pa3wimn2027qi1pp763m4q2icx6fy5dqn2jf7c8whl1s"))))
    (build-system trivial-build-system)
    (inputs
     (list bash-minimal
           coreutils
           xxd
           util-linux ;; For `blockdev', `lsblk', `umount'
           udftools))
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      #~(begin
          (use-modules (guix build utils)
                       (srfi srfi-1))
          (let* ((stripped (strip-store-file-name #$output))
                 (sbin (string-append #$output "/sbin/format-udf"))
                 (doc (simple-format #f "~A/share/doc/~A/" #$output stripped)))
            (with-directory-excursion #$source
              (mkdir-p (dirname sbin))
              (copy-file "format-udf.sh" sbin)
              (mkdir-p doc)
              (install-file "README.md" doc)
              (install-file "LICENSE" doc)
              (wrap-program sbin
                #:sh (string-append #$bash "/bin/bash")
                (list
                 "PATH" ":" 'prefix
                 (append-map
                  (lambda (dep)
                    (list (string-append dep "/bin")
                          (string-append dep "/sbin")))
                  (list #$@(list bash coreutils xxd util-linux udftools))))))))))
    (home-page "https://github.com/JElchison/format-udf")
    (synopsis "Formats a block device (hard drive or Flash drive) in UDF")
    (description "Bash script to format a block device (hard drive or Flash
drive) in UDF.  The output is a drive that can be used for reading/writing
across multiple operating system families: Windows, macOS, and Linux.  This
script should be capable of running in macOS or in Linux.")
    (license license:gpl2)))
