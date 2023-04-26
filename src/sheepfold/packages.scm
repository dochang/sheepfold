(define-module (sheepfold packages)
  #:use-module (ice-9 match)
  #:use-module ((gnu packages) #:prefix gnu:)
  #:export (search-patch
            search-patches
            %patch-path))

(define %sheepfold-root-directory
  (letrec-syntax ((dirname* (syntax-rules ()
                              ((_ file)
                               (dirname file))
                              ((_ file head tail ...)
                               (dirname (dirname* file tail ...)))))
                  (try      (syntax-rules ()
                              ((_ (file things ...) rest ...)
                               (match (search-path %load-path file)
                                 (#f
                                  (try rest ...))
                                 (absolute
                                  (dirname* absolute things ...))))
                              ((_)
                               #f))))
    (try ("sheepfold/packages.scm" sheepfold/))))

(define (search-patch file-name)
  "Search the patch FILE-NAME.  Raise an error if not found."
  (or (search-path (%patch-path) file-name)
      (raise (formatted-message (G_ "~a: patch not found")
                                file-name))))

(define-syntax-rule (search-patches file-name ...)
  "Return the list of absolute file names corresponding to each
FILE-NAME found in %PATCH-PATH."
  (list (search-patch file-name) ...))

(define %patch-path
  (make-parameter
   (map (lambda (directory)
          (if (string=? directory %sheepfold-root-directory)
              (string-append directory "/sheepfold/packages/patches")
              directory))
        (gnu:%patch-path))))
