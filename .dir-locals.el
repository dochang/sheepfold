;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((scheme-mode
  .
  ((eval
    .
    (progn
      ;; The following code is copied from:
      ;;
      ;; https://git.savannah.gnu.org/cgit/guix.git/commit/?id=c9fb789fe9eaa5dc0694ef14fe36e5aa821a646c
      ;;
      ;; Emacs 28 changed the behavior of 'lisp-fill-paragraph', which causes
      ;; the first line of package descriptions to extrude past 'fill-column',
      ;; and somehow that is deemed more correct upstream (see:
      ;; https://issues.guix.gnu.org/56197).
      (require 'lisp-mode)
      (defun emacs27-lisp-fill-paragraph (&optional justify)
        (interactive "P")
        (or (fill-comment-paragraph justify)
            (let ((paragraph-start
                   (concat paragraph-start
                           "\\|\\s-*\\([(;\"]\\|\\s-:\\|`(\\|#'(\\)"))
                  (paragraph-separate
                   (concat paragraph-separate "\\|\\s-*\".*[,\\.]$"))
                  (fill-column (if (and (integerp emacs-lisp-docstring-fill-column)
                                        (derived-mode-p 'emacs-lisp-mode))
                                   emacs-lisp-docstring-fill-column
                                 fill-column)))
              (fill-paragraph justify))
            ;; Never return nil.
            t))
      (setq-local fill-paragraph-function #'emacs27-lisp-fill-paragraph))))))
