
;; Prefix: ik:


(defvar ik:primitive-symbols
  (let (res)
    (mapatoms (lambda (sym) (setq res (cons sym res))))
    (nreverse res)))
(push "~/Dropbox/emacs/elisp-vc/imakado/" load-path)
'(princ (format "%S" load-path))

(require 'imakado)

(defvar ik:after-load-symbols
  (let (res)
    (mapatoms (lambda (sym) (setq res (cons sym res))))
    (nreverse res)))

(defsubst ik:lookup-function (object)
  (while (and (symbolp object) (fboundp object))
    (setq object (symbol-function object)))
  object)

(defun ik:macrop (object)
  "Return the macro named by OBJECT, or nil if it is not a macro."
  (setq object (ik:lookup-function object))
  (if (and (listp object)
	   (eq 'macro (car object))
	   (functionp (cdr object)))
      object))

(require 'cl-lib)
(let* ((syms (cl-set-difference ik:after-load-symbols ik:primitive-symbols))
       (syms
        (imakado-aand syms
                (imakado-remove-if (lambda (sym)
                               (and (not (functionp sym))
                                    (not (boundp sym))
                                    (not (ik:macrop sym))))
                             syms)
                (imakado-remove-if (lambda (sym) (imakado-=~ "^imakado-" (symbol-name sym))) it)
                (imakado-remove-if (lambda (sym) (imakado-=~ "^ik:" (symbol-name sym))) it)
                (imakado-remove-if (lambda (sym) (imakado-=~ "^:" (symbol-name sym))) it))))
  (message "%S" syms))
















