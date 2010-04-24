;;; simple-template.el - 

;; Author: Kenji IMAKADO <ken.imakado -at- gmail.com>
;; Keywords:

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

(eval-when-compile
  (require 'imakado)
  (require 'cl)
  (and (fboundp 'imakado-require-version)
       (imakado-require-version 0.01)))


(defcacheable do-simple-template-big-tmpl-1 ()
  (let* ((tmpl1 "\
\[% (cond (flag %]
\[% (insert \"flag is non-nil\") %]
\[%)%][%# end FLAG  %]
\[% (t %][%# ELSE %]
\[%# *commant*  %]
\[% (insert \"flag is nil\") %]
\[% )) %][%# end COND %]")
         (tmpl (loop for n from 1 to 1000
                     concat tmpl1)))
    tmpl))

(defun do-simple-template-big-tmpl ()
  (let* ((o "you")
         (flag t))
    (message "template length: %s"
             (length (do-simple-template-big-tmpl-1)))
    (let ((res nil))
      (garbage-collect)
      (list (benchmark-run 1
                (setq res (simple-template-string
                           (do-simple-template-big-tmpl-1))))
            res))))

(defun do-simple-template-big-tmpl-file ()
  (let* ((o "you")
         (flag t))
    (message "template length: %s"
             (length (do-simple-template-big-tmpl-1)))
    (do-temp-file (tmp-file (do-simple-template-big-tmpl-1))
      (do-temp-file (tmp-file2 (do-simple-template-big-tmpl-1))
        (let ((res nil))
          (garbage-collect)
          (list (benchmark-run 1
                    (setq res (simple-template
                               (list tmp-file
                                     tmp-file2))))
                res))))))

'(byte-compile 'do-simple-template-big-tmpl)
'(do-simple-template-big-tmpl)
'(do-simple-template-big-tmpl-file)


;; simple-template.el ends here.
