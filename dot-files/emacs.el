;; append ~/la-casa/emacs to 'load-path
(setq
 load-path
 (cons
  (file-truename (concat (file-name-directory load-file-name) "../emacs"))
  load-path))

(eval-when-compile (require 'use-package))
(require 'powershell-mode)
(require 'olegat)
(olegat-init)
