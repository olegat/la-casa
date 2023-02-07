;; append ~/la-casa/emacs to 'load-path
(setq
 load-path
 (cons
  (file-truename (concat (file-name-directory load-file-name) "../emacs"))
  load-path))

(require 'olegat)
(olegat-init-keybindings)

(require 'ediff)
(require 'powershell-mode)
(require 'term)


(olegat-init-platform)
(olegat-init-defaults)
(olegat-init-hooks)
(olegat-init-modes)


;;-----------------------------------------------------------------------------
;; MELPA
;;-----------------------------------------------------------------------------
(package-initialize)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
