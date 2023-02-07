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


;;-----------------------------------------------------------------------------
;; MELPA
;;-----------------------------------------------------------------------------
(package-initialize)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)


;;-----------------------------------------------------------------------------
;; EditorConfig
;;-----------------------------------------------------------------------------
;; TODO(olegat) - use use-package: https://www.emacswiki.org/emacs/UsePackage
(when (fboundp 'editorconfig-mode)
  (editorconfig-mode 1))


;;-----------------------------------------------------------------------------
;; CMake
;;-----------------------------------------------------------------------------
(when (boundp 'olegat-cmake-mode-path)
  (when (file-directory-p olegat-cmake-mode-path)
    (setq load-path (cons olegat-cmake-mode-path load-path))
    (require 'cmake-mode)))


;;-----------------------------------------------------------------------------
;; GN (Generate Ninja)
;;-----------------------------------------------------------------------------
(when (fboundp 'gn-mode)
  (add-to-list
   'auto-mode-alist
   '("BUILD\\.gn\\'" . gn-mode)
   '("\\.gni\\'" . gn-mode)))
