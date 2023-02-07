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


;;-----------------------------------------------------------------------------
;; Mode hooks
;;-----------------------------------------------------------------------------
;; R mode
(add-hook
 'ess-mode-hook
 (lambda ()
   (setq ess-indent-offset 2)
   (ess-toggle-underscore nil) ;leave underscore key alone!
   (setq ess-fancy-comments nil)))

;; term-mode
(add-hook
 'term-mode-hook
 (lambda ()
   (setq show-trailing-whitespace nil)))

;; C mode
;; Don't indent `extern "C" { ... }`
;; https://www.linuxquestions.org/questions/programming-9/calling-emacs-experts-can-indentation-ignore-extern-c-%7B-%7D-887812/
(add-hook 'c-mode-common-hook
	        (lambda()
	          (c-set-offset 'inextern-lang 0)))

;; Python mode
;; Google3 : The automatic formatting hook is a literal pain the ass.
;; Code formatting is an art not a science goddammit; piss off.
;; remove google3-build-try-cleanup from python-mode
(add-hook
 'python-mode-hook
 (lambda () "" nil
   (remove-hook 'before-save-hook 'google3-build-try-cleanup t)))


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
