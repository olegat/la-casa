;; append ~/la-casa/emacs to 'load-path
(setq
 load-path
 (cons
  (file-truename (concat (file-name-directory load-file-name) "../emacs"))
  load-path))

(require 'olegat)

(require 'ediff)
(require 'powershell-mode)
(require 'term)



;;-----------------------------------------------------------------------------
;;  Platform-specific config
;;-----------------------------------------------------------------------------
(setq olegat-msys nil)

(when (string-equal system-type "windows-nt")
  ;; Check if this subsystem is MSYS
  (when (string-match-p (regexp-quote "/usr/bin/bash") (getenv "SHELL"))
    (setq olegat-msys t))

  ;; Hacks to make Windows Emacs now Unix-like
  (unless olegat-msys
    ;; Use git-bash Unix environment
    (setenv "PATH"
            (concat (getenv "PATH") ";"
                    (getenv "GIT_ROOT") "\\usr\\bin"))
    (setq find-program "gfind.bat"))

  ;; Use CMake 3.16 by default (if it exists, and if version isn't specified)
  (unless (boundp 'olegat-cmake-mode-path)
    (setq olegat-cmake-mode-path
          "C:/Program Files/CMake/share/emacs/site-lisp")))


(when (string-equal system-type "darwin")
  (unless (boundp 'olegat-cmake-mode-path)
    (setq olegat-cmake-mode-path
          "/Users/olegat/homebrew/Cellar/cmake/3.25.1/share/emacs/site-lisp/cmake")))

(when (string-equal system-type "cygwin")
  ;; Use the same CMake as windows-nt (with cygdrive Unix path)
  (unless (boundp 'olegat-cmake-mode-path)
  (setq olegat-cmake-mode-path
        "/cygdrive/c/Program Files/CMake/share/emacs/site-lisp")))

(when (string-equal system-name "silenus-docker")
  (setq shell-file-name "bash")) ; change default from 'sh' to 'bash'

;; Use Dark mode in GUIs (w32, x, ns...)
(when window-system
  (olegat-chrome-mode nil)
  (setq default-frame-alist
        '((background-color . "gray10")
          (foreground-color . "white")
          (ns-appearance . dark)
          (ns-transparent-titlebar . nil)
          (frame-background-mode . 'dark))))


;;-----------------------------------------------------------------------------
;; Defaults
;;-----------------------------------------------------------------------------
(setq
 backup-by-copying t   ; don't clobber symlinks
 backup-directory-alist '(("." . "~/.emacs.saves/")) ; don't litter my fs tree
 column-number-mode t ; show column numbers
 delete-old-versions t
 ;; Don't use C/C++ syntax highlighting in diff mode.
 ;; https://github.com/magit/magit/issues/2942
 ;; See Week 10 & 27 (2021)
 diff-font-lock-syntax nil
 ;; Don't highlight which part of the diffs have changed, just color the +lines
 ;; in green and the -lines in red. This speed things up.
 ;; See Week 39 (2021)
 diff-refine nil
 kept-new-versions 6
 kept-old-versions 2
 version-control t   ; use versioned backups
 ;; OUCH!! MY EARS!!!!
 ;; https://tldp.org/HOWTO/Visual-Bell-8.html#:~:text=To%20disable%20the%20visible%20bell,visible%2Dbell%20nil)%20%22.
 visible-bell t)

(setq-default
 buffer-file-coding-system 'utf-8-unix
 c-basic-offset 2
 fill-column 80 ; this is the value in Google-Emacs on gLinux
 indent-tabs-mode nil
 js-indent-level 2
 markdown-command "pandoc"
 mode-require-final-newline nil
 ;; Don't add multiple newlines when scrolling past the end of the file.
 next-line-add-newlines nil
 require-final-newline nil  ; Don't mess with final newlines in files.
 rust-indent-offset 4
 show-trailing-whitespace t
 tab-width 2)

;; The default face on Windows GUI is Courier New which is so thin and unreadable.
;; Add this to ~/.emacs as needed.
;; Courtesy of Stackoverflow: https://stackoverflow.com/questions/4821984/emacs-osx-default-font-setting-does-not-persist/4822066#4822066
;;(custom-set-faces '(default ((t (:height 110 :family "Consolas")))))
(custom-set-variables
 '(ediff-split-window-function (quote split-window-horizontally))
 '(speedbar-show-unknown-files t))


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



;;-----------------------------------------------------------------------------
;; Face customization
;;-----------------------------------------------------------------------------
(when (string-equal window-system "ns")
  (set-face-foreground 'term-color-blue "systemBlueColor"))



;;-----------------------------------------------------------------------------
;; Enable -fdiagnostics-color=always in Compilation mode
;;-----------------------------------------------------------------------------
;; https://stackoverflow.com/questions/13397737/ansi-coloring-in-compilation-mode
;;
;; (ignore-errors
;;   (require 'ansi-color)
;;   (defun my-colorize-compilation-buffer ()
;;     (when (eq major-mode 'compilation-mode)
;;       (ansi-color-apply-on-region compilation-filter-start (point-max))))
;;   (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))
