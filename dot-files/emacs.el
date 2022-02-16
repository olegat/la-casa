;; Add this to ~/.emacs where necessary.
;; (setq frame-background-mode 'dark)


;;-----------------------------------------------------------------------------
;;  Key bindings
;;-----------------------------------------------------------------------------
(defun olegat-insert-§ ()
  (interactive)
  (insert "§"))
(defun olegat-select-speedbar-frame ()
  (interactive)
  (select-frame-by-name "Speedbar"))
(defun olegat-select-frame-F1 ()
  (interactive)
  (select-frame-by-name "F1"))
(defun olegat-select-frame-F2 ()
  (interactive)
  (select-frame-by-name "F2"))
(defun olegat-select-frame-F3 ()
  (interactive)
  (select-frame-by-name "F3"))
(defun olegat-select-frame-F4 ()
  (interactive)
  (select-frame-by-name "F4"))

;; Escape the § symbol
(global-set-key (kbd "§") nil)
(global-set-key (kbd "§ §") 'olegat-insert-§)

;; Frame selection
(global-set-key (kbd "§ /") 'olegat-select-speedbar-frame)
(global-set-key (kbd "§ 1") 'olegat-select-frame-F1)
(global-set-key (kbd "§ 2") 'olegat-select-frame-F2)
(global-set-key (kbd "§ 3") 'olegat-select-frame-F3)
(global-set-key (kbd "§ 4") 'olegat-select-frame-F4)

;; Opening files
(global-set-key (kbd "§ t t") 'toggle-truncate-lines)
(global-set-key (kbd "§ f f") 'find-file-at-point)
(global-set-key (kbd "§ j o") 'ff-find-other-file)

;; Window navigation
(global-set-key (kbd "§ <right>") 'windmove-right)
(global-set-key (kbd "§ <left>")  'windmove-left)
(global-set-key (kbd "§ <down>")  'windmove-down)
(global-set-key (kbd "§ <up>")    'windmove-up)

;; Misc
(global-set-key (kbd "§ TAB") 'imenu)
(global-set-key (kbd "§ e r") 'eval-region)
(global-set-key (kbd "§ b")   'recompile)
(global-set-key (kbd "§ s l") 'sort-lines)
(global-set-key (kbd "§ f n d") 'find-name-dired)


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
          "/Users/olegat/homebrew/Cellar/cmake/3.18.2/share/emacs/site-lisp/cmake")))

(when (string-equal system-type "cygwin")
  ;; Use the same CMake as windows-nt (with cygdrive Unix path)
  (unless (boundp 'olegat-cmake-mode-path)
  (setq olegat-cmake-mode-path
        "/cygdrive/c/Program Files/CMake/share/emacs/site-lisp")))

(when (string-equal system-name "silenus-docker")
  (setq shell-file-name "bash")) ; change default from 'sh' to 'bash'

;; Use Dark mode in GUIs (w32, x, ns...)
(when window-system
  (set-background-color "gray10")
  (set-face-foreground 'default "white")
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (setq frame-background-mode 'dark))


;;-----------------------------------------------------------------------------
;; Defaults
;;-----------------------------------------------------------------------------
;; Filesystem I/O Config
(setq
 backup-by-copying t   ; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/.emacs.saves/"))   ; don't litter my fs tree
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
 version-control t)   ; use versioned backups

(setq-default
 buffer-file-coding-system 'utf-8-unix
 show-trailing-whitespace t)

;; OUCH!! MY EARS!!!!
;; https://tldp.org/HOWTO/Visual-Bell-8.html#:~:text=To%20disable%20the%20visible%20bell,visible%2Dbell%20nil)%20%22.
(setq visible-bell t)

;; Code Styling
(setq-default
 c-basic-offset 2
 tab-width 2
 indent-tabs-mode nil
 require-final-newline nil  ; Don't mess with final newlines in files.
 js-indent-level 2
 mode-require-final-newline nil)

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

;; Don't add multiple newlines when scrolling past the end of the file.
(setq-default next-line-add-newlines nil)

(setq column-number-mode t) ; show column numbers

;; The default face on Windows GUI is Courier New which is so thin and unreadable.
;; Add this to ~/.emacs as needed.
;; Courtesy of Stackoverflow: https://stackoverflow.com/questions/4821984/emacs-osx-default-font-setting-does-not-persist/4822066#4822066
;;(custom-set-faces '(default ((t (:height 110 :family "Consolas")))))



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
;; Speedbar
;;-----------------------------------------------------------------------------
(custom-set-variables '(speedbar-show-unknown-files t))


;;-----------------------------------------------------------------------------
;; C
;;-----------------------------------------------------------------------------
;; Don't indent `extern "C" { ... }`
;; https://www.linuxquestions.org/questions/programming-9/calling-emacs-experts-can-indentation-ignore-extern-c-%7B-%7D-887812/
(add-hook 'c-mode-common-hook
	        (lambda()
	          (c-set-offset 'inextern-lang 0)))


;;-----------------------------------------------------------------------------
;; CMake
;;-----------------------------------------------------------------------------
(when (boundp 'olegat-cmake-mode-path)
  (when (file-directory-p olegat-cmake-mode-path)
    (setq load-path (cons olegat-cmake-mode-path load-path))
    (require 'cmake-mode)))


;;-----------------------------------------------------------------------------
;; Google3
;;-----------------------------------------------------------------------------
;; The automatic formatting hook is a literal pain the ass.
;; Code formatting is an art not a science goddammit; piss off.
;; remove google3-build-try-cleanup from python-mode
(add-hook
 'python-mode-hook
 (lambda () "" nil
   (remove-hook 'before-save-hook 'google3-build-try-cleanup t)))


;;-----------------------------------------------------------------------------
;; Silenus
;;-----------------------------------------------------------------------------
(defvar olegat-silenus-el-file nil)
(let ((paths
       (list
        "~/la-casa/scripts/silenus.el"
        )))
  (while (and paths (not olegat-silenus-el-file))
    (when (file-exists-p (car paths))
      (setq olegat-silenus-el-file (car paths)))
    (setq paths (cdr paths))))
(when olegat-silenus-el-file
  (load-file olegat-silenus-el-file))


;;-----------------------------------------------------------------------------
;; Custom
;;-----------------------------------------------------------------------------
(defvar olegat-chrome-mode-on t)
(defun olegat-chrome-mode (enabled)
  (interactive (list (not olegat-chrome-mode-on)))
  (setq olegat-chrome-mode-on enabled)
  (when olegat-chrome-mode-on
    (tool-bar-mode nil)
    (scroll-bar-mode nil)
    (menu-bar-mode nil))
  (unless olegat-chrome-mode-on
    (tool-bar-mode -1)
    (scroll-bar-mode -1)
    (menu-bar-mode -1)))

(defvar olegat-vc-handled-backends nil)
(defun olegat-toggle-vc ()
  (interactive)
  ;; Swap "vc-handled-backends" and "olegat-vc-handled-backends"
  (let ((tmp vc-handled-backends))
    (setq vc-handled-backends olegat-vc-handled-backends)
    (setq olegat-vc-handled-backends tmp)))

(defun copy-filepath-to-clipboard ()
  (interactive)
  (kill-new buffer-file-name)
  (let ((known-window-system nil))
    (when (string-equal window-system "w32")
      (setq known-window-system t)
      (w32-set-clipboard-data buffer-file-name))
    (unless known-window-system
      (message (concat "Unsupported window system: " window-system)))))

;;-----------------------------------------------------------------------------
;; Ediff
;;-----------------------------------------------------------------------------
(require 'ediff)

(custom-set-variables
 '(ediff-split-window-function (quote split-window-horizontally)))

;; Make the faces visible in a term without x256 colors (e.g. CMD on Windows)
;;
;; In a nutshell:
;;   current A = black on green
;;   current B = black on red
;;   current C = black on blue
;;   everything else = nil on gray

;; (when (string-equal system-type "windows-nt")
;;   ;; Current
;;   (set-face-foreground 'ediff-current-diff-A "black")
;;   (set-face-background 'ediff-current-diff-A "green")
;;   (set-face-foreground 'ediff-current-diff-B "black")
;;   (set-face-background 'ediff-current-diff-B "red")
;;   (set-face-foreground 'ediff-current-diff-C "black")
;;   (set-face-background 'ediff-current-diff-C "cyan")

;;   ;; Fine
;;   (set-face-foreground 'ediff-fine-diff-A "black")
;;   (set-face-background 'ediff-fine-diff-A "lightgreen")
;;   (set-face-foreground 'ediff-fine-diff-B "black")
;;   (set-face-background 'ediff-fine-diff-B "lightred")
;;   (set-face-foreground 'ediff-fine-diff-C "black")
;;   (set-face-background 'ediff-fine-diff-C "lightcyan")

;;   ;; Unselected
;;   (set-face-foreground 'ediff-even-diff-A nil)
;;   (set-face-background 'ediff-even-diff-A "darkgray")
;;   (set-face-foreground 'ediff-even-diff-B nil)
;;   (set-face-background 'ediff-even-diff-B "darkgray")
;;   (set-face-foreground 'ediff-even-diff-C nil)
;;   (set-face-background 'ediff-even-diff-C "darkgray")
;;   (set-face-foreground 'ediff-odd-diff-A nil)
;;   (set-face-background 'ediff-odd-diff-A "darkgray")
;;   (set-face-foreground 'ediff-odd-diff-B nil)
;;   (set-face-background 'ediff-odd-diff-B "darkgray")
;;   (set-face-foreground 'ediff-odd-diff-C nil)
;;   (set-face-background 'ediff-odd-diff-C "darkgray")
;; )



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
