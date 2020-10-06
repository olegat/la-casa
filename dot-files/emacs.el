

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


;;-----------------------------------------------------------------------------
;;  Platform-specific config
;;-----------------------------------------------------------------------------
(when (string-equal system-type "windows-nt")
  ;; Use git-bash Unix environment
  (setenv "PATH"
          (concat (getenv "PATH") ";"
                  (getenv "GIT_ROOT") "\\usr\\bin"))
  ;; Use CMake 3.16 by default (if it exists, and if version isn't specified)
  (unless (boundp 'olegat-cmake-mode-path)
    (setq olegat-cmake-mode-path
          "C:/Program Files/CMake/share/cmake-3.16/editors/emacs"))
  (setq find-program "gfind.bat"))


(when (string-equal system-type "darwin")
  (unless (boundp 'olegat-cmake-mode-path)
    (setq olegat-cmake-mode-path
          "/Users/olegat/homebrew/Cellar/cmake/3.18.2/share/emacs/site-lisp/cmake")))


(when (string-equal system-type "cygwin")
  ;; Use the same CMake as windows-nt (with cygdrive Unix path)
  (unless (boundp 'olegat-cmake-mode-path)
  (setq olegat-cmake-mode-path
        "/cygdrive/c/Program Files/CMake/share/cmake-3.16/editors/emacs")))



;;-----------------------------------------------------------------------------
;; Defaults
;;-----------------------------------------------------------------------------
;; Filesystem I/O Config
(setq
 backup-by-copying t   ; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/.emacs.saves/"))   ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)   ; use versioned backups

(setq-default
 buffer-file-coding-system 'utf-8-unix
 show-trailing-whitespace t)


;; Code Styling
(setq-default
 c-basic-offset 2
 tab-width 2
 indent-tabs-mode nil
 require-final-newline nil  ; Don't mess with final newlines in files.
 js-indent-level 2
 mode-require-final-newline nil)

;; Don't add multiple newlines when scrolling past the end of the file.
(setq-default next-line-add-newlines nil)

(setq column-number-mode t) ; show column numbers


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
;; CMake
;;-----------------------------------------------------------------------------
(when (boundp 'olegat-cmake-mode-path)
  (when (file-directory-p olegat-cmake-mode-path)
    (setq load-path (cons olegat-cmake-mode-path load-path))
    (require 'cmake-mode)))


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

;; Current
(set-face-foreground 'ediff-current-diff-A "black")
(set-face-background 'ediff-current-diff-A "green")
(set-face-foreground 'ediff-current-diff-B "black")
(set-face-background 'ediff-current-diff-B "red")
(set-face-foreground 'ediff-current-diff-C "black")
(set-face-background 'ediff-current-diff-C "cyan")

;; Fine
(set-face-foreground 'ediff-fine-diff-A "black")
(set-face-background 'ediff-fine-diff-A "lightgreen")
(set-face-foreground 'ediff-fine-diff-B "black")
(set-face-background 'ediff-fine-diff-B "lightred")
(set-face-foreground 'ediff-fine-diff-C "black")
(set-face-background 'ediff-fine-diff-C "lightcyan")

;; Unselected
(set-face-foreground 'ediff-even-diff-A nil)
(set-face-background 'ediff-even-diff-A "darkgray")
(set-face-foreground 'ediff-even-diff-B nil)
(set-face-background 'ediff-even-diff-B "darkgray")
(set-face-foreground 'ediff-even-diff-C nil)
(set-face-background 'ediff-even-diff-C "darkgray")
(set-face-foreground 'ediff-odd-diff-A nil)
(set-face-background 'ediff-odd-diff-A "darkgray")
(set-face-foreground 'ediff-odd-diff-B nil)
(set-face-background 'ediff-odd-diff-B "darkgray")
(set-face-foreground 'ediff-odd-diff-C nil)
(set-face-background 'ediff-odd-diff-C "darkgray")
