
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
 version-control t   ; use versioned backups
 set-default-coding-systems 'utf-8-unix)


;; Code Styling
(setq-default
 c-basic-offset 2
 tab-width 2
 indent-tabs-mode nil
 require-final-newline nil  ; Don't mess with final newlines in files.
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
;; Speedbar
;;-----------------------------------------------------------------------------
(custom-set-variables '(speedbar-show-unknown-files t))


;;-----------------------------------------------------------------------------
;; CMake
;;-----------------------------------------------------------------------------
;; E.g.
;;  (setq olegat-cmake-share-path "~/cmake/share/cmake-3.12/")
;;  (setq olegat-cmake-share-path "C:/Program Files/CMake/share/cmake-3.12")
(when (boundp 'olegat-cmake-share-path)
  (setq load-path
        (cons
         (expand-file-name (concat olegat-cmake-share-path "/editors/emacs"))
         load-path))
  (require 'cmake-mode))


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

;; Window navigation
(global-set-key (kbd "§ <right>") 'windmove-right)
(global-set-key (kbd "§ <left>")  'windmove-left)
(global-set-key (kbd "§ <down>")  'windmove-down)
(global-set-key (kbd "§ <up>")    'windmove-up)

;; Misc
(global-set-key (kbd "§ TAB") 'imenu)
(global-set-key (kbd "§ e r") 'eval-region)
