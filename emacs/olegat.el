(require 'ansi-color)
(require 'ediff)
(require 'term)


;;-----------------------------------------------------------------------------
;;  Platform-specific config
;;-----------------------------------------------------------------------------
(defun olegat-init-platform ()
  "Internal use."

  ;; MELPA
  (package-initialize)
  (add-to-list 'package-archives
               '("melpa-stable" . "https://stable.melpa.org/packages/") t)

  (when (string-equal system-type "windows-nt")
    (let (olegat-msys)
      ;; Check if this subsystem is MSYS
      (when (string-match-p (regexp-quote "/usr/bin/bash") (getenv "SHELL"))
        (setq olegat-msys t))

      ;; Hacks to make Windows Emacs now Unix-like
      (unless olegat-msys
        ;; Use git-bash Unix environment
        (setenv "PATH"
                (concat (getenv "PATH") ";"
                        (getenv "GIT_ROOT") "\\usr\\bin"))
        (setq find-program "gfind.bat")))))


;;-----------------------------------------------------------------------------
;; Defaults
;;-----------------------------------------------------------------------------
(defun olegat-init-defaults ()
  "Internal use."
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
   ;; Use a window instead of a frame for ediff GUI mode (like on a terminal)
   ediff-window-setup-function 'ediff-setup-windows-plain
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

  ;; Always use dark mode
  (setq frame-background-mode 'dark) ; for terminals
  (when window-system ; for GUIs (w32, x, ns...)
    (olegat-chrome-mode nil)
    (set-face-foreground 'term-color-blue "systemBlueColor")
    (setq default-frame-alist
          '((background-color . "gray10")
            (foreground-color . "white")
            (ns-appearance . dark)
            (ns-transparent-titlebar . nil)))))


;;-----------------------------------------------------------------------------
;; Modes
;;-----------------------------------------------------------------------------
(defun olegat-init-hooks ()
  "Internal use."
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
     (remove-hook 'before-save-hook 'google3-build-try-cleanup t))))

(defun olegat-init-modes ()
  (when (fboundp 'editorconfig-mode)
    ;; EditorConfig
    ;; TODO(olegat) - use use-package: https://www.emacswiki.org/emacs/UsePackage
    (editorconfig-mode 1))

  ;; CMake
  (when (boundp 'olegat-cmake-mode-path)
    (when (file-directory-p olegat-cmake-mode-path)
      (setq load-path (cons olegat-cmake-mode-path load-path))
      (require 'cmake-mode)))

  ;; Makedown mode
  (when (fboundp 'markdown-mode)
    (add-to-list 'auto-mode-alist '("\\.mdoc\\'" . markdown-mode)))

  ;; GN (Generate Ninja)
  (when (fboundp 'gn-mode)
    (add-to-list
     'auto-mode-alist
     '("BUILD\\.gn\\'" . gn-mode)
     '("\\.gni\\'" . gn-mode)))

  ;; Magit
  (setq-default magit-auto-revert-mode nil)
  (use-package magit))


;;-----------------------------------------------------------------------------
;;  Key bindings
;;-----------------------------------------------------------------------------
(defvar olegat-keychar "§"
  "Prefix character of custom keybindings (olegat-keybindings).")

(defvar olegat-keybindings
  '(;; Opening files
    ("t t" . toggle-truncate-lines)
    ("f f" . find-file-at-point)
    ("j o" . ff-find-other-file)

     ;; Window navigation
    ("/"       . olegat-select-speedbar-frame)
    ("<right>" . windmove-right)
    ("<left>"  . windmove-left)
    ("<down>"  . windmove-down)
    ("<up>"    . windmove-up)

     ;; Buffer selection
    ("x <right>" . previous-buffer)
    ("x <left>"  . next-buffer)

     ;; Misc
    ("TAB"   . imenu)
    ("e r"   . eval-region)
    ("b"     . recompile)
    ("s l"   . sort-lines)
    ("f n d" . find-name-dired)
    ("3"     . olegat-insert-pound-sign)

     ;; hs-minor-mode
    ("["     . olegat-hs-show-block)
    ("]"     . olegat-hs-hide-block)

     ;; Prettier
    ("p p"   . prettier-prettify)
    ("p r"   . prettier-prettify-region)

    ("SPC"   . company-complete)
    )
  "An alist of key-sequences and function names")

(defun olegat-insert-keychar ()
  "Internal use. Insert the prefix olegat-keychar."
  (interactive)
  (insert olegat-keychar))

(defun olegat-insert-pound-sign ()
  "Internal use. Insert a £ character (Sterling Pound Sign)."
  (interactive)
  (insert "£"))

(defun olegat-set-key (keys function)
  "Internal use."
  (global-set-key
   (kbd (concat olegat-keychar " " keys)) function))

(defun olegat-init-keybindings ()
  "Internal use."
  ;; Escape the § symbol
  (global-set-key (kbd olegat-keychar) nil)
  (olegat-set-key olegat-keychar 'olegat-insert-keychar)
  (dolist (elem olegat-keybindings)
    (olegat-set-key (car elem) (cdr elem))))

(defun olegat-select-speedbar-frame ()
  "Internal use."
  (interactive)
  (select-frame-by-name "Speedbar"))

(defun olegat-hs-show-block ()
  "Internal use."
  (interactive)
  (unless hs-minor-mode (hs-minor-mode t))
  (hs-show-block))

(defun olegat-hs-hide-block ()
  "Internal use."
  (interactive)
  (unless hs-minor-mode (hs-minor-mode t))
  (hs-hide-block))


;;-----------------------------------------------------------------------------
;; Custom
;;-----------------------------------------------------------------------------
(defvar olegat-chrome-mode-on t "Internal use.")
(defun olegat-chrome-mode (enabled)
  "Toggle toolbar, scrollbars and menubar. The menubar on macOS is always on."
  (interactive (list (not olegat-chrome-mode-on)))
  (setq olegat-chrome-mode-on enabled)
  (let ((val (if enabled nil -1)))
    (tool-bar-mode val)
    (scroll-bar-mode val)
    (unless (string-equal window-system "ns")
      (menu-bar-mode val))))

(defvar olegat-vc-handled-backends nil "Internal use.")
(defun olegat-toggle-vc ()
  "Set/Unset vc-handled-backends to nil. Disabling vc can improve performance."
  (interactive)
  ;; Swap "vc-handled-backends" and "olegat-vc-handled-backends"
  (let ((tmp vc-handled-backends))
    (setq vc-handled-backends olegat-vc-handled-backends)
    (setq olegat-vc-handled-backends tmp)))

(defun copy-filepath-to-clipboard ()
  "Copy the path of current buffer to the clipboard."
  (interactive)
  (kill-new buffer-file-name)
  (let ((known-window-system nil))
    (when (string-equal window-system "w32")
      (setq known-window-system t)
      (w32-set-clipboard-data buffer-file-name))
    (unless known-window-system
      (message (concat "Unsupported window system: " window-system)))))


;;-----------------------------------------------------------------------------
;;  Find 'cmake-mode.el'
;;-----------------------------------------------------------------------------
(defvar olegat-cmake-mode-search-paths
  (append
   (when (string-equal system-type "windows-nt")
     '("C:/Program Files/CMake/share/emacs/site-lisp"
       "D:/CMake/share/emacs/sitp-lisp"))

   (when (string-equal system-type "cygwin")
     '("/cygdrive/c/Program Files/CMake/share/emacs/site-lisp"
       "/cygdrive/d/CMake/share/emacs/site-lisp"))

   (unless (string-equal system-type "windows-nt")
     '("/usr/local/share/emacs/site-lisp/cmake"
       "~/homebrew/share/emacs/site-lisp/cmake"
       "/mnt/c/Program Files/CMake/share/emacs/site-lisp"
       "/mnt/d/CMake/share/emacs/site-lisp")))

  "A list of directories to search through to find 'cmake-mode.el")

(defun olegat-find-cmake-mode-path ()
  "Loop through olegat-cmake-mode-search-paths and return the first
directory that exists and contains a 'cmake-mode.el'"
  (let (result)
    (dolist (elem olegat-cmake-mode-search-paths result)
      (unless result
        (when (file-exists-p (concat elem "/cmake-mode.el"))
          (setq result elem))))))


;;-----------------------------------------------------------------------------
;; Enable -fdiagnostics-color=always in Compilation mode
;;-----------------------------------------------------------------------------
;; https://stackoverflow.com/questions/13397737/ansi-coloring-in-compilation-mode
(defvar olegat-colorize-compilation-buffer-enabled nil "Internal use.")

(defun olegat-colorize-compilation-buffer () "Internal use."
  (when (eq major-mode 'compilation-mode)
    (ansi-color-apply-on-region compilation-filter-start (point-max))))

(defun olegat-toggle-compilation-color (&optional enable)
  "Toggle SGR ANSI color code parsing in the compile buffer.

Enabling ANSI colors can make the compile output more readable,
whereas disabling drastically improves performance."
  (interactive (list (not olegat-colorize-compilation-buffer-enabled)))
  (setq olegat-colorize-compilation-buffer-enabled enable)
  (if enable
    (add-hook    'compilation-filter-hook 'olegat-colorize-compilation-buffer)
    (remove-hook 'compilation-filter-hook 'olegat-colorize-compilation-buffer))
  (message (concat "Compilation color " (if enable "enabled" "disabled"))))


;;-----------------------------------------------------------------------------
;; TypeScript
;;-----------------------------------------------------------------------------
(defun olegat-init-typescript ()
  (use-package typescript-mode :ensure t)

  (use-package company
    :ensure t
    :hook ((typescript-mode . company-mode)))

  (use-package flycheck
    :ensure t
    :hook ((typescript-mode . flycheck-mode)))

  (use-package tide
    :ensure t
    :after (typescript-mode company flycheck)
    :hook ((typescript-mode . tide-setup)
           (typescript-mode . tide-hl-identifier-mode))))

(defun olegat-ag-charts-options ()
  (olegat-toggle-compilation-color t)
  (setq compile-command
        "~/la-casa/scripts/nx-emacs-adapter.bash build --skip-nx-cache")
  (setq ag-arguments
        '("--smart-case"
          "--stats"
          "--ignore-dir=node_modules"
          "--ignore-dir=dist")))


;;-----------------------------------------------------------------------------
;; Main (entry point)
;;-----------------------------------------------------------------------------
(defun olegat-init ()
  "Main entry point for 'olegat.el'"
  (olegat-init-keybindings)
  (olegat-init-platform)
  (olegat-init-defaults)
  (olegat-init-typescript)
  (olegat-init-hooks)
  (olegat-init-modes))

(provide 'olegat)
