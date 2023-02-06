(require 'ansi-color)

;;-----------------------------------------------------------------------------
;;  Key bindings
;;-----------------------------------------------------------------------------
(defvar olegat-keychar "§")

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
    ))

(defun olegat-insert-keychar ()
  (interactive)
  (insert olegat-keychar))

(defun olegat-insert-pound-sign ()
  (interactive)
  (insert "£"))

(defun olegat-set-key (keys function)
  (global-set-key
   (kbd (concat olegat-keychar " " keys)) function))

(defun olegat-apply-keybindings ()
  ;; Escape the § symbol
  (global-set-key (kbd olegat-keychar) nil)
  (olegat-set-key olegat-keychar 'olegat-insert-keychar)
  (dolist (elem olegat-keybindings)
    (olegat-set-key (car elem) (cdr elem))))

(defun olegat-select-speedbar-frame ()
  (interactive)
  (select-frame-by-name "Speedbar"))


;;-----------------------------------------------------------------------------
;; Custom
;;-----------------------------------------------------------------------------
(defvar olegat-chrome-mode-on t)
(defun olegat-chrome-mode (enabled)
  (interactive (list (not olegat-chrome-mode-on)))
  (setq olegat-chrome-mode-on enabled)
  (let ((val (if enabled nil -1)))
    (tool-bar-mode val)
    (scroll-bar-mode val)
    (unless (string-equal window-system "ns")
      (menu-bar-mode val))))

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
       "/mnt/d/CMake/share/emacs/site-lisp"))))

(defun olegat-find-cmake-mode-path ()
  (let (result)
    (dolist (elem olegat-cmake-mode-search-paths result)
      (unless result
        (when (file-exists-p (concat elem "/cmake-mode.el"))
          (setq result elem))))))


;;-----------------------------------------------------------------------------
;; Enable -fdiagnostics-color=always in Compilation mode
;;-----------------------------------------------------------------------------
;; https://stackoverflow.com/questions/13397737/ansi-coloring-in-compilation-mode
(defvar olegat-colorize-compilation-buffer-enabled nil)

(defun olegat-colorize-compilation-buffer ()
  (when (eq major-mode 'compilation-mode)
    (ansi-color-apply-on-region compilation-filter-start (point-max))))

(defun olegat-toggle-compilation-color (&optional enable)
  (interactive (list (not olegat-colorize-compilation-buffer-enabled)))
  (setq olegat-colorize-compilation-buffer-enabled enable)
  (if enable
    (add-hook    'compilation-filter-hook 'olegat-colorize-compilation-buffer)
    (remove-hook 'compilation-filter-hook 'olegat-colorize-compilation-buffer))
  (message (concat "Compilation color " (if enable "enabled" "disabled"))))



(provide 'olegat)
