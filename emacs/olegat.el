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

;; Buffer selection
(global-set-key (kbd "§ x <right>") 'previous-buffer)
(global-set-key (kbd "§ x <left>")  'next-buffer)

;; Misc
(global-set-key (kbd "§ TAB") 'imenu)
(global-set-key (kbd "§ e r") 'eval-region)
(global-set-key (kbd "§ b")   'recompile)
(global-set-key (kbd "§ s l") 'sort-lines)
(global-set-key (kbd "§ f n d") 'find-name-dired)


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

(provide 'olegat)
