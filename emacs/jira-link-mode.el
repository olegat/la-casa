;;; jira-link-mode.el --- Minor mode to hyperlink JIRA ticket IDs -*- lexical-binding: t -*-

;; Author: Oli Legat
;; URL: https://github.com/olegat/la-casa
;; Version: 1.0
;; Package-Requires: ((emacs "24.3"))

;;; Commentary:

;; This minor mode automatically hyperlinks JIRA ticket IDs (e.g., AG-1234)
;; in text buffers. The list of valid ticket prefixes and the base URL
;; can be customized.

;;; Usage:

;; (setq jira-link-prefixes '("AG" "CRT" "DEV"))
;; (setq jira-link-base-url "https://your-jira-instance.atlassian.net/browse/")

;; (add-to-list 'load-path "/path/to/jira-link-mode")
;; (require 'jira-link-mode)

;;; Code:

(require 'thingatpt)

(defgroup jira-link-mode nil
  "Minor mode to hyperlink JIRA ticket IDs."
  :group 'convenience
  :prefix "jira-link-")

(defcustom jira-link-prefixes '("AG" "CRT")
  "List of valid JIRA ticket prefixes."
  :type '(repeat string)
  :group 'jira-link-mode)

(defcustom jira-link-base-url "https://ag-grid.atlassian.net/browse/"
  "Base URL for JIRA ticket links."
  :type 'string
  :group 'jira-link-mode)

(defvar jira-link--regex nil
  "Regex pattern to match JIRA ticket IDs.")
(make-variable-buffer-local 'jira-link--regex)

(defun jira-link--update-regex ()
  "Update the regex used to match JIRA ticket IDs."
  (setq jira-link--regex
        (concat "\\b\\("
                (mapconcat 'identity jira-link-prefixes "\\|")
                "\\)-[0-9]+\\b")))

(defun jira-link--make-url (ticket-id)
  "Generate the full URL for the given TICKET-ID."
  (concat jira-link-base-url ticket-id))

(defun jira-link--add-overlays ()
  "Add overlays to JIRA ticket IDs in the buffer."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward jira-link--regex nil t)
      (let ((url (jira-link--make-url (match-string 0))))
        (jira-link--add-overlay (match-beginning 0) (match-end 0) url)))))

(defun jira-link--add-overlay (start end url)
  "Add an overlay from START to END that opens URL when clicked."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face 'link)
    (overlay-put overlay 'mouse-face 'highlight)
    (overlay-put overlay 'help-echo url)
    (overlay-put overlay 'keymap (let ((map (make-sparse-keymap)))
                                   (define-key map [mouse-1]
                                     `(lambda ()
                                        (interactive)
                                        (browse-url ,url)))
                                   map))))

(defun jira-link--remove-overlays ()
  "Remove all JIRA link overlays from the buffer."
  (remove-overlays (point-min) (point-max)))

;;;###autoload
(define-minor-mode jira-link-mode
  "Minor mode to hyperlink JIRA ticket IDs."
  :lighter " JIRA"
  :group 'jira-link-mode
  (if jira-link-mode
      (progn
        (jira-link--update-regex)
        (add-hook 'after-change-functions 'jira-link--on-change nil t)
        (jira-link--add-overlays))
    (remove-hook 'after-change-functions 'jira-link--on-change t)
    (jira-link--remove-overlays)))

(defun jira-link--on-change (_beg _end _len)
  "Callback for `after-change-functions' to update overlays."
  (jira-link--remove-overlays)
  (jira-link--add-overlays))

(provide 'jira-link-mode)

;;; jira-link-mode.el ends here
