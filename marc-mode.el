;;; marc-mode.el --- Major mode for editing MARC records -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Raiden van Bronkhorst
;; Version: 1.0
;; URL: https://github.com/RvanB/marc-mode

;;; Commentary:

;; A major mode for editing MARC (Machine-Readable Cataloging) records.
;; This mode provides syntax highlighting for MARC records in MRK format
;; and automatic conversion between binary MARC (.mrc) and readable MRK format.
;;
;; Features:
;; - Syntax highlighting for MARC tags and subfields
;; - Automatic conversion of binary .mrc files to editable MRK format
;; - Background saving to binary MARC format
;; - Integration with MarcEdit command-line tools

(require 'cl-lib)

(defgroup marc nil
  "Major mode for editing MARC records."
  :group 'text
  :prefix "marc-")

(defcustom marc-edit-command "cmarcedit"
  "Command to use for MARC format conversion.
This should be the path to MarcEdit's command-line tool (cmarcedit)
or another MARC conversion utility."
  :type 'string
  :group 'marc)

(defface marc-tag-face
  '((t :inherit font-lock-keyword-face))
  "Face for MARC tags (e.g., =245, =100)."
  :group 'marc)

(defface marc-subfield-face
  '((t :inherit font-lock-variable-name-face))
  "Face for MARC subfield codes (e.g., $a, $b)."
  :group 'marc)

(defvar marc-font-lock-keywords
  '(("^=[0-9A-Z][0-9A-Z][0-9A-Z]" . 'marc-tag-face)
    ("$[a-z0-9]" . 'marc-subfield-face))
  "Font lock keywords for MARC mode.")

;;;###autoload
(define-derived-mode marc-mode text-mode "MARC"
  "Major mode for editing MARC records in MRK format.

\\{marc-mode-map}"
  (setq font-lock-defaults '(marc-font-lock-keywords))
  (setq comment-start "")
  (setq comment-end ""))

;; File conversion functions

(defun marc-mrc-to-mrk (filename)
  "Convert binary MARC file FILENAME to MRK format using MarcEdit.
Returns the path to the temporary MRK file."
  (let ((temp-file (make-temp-file "marc-" nil ".mrk")))
    (shell-command (format "%s -s %s -d %s -break" 
                          marc-edit-command
                          (shell-quote-argument filename)
                          (shell-quote-argument temp-file)))
    temp-file))

(defun marc-mrk-to-mrc (mrk-content mrc-filename)
  "Convert MRK-CONTENT back to binary MARC file MRC-FILENAME using MarcEdit."
  (let ((temp-mrk-file (make-temp-file "marc-" nil ".mrk")))
    (with-temp-file temp-mrk-file
      (insert mrk-content))
    (shell-command (format "%s -s %s -d %s -make"
                          marc-edit-command
                          (shell-quote-argument temp-mrk-file)
                          (shell-quote-argument mrc-filename)))
    (delete-file temp-mrk-file)))

;; File handling hooks

(defun marc-find-file-hook ()
  "Handle opening of .mrc files by converting to MRK format."
  (when (and buffer-file-name
             (string-match "\\.mrc\\'" buffer-file-name))
    (let ((mrk-temp-file (make-temp-file "marc-" nil ".mrk")))
      (shell-command (format "%s -s %s -d %s -break" 
                            marc-edit-command
                            (shell-quote-argument buffer-file-name)
                            (shell-quote-argument mrk-temp-file)))
      (erase-buffer)
      (insert-file-contents mrk-temp-file)
      (marc-mode)
      (set-buffer-modified-p nil)
      ;; Store original filename and temp file for saving
      (setq-local marc-original-file buffer-file-name)
      (setq-local marc-temp-file mrk-temp-file)
      (setq-local marc-is-converted t)
      ;; Change buffer file name to temp file so normal saving works
      (setq buffer-file-name mrk-temp-file))))

(defun marc-after-save-hook ()
  "Convert MRK temp file back to binary MARC after saving."
  (when (and (boundp 'marc-is-converted) marc-is-converted
             (boundp 'marc-original-file) marc-original-file
             (boundp 'marc-temp-file) marc-temp-file)
    ;; Convert temp MRK file back to binary MARC in background
    (run-with-idle-timer 0.1 nil
                         (lambda (temp-file orig-file)
                           (marc-mrk-to-mrc 
                            (with-temp-buffer
                              (insert-file-contents temp-file)
                              (buffer-string))
                            orig-file))
                         marc-temp-file marc-original-file)))

(defun marc-kill-buffer-hook ()
  "Clean up temp file when buffer is killed."
  (when (and (boundp 'marc-temp-file) marc-temp-file
             (file-exists-p marc-temp-file))
    (delete-file marc-temp-file)))

;; Setup hooks and auto-mode-alist

;;;###autoload
(defun marc-mode-setup ()
  "Set up MARC mode hooks and file associations."
  (add-hook 'find-file-hook 'marc-find-file-hook)
  (add-hook 'after-save-hook 'marc-after-save-hook)
  (add-hook 'kill-buffer-hook 'marc-kill-buffer-hook)
  (add-to-list 'auto-mode-alist '("\\.mrk\\'" . marc-mode))
  (add-to-list 'auto-mode-alist '("\\.mrc\\'" . marc-mode)))

;; Initialize when loaded
(marc-mode-setup)

(provide 'marc-mode)
