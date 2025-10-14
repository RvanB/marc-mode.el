;;; marc-mode.el --- Major mode for editing MARC records -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: Raiden van Bronkhorst
;; Version: 1.1
;; URL: https://github.com/RvanB/marc-mode.el

;;; Commentary:

;; A major mode for editing MARC (Machine-Readable Cataloging) records in MRK format.
;; This mode provides syntax highlighting and field extraction tools for MRK files.
;;
;; Features:
;; - Syntax highlighting for MARC tags and subfields
;; - Field extraction commands with asynchronous processing
;; - Command for converting binary MARC files (.mrc) to MRK format manually
;;
;; Usage:
;; - Open `.mrk` files directly to use marc-mode.
;; - Use the `marc-convert-mrc-to-mrk` command for converting .mrc files to .mrk.

(require 'cl-lib)

;;; Constants

(defconst marc-ldr-pattern "^=LDR"
  "Regular expression pattern to match MARC leader fields.")

(defconst marc-tag-pattern "^=[0-9A-Z][0-9A-Z][0-9A-Z]"
  "Regular expression pattern to match MARC tag fields.")

(defconst marc-subfield-pattern "$[a-z0-9]"
  "Regular expression pattern to match MARC subfield codes.")

;;; Customization Variables and Faces

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
  `((,marc-tag-pattern . 'marc-tag-face)
    (,marc-subfield-pattern . 'marc-subfield-face))
  "Font lock keywords for MARC mode.")

;;; File Conversion Functions

(defun marc-mrc-to-mrk (filename)
  "Convert binary MARC file FILENAME to MRK format using MarcEdit.
Returns the path to the temporary MRK file."
  (unless (stringp filename)
    (error "Filename must be a string, got: %s" (type-of filename)))
  (unless (file-exists-p filename)
    (error "File does not exist: %s" filename))
  (message "marc-mrc-to-mrk: Starting conversion of %s" filename)
  (let ((temp-file (make-temp-file "marc-" nil ".mrk")))
    (shell-command (format "%s -s %s -d %s -break" 
                          marc-edit-command
                          (shell-quote-argument filename)
                          (shell-quote-argument temp-file)))
    temp-file))

;;; Interactive Functions

;;;###autoload
(defun marc-convert-mrc-to-mrk (mrc-file)
  "Convert the specified binary MARC file (MRC-FILE) to a human-readable MRK file.
Prompts for the binary MARC file and saves the MRK output to a user-specified location."
  (interactive "fSelect MRC file to convert: ")
  (let ((output-file (read-file-name "Save MRK file as: " nil nil nil 
                                     (concat (file-name-sans-extension mrc-file) ".mrk"))))
    (message "marc-convert-mrc-to-mrk: Converting %s to %s..." mrc-file output-file)
    (shell-command (format "%s -s %s -d %s -break"
                           marc-edit-command
                           (shell-quote-argument mrc-file)
                           (shell-quote-argument output-file)))
    (message "Conversion complete: %s" output-file)))

;;; Major Mode Definition

;;;###autoload
(define-derived-mode marc-mode text-mode "MARC"
  "Major mode for editing MARC records in MRK format.

Key bindings:
\\{marc-mode-map}"
  (setq font-lock-defaults '(marc-font-lock-keywords))
  (setq comment-start "")
  (setq comment-end ""))

;;; Setup and Initialization

;;;###autoload
(defun marc-mode-setup ()
  "Set up file associations for MARC mode."
  (add-to-list 'auto-mode-alist '("\\.mrk\\'" . marc-mode)))

;; Initialize when loaded
(marc-mode-setup)

(provide 'marc-mode)
;;; marc-mode.el ends here
