;;; marc-mode.el --- Major mode for editing MARC records -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Raiden van Bronkhorst
;; Version: 1.0
;; URL: https://github.com/RvanB/marc-mode.el

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

(defcustom marc-auto-convert-on-open t
  "Whether to automatically convert .mrc files to .mrk format when opening.
If nil, .mrc files will be opened as binary files."
  :type 'boolean
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

(defun marc-mode-line-status ()
  "Return mode-line status indicator for MARC files."
  (when (and (boundp 'marc-is-converted) marc-is-converted
             (boundp 'marc-original-file) marc-original-file)
    (let* ((mrk-time (nth 5 (file-attributes buffer-file-name)))
           (mrc-time (nth 5 (file-attributes marc-original-file)))
           (out-of-sync (and mrk-time mrc-time (time-less-p mrc-time mrk-time))))
      (if out-of-sync " [MRC-STALE]" " [MRC-SYNC]"))))

(defvar marc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-s") 'marc-save-to-binary)
    (define-key map (kbd "C-c C-j") 'marc-jump-to-original)
    map)
  "Keymap for MARC mode.")

;;;###autoload
(define-derived-mode marc-mode text-mode "MARC"
  "Major mode for editing MARC records in MRK format.

Key bindings:
\\[marc-save-to-binary] - Save MRK buffer to binary MARC file
\\[marc-jump-to-original] - Jump to original .mrc file location

\\{marc-mode-map}"
  (setq font-lock-defaults '(marc-font-lock-keywords))
  (setq comment-start "")
  (setq comment-end "")
  ;; Add mode-line status for converted files
  (when (boundp 'marc-is-converted)
    (setq mode-line-format 
          (append mode-line-format '((:eval (marc-mode-line-status)))))))

;; File conversion functions

(defun marc-mrc-to-mrk (filename)
  "Convert binary MARC file FILENAME to MRK format using MarcEdit.
Returns the path to the temporary MRK file."
  (message "marc-mrc-to-mrk: Starting conversion of %s" filename)
  (let ((temp-file (make-temp-file "marc-" nil ".mrk")))
    (shell-command (format "%s -s %s -d %s -break" 
                          marc-edit-command
                          (shell-quote-argument filename)
                          (shell-quote-argument temp-file)))
    temp-file))

(defun marc-mrk-to-mrc (mrk-file mrc-filename)
  "Convert MRK file to binary MARC file MRC-FILENAME using MarcEdit."
  (message "marc-mrk-to-mrc: Converting %s to %s" mrk-file mrc-filename)
  (let ((cmd (format "%s -s %s -d %s -make"
                    marc-edit-command
                    (shell-quote-argument mrk-file)
                    (shell-quote-argument mrc-filename))))
    (message "marc-mrk-to-mrc: Executing command: %s" cmd)
    (shell-command cmd)
    (message "marc-mrk-to-mrc: Conversion complete")))

;; File handling hooks

(defun marc-find-all-auto-mrk (mrc-file)
  "Find all .auto.mrk files for MRC-FILE, sorted by modification time (newest first)."
  (let* ((base (file-name-sans-extension mrc-file))
         (candidates (list (concat base ".auto.mrk")))
         (counter 1))
    ;; Collect all .auto.N.mrk files
    (while (file-exists-p (format "%s.auto.%d.mrk" base counter))
      (push (format "%s.auto.%d.mrk" base counter) candidates)
      (setq counter (1+ counter)))
    ;; Return all existing files sorted by modification time
    (sort (cl-remove-if-not #'file-exists-p candidates)
          (lambda (a b)
            (time-less-p (nth 5 (file-attributes b))
                        (nth 5 (file-attributes a)))))))

(defun marc-get-next-auto-mrk-file (mrc-file)
  "Get the next available .auto.mrk filename for MRC-FILE."
  (let* ((base (file-name-sans-extension mrc-file))
         (auto-mrk (concat base ".auto.mrk")))
    (if (not (file-exists-p auto-mrk))
        auto-mrk
      (let ((counter 1))
        (while (file-exists-p (format "%s.auto.%d.mrk" base counter))
          (setq counter (1+ counter)))
        (format "%s.auto.%d.mrk" base counter)))))

(defun marc-find-file-hook ()
  "Handle opening of .mrc files by converting to MRK format."
  (when (and buffer-file-name
             (string-match "\\.mrc\\'" buffer-file-name)
             marc-auto-convert-on-open)
    (condition-case err
        (progn
          (message "marc-find-file-hook: Processing .mrc file %s" buffer-file-name)
          (let* ((auto-mrk-files (marc-find-all-auto-mrk buffer-file-name))
                 (selected-mrk (when (and auto-mrk-files
                                          (y-or-n-p "Recover from existing MRK file? "))
                                 (read-file-name 
                                  "Select MRK file to recover: "
                                  (file-name-directory (car auto-mrk-files))
                                  nil t
                                  (file-name-nondirectory (car auto-mrk-files))
                                  (lambda (name)
                                    (member (expand-file-name name (file-name-directory (car auto-mrk-files)))
                                           auto-mrk-files)))))
                 (mrk-file (or selected-mrk (marc-get-next-auto-mrk-file buffer-file-name)))
                 (use-existing (and selected-mrk t)))
      
      (if use-existing
          (progn
            (message "marc-find-file-hook: Recovering from %s" selected-mrk)
            (erase-buffer)
            (insert-file-contents selected-mrk))
        (progn
          (message "marc-find-file-hook: Converting to new MRK file %s" mrk-file)
          (message "Converting %s to MRK format..." (file-name-nondirectory buffer-file-name))
          (let ((cmd (format "%s -s %s -d %s -break" 
                            marc-edit-command
                            (shell-quote-argument buffer-file-name)
                            (shell-quote-argument mrk-file))))
            (message "marc-find-file-hook: Executing command: %s" cmd)
            (shell-command cmd))
          (message "marc-find-file-hook: Loading MRK content from %s" mrk-file)
          (erase-buffer)
          (insert-file-contents mrk-file)))
      
      (marc-mode)
      (set-buffer-modified-p nil)
      ;; Clear undo history to prevent undoing back to binary content
      (buffer-disable-undo)
      (buffer-enable-undo)
      ;; Store original filename and MRK file for saving
      (setq-local marc-original-file buffer-file-name)
      (setq-local marc-temp-file mrk-file)
      (setq-local marc-is-converted t)
      ;; Change buffer file name to MRK file so normal saving works
      (setq buffer-file-name mrk-file)
      ;; Update buffer name to reflect MRK editing of original file
      (let ((orig-name (file-name-nondirectory marc-original-file)))
        (rename-buffer (format "%s (MRK)" (file-name-sans-extension orig-name)) t))
      ;; Add mode-line status indicator
      (setq mode-line-format 
            (append mode-line-format '((:eval (marc-mode-line-status)))))
            (message "marc-find-file-hook: Setup complete for converted file")))
      (quit 
       (message "MARC conversion cancelled")
       ;; Kill the buffer to ensure clean state next time
       (kill-buffer (current-buffer))
       (signal 'quit nil))
      (error 
       (message "Error during MARC conversion: %s" err)
       (signal (car err) (cdr err))))))



(defun marc-save-to-binary ()
  "Convert current MRK buffer to binary MARC file."
  (interactive)
  (if (and (boundp 'marc-is-converted) marc-is-converted
           (boundp 'marc-original-file) marc-original-file
           (boundp 'marc-temp-file) marc-temp-file)
      (progn
        (message "Converting MRK to binary MARC file %s..." marc-original-file)
        (marc-mrk-to-mrc marc-temp-file marc-original-file)
        (message "Successfully saved to %s" marc-original-file))
    (let ((mrc-file (read-file-name "Save as binary MARC file: " nil nil nil 
                                    (concat (file-name-sans-extension 
                                            (buffer-name)) ".mrc"))))
      (message "Converting MRK to binary MARC file %s..." mrc-file)
      (marc-mrk-to-mrc buffer-file-name mrc-file)
      (message "Successfully saved to %s" mrc-file))))

(defun marc-jump-to-original ()
  "Jump to dired for the original .mrc file location."
  (interactive)
  (if (and (boundp 'marc-is-converted) marc-is-converted
           (boundp 'marc-original-file) marc-original-file)
      (dired-jump nil marc-original-file)
    (message "Not a converted MARC file")))

;; Setup hooks and auto-mode-alist

;;;###autoload
(defun marc-mode-setup ()
  "Set up MARC mode hooks and file associations."
  (add-hook 'find-file-hook 'marc-find-file-hook)
  (add-to-list 'auto-mode-alist '("\\.mrk\\'" . marc-mode))
  (add-to-list 'auto-mode-alist '("\\.mrc\\'" . marc-mode)))

;; Initialize when loaded
(marc-mode-setup)

(provide 'marc-mode)
