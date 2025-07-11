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

;;; Constants

(defconst marc-ldr-pattern "^=LDR"
  "Regular expression pattern to match MARC leader fields.")

(defconst marc-tag-pattern "^=[0-9A-Z][0-9A-Z][0-9A-Z]"
  "Regular expression pattern to match MARC tag fields.")

(defconst marc-subfield-pattern "$[a-z0-9]"
  "Regular expression pattern to match MARC subfield codes.")

(defconst marc-timer-delay 0.01
  "Delay in seconds between asynchronous record processing operations.")

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
  `((,marc-tag-pattern . 'marc-tag-face)
    (,marc-subfield-pattern . 'marc-subfield-face))
  "Font lock keywords for MARC mode.")

;;; Core MARC Parsing Utilities

(defun marc-mode-line-status ()
  "Return mode-line status indicator for MARC files."
  (when (and (boundp 'marc-is-converted) marc-is-converted
             (boundp 'marc-original-file) marc-original-file)
    (let* ((mrk-modification-time (nth 5 (file-attributes buffer-file-name)))
           (mrc-modification-time (nth 5 (file-attributes marc-original-file)))
           (mrc-out-of-sync (and mrk-modification-time mrc-modification-time 
                                (time-less-p mrc-modification-time mrk-modification-time))))
      (if mrc-out-of-sync " [MRC-STALE]" " [MRC-SYNC]"))))

(defun marc-extract-field-value (field-tag &optional subfield-code)
  "Extract the value of FIELD-TAG from the current MARC record.
If SUBFIELD-CODE is provided (e.g., \"a\"), extract only that specific subfield."
  (unless (stringp field-tag)
    (error "Field tag must be a string, got: %s" (type-of field-tag)))
  (when (and subfield-code (not (stringp subfield-code)))
    (error "Subfield code must be a string, got: %s" (type-of subfield-code)))
  (save-excursion
    (let ((extracted-field-values nil)
          (current-record-start (save-excursion
                                  (re-search-backward marc-ldr-pattern nil t)
                                  (point)))
          (current-record-end (save-excursion
                                (forward-line)
                                (if (re-search-forward marc-ldr-pattern nil t)
                                    (progn (beginning-of-line) (point))
                                  (point-max)))))
      (goto-char current-record-start)
      (while (re-search-forward (format "^=%s\\(.*\\)$" field-tag) current-record-end t)
        (let ((field-content (match-string 1)))
          (if subfield-code
              ;; Extract specific subfield
              (let ((subfield-values nil)
                    (start 0))
                (while (string-match (format "\\$%s\\([^$]+\\)" subfield-code) field-content start)
                  (push (match-string 1 field-content) subfield-values)
                  (setq start (match-end 0)))
                (when subfield-values
                  (push (mapconcat 'identity (nreverse subfield-values) "; ") extracted-field-values)))
            ;; Return entire field
            (push field-content extracted-field-values))))
      (nreverse extracted-field-values))))

(defun marc-initialize-extraction-buffer (results-buffer source-buffer tag subfield)
  "Initialize RESULTS-BUFFER for field extraction display."
  (with-current-buffer results-buffer
    (marc-extraction-results-mode)
    (setq-local marc-source-buffer source-buffer)
    (setq-local marc-extracted-tag tag)
    (setq-local marc-extracted-subfield subfield)
    (let ((inhibit-read-only t))
      (erase-buffer))))

(defun marc-extract-single-record (pos tag subfield source-buffer results-buffer field-count)
  "Extract fields from a single MARC record at POS.
Returns the position of the next record or nil if no more records."
  (with-current-buffer source-buffer
    (save-excursion
      (goto-char pos)
      ;; Move to a record if not already at one
      (unless (looking-at marc-ldr-pattern)
        (if (re-search-forward marc-ldr-pattern nil t)
            (beginning-of-line)
          (goto-char (point-max))))
      
      ;; Process the record if we found one
      (when (looking-at marc-ldr-pattern)
        (let ((values (marc-extract-field-value tag subfield)))
          (when values
            (with-current-buffer results-buffer
              (let ((inhibit-read-only t))
                (goto-char (point-max))
                (dolist (value values)
                  (insert value "\n")
                  (setq field-count (1+ field-count)))))))
        
        ;; Move to next position for future processing
        (forward-line)
        (if (re-search-forward marc-ldr-pattern nil t)
            (progn
              (beginning-of-line)
              ;; Return position for next call
              (point))
          nil)))))

(defun marc-process-records-async (source-buffer results-buffer tag subfield)
  "Process all MARC records asynchronously using timers."
  (let* ((next-pos (with-current-buffer source-buffer
                     (save-excursion
                       (goto-char (point-min))
                       (point))))
         (field-count 0))
    
    ;; The worker function that processes one record at a time
    (letrec ((worker
              (lambda ()
                (when next-pos
                  (setq next-pos (marc-extract-single-record 
                                 next-pos tag subfield source-buffer 
                                 results-buffer field-count))
                  (if next-pos
                      (run-with-timer marc-timer-delay nil worker)
                    ;; No more records, we're done
                    (with-current-buffer results-buffer
                      (goto-char (point-min))))))))
      ;; Start the extraction process
      (funcall worker))))

(defun marc-buffer-extract-fields (tag buffer-name &optional subfield)
  "Extract all occurrences of TAG field from MARC records and display in BUFFER-NAME.
If SUBFIELD is provided, extract only that specific subfield (e.g., \"a\")."
  (unless (stringp tag)
    (error "Tag must be a string, got: %s" (type-of tag)))
  (unless (stringp buffer-name)
    (error "Buffer name must be a string, got: %s" (type-of buffer-name)))
  (when (and subfield (not (stringp subfield)))
    (error "Subfield must be a string, got: %s" (type-of subfield)))
  (let* ((results-buffer (get-buffer-create buffer-name))
         (source-buffer (current-buffer)))
    
    ;; Initialize results buffer
    (marc-initialize-extraction-buffer results-buffer source-buffer tag subfield)
    
    ;; Process all records asynchronously
    (marc-process-records-async source-buffer results-buffer tag subfield)
    
    ;; Show the results buffer and return it
    (pop-to-buffer results-buffer)
    results-buffer))

(defun marc-extract-fields (tag)
  "Extract all occurrences of TAG field from current MARC file.
Results are displayed in a separate buffer."
  (interactive "sExtract field tag (use tag$subfield for specific subfield, e.g. 245$a): ")
  (if (not (derived-mode-p 'marc-mode))
      (message "Not in a MARC buffer")
    (let* ((parts (split-string tag "\\$"))
           (field-tag (car parts))
           (subfield (when (> (length parts) 1) (cadr parts)))
           (buffer-name (format "*MARC Fields %s%s from %s*" 
                              field-tag
                              (if subfield (format "$%s" subfield) "")
                              (buffer-name))))
      (marc-buffer-extract-fields field-tag buffer-name subfield))))

;;; Extraction Results Mode
(define-derived-mode marc-extraction-results-mode special-mode "MARC Fields"
  "Major mode for displaying extracted MARC fields.

\\{marc-extraction-results-mode-map}"
  (setq buffer-read-only t)
  (setq truncate-lines nil)
  (setq header-line-format 
        (format "MARC Field Extraction Results [%s%s]" 
                (if (boundp 'marc-extracted-tag) marc-extracted-tag "unknown")
                (if (and (boundp 'marc-extracted-subfield) marc-extracted-subfield)
                    (format "$%s" marc-extracted-subfield) "")))
  (goto-char (point-min)))

(defvar marc-extraction-results-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "g") 'marc-refresh-extracted-fields)
    map)
  "Keymap for MARC extraction results mode.")

(defun marc-refresh-extracted-fields ()
  "Refresh the extracted fields from the source buffer."
  (interactive)
  (if (and (boundp 'marc-source-buffer) 
           (buffer-live-p marc-source-buffer)
           (boundp 'marc-extracted-tag))
      (marc-buffer-extract-fields 
       marc-extracted-tag 
       (buffer-name)
       (when (boundp 'marc-extracted-subfield) marc-extracted-subfield))
    (message "Cannot refresh: source information not available")))

;;; Major Mode Definition

;;;###autoload
(define-derived-mode marc-mode text-mode "MARC"
  "Major mode for editing MARC records in MRK format.

Key bindings:
\\[marc-save-to-binary] - Save MRK buffer to binary MARC file
\\[marc-jump-to-original] - Jump to original .mrc file location
\\[marc-next-record] - Move to next MARC record
\\[marc-previous-record] - Move to previous MARC record
\\[marc-extract-fields] - Extract field values across all records

\\{marc-mode-map}"
  (setq font-lock-defaults '(marc-font-lock-keywords))
  (setq comment-start "")
  (setq comment-end "")
  ;; Add mode-line status for converted files
  (when (boundp 'marc-is-converted)
    (setq mode-line-format 
          (append mode-line-format '((:eval (marc-mode-line-status)))))))

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

(defun marc-mrk-to-mrc (mrk-file mrc-filename)
  "Convert MRK file to binary MARC file MRC-FILENAME using MarcEdit."
  (unless (stringp mrk-file)
    (error "MRK file must be a string, got: %s" (type-of mrk-file)))
  (unless (stringp mrc-filename)
    (error "MRC filename must be a string, got: %s" (type-of mrc-filename)))
  (unless (file-exists-p mrk-file)
    (error "MRK file does not exist: %s" mrk-file))
  (message "marc-mrk-to-mrc: Converting %s to %s" mrk-file mrc-filename)
  (let ((cmd (format "%s -s %s -d %s -make"
                    marc-edit-command
                    (shell-quote-argument mrk-file)
                    (shell-quote-argument mrc-filename))))
    (message "marc-mrk-to-mrc: Executing command: %s" cmd)
    (shell-command cmd)
    (message "marc-mrk-to-mrc: Conversion complete")))

;;; File Handling Hooks

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

(defun marc-handle-conversion-error (err buffer-file-name)
  "Handle errors during MARC file conversion."
  (cond
   ((eq (car err) 'quit)
    (message "MARC conversion cancelled")
    ;; Kill the buffer to ensure clean state next time
    (kill-buffer (current-buffer))
    (signal 'quit nil))
   (t
    (message "Error during MARC conversion of %s: %s" 
             (file-name-nondirectory buffer-file-name) err)
    (signal (car err) (cdr err)))))

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
      (error (marc-handle-conversion-error err buffer-file-name)))))



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

;;; Setup and Initialization

;;;###autoload
(defun marc-mode-setup ()
  "Set up MARC mode hooks and file associations."
  (add-hook 'find-file-hook 'marc-find-file-hook)
  (add-to-list 'auto-mode-alist '("\\.mrk\\'" . marc-mode))
  (add-to-list 'auto-mode-alist '("\\.mrc\\'" . marc-mode)))

;; Initialize when loaded
(marc-mode-setup)

(provide 'marc-mode)
;;; marc-mode.el ends here
