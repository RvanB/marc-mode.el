;;; marc-mode.el --- Major mode for editing MARC records -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: Raiden van Bronkhorst
;; Version: 1.1
;; URL: https://github.com/RvanB/marc-mode.el

;;; Commentary:

;; A major mode for editing MARC (Machine-Readable Cataloging) records in MRK format.
;; This mode provides syntax highlighting and transient interfaces to MARC conversion tools.
;;
;; Features:
;; - Syntax highlighting for MARC tags and subfields
;; - Transient menu for yaz-marcdump (format conversion, charset conversion, etc.)
;; - Transient menu for MarcEdit/cmarcedit (MarcBreaker, MarcMaker, etc.)
;;
;; Usage:
;; - Open `.mrk` files directly to use marc-mode.
;; - Use `M-x marc-yaz-convert` for yaz-marcdump conversions.
;; - Use `M-x marc-marcedit-convert` for MarcEdit conversions.

(require 'cl-lib)
(require 'transient)

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

(defcustom marc-yaz-command "yaz-marcdump"
  "Command to use for yaz-marcdump conversion."
  :type 'string
  :group 'marc)

(defcustom marc-marcedit-command "cmarcedit"
  "Command to use for MarcEdit conversion.
This should be the path to cmarcedit."
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

(defconst marc-formats '("marc" "marcxml" "marcxchange" "line" "turbomarc" "json")
  "Valid format options for yaz-marcdump.")

(defun marc--shell-command (cmd)
  "Run CMD and display output in the echo area."
  (let ((output (string-trim (shell-command-to-string (concat cmd " 2>&1")))))
    (if (string-empty-p output)
        (message "marc: Done.")
      (message "%s" output))))

(defun marc--build-command (source &optional args)
  "Build yaz-marcdump command for SOURCE with ARGS."
  (concat marc-yaz-command
          (when args (concat " " (string-join args " ")))
          " " (shell-quote-argument source)))

(defun marc--run-command (source dest &optional args)
  "Run yaz-marcdump on SOURCE, writing to DEST with optional ARGS.
Both paths should already be expanded."
  (let ((cmd (concat (marc--build-command source args)
                     " > " (shell-quote-argument dest))))
    (marc--shell-command cmd)))

(defun marc-mrc-to-mrk (filename)
  "Convert binary MARC file FILENAME to line format using yaz-marcdump.
Returns the path to the temporary MRK file."
  (unless (stringp filename)
    (error "Filename must be a string, got: %s" (type-of filename)))
  (unless (file-exists-p filename)
    (error "File does not exist: %s" filename))
  (let ((temp-file (make-temp-file "marc-" nil ".mrk")))
    (marc--run-command (expand-file-name filename) temp-file '("-i" "marc" "-o" "line"))
    temp-file))

;;; Transient Menu

(defvar-local marc--input-file nil
  "Input file for current conversion.")

(defvar-local marc--output-file nil
  "Output file for current conversion.")

(defun marc--format-from-extension (filename)
  "Infer yaz-marcdump format from FILENAME extension."
  (pcase (downcase (or (file-name-extension filename) ""))
    ((or "mrc" "marc") "marc")
    ("mrk" "line")
    ("xml" "marcxml")
    ("json" "json")
    (_ nil)))

(defun marc--extension-from-format (format)
  "Get file extension for FORMAT."
  (pcase format
    ("line" "mrk")
    ("marc" "mrc")
    ((or "marcxml" "marcxchange" "turbomarc") "xml")
    ("json" "json")
    (_ "out")))

(defun marc--get-input-file ()
  "Get input file, defaulting to current buffer if it's a MARC file."
  (let ((default (when (and buffer-file-name
                            (string-match-p "\\.\\(mrc\\|mrk\\|marc\\|xml\\|json\\)\\'" buffer-file-name))
                   buffer-file-name)))
    (expand-file-name
     (read-file-name "Input file: " nil default t default))))

(defun marc--get-output-file (input-file)
  "Get output file based on INPUT-FILE."
  (let* ((input-format (marc--format-from-extension input-file))
         (default-format (if (equal input-format "marc") "line" "marc"))
         (ext (marc--extension-from-format default-format))
         (default-name (concat (file-name-sans-extension (file-name-nondirectory input-file)) "." ext)))
    (expand-file-name
     (read-file-name "Output file: "
                     (file-name-directory input-file)
                     nil nil default-name))))

(defun marc--description ()
  "Description for transient header."
  (format "Convert: %s -> %s"
          (file-name-nondirectory (or marc--input-file "?"))
          (file-name-nondirectory (or marc--output-file "?"))))

(transient-define-argument marc:--input-format ()
  :description "Input format"
  :class 'transient-option
  :shortarg "-i"
  :argument "-i "
  :choices marc-formats
  :init-value (lambda (obj)
                (oset obj value
                      (when marc--input-file
                        (marc--format-from-extension marc--input-file)))))

(transient-define-argument marc:--output-format ()
  :description "Output format"
  :class 'transient-option
  :shortarg "-o"
  :argument "-o "
  :choices marc-formats
  :init-value (lambda (obj)
                (oset obj value
                      (when marc--output-file
                        (marc--format-from-extension marc--output-file)))))

(transient-define-argument marc:--from-charset ()
  :description "From charset"
  :class 'transient-option
  :shortarg "-f"
  :argument "-f ")

(transient-define-argument marc:--to-charset ()
  :description "To charset"
  :class 'transient-option
  :shortarg "-t"
  :argument "-t ")

(transient-define-argument marc:--offset ()
  :description "Record offset"
  :class 'transient-option
  :shortarg "-O"
  :argument "-O ")

(transient-define-argument marc:--limit ()
  :description "Record limit"
  :class 'transient-option
  :shortarg "-L"
  :argument "-L ")

(transient-define-argument marc:--split-prefix ()
  :description "Split prefix"
  :class 'transient-option
  :shortarg "-s"
  :argument "-s ")

(transient-define-argument marc:--chunk-size ()
  :description "Chunk size"
  :class 'transient-option
  :shortarg "-C"
  :argument "-C ")

(transient-define-argument marc:--leader-spec ()
  :description "Leader spec"
  :class 'transient-option
  :shortarg "-l"
  :argument "-l ")

(defun marc--yaz-do-convert (&optional args)
  "Run the yaz-marcdump conversion with ARGS."
  (interactive (list (transient-args 'marc-yaz-convert--transient)))
  (unless (and marc--input-file marc--output-file)
    (user-error "Input and output files not set"))
  (marc--run-command marc--input-file marc--output-file args))

(transient-define-prefix marc-yaz-convert--transient ()
  "Transient menu for yaz-marcdump conversion."
  [:description marc--description]
  ["Format"
   (marc:--input-format)
   (marc:--output-format)]
  ["Character Set"
   (marc:--from-charset)
   (marc:--to-charset)]
  ["Record Range"
   (marc:--offset)
   (marc:--limit)]
  ["Other"
   (marc:--leader-spec)
   ("-v" "Verbose" "-v")
   ("-r" "Show summary" "-r")
   ("-p" "Print positions" "-p")]
  ["Actions"
   ("RET" "Convert" marc--yaz-do-convert)
   ("q" "Quit" transient-quit-one)])

;;;###autoload
(defun marc-yaz-convert ()
  "Convert MARC files using yaz-marcdump.
Prompts for input and output files, then opens a transient menu
with format options inferred from file extensions."
  (interactive)
  (let ((input (marc--get-input-file)))
    (setq marc--input-file input)
    (setq marc--output-file (marc--get-output-file input)))
  (marc-yaz-convert--transient))

;;; MarcEdit Transient

(defun marc--marcedit-run-command (source dest algorithm &optional args)
  "Run MarcEdit on SOURCE to DEST using ALGORITHM with ARGS."
  (let ((cmd (format "%s -s %s -d %s -%s%s"
                     marc-marcedit-command
                     (shell-quote-argument source)
                     (shell-quote-argument dest)
                     algorithm
                     (if args (concat " " (string-join args " ")) ""))))
    (marc--shell-command cmd)))

(defun marc--marcedit-get-files-and-convert (algorithm in-prompt in-ext out-prompt out-ext args)
  "Prompt for files and run ALGORITHM.
IN-PROMPT and IN-EXT describe the input file.
OUT-PROMPT and OUT-EXT describe the output file.
ARGS are additional flags from the transient."
  (let* ((default-input (when (and buffer-file-name
                                   (string-match-p (concat "\\." in-ext "\\'") buffer-file-name))
                          buffer-file-name))
         (input-file (expand-file-name
                      (read-file-name (format "%s (.%s): " in-prompt in-ext)
                                      nil default-input t default-input)))
         (default-output (concat (file-name-sans-extension (file-name-nondirectory input-file))
                                 "." out-ext))
         (output-file (expand-file-name
                       (read-file-name (format "%s (.%s): " out-prompt out-ext)
                                       (file-name-directory input-file)
                                       nil nil default-output))))
    (marc--marcedit-run-command input-file output-file algorithm args)))

(defun marc--marcedit-break (&optional args)
  "Run MarcBreaker (MARC -> MRK) with ARGS."
  (interactive (list (transient-args 'marc-marcedit-convert)))
  (marc--marcedit-get-files-and-convert "break" "MARC file" "mrc" "MRK file" "mrk" args))

(defun marc--marcedit-make (&optional args)
  "Run MarcMaker (MRK -> MARC) with ARGS."
  (interactive (list (transient-args 'marc-marcedit-convert)))
  (marc--marcedit-get-files-and-convert "make" "MRK file" "mrk" "MARC file" "mrc" args))

(defun marc--marcedit-marcxml (&optional args)
  "Run MARC -> MARCXML with ARGS."
  (interactive (list (transient-args 'marc-marcedit-convert)))
  (marc--marcedit-get-files-and-convert "marcxml" "MARC file" "mrc" "MARCXML file" "xml" args))

(defun marc--marcedit-xmlmarc (&optional args)
  "Run MARCXML -> MARC with ARGS."
  (interactive (list (transient-args 'marc-marcedit-convert)))
  (marc--marcedit-get-files-and-convert "xmlmarc" "MARCXML file" "xml" "MARC file" "mrc" args))

(defun marc--marcedit-marc2json (&optional args)
  "Run MARC -> JSON with ARGS."
  (interactive (list (transient-args 'marc-marcedit-convert)))
  (marc--marcedit-get-files-and-convert "marc2json" "MARC file" "mrc" "JSON file" "json" args))

(defun marc--marcedit-json2marc (&optional args)
  "Run JSON -> MARC with ARGS."
  (interactive (list (transient-args 'marc-marcedit-convert)))
  (marc--marcedit-get-files-and-convert "json2marc" "JSON file" "json" "MARC file" "mrc" args))

;;;###autoload
(transient-define-prefix marc-marcedit-convert ()
  "Convert MARC files using MarcEdit."
  ["Options"
   ("-u" "UTF-8 mode" "-utf8")
   ("-8" "MARC-8 mode" "-marc8")
   ("-p" "Continue on errors" "-pd")]
  ["Convert"
   ("b" "MarcBreaker (MARC -> MRK)" marc--marcedit-break)
   ("m" "MarcMaker (MRK -> MARC)" marc--marcedit-make)
   ("x" "MARC -> MARCXML" marc--marcedit-marcxml)
   ("X" "MARCXML -> MARC" marc--marcedit-xmlmarc)
   ("j" "MARC -> JSON" marc--marcedit-marc2json)
   ("J" "JSON -> MARC" marc--marcedit-json2marc)])

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
