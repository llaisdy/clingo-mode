;;; clingo-mode.el --- A major mode for editing Answer Set Programs -*- lexical-binding: t -*-

;; Copyright (c) 2020 by Ivan Uemlianin
;; Copyright (c) 2017 by Henrik Jürges

;; Maintainer: Ivan Uemlianin <ivan@llaisdy.com>
;; Author: Ivan Uemlianin <ivan@llaisdy.com>
;;         Henrik Jürges <juerges.henrik@gmail.com>
;; URL: https://github.com/llaisdy/clingo-mode
;; Version: 0.4.1
;; X-Bogus-Bureaucratic-Cruft: see also <https://github.com/santifa/pasp-mode>
;; Package-requires: ((emacs "24.3"))
;; Keywords: asp, clingo, Answer Set Programs, Potassco, Major mode, languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A major mode for editing Answer Set Programs, formally Potassco
;; Answer Set Programs (https://potassco.org/).
;;
;; Answer Set Programs are mainly used to solve complex combinatorial
;; problems by impressive search methods.
;; The modeling language follows a declarative approach with a minimal
;; amount of fixed syntax constructs.

;;; Install

;; Open the file with Emacs and run "M-x eval-buffer"
;; Open an ASP file and run "M-x clingo-mode"

;; To manually load this file within your Emacs config
;; add this file to your load path and place
;; (require 'clingo-mode)
;; in your init file.

;; See "M-x customize-mode clingo-mode" for information
;; about mode configuration.

;;; Features

;; - Syntax highlighting (predicates can be toggled)
;; - Commenting of blocks and standard
;; - Run ASP program from within Emacs and get the compilation output
;; - Auto-load mode when a *.lp file is opened

;;; Keybindings

;; "C-c C-e" - Call clingo with current buffer and an instance file
;; "C-c C-b" - Call clingo with current buffer
;; "C-c C-r" - Call clingo with current region
;; "C-c C-c" - Comment region
;; "C-c C-u" - Uncomment region

;; Remark

;; I'm not an elisp expert, this is a very basic major mode.
;; It is intended to get my hands dirty with elisp but also
;; to be a help full tool.
;; This mode should provide a basic environment for further
;; integration of Answer Set Programs into Emacs.

;; Ideas, issues and pull requests are highly welcome!

;;; Todo:

;; - TOP: make clingo-generate-echo safe
;; - Smart indentation based on nesting depth
;; - Refactoring of predicates/variables (complete buffer and #program parts)
;; - Color compilation output
;; - Smart rearrangement of compilation output (predicates separated, table...)
;; - yas-snippet for rules; constraints; soft constraints; generation?
;; - sync as much as possible with vim-syntax-clingo

;;; Code:

(require 'compile)

;;; Customization

(defgroup clingo nil
  "Major mode for editing Answer Set Programs."
  :group 'languages
  :prefix "clingo-")

(defconst clingo-mode-version "0.4.0" "Version of `clingo-mode'.")

(defcustom clingo-indentation 2
  "Level of indentation."
  :type 'integer
  :group 'clingo-mode)

(defcustom clingo-path (executable-find "clingo")
  "Path to clingo binary used for execution."
  :type 'string
  :group 'clingo-mode)

(defcustom clingo-options '()
  "Command line options passed to clingo."
  :type '(repeat string)
  :group 'clingo-mode
  :safe #'list-of-strings-p)

(defcustom clingo-pretty-symbols-p t
  "Use Unicode characters where appropriate."
  :type 'boolean
  :group 'clingo-mode)

;;; Pretty Symbols

(defvar clingo-mode-hook
  (lambda ()
    (when clingo-pretty-symbols-p
      (push '(":-" . ?⊢) prettify-symbols-alist)
      (push '(">=" . ?≥) prettify-symbols-alist)
      (push '("<=" . ?≤) prettify-symbols-alist)
      (push '("!=" . ?≠) prettify-symbols-alist)
      (push '("not" . ?¬) prettify-symbols-alist))))

;;; Syntax table
(defvar clingo-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?' "w" table)
    (modify-syntax-entry ?% "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?, "_ p" table)
    table)
  "Syntax table for `clingo-mode`.")

;;; Syntax highlighting faces

(defface clingo-atom-face
  '((t (:inherit font-lock-keyword-face :weight normal)))
  "Face for ASP atoms (starting with lower case).")

(defface clingo-construct-face
  '((default (:inherit font-lock-builtin-face :height 1.1)))
  "Face for ASP base constructs.")

(defface clingo-statistics-face
  '((default (:inherit compilation-info :weight bold)))
  "Face for Clingo statistics.")

;; Syntax highlighting

(defvar clingo--constructs
  '("\\.\\|:-\\|:\\|;\\|:~\\|,\\|(\\|)\\|{\\|}\\|[\\|]\\|not " . 'clingo-construct-face)
   "ASP constructs.")

(defconst clingo--constant
  '("#[[:word:]]+" . font-lock-builtin-face)
  "ASP constants.")

(defconst clingo--variable
  '("[^[:word:]]\\(_*\\([[:upper:]][[:word:]_']*\\)?\\)" . (1 font-lock-variable-name-face))
  "ASP variable.")

(defconst clingo--variable2
  '("\\_<\\(_*[[:upper:]][[:word:]_']*\\)\\_>" . (1 font-lock-variable-name-face))
  "ASP variable 2.")

(defconst clingo--atom
  '("_*[[:lower:]][[:word:]_']*" . 'clingo-atom-face)
  "ASP atoms.")

(defvar clingo-font-lock-keywords
  (list clingo--constructs
        clingo--constant
        clingo--variable2
        clingo--variable
        clingo--atom)
  "Font lock keywords in `clingo-mode'.")

;;; Compilation

(defvar clingo-error-regexp
  "^\\(<?[a-zA-Z\.0-9_/\\-]+>?\\):\\([0-9]+\\):\\([0-9-]+\\)"
  "Regular expression to match clingo errors.")

(defvar clingo-compilation-keywords
  `(("\\(Answer\\): \\([0-9]+\\)\n"
     (1 font-lock-function-name-face) (2 compilation-line-face)
     (,(car clingo--atom) nil nil
      (0 'clingo-atom-face)))
    ("\\(Answer\\): \\([0-9]+\\)\n"
     (,(car clingo--constructs) nil nil
      (0 'clingo-construct-face)))
    ("^ *\\<\\([[:alnum:] _/.+-]+\\)\\> +:"
     (1 'clingo-statistics-face))))

(defvar clingo-error-regexp-alist
  `((,clingo-error-regexp 1 2 3))
  "Alist that specifies how to match Clingo errors as per
`compilation-error-regexp-alist'.")

(defconst clingo-exit-codes
  ;; Exit codes as per Clasp::Cli::ExitCode enumeration
  '((0 . "nothing to be done")
    (1 . "interrupted")
    (10 . "satisfiable")
    (11 . "satisfiable, interrupted")
    (20 . "unsatisfiable")
    (30 . "satisfiable, all models found")
    (33 . "memory error")
    (65 . "internal error")
    (128 . "syntax or command line error")))

(defun clingo-exit-status-success-p (exit-status)
  "Return `t' if EXIT_STATUS conforms to a successful run of Clingo."
  (< exit-status 32))

(defun clingo-compilation-filter ()
  "Filter Clingo output.

Currently, this function merely deletes ANSI terminal escape codes."
  (ansi-color-apply-on-region compilation-filter-start (point-max))
  (save-excursion
    (while (re-search-forward "^[\\[[0-9]+[a-z]" nil t)
      (replace-match ""))))

(defvar-local clingo--cached-exit-message nil
  "Cached exit message for use in `clingo-compilation-finish'.")
(defvar-local clingo--cached-process-status nil
  "Cached process status for use in `clingo-compilation-finish'.")

(defun clingo-exit-message-function (process-status exit-status msg)
  "Return an exit message appropriate for EXIT_STATUS."
  (setq clingo--cached-process-status process-status)
  (setq clingo--cached-exit-message
        (cons (or (cdr-safe (assoc exit-status clingo-exit-codes))
                  "unknown status code")
              exit-status)))

(defun clingo-compilation-finish (buffer message)
  "Hook run when a clingo process is finished in BUFFER."
  (let* ((process-status clingo--cached-process-status)
         (status clingo--cached-exit-message)
         (exit-status (cdr status)))
    (setq mode-line-process
          (list
           (let ((out-string (format ":%s [%s]" process-status (cdr status)))
                 (msg (format "%s %s" mode-name
                              (replace-regexp-in-string "\n?$" ""
                                                        (car status)))))
             (propertize out-string
                         'help-echo msg
                         'face (if (clingo-exit-status-success-p exit-status)
                                   'compilation-mode-line-exit
                                 'compilation-mode-line-fail)))
           compilation-mode-line-errors))
    (force-mode-line-update)))

(define-compilation-mode clingo-compilation-mode "ASP"
  "Major mode for running ASP files."
  (set (make-local-variable 'compilation-error-regexp-alist)
       clingo-error-regexp-alist)
  (set (make-local-variable 'font-lock-multiline) t)
  (set (make-local-variable 'compilation-exit-message-function)
       #'clingo-exit-message-function)
  (add-hook 'compilation-filter-hook #'clingo-compilation-filter nil t)
  (add-hook 'compilation-finish-functions #'clingo-compilation-finish nil t))

(font-lock-add-keywords 'clingo-compilation-mode clingo-compilation-keywords)

(defun clingo-generate-command (encoding options &optional instance)
  "Generate Clingo call with some ASP input file.

Argument ENCODING The current buffer which holds the problem encoding.
Argument OPTIONS Options (possibly empty string) sent to clingo.
Optional argument INSTANCE The problem instance which is solved by the encoding.
  If no instance it is assumed to be also in the encoding file."
  (let ((options (if options options ""))
        (files (if instance
                   (list encoding instance)
                 (list encoding))))
    (concat clingo-path " " options " "
            (mapconcat #'identity files " "))))

(defun clingo-run-clingo (encoding options &optional instance)
  "Run Clingo with some ASP input files.
Be aware: Partial ASP code may lead to abnormal exits
  while the result is sufficient.

Argument ENCODING The current buffer which holds the problem encoding.
Argument OPTIONS Options (possibly empty string) sent to clingo.
Optional argument INSTANCE The problem instance which is solved by the encoding.
  If no instance it is assumed to be also in the encoding file."
  (when (get-buffer "*clingo output*")
    (kill-buffer "*clingo output*"))
  (let ((test-command-to-run (clingo-generate-command encoding options instance))
        (compilation-buffer-name-function (lambda (_) "" "*clingo output*")))
    (compile test-command-to-run 'clingo-compilation-mode)))

(defun clingo-generate-echo (region options &optional instance)
  "Generate Clingo call with region echoed to it.

Argument REGION The selected region which holds the problem encoding.
Argument OPTIONS Options (possibly empty string) sent to clingo.
Optional argument INSTANCE The problem instance which is solved by the encoding.
  If no instance it is assumed to be also in the encoding file."
  (if 'instance
      (concat "echo \"" region "\" | " clingo-path " " options " " instance)
    (concat "echo \"" region "\" | " clingo-path " " options)))

;; (defun clingo-echo-clingo (region-begin region-end options &optional instance)
(defun clingo-echo-clingo (region options &optional instance)
  "Run Clingo on selected region (prompts for options).

Argument REGION The selected region as a string, which has the problem encoding.
Argument OPTIONS Options (possibly empty string) sent to clingo.
Optional argument INSTANCE The problem instance which is solved by the encoding.
  If no instance it is assumed to be also in the encoding file."
  (when (get-buffer "*clingo output*")
    (kill-buffer "*clingo output*"))
  (let ((test-command-to-run (clingo-generate-echo region options instance))
        (compilation-buffer-name-function (lambda (_) "" "*clingo output*")))
    (compile test-command-to-run 'clingo-compilation-mode)))

;; save the last user input
(defvar clingo-last-instance "")
(defvar clingo-last-options "")

(defun get-options ()
  "Get options string in minibuffer."
    (read-string (format "Options [%s]: " clingo-last-options) nil nil clingo-last-options))

;;;###autoload
(defun clingo-run-region (region-beginning region-end options)
  "Run Clingo on selected region (prompts for options).

Argument REGION-BEGINNING point marking beginning of region.
Argument REGION-END point marking end of region.
Argument OPTIONS Options (possibly empty string) sent to clingo."
  (interactive
   (let ((string (get-options))
         (list (region-beginning) (region-end) string)))
  (setq-local clingo-last-options options)
  (clingo-echo-clingo
   (buffer-substring-no-properties region-beginning region-end)
   options)))

;;;###autoload
(defun clingo-run-buffer (options)
  "Run clingo with the current buffer as input; prompts for OPTIONS."
  (interactive
   (list (get-options)))
  (setq-local clingo-last-options options)
  (clingo-run-clingo (buffer-file-name) options))

;;;###autoload
(defun clingo-run (options instance)
  "Run clingo with the current buffer and some user provided INSTANCE as input; prompts for OPTIONS."
  (interactive
   (list
    (get-options)
    (read-file-name
     (format "Instance [%s]:" (file-name-nondirectory clingo-last-instance))
     nil clingo-last-instance)))
  (setq-local clingo-last-options options)
  (setq-local clingo-last-instance instance)
  (clingo-run-clingo (buffer-file-name) options instance))

;;; Utility functions

(defun clingo--reload-mode ()
    "Reload the CLINGO major mode."
  (unload-feature 'clingo-mode)
  (require 'clingo-mode)
    (clingo-mode))

;;; File ending

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.lp\\'" . clingo-mode))

;;; Define clingo mode

;;; Keymap

(defvar
  clingo-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "C-c C-c") 'comment-region)
    (define-key km (kbd "C-c C-u") 'uncomment-region)
    (define-key km (kbd "C-c C-b") 'clingo-run-buffer)
    (define-key km (kbd "C-c C-r") 'clingo-run-region)
    (define-key km (kbd "C-c C-e") 'clingo-run)
    km))

;;;###autoload
(define-derived-mode clingo-mode prolog-mode "Clingo"
  "A major mode for editing Answer Set Programs."
  (setq font-lock-defaults '(clingo-font-lock-keywords))

  ;; define the syntax for un/comment region and dwim
  (setq-local comment-start "%")
  (setq-local comment-end "")
  (setq-local tab-width clingo-indentation))

;; add mode to feature list
(provide 'clingo-mode)

;;; clingo-mode.el ends here
