;;; latex-table-wizard.el --- Magic editing of LaTeX tables  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026 Free Software Foundation, Inc.

;; Author: Enrico Flor <enrico@eflor.net>
;; Maintainer: Enrico Flor <enrico@eflor.net>
;; URL: https://github.com/enricoflor/latex-table-wizard
;; Version: 1.6.1
;; Keywords: convenience, tex

;; Package-Requires: ((emacs "27.1") (auctex "12.1") (transient "0.3.7"))

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides you with commands to smartly navigate and
;; edit large and complex LaTeX table-like environments with a
;; transient.el-based interface.  Table-like environments are portions
;; of text delimited by a pair of matching "\begin" and "\end" macros
;; that organize output text into aligned columns.

;; The entry point of the package is

;;     M-x latex-table-wizard

;; while point is inside of a table(-like) environment.  From there, you
;; can do several things such as:

;;   + navigate "logically" (that is, move by cells);
;;   + insert or kill rows or columns;
;;   + move arbitrary cells or groups of cells around;
;;   + align the table in different ways (however alignment is not
;;     needed for the functionalities above).

;; Standard LaTeX2e table environments are supported out of the box,
;; but you can define additional ones.  The entry point for
;; customization is

;;     M-x latex-table-wizard-customize

;; The keybinding set by default in the transient prefix are inspired
;; to some extent by Emacs defaults.  If you want to change these
;; keybindings you should change the value of the variable
;; latex-table-wizard-transient-keys.

;; The package is designed to be perfectly usable without the
;; transient interface, whose main purpose here is for
;; discoverability.  All the commands defined here can be called the
;; usual way without any loss of functionality.

;; By default, the syntax this package expects is the one of standard
;; LaTeX tabular environments, whereby "&" separates columns and "\\"
;; separates rows.  Additional, or different, types of table-like
;; environments (with their own syntax separators) can be added by the
;; user.  This is done by adding mappings to
;; latex-table-wizard-new-environments-alist.  Suppose I want to
;; define a new table like environment whose name is "mytable", whose
;; column and row separators are strings like "\COL" and "\ROW", and
;; the LaTeX macro to add a horizontal line is "\myhline{}":

;;    \begin{mytable}
;;        ...
;;    \end{mytable}

;; For latex-table-wizard to handle this table, just add the following
;; cons cell to latex-table-wizard-new-environments-alist:

;;    '("mytable" . (:col '("\\COL")
;;                   :row '("\\ROW")
;;                   :lines '("myhline")))

;; Each value is a list of strings to allow for more than one macro to
;; have the same function.

;; See the Info page for a complete overview of the package.

;; Data model:
;;
;; A table is represented by a `latex-table-wizard--table' struct:
;;
;;   mod-tick   -- value of (buffer-chars-modified-tick) at parse time.
;;   cells      -- hash: (col . row) -> (beg-marker . end-marker).
;;   height     -- number of rows.
;;   width      -- number of columns, or nil for ragged tables.
;;   row-widths -- alist (row . ncols) for ragged tables; nil when uniform.
;;   inner-beg  -- marker at the start of the (0,0) cell.
;;   inner-end  -- marker at the end of the last cell.
;;
;; During a parse, every cell range in the buffer is stamped with the
;; text property `ltw-coord' whose value is (col . row).  This makes
;; "what cell is point in?" a constant-time read and "what are its
;; buffer bounds?" a constant-time hash lookup.  Text properties are
;; removed when the cache is invalidated.
;;
;; A COORD is a (col . row) cons.  All navigation, selection, and
;; mutation functions take or return COORDs.  Buffer positions appear
;; only in the `--cell-beg' / `--cell-end' accessors.
;;
;; `latex-table-wizard--selection' is a list of COORDs.

;;; Code:

(require 'cl-lib)
(require 'tex)
(require 'latex)
(require 'regexp-opt)
(require 'text-property-search)
(require 'transient)
(eval-when-compile (require 'rx))
(eval-when-compile (require 'subr-x))

;; Suppress the compiler warning about the prefix defined at runtime.
(declare-function latex-table-wizard-prefix "latex-table-wizard")

;; Forward declarations of variables defined further down.
(defvar latex-table-wizard--current-table)
(defvar latex-table-wizard--selection)
(defvar latex-table-wizard-mode)

(defgroup latex-table-wizard nil
  "LaTeX table wizard configuration options."
  :prefix "latex-table-wizard-"
  :group 'convenience)

;;; User options

(defcustom latex-table-wizard-allow-detached-args nil
  "If t, allow arguments of macros to be detached in parsing.

This means that, if non-nil, this package will parse argument
groups (strings in brackets or in braces) as arguments of the
macro even if they are separated by whitespace, one line break,
and comments.  This conforms to how LaTeX interprets them.

However, doing this may cause some troubles if you happen to have
a string in braces at the start of the first cell (position
(0,0)).  This is because, if there is no blank line between that
cell and the table opening \\='\\begin\\=' macro with its
arguments, that string which should be in the first cell may end
up being parsed as an additional argument to the
\\='\\begin\\=' macro.

You avoid this danger if you set this variable to nil, but then
you should never have whitespace between the macro and its
arguments and between the arguments themselves."
  :type 'boolean)

(defcustom latex-table-wizard-warn-about-detached-args t
  "If t, warn about suspect cases of non-allowed detached arguments.

The warning will be echoed in the echo area any time that, while
parsing the table, cases in which a LaTeX macro and its arguments
might be separated from its arguments by whitespace or comment
are found.

Since the parser doesn't quite know what string preceded by an
unescaped backslash is a valid LaTeX macro and whether it accepts
what number of arguments, false positives are likely to be found.

If \\='latex-table-wizard-allow-detached-args\\=' is non-nil, detached
arguments are allowed and so no warning will ever be issued regardless
of the value of this variable."
  :type 'boolean
  :link '(variable-link latex-table-wizard-allow-detached-args))

(defcustom latex-table-wizard-column-delimiters '("&")
  "List of strings that are column delimiters if unescaped."
  :type '(repeat string))

(defcustom latex-table-wizard-row-delimiters '("\\\\")
  "List of strings that are row delimiters if unescaped."
  :type '(repeat string))

(defcustom latex-table-wizard-hline-macros '("cline"
                                             "vline"
                                             "midrule"
                                             "hline"
                                             "toprule"
                                             "bottomrule")
  "Names of macros that draw horizontal lines.

Each member is the string appearing between \"\\\\\" and the
arguments, e.g. \"hline\" for \\\\hline."
  :type '(repeat string))

(defcustom latex-table-wizard-new-environments-alist nil
  "Alist mapping environment names to property lists.

The environment name is a string, for example \"foo\" for

  \\begin{foo}
      ...
  \\end{foo}

The cdr of each mapping is a property list with three keys:

   :col   -- list of column-delimiter strings
   :row   -- list of row-delimiter strings
   :lines -- list of hline macro name strings"
  :type '(alist :key-type (string :tag "Name of the environment:")
                :value-type (plist :key-type symbol
                                   :options (:col :row :lines)
                                   :value-type (repeat string)))
  :link '(variable-link latex-table-wizard-hline-macros))

(defcustom latex-table-wizard-no-highlight nil
  "If non-nil, do not highlight the current or selected cells."
  :type 'boolean
  :group 'latex-table-wizard)

(defcustom latex-table-wizard-no-focus nil
  "If non-nil, do not grey out the buffer content outside the table."
  :type 'boolean
  :group 'latex-table-wizard)



;;; Hooks

(defvar latex-table-wizard-after-table-modified-hook nil
  "Hook run after the table has been modified by a latex-table-wizard command.")

(defvar latex-table-wizard-after-movement-hook nil
  "Hook run after execution of a latex-table-wizard movement command.")

;;; Faces

(defgroup latex-table-wizard-faces nil
  "Faces used by \\='latex-table-wizard\\='."
  :group 'latex-table-wizard
  :group 'faces)

(defface latex-table-wizard-background
  '((nil (:foreground "gray40")))
  "Face applied to buffer content outside the table."
  :group 'latex-table-wizard-faces)

(defface latex-table-wizard-highlight
  '((nil :inherit region))
  "Face for highlighting the current or selected cells."
  :group 'latex-table-wizard-faces)



;;; Parsing layer

;; Per-environment syntax, set once at parse time.
(defvar latex-table-wizard--current-col-delims nil)
(defvar latex-table-wizard--current-row-delims nil)
(defvar latex-table-wizard--current-hline-macros nil)

(defun latex-table-wizard--set-current-values ()
  "Set current-{col,row,hline} variables for the environment at point."
  (let* ((values (cdr (assoc (LaTeX-current-environment)
                             latex-table-wizard-new-environments-alist)))
         (col    (plist-get values :col))
         (row    (plist-get values :row))
         (lines  (plist-get values :lines)))
    (setq latex-table-wizard--current-col-delims
          (or col latex-table-wizard-column-delimiters)
          latex-table-wizard--current-row-delims
          (or row latex-table-wizard-row-delimiters)
          latex-table-wizard--current-hline-macros
          (or lines latex-table-wizard-hline-macros))))

(defconst latex-table-wizard--blank-detach-arg-re
  (rx (seq (* space)
           (? (seq "%" (* not-newline)))
           (? "\n")
           (* (seq (* space) "%" (* not-newline) "\n"))
           (* space)))
  "Regexp matching whitespace/comments between a macro and its arguments.")

(defvar latex-table-wizard--detached nil
  "Set to t during a parse if a detached macro argument is encountered.")

(defun latex-table-wizard--warn-detached ()
  "Warn about suspect detached macro arguments found during parsing."
  (unless latex-table-wizard-allow-detached-args
    (let ((message-log-max 0))
      (message (concat "Warning: suspect detached macro found.\n"
                       "If the table is parsed incorrectly, "
                       "try not to separate arguments from their macro,\n"
                       "or set `latex-table-wizard-allow-detached-args' to t.")))))

(defun latex-table-wizard--macro-at-point (&optional pos bound detached-args)
  "Return data about the LaTeX macro at POS, or nil if none.

POS defaults to point.  BOUND limits the backward search (default:
point-min).  DETACHED-ARGS mirrors
\\='latex-table-wizard-allow-detached-args\\='.

Returns (BEGIN END NAME ARG...) as buffer positions and strings."
  (save-match-data
    (let* ((limit (or bound (point-min)))
           (skip (lambda ()
                   (when detached-args (skip-chars-forward " \t"))
                   (while (looking-at-p "%.*")
                     (goto-char (line-beginning-position 2)))
                   (when detached-args
                     (skip-chars-forward "\n" (line-end-position 2))
                     (skip-chars-forward " \t"))))
           (start (or pos (point)))
           guess b e return intermediate)
      (save-excursion
        (goto-char start)
        (when (and (not (TeX-escaped-p (1- (point))))
                   (looking-back "[\]}]" (line-beginning-position)))
          (forward-char -1))
        (setq guess (ignore-errors (LaTeX-what-macro limit)))
        (cond
         ((and (looking-back "\\\\[[:alpha:]]*" (line-beginning-position))
               (not (TeX-escaped-p (match-beginning 0))))
          (goto-char (match-beginning 0)))
         ((and (not guess) (looking-at-p "\\\\") (not (TeX-escaped-p)))
          nil)
         ((not guess)
          (TeX-search-unescaped "\\\\[[:alpha:]]" 'backward t nil t))
         ((eq (nth 1 guess) 'env)
          (TeX-search-unescaped "\\begin" 'backward nil nil t))
         ((eq (nth 1 guess) 'mac)
          (TeX-search-unescaped (concat "\\" (nth 0 guess))
                                'backward nil nil t))
         (t
          (TeX-search-unescaped (concat "\\begin{" (nth 0 guess))
                                'backward nil nil t)))
        (setq b (point) intermediate (point))
        ;; A macro name is either a run of letters (optionally starred)
        ;; or a single non-letter character.  It must not run on past
        ;; a newline or into a following macro, as in "\hline\nfoo".
        (when (looking-at "\\\\\\(?:[[:alpha:]@]+\\*?\\|.\\)")
          (goto-char (match-end 0)))
        (push (buffer-substring-no-properties intermediate (point)) return)
        (funcall skip)
        (while (looking-at-p "[\[{]")
          (setq intermediate (point))
          (forward-sexp 1)
          (push (buffer-substring-no-properties intermediate (point)) return)
          (funcall skip))
        (skip-chars-backward " \t\n")
        (setq e (point))
        (unless (>= start e)
          (cons b (cons e (nreverse (mapcar #'string-trim return)))))))))

(defun latex-table-wizard--goto-end-of-macro (&optional pos names re)
  "If looking at a macro, move point past it and its arguments.

POS is the starting position (default: point).
NAMES is a list of macro name strings to restrict to.
RE is a regexp matching the macro name (overrides NAMES)."
  (when-let* ((macro (latex-table-wizard--macro-at-point
                      pos nil latex-table-wizard-allow-detached-args))
              (mname (string-trim-left (nth 2 macro) "\\\\")))
    (when (or (and (not names) (not re))
              (member mname names)
              (when re (string-match re mname)))
      (goto-char (nth 1 macro)))))

(defun latex-table-wizard--get-out ()
  "If point is on a \\\\begin or \\\\end macro, move out of it."
  (latex-table-wizard--set-current-values)
  (when-let* ((macro (latex-table-wizard--macro-at-point))
              (name  (string-trim-left (nth 2 macro) "\\\\")))
    ;; Move into the table body, not away from it.
    (cond ((equal name "begin") (goto-char (nth 1 macro)))
          ((equal name "end")   (goto-char (nth 0 macro))))))

(defun latex-table-wizard--skip-stuff (&optional bound)
  "Skip forward over whitespace, comments, and hline macros.

BOUND is the furthest position to advance to.  Defaults to the
inner-end of the current cached table, or point-max."
  (let ((lim (or bound
                 (when latex-table-wizard--current-table
                   (latex-table-wizard--table-inner-end
                    latex-table-wizard--current-table))
                 (point-max)))
        (col-re (concat "[[:space:]]*"
                        (regexp-opt latex-table-wizard--current-col-delims)))
        new-start-of-line)
    (catch 'stop
      (while (<= (point) lim)
        (skip-syntax-forward " ")
        (let ((start-pos (point)))
          (when (looking-at "\n\\|%")
            (forward-line)
            (setq new-start-of-line (point))
            (when (looking-at col-re)
              (throw 'stop nil)))
          (ignore-errors
            (latex-table-wizard--goto-end-of-macro
             nil latex-table-wizard--current-hline-macros))
          (when (looking-at "\n\\|%")
            (forward-line)
            (setq new-start-of-line (point)))
          (when (= (point) start-pos)
            (throw 'stop t)))))
    (when new-start-of-line
      (goto-char new-start-of-line))))

(defun latex-table-wizard--get-cell-boundaries (col-re row-re beginning limit)
  "Return (BEG END EOR) for the cell starting at point.

BEG and END are markers.  EOR is t if this is the last cell in
its row, nil otherwise.  COL-RE and ROW-RE match delimiters.
BEGINNING is the minimum position for a cell left boundary.
LIMIT is the parse stop position."
  (save-match-data
    (let ((beg (point-marker))
          end end-of-row)
      (latex-table-wizard--skip-stuff limit)
      ;; Leading whitespace belongs to the cell, but not what comes
      ;; before it on a previous line (the line break after the row
      ;; delimiter, hline macros, comments).
      (let ((skipped (buffer-substring-no-properties beg (point))))
        (unless (and (string-blank-p skipped)
                     (not (string-match-p "\n" skipped)))
          (setq beg (point-marker))))
      (while (and (< (point) limit) (not end))
        (let ((macro (latex-table-wizard--macro-at-point
                      nil beginning latex-table-wizard-allow-detached-args)))
          (cond
           ((looking-at-p "[[:space:]]+%")
            (TeX-comment-forward 1))
           ((TeX-escaped-p)
            (forward-char 1))
           ((looking-at col-re)
            (setq end (point-marker))
            (goto-char (match-end 0)))
           ((looking-at row-re)
            (let ((after-del   (save-excursion
                                 (goto-char (match-end 0))
                                 (point-marker)))
                  (end-of-prev (progn (goto-char (match-beginning 0))
                                      (point-marker))))
              (goto-char after-del)
              (setq end end-of-prev end-of-row t)
              (unless (or (eolp) (bolp))
                (latex-table-wizard--skip-stuff limit))))
           ((looking-at "\\$\\|{")
            ;; `forward-sexp' returns nil even on success, so its return
            ;; value can't tell us whether it moved.
            (condition-case nil
                (forward-sexp)
              (scan-error (forward-char 1))))
           ((looking-at "\\\\(\\|\\\\\\[")
            (TeX-search-unescaped "\\\\)\\|\\\\\\]" 'forward t nil t))
           ((looking-at "[[:space:]]*\\\\\\(begin[\[{]\\)")
            (goto-char (match-beginning 1))
            (LaTeX-find-matching-end))
           (macro
            (goto-char (nth 1 macro)))
           (t
            (forward-char 1)))))
      (list beg end end-of-row))))

;;; The struct

(cl-defstruct (latex-table-wizard--table
               (:constructor latex-table-wizard--make-table))
  "Cached parse of a LaTeX table-like environment."
  mod-tick    ; (buffer-chars-modified-tick) at parse time
  cells       ; hash-table: (col . row) -> (beg-marker . end-marker)
  height      ; integer: number of rows
  width       ; integer or nil: number of columns (nil = ragged)
  row-widths  ; alist (row . ncols) for ragged tables; nil when uniform
  inner-beg   ; marker: start of the (0,0) cell
  inner-end)  ; marker: end of the last cell

(defvar-local latex-table-wizard--current-table nil
  "Cached \\='latex-table-wizard--table\\=' for the current buffer, or nil.")

;;; Struct accessors
;;
;; These are the only functions that convert between COORDs and buffer
;; positions.  Everything else uses COORDs.

(defsubst latex-table-wizard--cell-bounds (coord)
  "Return (beg . end) markers for the cell at COORD."
  (gethash coord (latex-table-wizard--table-cells
                  latex-table-wizard--current-table)))

(defsubst latex-table-wizard--cell-beg (coord)
  "Return the beginning marker of the cell at COORD."
  (car (latex-table-wizard--cell-bounds coord)))

(defsubst latex-table-wizard--cell-end (coord)
  "Return the end marker of the cell at COORD."
  (cdr (latex-table-wizard--cell-bounds coord)))

;;; Cache management

(defun latex-table-wizard--table-valid-p ()
  "Return the cached struct if it is still valid, or nil.

A struct is valid when its mod-tick matches the current
buffer-chars-modified-tick and point is within inner-beg..inner-end."
  (when-let* ((tbl latex-table-wizard--current-table))
    ;; FIXME: alignment modifies the buffer but should not invalidate.
    (when (and (= (buffer-chars-modified-tick)
                  (latex-table-wizard--table-mod-tick tbl))
               (<= (latex-table-wizard--table-inner-beg tbl)
                   (point)
                   (latex-table-wizard--table-inner-end tbl)))
      tbl)))

(defun latex-table-wizard--invalidate ()
  "Discard the cached struct and remove ltw-coord text properties."
  (when latex-table-wizard--current-table
    (let ((beg (latex-table-wizard--table-inner-beg
                latex-table-wizard--current-table))
          (end (latex-table-wizard--table-inner-end
                latex-table-wizard--current-table)))
      (when (and beg end (buffer-live-p (marker-buffer beg)))
        (with-silent-modifications
          (remove-text-properties beg end '(ltw-coord nil))))))
  (setq latex-table-wizard--current-table nil))



;;; Parse

(defun latex-table-wizard--do-parse (env-beg env-end)
  "Walk the table body between ENV-BEG and ENV-END and return a struct.

Stamps every cell range with the \\='ltw-coord\\=' text property."
  (let ((col-re (regexp-opt latex-table-wizard--current-col-delims))
        (row-re (regexp-opt latex-table-wizard--current-row-delims))
        (col 0) (row 0)
        (cells (make-hash-table :test #'equal))
        row-widths last-max-col ragged)
    ;; Ensure at least one space between \begin and the first cell.
    ;; This is a real edit, so it must not be hidden from undo and
    ;; from `buffer-modified-p'.
    (save-excursion
      (goto-char env-beg)
      (unless (looking-at-p "[[:space:]]")
        (insert " ")))
    (with-silent-modifications
      (remove-text-properties env-beg env-end '(ltw-coord nil))
      (save-excursion
        (goto-char env-beg)
        (forward-char 1)
        (TeX-comment-forward 1)
        (while (looking-at-p "[[:space:]]*%")
          (TeX-comment-forward 1))
        (skip-syntax-backward " ")
        (while (< (point) env-end)
          (let* ((data  (latex-table-wizard--get-cell-boundaries
                         col-re row-re (point) env-end))
                 (c-beg (nth 0 data))
                 (c-end (or (nth 1 data) env-end))
                 (coord (cons col row)))
            (puthash coord (cons c-beg c-end) cells)
            (put-text-property c-beg c-end 'ltw-coord coord)
            (if (nth 2 data)
                (progn
                  (unless ragged
                    (if last-max-col
                        (setq ragged (/= last-max-col col))
                      (setq last-max-col col)))
                  (push (cons row (1+ col)) row-widths)
                  (cl-incf row)
                  (setq col 0))
              (cl-incf col))
            ;; Skip a run of consecutive row delimiters.
            (while (and (nth 2 data)
                        (save-excursion
                          (skip-syntax-forward " ")
                          (looking-at-p row-re)))
              (re-search-forward row-re nil t))))))
    ;; The last row need not end with a row delimiter: count it too.
    (when (> col 0)
      (unless ragged
        (if last-max-col
            (setq ragged (/= last-max-col (1- col)))
          (setq last-max-col (1- col))))
      (push (cons row col) row-widths))
    (latex-table-wizard--make-table
     :mod-tick   (buffer-chars-modified-tick)
     :cells      cells
     :height     (length row-widths)
     :width      (and (not ragged) last-max-col (1+ last-max-col))
     :row-widths (when ragged row-widths)
     :inner-beg  (car (gethash (cons 0 0) cells))
     :inner-end  env-end)))

(defun latex-table-wizard--parse ()
  "Return valid \\='latex-table-wizard--table\\=' for table at point.

Returns the cached struct if still valid.  Otherwise, finds environment
boundaries, calls \\='latex-table-wizard--do-parse\\=', caches and
returns the result.  If the environment is empty, delegates to
\\='latex-table-wizard--create-table-content\\='."
  (or (latex-table-wizard--table-valid-p)
      (progn
        (setq latex-table-wizard--detached nil)
        ;; Drop the previous table's cache and text properties, so that
        ;; stale coords from another table can never be picked up.
        (latex-table-wizard--invalidate)
        ;; The hline macros are needed to find the table boundaries.
        (latex-table-wizard--set-current-values)
        (let* ((bl-rx (if latex-table-wizard-allow-detached-args
                          latex-table-wizard--blank-detach-arg-re
                        ""))
               (env-beg
                (save-excursion
                  (LaTeX-find-matching-begin)
                  (latex-table-wizard--goto-end-of-macro (1+ (point)))
                  (ignore-errors
                    (latex-table-wizard--goto-end-of-macro
                     nil latex-table-wizard--current-hline-macros))
                  (point-marker)))
               (env-end
                (save-excursion
                  (LaTeX-find-matching-end)
                  (if-let* ((end-macro
                             (latex-table-wizard--macro-at-point
                              (1- (point)) env-beg
                              latex-table-wizard-allow-detached-args)))
                      (goto-char (car end-macro))
                    (TeX-search-unescaped (concat "\\\\end" bl-rx "[{\[]")
                                          'backward t env-beg t))
                  (re-search-backward "[^[:space:]]" nil t)
                  (while (TeX-in-comment)
                    (TeX-search-unescaped "%" 'backward t env-beg t)
                    (re-search-backward "[^[:space:]]" nil t))
                  (unless (eolp) (forward-char 1))
                  ;; Exclude trailing hline macros (\bottomrule etc.),
                  ;; otherwise they are parsed as an extra cell.
                  (let (m)
                    (while (and (> (point) env-beg)
                                (setq m (latex-table-wizard--macro-at-point
                                         (1- (point)) env-beg
                                         latex-table-wizard-allow-detached-args))
                                (= (nth 1 m) (point))
                                (> (nth 0 m) env-beg)
                                (member (string-trim-left (nth 2 m) "\\\\")
                                        latex-table-wizard--current-hline-macros))
                      (goto-char (nth 0 m))
                      (re-search-backward "[^[:space:]]" env-beg t)
                      (while (TeX-in-comment)
                        (TeX-search-unescaped "%" 'backward t env-beg t)
                        (re-search-backward "[^[:space:]]" env-beg t))
                      (unless (eolp) (forward-char 1))))
                  (point-marker))))
          (if (string-blank-p
               (buffer-substring-no-properties env-beg env-end))
              (latex-table-wizard--create-table-content)
            (save-excursion
              (goto-char env-beg)
              (latex-table-wizard--set-current-values))
            (setq latex-table-wizard--current-table
                  (latex-table-wizard--do-parse env-beg env-end))
            (when latex-table-wizard--detached
              (latex-table-wizard--warn-detached))
            (latex-table-wizard--snap-to-cell)
            latex-table-wizard--current-table)))))

(defun latex-table-wizard--create-table-content ()
  "Interactively populate an empty tabular environment, then parse it."
  (let ((coln (read-number
               "There's no table here.  How many columns do you want? "))
        (rown (read-number "How many rows? ")))
    (when (and (> coln 0) (> rown 0))
      (save-excursion
        (dotimes (_ rown)
          (dotimes (_ (1- coln))
            (insert " & "))
          (insert "\\\\\n")))
      (latex-table-wizard--parse))))



;;; Coord-level API
;;
;; Everything from here down works with COORDs only.

(defsubst latex-table-wizard--coord-at-point ()
  "Return the ltw-coord at point, or nil."
  (get-text-property (point) 'ltw-coord))

(defun latex-table-wizard--snap-to-cell ()
  "Move point into the nearest cell if it is currently between cells.

Between cells means point is on a delimiter, whitespace, or hline macro
(anywhere the \\='ltw-coord\\=' text property is absent).  We snap
backward because a position between two cells belongs conceptually to
the cell on its left: if you were to insert text there, it would become
part of that cell's content."
  (unless (latex-table-wizard--coord-at-point)
    (let* ((tbl   latex-table-wizard--current-table)
           (first (and tbl (latex-table-wizard--table-inner-beg tbl)))
           (found (and first
                       (> (point) first)
                       (text-property-search-backward 'ltw-coord))))
      ;; Never snap to a position before the first cell (e.g. when
      ;; point is on the \begin line): go to the first cell instead.
      (cond ((and found (>= (prop-match-beginning found) first))
             (goto-char (prop-match-beginning found)))
            (first (goto-char first)))))
  (when (eolp) (forward-char 1)))

(defun latex-table-wizard--current-coord ()
  "Return the (col . row) coord at point, snapping if necessary."
  (or (latex-table-wizard--coord-at-point)
      (progn (latex-table-wizard--snap-to-cell)
             (latex-table-wizard--coord-at-point))))

(defsubst latex-table-wizard--all-coords ()
  "Return all coords in the current table as a list."
  (hash-table-keys (latex-table-wizard--table-cells
                    latex-table-wizard--current-table)))

(defun latex-table-wizard--row-coords (row &optional reverse)
  "Return coords in ROW, sorted by column.

With REVERSE non-nil, sort descending so the rightmost cell is first."
  (sort (cl-remove-if-not (lambda (c) (= (cdr c) row))
                          (latex-table-wizard--all-coords))
        (lambda (a b) (funcall (if reverse #'> #'<)
                               (car a) (car b)))))

(defun latex-table-wizard--col-coords (col &optional reverse)
  "Return coords in COL, sorted by row.

With REVERSE non-nil, sort descending so the bottom cell is first."
  (sort (cl-remove-if-not (lambda (c) (= (car c) col))
                          (latex-table-wizard--all-coords))
        (lambda (a b) (funcall (if reverse #'> #'<)
                               (cdr a) (cdr b)))))

(defun latex-table-wizard--current-row-coords (&optional reverse)
  "Return coords of the row containing point, sorted by column.

With REVERSE non-nil, the rightmost cell comes first."
  (latex-table-wizard--row-coords
   (cdr (latex-table-wizard--current-coord)) reverse))

(defun latex-table-wizard--current-col-coords (&optional reverse)
  "Return coords of the column containing point, sorted by row.

With REVERSE non-nil, the bottom cell comes first."
  (latex-table-wizard--col-coords
   (car (latex-table-wizard--current-coord)) reverse))



;;; Navigation

(defun latex-table-wizard--shift-coord (coord dir)
  "Return the coord one step from COORD in direction DIR, or nil if none exists.

DIR is one of \\='forward\\=', \\='backward\\=', \\='next\\=',
\\='previous\\='."
  (let* ((offsets '((forward   .  (1  .  0))
                    (backward  .  (-1 .  0))
                    (next      .  (0  .  1))
                    (previous  .  (0  . -1))))
         (delta  (cdr (assq dir offsets)))
         (target (cons (+ (car coord) (car delta))
                       (+ (cdr coord) (cdr delta)))))
    (when (gethash target (latex-table-wizard--table-cells
                           latex-table-wizard--current-table))
      target)))

(defun latex-table-wizard--jump (dir &optional absolute count same-line nocycle)
  "Move point COUNT cells in direction DIR.

DIR is one of \\='forward\\=', \\='backward\\=', \\='next\\=', \\='previous\\='.

Horizontal directions (\\='forward\\=', \\='backward\\=') traverse cells in
row-major order.  Vertical directions (\\='next\\=', \\='previous\\=') traverse
in column-major order.

ABSOLUTE: jump to the first or last cell in the current row (horizontal
DIR) or column (vertical DIR) without wrapping.
\\='forward\\='/\\='next\\=' go to the end;
\\='backward\\='/\\='previous\\=' to the start.

COUNT: number of steps to take (default 1).

SAME-LINE: loop within the current row (horizontal) or column
\(vertical), never leaving it.

NOCYCLE: do not move if the immediate next step would leave the
current row or column."
  (when (latex-table-wizard--in-tabular-env-p)
    (latex-table-wizard--setup)
    (latex-table-wizard--parse)
    (let* ((message-log-max nil)
           (tbl    latex-table-wizard--current-table)
           (ht     (latex-table-wizard--table-cells tbl))
           (curr   (latex-table-wizard--current-coord))
           (vert   (memq dir '(next previous)))
           (fwd-p  (memq dir '(forward next)))
           (steps  (or count 1))
           (stop   (and nocycle
                        (not (latex-table-wizard--shift-coord curr dir))))
           (target
            (cond
             (stop curr)
             (absolute
              (car (if vert
                       (latex-table-wizard--current-col-coords fwd-p)
                     (latex-table-wizard--current-row-coords fwd-p))))
             (same-line
              (let* ((line (if vert
                               (latex-table-wizard--current-col-coords)
                             (latex-table-wizard--current-row-coords)))
                     (n    (length line))
                     (now  (cl-position curr line :test #'equal)))
                (nth (mod (+ now (if fwd-p steps (- steps))) n) line)))
             (t
              ;; General case: COUNT steps with wrapping.
              ;; Try the direct hash neighbour first (common case, O(1)).
              ;; At a boundary, compute the wrap target from the coord
              ;; arithmetic and the struct, then loop for remaining steps.
              (let ((max-col
                     (or (latex-table-wizard--table-width tbl)
                         (apply #'max (mapcar #'car (hash-table-keys ht)))))
                    (height (latex-table-wizard--table-height tbl))
                    (cur curr))
                (dotimes (_ steps cur)
                  (setq cur
                        (or (latex-table-wizard--shift-coord cur dir)
                            ;; At a boundary: find the wrap target.
                            (pcase dir
                              ('forward
                               (let ((row (mod (1+ (cdr cur)) height)))
                                 (or (and (gethash (cons 0 row) ht)
                                          (cons 0 row))
                                     (car (latex-table-wizard--row-coords
                                           row)))))
                              ('backward
                               (let ((row (mod (1- (cdr cur)) height)))
                                 (car (latex-table-wizard--row-coords
                                       row t))))
                              ('next
                               (let ((col (mod (1+ (car cur))
                                               (1+ max-col))))
                                 (or (and (gethash (cons col 0) ht)
                                          (cons col 0))
                                     (car (latex-table-wizard--col-coords
                                           col)))))
                              ('previous
                               (let ((col (mod (1- (car cur))
                                               (1+ max-col))))
                                 (car (latex-table-wizard--col-coords
                                       col t)))))))))))))
      (latex-table-wizard--remove-overlays)
      (unless stop
        (goto-char (latex-table-wizard--cell-beg target))
        (when (eolp) (forward-char 1))
        (latex-table-wizard--hl-coords (list target))
        (latex-table-wizard--hl-coords latex-table-wizard--selection)
        (message "Col x Row (%d,%d)" (car target) (cdr target))))
    (run-hooks 'latex-table-wizard-after-movement-hook)))

;;; Overlays

(defun latex-table-wizard--remove-overlays ()
  "Remove all ltw cell-highlight overlays within the current table."
  (when latex-table-wizard--current-table
    (remove-overlays (latex-table-wizard--table-inner-beg
                      latex-table-wizard--current-table)
                     (latex-table-wizard--table-inner-end
                      latex-table-wizard--current-table)
                     'ltw-hl t)))

(defun latex-table-wizard--hl-coords (coords)
  "Highlight each cell in the list of COORDS."
  (unless latex-table-wizard-no-highlight
    (dolist (coord coords)
      (when-let* ((bounds (latex-table-wizard--cell-bounds coord)))
        (let ((ov (make-overlay (car bounds) (cdr bounds))))
          (overlay-put ov 'ltw-hl t)
          (overlay-put ov 'face 'latex-table-wizard-highlight))))))

(defun latex-table-wizard--hide-rest ()
  "Apply \\='latex-table-wizard-background\\=' outside the current table."
  (unless latex-table-wizard-no-focus
    (when latex-table-wizard--current-table
      (let* ((tbl   latex-table-wizard--current-table)
             (tab-b (latex-table-wizard--table-inner-beg tbl))
             (tab-e (1+ (latex-table-wizard--table-inner-end tbl))))
        (dolist (ov (list (make-overlay (point-min) tab-b)
                          (make-overlay tab-e (point-max))))
          (overlay-put ov 'ltw-focus t)
          (overlay-put ov 'face 'latex-table-wizard-background))))))

;;; Selection

(defvar-local latex-table-wizard--selection nil
  "Current selection: a list of (col . row) coords.")

(defun latex-table-wizard--type-of-selection (sel)
  "Return \\='cell\\=', \\='column\\=', \\='row\\=', or nil for the coord
list SEL."
  (declare (pure t))
  (cond ((not sel)                      nil)
        ((= 1 (length sel))             'cell)
        ((apply #'= (mapcar #'car sel)) 'column)
        ((apply #'= (mapcar #'cdr sel)) 'row)
        (t                              nil)))

(defun latex-table-wizard--selection-in-current-table-p ()
  "Return non-nil if every coord in the selection is in the current table."
  (and latex-table-wizard--selection
       latex-table-wizard--current-table
       (let ((ht (latex-table-wizard--table-cells
                  latex-table-wizard--current-table)))
         (cl-every (lambda (c) (gethash c ht))
                   latex-table-wizard--selection))))

(defun latex-table-wizard--echo-selection ()
  "Display the current selection in the echo area."
  (let ((sel (sort (copy-sequence latex-table-wizard--selection)
                   (lambda (a b)
                     (or (< (cdr a) (cdr b))
                         (and (= (cdr a) (cdr b)) (< (car a) (car b))))))))
    (if sel
        (message "Current selection: %s"
                 (string-join
                  (mapcar (lambda (c) (format "(%d,%d)" (car c) (cdr c))) sel)
                  ", "))
      (message "Nothing is selected"))))

(defun latex-table-wizard--select-thing (thing &optional no-message)
  "Add THING at point to \\='latex-table-wizard--selection\\='.

THING is \\='cell\\=', \\='column\\=' or \\='row\\='."
  (latex-table-wizard--parse)
  (let ((coords (pcase thing
                  ('cell   (list (latex-table-wizard--current-coord)))
                  ('row    (latex-table-wizard--current-row-coords))
                  ('column (latex-table-wizard--current-col-coords)))))
    (if (eq thing 'cell)
        (cl-pushnew (car coords) latex-table-wizard--selection :test #'equal)
      (setq latex-table-wizard--selection coords))
    (latex-table-wizard--hl-coords coords)
    (unless no-message
      (pcase thing
        ('cell   (message "Cell (%d,%d) selected for swapping"
                          (caar coords) (cdar coords)))
        ('row    (message "Row %d selected for swapping" (cdar coords)))
        ('column (message "Column %d selected for swapping" (caar coords)))))))

;;; Swapping

(defun latex-table-wizard--swap-cells (cx cy)
  "Swap the buffer content of cells at coords CX and CY."
  (let* ((bx (latex-table-wizard--cell-beg cx))
         (ex (latex-table-wizard--cell-end cx))
         (by (latex-table-wizard--cell-beg cy))
         (ey (latex-table-wizard--cell-end cy))
         (sx (concat " " (string-trim (buffer-substring bx ex)) " "))
         (sy (concat " " (string-trim (buffer-substring by ey)) " ")))
    (save-excursion
      (goto-char ex) (delete-region bx ex) (insert sy) (just-one-space)
      (goto-char ey) (delete-region by ey) (insert sx) (just-one-space))))

(defun latex-table-wizard--swap-lines (type line1 line2)
  "Swap every cell in LINE1 with its counterpart in LINE2.

TYPE is \\='row\\=' or \\='column\\='; LINE1 and LINE2 are lists of
coords."
  (dolist (c1 line1)
    (let* ((key (if (eq type 'column) (cdr c1) (car c1)))
           (c2  (cl-find-if
                 (lambda (c)
                   (= (if (eq type 'column) (cdr c) (car c)) key))
                 line2)))
      (when c2 (latex-table-wizard--swap-cells c1 c2)))))



;;; Interactive — movement

;;;###autoload
(defun latex-table-wizard-right (&optional n nocycle)
  "Move point N cells to the right, wrapping to the row below if needed.

With NOCYCLE non-nil, do not wrap."
  (interactive "p")
  (latex-table-wizard--jump 'forward nil n nil nocycle))

;;;###autoload
(defun latex-table-wizard-left (&optional n nocycle)
  "Move point N cells to the left, wrapping to the row above if needed.

With NOCYCLE non-nil, do not wrap."
  (interactive "p")
  (latex-table-wizard--jump 'backward nil n nil nocycle))

;;;###autoload
(defun latex-table-wizard-down (&optional n nocycle)
  "Move point N cells down, wrapping to the next column if needed.

With NOCYCLE non-nil, do not wrap."
  (interactive "p")
  (latex-table-wizard--jump 'next nil n nil nocycle))

;;;###autoload
(defun latex-table-wizard-up (&optional n nocycle)
  "Move point N cells up, wrapping to the previous column if needed.

With NOCYCLE non-nil, do not wrap."
  (interactive "p")
  (latex-table-wizard--jump 'previous nil n nil nocycle))

;;;###autoload
(defun latex-table-wizard-end-of-row ()
  "Move point to the rightmost cell in the current row."
  (interactive)
  (latex-table-wizard--jump 'forward t))

;;;###autoload
(defun latex-table-wizard-beginning-of-row ()
  "Move point to the leftmost cell in the current row."
  (interactive)
  (latex-table-wizard--jump 'backward t))

;;;###autoload
(defun latex-table-wizard-bottom ()
  "Move point to the bottom cell in the current column."
  (interactive)
  (latex-table-wizard--jump 'next t))

;;;###autoload
(defun latex-table-wizard-top ()
  "Move point to the top cell in the current column."
  (interactive)
  (latex-table-wizard--jump 'previous t))

;;;###autoload
(defun latex-table-wizard-end-of-cell ()
  "Move point to the end of the current cell."
  (interactive)
  (when (latex-table-wizard--in-tabular-env-p)
    (latex-table-wizard--parse)
    (goto-char (latex-table-wizard--cell-end
                (latex-table-wizard--current-coord)))
    (run-hooks 'latex-table-wizard-after-movement-hook)))

;;;###autoload
(defun latex-table-wizard-beginning-of-cell ()
  "Move point to the beginning of the current cell."
  (interactive)
  (when (latex-table-wizard--in-tabular-env-p)
    (latex-table-wizard--parse)
    (goto-char (latex-table-wizard--cell-beg
                (latex-table-wizard--current-coord)))
    (run-hooks 'latex-table-wizard-after-movement-hook)))

;;;###autoload
(defun latex-table-wizard-mark-cell ()
  "Mark the current cell (mark at beginning, point at end)."
  (interactive)
  (when (latex-table-wizard--in-tabular-env-p)
    (latex-table-wizard--setup)
    (latex-table-wizard--parse)
    (let ((coord (latex-table-wizard--current-coord)))
      (push-mark (latex-table-wizard--cell-beg coord) nil t)
      (goto-char (latex-table-wizard--cell-end coord)))))

;;; Interactive — selection

;;;###autoload
(defun latex-table-wizard-select-row (&optional no-message)
  "Add the current row to the selection."
  (interactive)
  (when (latex-table-wizard--in-tabular-env-p)
    (latex-table-wizard--setup)
    (latex-table-wizard--select-thing 'row no-message)
    (latex-table-wizard--echo-selection)))

;;;###autoload
(defun latex-table-wizard-select-column (&optional no-message)
  "Add the current column to the selection."
  (interactive)
  (when (latex-table-wizard--in-tabular-env-p)
    (latex-table-wizard--setup)
    (latex-table-wizard--select-thing 'column no-message)
    (latex-table-wizard--echo-selection)))

;;;###autoload
(defun latex-table-wizard-select-deselect-cell (&optional no-message select)
  "Toggle the current cell in the selection.
With SELECT non-nil, always add."
  (interactive)
  (when (latex-table-wizard--in-tabular-env-p)
    (latex-table-wizard--setup)
    (latex-table-wizard--parse)
    (let ((coord (latex-table-wizard--current-coord)))
      (if (and (member coord latex-table-wizard--selection) (not select))
          (progn
            (setq latex-table-wizard--selection
                  (delete coord latex-table-wizard--selection))
            (latex-table-wizard--remove-overlays)
            (latex-table-wizard--hl-coords latex-table-wizard--selection))
        (latex-table-wizard--select-thing 'cell no-message)))
    (latex-table-wizard--echo-selection)))

;;;###autoload
(defun latex-table-wizard-deselect-all ()
  "Clear the selection."
  (interactive)
  (when (latex-table-wizard--in-tabular-env-p)
    (latex-table-wizard--setup)
    (latex-table-wizard--remove-overlays)
    (setq latex-table-wizard--selection nil)
    (latex-table-wizard--echo-selection)))

;;; Interactive — swapping

(defun latex-table-wizard--swap-adjacent-line (dir type)
  "Swap the current TYPE (cell/column/row) with the adjacent one in DIR."
  (latex-table-wizard--remove-overlays)
  (when (latex-table-wizard--in-tabular-env-p)
    (latex-table-wizard--setup)
    (cond ((eq type 'cell)
           (latex-table-wizard-select-deselect-cell t t))
          ((memq dir '(forward backward))
           (latex-table-wizard-select-column t))
          (t
           (latex-table-wizard-select-row t)))
    (setq latex-table-wizard--selection
          (cl-remove-duplicates latex-table-wizard--selection :test #'equal))
    (when-let* ((neighbour (latex-table-wizard--shift-coord
                           (latex-table-wizard--current-coord) dir)))
      (goto-char (latex-table-wizard--cell-beg neighbour))
      (latex-table-wizard-swap)
      (latex-table-wizard--parse)
      (latex-table-wizard--hl-coords
       (if (eq type 'cell)
           (list (latex-table-wizard--current-coord))
         (if (memq dir '(forward backward))
             (latex-table-wizard--current-col-coords)
           (latex-table-wizard--current-row-coords)))))))

;;;###autoload
(defun latex-table-wizard-swap ()
  "Swap the selection with the cell/column/row at point."
  (interactive)
  (when (latex-table-wizard--in-tabular-env-p)
    (latex-table-wizard--setup)
    (unless latex-table-wizard--selection
      (user-error "Select something to swap first"))
    (latex-table-wizard--parse)
    (let* ((curr  (latex-table-wizard--current-coord))
           (other (delete curr (copy-sequence latex-table-wizard--selection)))
           (type  (latex-table-wizard--type-of-selection other))
           (at-pt (pcase type
                    ('cell   curr)
                    ('row    (latex-table-wizard--current-row-coords))
                    ('column (latex-table-wizard--current-col-coords))
                    (_       nil))))
      (if type
        (progn
          (if (eq type 'cell)
              (latex-table-wizard--swap-cells (car other) at-pt)
            (latex-table-wizard--swap-lines type other at-pt))
          (latex-table-wizard--remove-overlays)
          (latex-table-wizard--hl-coords other)
          (setq latex-table-wizard--selection nil)
          (run-hooks 'latex-table-wizard-after-table-modified-hook))
        (latex-table-wizard--cleanup)
        (setq latex-table-wizard--selection nil)
        (message "Invalid selection")))))

;;;###autoload
(defun latex-table-wizard-swap-column-right ()
  "Swap the current column with the one to the right."
  (interactive)
  (latex-table-wizard--swap-adjacent-line 'forward 'column))

;;;###autoload
(defun latex-table-wizard-swap-column-left ()
  "Swap the current column with the one to the left."
  (interactive)
  (latex-table-wizard--swap-adjacent-line 'backward 'column))

;;;###autoload
(defun latex-table-wizard-swap-row-up ()
  "Swap the current row with the one above."
  (interactive)
  (latex-table-wizard--swap-adjacent-line 'previous 'row))

;;;###autoload
(defun latex-table-wizard-swap-row-down ()
  "Swap the current row with the one below."
  (interactive)
  (latex-table-wizard--swap-adjacent-line 'next 'row))

;;;###autoload
(defun latex-table-wizard-swap-cell-right ()
  "Swap the current cell with the one to the right."
  (interactive)
  (latex-table-wizard--swap-adjacent-line 'forward 'cell))

;;;###autoload
(defun latex-table-wizard-swap-cell-left ()
  "Swap the current cell with the one to the left."
  (interactive)
  (latex-table-wizard--swap-adjacent-line 'backward 'cell))

;;;###autoload
(defun latex-table-wizard-swap-cell-down ()
  "Swap the current cell with the one below."
  (interactive)
  (latex-table-wizard--swap-adjacent-line 'next 'cell))

;;;###autoload
(defun latex-table-wizard-swap-cell-up ()
  "Swap the current cell with the one above."
  (interactive)
  (latex-table-wizard--swap-adjacent-line 'previous 'cell))

;;; Interactive — structural edits

;;;###autoload
(defun latex-table-wizard-insert-column ()
  "Insert an empty column to the right of the current column."
  (interactive)
  (when (latex-table-wizard--in-tabular-env-p)
    (latex-table-wizard--setup)
    (latex-table-wizard--parse)
    (save-excursion
      (let ((col-del (car latex-table-wizard--current-col-delims)))
        (dolist (coord (latex-table-wizard--current-col-coords))
          (goto-char (latex-table-wizard--cell-end coord))
          (insert " " col-del " "))))
    (run-hooks 'latex-table-wizard-after-table-modified-hook)))

(defun latex-table-wizard--cell-and-delimiter-bounds (coord)
  "Return (BEG . END) markers spanning the cell at COORD and one delimiter.

The delimiter is the one on the left of the cell, or, for a cell in
the first column, the one on its right.  The span never leaves the
cell's row: if the cell has no neighbour in its row, only the cell
itself is spanned."
  (let* ((prev   (cons (1- (car coord)) (cdr coord)))
         (col-re (regexp-opt latex-table-wizard--current-col-delims))
         (beg    (latex-table-wizard--cell-beg coord))
         (end    (latex-table-wizard--cell-end coord)))
    (if (and (> (car coord) 0) (latex-table-wizard--cell-bounds prev))
        (cons (copy-marker (latex-table-wizard--cell-end prev))
              (copy-marker end))
      (save-excursion
        (goto-char end)
        (cons (copy-marker beg)
              (copy-marker (if (looking-at col-re) (match-end 0) end)))))))

;;;###autoload
(defun latex-table-wizard-delete-column ()
  "Delete the current column, including its adjacent delimiter.

The deleted content is added to the kill ring, one cell per line."
  (interactive)
  (when (latex-table-wizard--in-tabular-env-p)
    (latex-table-wizard--setup)
    (latex-table-wizard--parse)
    (save-excursion
      (let* ((col-coords (latex-table-wizard--current-col-coords))
             (ind        (caar col-coords))
             kills regions)
        ;; Compute every region before deleting anything.
        (dolist (coord col-coords)
          (push (string-trim
                 (buffer-substring (latex-table-wizard--cell-beg coord)
                                   (latex-table-wizard--cell-end coord)))
                kills)
          (push (latex-table-wizard--cell-and-delimiter-bounds coord)
                regions))
        (dolist (r regions)
          (delete-region (car r) (cdr r))
          (set-marker (car r) nil)
          (set-marker (cdr r) nil))
        (kill-new (string-join (nreverse kills) "\n"))
        (run-hooks 'latex-table-wizard-after-table-modified-hook)
        (message "Column %d deleted" ind)))))

(defalias 'latex-table-wizard-kill-column #'latex-table-wizard-kill-column-content)

;;;###autoload
(defun latex-table-wizard-kill-column-content ()
  "Kill the content of the current column, leaving delimiters in place."
  (interactive)
  (when (latex-table-wizard--in-tabular-env-p)
    (latex-table-wizard--setup)
    (latex-table-wizard--parse)
    (save-excursion
      (let* ((col-coords (latex-table-wizard--current-col-coords))
             (ind        (caar col-coords))
             kills poss)
        (dolist (coord col-coords)
          (push (buffer-substring (latex-table-wizard--cell-beg coord)
                                  (latex-table-wizard--cell-end coord))
                kills)
          (push (cons (latex-table-wizard--cell-beg coord)
                      (latex-table-wizard--cell-end coord))
                poss))
        (dolist (p poss)
          (delete-region (car p) (cdr p))
          (goto-char (car p))
          (insert " "))
        (kill-new (string-join (nreverse kills) "\n"))
        (run-hooks 'latex-table-wizard-after-table-modified-hook)
        (message "Content of column %d added to kill ring" ind)))))

;;;###autoload
(defun latex-table-wizard-insert-row ()
  "Insert an empty row below the current row."
  (interactive)
  (when (latex-table-wizard--in-tabular-env-p)
    (latex-table-wizard--setup)
    (save-excursion
      (latex-table-wizard--parse)
      (let* ((tbl         latex-table-wizard--current-table)
             (end-marker  (make-marker))
             (row-coords  (latex-table-wizard--current-row-coords))
             (last-coord  (car (latex-table-wizard--current-row-coords t)))
             (row-del     (car latex-table-wizard--current-row-delims))
             (col-del     (car latex-table-wizard--current-col-delims))
             (ncols       (length row-coords)))
        (set-marker end-marker (latex-table-wizard--table-inner-end tbl))
        (goto-char (latex-table-wizard--cell-end last-coord))
        (if (looking-at (concat "[[:space:]]*"
                                (regexp-opt
                                 latex-table-wizard--current-row-delims)))
            (progn (goto-char (match-end 0))
                   (latex-table-wizard--skip-stuff end-marker))
          (insert row-del "\n"))
        (dotimes (_ (1- ncols)) (insert " " col-del))
        (insert " " row-del "\n")))
    (run-hooks 'latex-table-wizard-after-table-modified-hook)))

(defalias 'latex-table-wizard-kill-row #'latex-table-wizard-kill-row-content)

;;;###autoload
(defun latex-table-wizard-kill-row-content ()
  "Kill the content of the current row, leaving delimiters in place."
  (interactive)
  (when (latex-table-wizard--in-tabular-env-p)
    (latex-table-wizard--setup)
    (save-excursion
      (latex-table-wizard--parse)
      (let* ((row-coords (latex-table-wizard--current-row-coords))
             kills poss)
        (dolist (coord row-coords)
          (push (buffer-substring (latex-table-wizard--cell-beg coord)
                                  (latex-table-wizard--cell-end coord))
                kills)
          (push (cons (latex-table-wizard--cell-beg coord)
                      (latex-table-wizard--cell-end coord))
                poss))
        (dolist (p poss)
          (let ((repl (replace-regexp-in-string
                       "[^\n]" " "
                       (buffer-substring-no-properties (car p) (cdr p)))))
            (delete-region (car p) (cdr p))
            (goto-char (car p))
            (insert repl)))
        (kill-new (string-join (nreverse kills) " "))
        (run-hooks 'latex-table-wizard-after-table-modified-hook)))))

;;;###autoload
(defun latex-table-wizard-delete-row ()
  "Delete the current row from the table."
  (interactive)
  (when (latex-table-wizard--in-tabular-env-p)
    (latex-table-wizard--setup)
    (save-excursion
      (latex-table-wizard--parse)
      (let* ((row-coords (latex-table-wizard--current-row-coords))
             (beg (latex-table-wizard--cell-beg (car row-coords)))
             (end (latex-table-wizard--cell-end
                   (car (latex-table-wizard--current-row-coords t))))
             (end+ (save-excursion
                     (goto-char end)
                     (if (looking-at (regexp-opt
                                      latex-table-wizard--current-row-delims))
                         (match-end 0)
                       end))))
        ;; If the row occupies whole lines, delete those lines entirely
        ;; rather than leaving a blank line behind.
        (let ((bol (save-excursion (goto-char beg)
                                   (skip-chars-backward " \t")
                                   (and (bolp) (point))))
              (eol (save-excursion (goto-char end+)
                                   (skip-chars-forward " \t")
                                   (and (eolp) (not (eobp)) (1+ (point))))))
          (when (and bol eol)
            (setq beg bol end+ eol)))
        (kill-region beg end+)
        (run-hooks 'latex-table-wizard-after-table-modified-hook)))))

;;; Interactive — commenting

(defun latex-table-wizard--comment-region (beg end)
  "Comment out BEG..END unless already in a comment."
  (save-excursion
    (unless (markerp end)
      (goto-char end)
      (setq end (point-marker)))
    (goto-char beg)
    (unless (TeX-in-comment)
      (comment-region beg end))))

;;;###autoload
(defun latex-table-wizard-comment-out-content ()
  "Comment out the content of the selection, or the current cell."
  (interactive)
  (when (latex-table-wizard--in-tabular-env-p)
    (latex-table-wizard--setup)
    (latex-table-wizard--parse)
    (let ((coords (or latex-table-wizard--selection
                      (list (latex-table-wizard--current-coord)))))
      (dolist (coord coords)
        (latex-table-wizard--comment-region
         (latex-table-wizard--cell-beg coord)
         (latex-table-wizard--cell-end coord)))
      (run-hooks 'latex-table-wizard-after-table-modified-hook)
      (message "Content of %d cell(s) commented out" (length coords)))))

;;;###autoload
(defun latex-table-wizard-comment-out ()
  "Comment out the selection including its delimiter, or the current cell.

Unlike \\='latex-table-wizard-comment-out-content\\=', this modifies the
table structure by also commenting out an adjacent delimiter."
  (interactive)
  (when (latex-table-wizard--in-tabular-env-p)
    (latex-table-wizard--setup)
    (latex-table-wizard--parse)
    (let* ((coords  (or latex-table-wizard--selection
                        (list (latex-table-wizard--current-coord))))
           ;; Compute every region before modifying the buffer.
           (regions (mapcar #'latex-table-wizard--cell-and-delimiter-bounds
                            coords)))
      (dolist (r regions)
        (latex-table-wizard--comment-region (car r) (cdr r)))
      (run-hooks 'latex-table-wizard-after-table-modified-hook)
      (message "%d cell(s) commented out" (length coords)))))

;;; Interactive — alignment

(defvar latex-table-wizard--align-status '(left center right compress)
  "Cycle state for \\='latex-table-wizard-align\\='.")

;;;###autoload
(defun latex-table-wizard-align (&optional mode)
  "Align and format the table at point.

Cycles through left, center, right, and compress modes.
Pass MODE directly to skip to a specific mode."
  (interactive)
  (when (latex-table-wizard--in-tabular-env-p)
    (latex-table-wizard--setup)
    (latex-table-wizard--remove-overlays)
    (unless (member last-command
                    '(latex-table-wizard-align
                      latex-table-wizard-align-left
                      latex-table-wizard-align-right
                      latex-table-wizard-center
                      latex-table-wizard-compress))
      (setq latex-table-wizard--align-status '(left center right compress)))
    (save-excursion
      (let* ((md  (or mode (car latex-table-wizard--align-status)))
             (tbl (latex-table-wizard--parse))
             (ht  (latex-table-wizard--table-cells tbl))
             (max-col (apply #'max (mapcar #'car (hash-table-keys ht))))
             (wh  (lambda (n) (insert (make-string n ?\s)))))
        (setq latex-table-wizard--align-status
              (append (cdr latex-table-wizard--align-status)
                      (list (car latex-table-wizard--align-status))))
        ;; 1. Ensure every row starts on its own line.
        (maphash (lambda (coord bounds)
                   (when (= (car coord) 0)
                     (goto-char (car bounds))
                     (unless (save-excursion
                               (skip-chars-backward " \t") (bolp))
                       (insert "\n"))))
                 ht)
        ;; 2. Normalize whitespace (invalidates markers, must re-parse).
        (whitespace-cleanup-region
         (latex-table-wizard--table-inner-beg tbl)
         (latex-table-wizard--table-inner-end tbl))
        (latex-table-wizard--invalidate)
        (setq tbl (latex-table-wizard--parse)
              ht  (latex-table-wizard--table-cells tbl))
        ;; 3. One space around every cell boundary.
        (maphash (lambda (_coord bounds)
                   (goto-char (car bounds)) (just-one-space)
                   (goto-char (cdr bounds)) (just-one-space))
                 ht)
        ;; 4. Column alignment.
        (unless (eq md 'compress)
          (dotimes (col (1+ max-col))
            (let* ((col-coords (latex-table-wizard--col-coords col))
                   (col-widths (mapcar (lambda (c)
                                         (save-excursion
                                           (goto-char
                                            (latex-table-wizard--cell-end c))
                                           (current-column)))
                                       col-coords))
                   (longest (apply #'max col-widths)))
              (dolist (coord col-coords)
                (goto-char (latex-table-wizard--cell-end coord))
                (let ((deficit (- longest (current-column))))
                  (when (> deficit 0)
                    (pcase md
                      ('left
                       (funcall wh deficit))
                      ('right
                       (goto-char (latex-table-wizard--cell-beg coord))
                       (funcall wh deficit))
                      ('center
                       (let ((post (/ deficit 2))
                             (pre  (- deficit (/ deficit 2))))
                         (funcall wh post)
                         (goto-char (latex-table-wizard--cell-beg coord))
                         (funcall wh pre))))))))))
        (message (pcase md
                   ('compress "Table compressed")
                   ('left     "Table content aligned left")
                   ('right    "Table content aligned right")
                   (_         "Table content centered")))))
    (run-hooks 'latex-table-wizard-after-table-modified-hook)))

;;;###autoload
(defun latex-table-wizard-align-left ()
  "Align table content to the left."
  (interactive)
  (latex-table-wizard-align 'left))

;;;###autoload
(defun latex-table-wizard-align-right ()
  "Align table content to the right."
  (interactive)
  (latex-table-wizard-align 'right))

;;;###autoload
(defun latex-table-wizard-center ()
  "Center table content in each column."
  (interactive)
  (latex-table-wizard-align 'center))

;;;###autoload
(defun latex-table-wizard-compress ()
  "Remove extra whitespace from cell margins."
  (interactive)
  (latex-table-wizard-align 'compress))

;;; Interactive — cell editing

(defun latex-table-wizard--fit-string (str len)
  "Trim STR if it exceeds LEN, otherwise pad it to LEN, centered."
  (let ((s (string-trim str)))
    (if (>= (length s) len)
        s
      (let* ((diff  (- len (length s)))
             (left  (make-string (/ diff 2) ?\s))
             (right (make-string (- diff (/ diff 2)) ?\s)))
        (concat left s right)))))

(defun latex-table-wizard--replace-cell-content (beg end str len)
  "Replace BEG..END with STR, padded to width LEN if it is shorter.

STR is always surrounded by at least one space, so that it never
touches the delimiters.  Runs
\\='latex-table-wizard-after-table-modified-hook\\='."
  (let ((width (max len (+ 2 (length (string-trim str))))))
    (delete-region beg end)
    (goto-char beg)
    (insert (latex-table-wizard--fit-string str width))
    (run-hooks 'latex-table-wizard-after-table-modified-hook)))

;;;###autoload
(defun latex-table-wizard-edit-cell ()
  "Interactively edit the content of the current cell."
  (interactive)
  (when (latex-table-wizard--in-tabular-env-p)
    (latex-table-wizard--parse)
    (let* ((coord   (latex-table-wizard--current-coord))
           (beg     (latex-table-wizard--cell-beg coord))
           (end     (latex-table-wizard--cell-end coord))
           (current (buffer-substring beg end))
           (len     (length current))
           (new     (read-string (format "Edit cell (%d,%d): "
                                         (car coord) (cdr coord))
                                 (string-trim current) nil nil t)))
      (latex-table-wizard--replace-cell-content beg end new len))))

(defvar-local latex-table-wizard--copied-cell-content nil
  "Last cell content stored by
\\='latex-table-wizard-copy-cell-content\\='.")

(defun latex-table-wizard--get-cell-content (&optional kill)
  "Copy (or kill, if KILL is non-nil) the current cell content."
  (when (latex-table-wizard--in-tabular-env-p)
    (latex-table-wizard--parse)
    (let* ((coord (latex-table-wizard--current-coord))
           (beg   (latex-table-wizard--cell-beg coord))
           (end   (latex-table-wizard--cell-end coord))
           (cont  (buffer-substring beg end)))
      (when kill
        (delete-region beg end)
        (goto-char beg)
        (insert " ")
        (run-hooks 'latex-table-wizard-after-table-modified-hook))
      (kill-new (string-trim cont))
      (setq latex-table-wizard--copied-cell-content cont)
      (message "Content of cell (%d,%d) %s"
               (car coord) (cdr coord) (if kill "killed" "copied")))))

;;;###autoload
(defun latex-table-wizard-copy-cell-content ()
  "Copy the current cell content to the kill ring."
  (interactive)
  (latex-table-wizard--get-cell-content))

;;;###autoload
(defun latex-table-wizard-kill-cell-content ()
  "Kill the current cell content."
  (interactive)
  (latex-table-wizard--get-cell-content t))

;;;###autoload
(defun latex-table-wizard-yank-cell-content ()
  "Replace the current cell content with the last copied cell content."
  (interactive)
  (unless latex-table-wizard--copied-cell-content
    (user-error "No cell content copied yet"))
  (when (latex-table-wizard--in-tabular-env-p)
    (latex-table-wizard--parse)
    (let* ((coord (latex-table-wizard--current-coord))
           (beg   (latex-table-wizard--cell-beg coord))
           (end   (latex-table-wizard--cell-end coord))
           (len   (- end beg)))
      (latex-table-wizard--replace-cell-content
       beg end latex-table-wizard--copied-cell-content len))))



;;; Transient interface

(defconst latex-table-wizard-default-transient-keys
  '((latex-table-wizard-copy-cell-content    "w"   "copy cell content")
    (latex-table-wizard-yank-cell-content    "y"   "yank cell content")
    (latex-table-wizard-edit-cell            "."   "edit cell")
    (toggle-truncate-lines                   "t"   "toggle truncate-lines")
    (latex-table-wizard-kill-cell-content    "k k" "kill cell content")
    (latex-table-wizard-kill-row-content     "k r" "kill row content")
    (latex-table-wizard-kill-column-content  "k c" "kill column content")
    (latex-table-wizard-delete-row           "D r" "delete row")
    (latex-table-wizard-delete-column        "D c" "delete column")
    (latex-table-wizard-comment-out          "; ;" "comment out")
    (latex-table-wizard-comment-out-content  "; c" "comment out content")
    (latex-table-wizard-insert-row           "i r" "insert row below")
    (latex-table-wizard-insert-column        "i c" "insert column right")
    (latex-table-wizard-swap                 "s"   "swap selection")
    (latex-table-wizard-deselect-all         "d"   "deselect all")
    (latex-table-wizard-select-row           "r"   "select row")
    (latex-table-wizard-select-column        "c"   "select column")
    (latex-table-wizard-select-deselect-cell "SPC" "select/deselect cell")
    (transient-quit-all                      "C-g" "done")
    (universal-argument                      "u"   "universal argument")
    (undo                                    "/"   "undo")
    (latex-table-wizard-align                "TAB" "cycle alignment")
    (latex-table-wizard-swap-row-down        "M-n" "swap row down")
    (latex-table-wizard-swap-row-up          "M-p" "swap row up")
    (latex-table-wizard-swap-column-left     "M-b" "swap column left")
    (latex-table-wizard-swap-column-right    "M-f" "swap column right")
    (latex-table-wizard-swap-cell-down       "C-n" "swap cell down")
    (latex-table-wizard-swap-cell-up         "C-p" "swap cell up")
    (latex-table-wizard-swap-cell-left       "C-b" "swap cell left")
    (latex-table-wizard-swap-cell-right      "C-f" "swap cell right")
    (exchange-point-and-mark                 "x"   "exchange point and mark")
    (latex-table-wizard-mark-cell            "m c" "mark cell")
    (latex-table-wizard-end-of-cell          "e"   "end of cell")
    (latex-table-wizard-beginning-of-cell    "a"   "beginning of cell")
    (latex-table-wizard-bottom               "N"   "bottom")
    (latex-table-wizard-top                  "P"   "top")
    (latex-table-wizard-beginning-of-row     "B"   "beginning of row")
    (latex-table-wizard-end-of-row           "F"   "end of row")
    (latex-table-wizard-down                 "n"   "move down")
    (latex-table-wizard-up                   "p"   "move up")
    (latex-table-wizard-left                 "b"   "move left")
    (latex-table-wizard-right                "f"   "move right"))
  "Default keybindings for the transient prefix.

Each element is (COMMAND KEY DESCRIPTION).")

(defconst latex-table-wizard--interactive-commands
  (append '(latex-table-wizard-align-left
            latex-table-wizard-align-right
            latex-table-wizard-center
            latex-table-wizard-compress
            latex-table-wizard
            latex-table-wizard-do)
          (mapcar #'car latex-table-wizard-default-transient-keys))
  "All interactive commands that form part of a latex-table-wizard chain.")

(defcustom latex-table-wizard-transient-keys
  (mapcar (lambda (x) (cons (nth 0 x) (nth 1 x)))
          latex-table-wizard-default-transient-keys)
  "Alist mapping command symbols to key description strings.

Changing this variable causes \\='latex-table-wizard-prefix\\=' to be
redefined.  See \\='latex-table-wizard-default-transient-keys\\=' for
the full list of commands that can be bound."
  :type `(alist :key-type
                (symbol :tag "Command:"
                        :options
                        ,(mapcar #'car
                                 latex-table-wizard-default-transient-keys))
                :value-type string)
  :group 'latex-table-wizard)

(defun latex-table-wizard--make-suffix (symbol)
  "Return the transient suffix spec for SYMBOL, or nil if not bound."
  (let ((descr (nth 2 (assq symbol latex-table-wizard-default-transient-keys)))
        (custom-key (cdr (assq symbol latex-table-wizard-transient-keys))))
    (when custom-key
      (list custom-key descr symbol :transient t))))

(defun latex-table-wizard--make-prefix ()
  "Define or redefine the \\='latex-table-wizard-prefix\\=' transient."
  (eval
   `(transient-define-prefix latex-table-wizard-prefix ()
      [:description
       "      LaTeX table wizard"
       [,(latex-table-wizard--make-suffix 'latex-table-wizard-right)
        ,(latex-table-wizard--make-suffix 'latex-table-wizard-left)
        ,(latex-table-wizard--make-suffix 'latex-table-wizard-up)
        ,(latex-table-wizard--make-suffix 'latex-table-wizard-down)
        ,(latex-table-wizard--make-suffix 'universal-argument)
        ""
        ,(latex-table-wizard--make-suffix 'latex-table-wizard-end-of-row)
        ,(latex-table-wizard--make-suffix 'latex-table-wizard-beginning-of-row)
        ,(latex-table-wizard--make-suffix 'latex-table-wizard-top)
        ,(latex-table-wizard--make-suffix 'latex-table-wizard-bottom)
        ,(latex-table-wizard--make-suffix 'latex-table-wizard-beginning-of-cell)
        ,(latex-table-wizard--make-suffix 'latex-table-wizard-end-of-cell)
        ""
        ,(latex-table-wizard--make-suffix 'latex-table-wizard-mark-cell)
        ,(latex-table-wizard--make-suffix 'exchange-point-and-mark)]
       [,(latex-table-wizard--make-suffix 'latex-table-wizard-swap-cell-right)
        ,(latex-table-wizard--make-suffix 'latex-table-wizard-swap-cell-left)
        ,(latex-table-wizard--make-suffix 'latex-table-wizard-swap-cell-up)
        ,(latex-table-wizard--make-suffix 'latex-table-wizard-swap-cell-down)
        ,(latex-table-wizard--make-suffix 'latex-table-wizard-swap-column-right)
        ,(latex-table-wizard--make-suffix 'latex-table-wizard-swap-column-left)
        ,(latex-table-wizard--make-suffix 'latex-table-wizard-swap-row-up)
        ,(latex-table-wizard--make-suffix 'latex-table-wizard-swap-row-down)
        ""
        ,(latex-table-wizard--make-suffix 'latex-table-wizard-insert-column)
        ,(latex-table-wizard--make-suffix 'latex-table-wizard-insert-row)
        ,(latex-table-wizard--make-suffix 'latex-table-wizard-kill-column-content)
        ,(latex-table-wizard--make-suffix 'latex-table-wizard-kill-row-content)
        ,(latex-table-wizard--make-suffix 'latex-table-wizard-kill-cell-content)
        ,(latex-table-wizard--make-suffix 'latex-table-wizard-delete-column)
        ,(latex-table-wizard--make-suffix 'latex-table-wizard-delete-row)]
       [,(latex-table-wizard--make-suffix 'latex-table-wizard-select-deselect-cell)
        ,(latex-table-wizard--make-suffix 'latex-table-wizard-select-column)
        ,(latex-table-wizard--make-suffix 'latex-table-wizard-select-row)
        ,(latex-table-wizard--make-suffix 'latex-table-wizard-deselect-all)
        ,(latex-table-wizard--make-suffix 'latex-table-wizard-swap)
        ""
        ,(latex-table-wizard--make-suffix 'latex-table-wizard-comment-out)
        ,(latex-table-wizard--make-suffix 'latex-table-wizard-comment-out-content)
        ""
        ,(latex-table-wizard--make-suffix 'latex-table-wizard-edit-cell)
        ,(latex-table-wizard--make-suffix 'latex-table-wizard-copy-cell-content)
        ,(latex-table-wizard--make-suffix 'latex-table-wizard-yank-cell-content)
        ,(latex-table-wizard--make-suffix 'toggle-truncate-lines)
        ,(latex-table-wizard--make-suffix 'latex-table-wizard-align)
        ,(latex-table-wizard--make-suffix 'undo)
        ,(latex-table-wizard--make-suffix 'transient-quit-all)]])))



;;; Environment detection

(defun latex-table-wizard--environments ()
  "Return the LaTeX environments \\='latex-table-wizard\\=' can operate on.

Computed on each call, so that changes to
\\='latex-table-wizard-new-environments-alist\\=' made after loading
the package are taken into account."
  (append (mapcar #'car latex-table-wizard-new-environments-alist)
          (mapcar #'car
                  (cl-remove-if-not
                   (lambda (c) (eq (nth 1 c) 'LaTeX-indent-tabular))
                   LaTeX-indent-environment-list))))

(defun latex-table-wizard--in-tabular-env-p (&optional pos)
  "Return non-nil if POS (default: point) is inside a supported environment."
  (member (save-excursion
            (goto-char (or pos (point)))
            (LaTeX-current-environment))
          (latex-table-wizard--environments)))

;;; Setup and teardown

(defun latex-table-wizard--setup ()
  "Prepare for the first command in an ltw chain.

Does nothing if \\='last-command\\=' is already part of a chain.
Otherwise: deactivates the mark, activates the mode if needed,
snaps point into the nearest cell, and applies overlays."
  (unless (memq last-command latex-table-wizard--interactive-commands)
    (when (region-active-p) (deactivate-mark))
    (if latex-table-wizard-mode
        (latex-table-wizard--make-prefix)
      (latex-table-wizard-mode 1))
    (latex-table-wizard--get-out)
    (latex-table-wizard--parse)
    (latex-table-wizard--hide-rest)
    (let ((coord (latex-table-wizard--current-coord)))
      (unless (<= (latex-table-wizard--cell-beg coord)
                  (point)
                  (latex-table-wizard--cell-end coord))
        (goto-char (latex-table-wizard--cell-beg coord)))
      (latex-table-wizard--hl-coords (list coord)))
    (if (latex-table-wizard--selection-in-current-table-p)
        (latex-table-wizard--echo-selection)
      (setq latex-table-wizard--selection nil))))

(defun latex-table-wizard--cleanup (&optional if-not-in-chain)
  "Remove all ltw overlays in the current buffer.

Removes unconditionally unless IF-NOT-IN-CHAIN is non-nil, in
which case removes only when exiting a chain of ltw commands."
  (let* ((this-wiz (memq this-command latex-table-wizard--interactive-commands))
         (exited   (and (memq last-command latex-table-wizard--interactive-commands)
                        (not this-wiz))))
    (when (or (not if-not-in-chain) exited (not this-wiz))
      (remove-overlays (point-min) (point-max) 'ltw-hl    t)
      (remove-overlays (point-min) (point-max) 'ltw-focus t))))

(defun latex-table-wizard--cleanup-if-not-in-chain ()
  "Remove ltw overlays when exiting a chain of ltw commands.

A named function (rather than a closure) so that it can be removed
from \\='pre-command-hook\\='."
  (latex-table-wizard--cleanup t))

;;; Minor mode

(define-minor-mode latex-table-wizard-mode
  "Minor mode for editing LaTeX table-like environments."
  :init-value nil
  :global nil
  :lighter " ltw"
  :group 'convenience
  (if latex-table-wizard-mode
      (progn
        (latex-table-wizard--make-prefix)
        (add-hook 'latex-table-wizard-after-table-modified-hook
                  #'latex-table-wizard--invalidate nil t)
        (add-hook 'before-save-hook    #'latex-table-wizard--cleanup nil t)
        (add-hook 'transient-exit-hook #'latex-table-wizard--cleanup nil t)
        (add-hook 'pre-command-hook
                  #'latex-table-wizard--cleanup-if-not-in-chain nil t))
    (remove-hook 'latex-table-wizard-after-table-modified-hook
                 #'latex-table-wizard--invalidate t)
    (remove-hook 'before-save-hook    #'latex-table-wizard--cleanup t)
    (remove-hook 'transient-exit-hook #'latex-table-wizard--cleanup t)
    (remove-hook 'pre-command-hook
                 #'latex-table-wizard--cleanup-if-not-in-chain t)
    (latex-table-wizard--invalidate)))

;;; Entry points

(defalias 'latex-table-wizard-do #'latex-table-wizard)

;;;###autoload
(defun latex-table-wizard ()
  "Edit the LaTeX table at point with a transient interface."
  (interactive)
  (when (latex-table-wizard--in-tabular-env-p)
    (latex-table-wizard--setup)
    (call-interactively #'latex-table-wizard-prefix)))

;;;###autoload
(defun latex-table-wizard-customize ()
  "Open the customization interface for \\='latex-table-wizard\\='."
  (interactive)
  (customize-browse 'latex-table-wizard))

(provide 'latex-table-wizard)
;;; latex-table-wizard.el ends here
