;;; latex-table-wizard.el --- Magic editing of LaTeX tables  -*- lexical-binding: t; -*-

;; Copyright (C) 2022, 2023 Free Software Foundation, Inc.

;; Author: Enrico Flor <enrico@eflor.net>
;; Maintainer: Enrico Flor <enrico@eflor.net>
;; URL: https://github.com/enricoflor/latex-table-wizard
;; Version: 1.5.3
;; Keywords: convenience

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
;; that organize output text into aligned colums.

;; The entry point of the package is

;;     M-x latex-table-wizard

;; while point is inside of a table(-like) environment.  From there, you
;; can do several things such as:

;;   + navigate "logically" (that is, move by cells);
;;   + insert or kill rows or column;
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

;; By default, the syntax this package expects is the one of standards
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

;;; Code:

;;; Dependencies

(require 'tex)
(require 'latex)
(require 'seq)
(eval-when-compile (require 'rx))
(require 'regexp-opt)
(eval-when-compile (require 'subr-x))
(require 'transient)

(defgroup latex-table-wizard nil
  "LaTeX table wizard configuration options."
  :prefix "latex-table-wizard-"
  :group 'convenience)

;;; Regular expressions and configuration options

(defcustom latex-table-wizard-allow-detached-args nil
  "If t, allow arguments of macros to be detached in parsing.

This means that if non-nil, this package will parse argument
groups (strings in brackets or in braces) as arguments of the
macro even if they are separated by whitespace, one line break,
and comments.  This conforms to how LaTeX interprets them.

However, doing this may cause some troubles if you happen to have
a string in braces at the start of the first
cell (position (0,0)): this is because if there is no blank line
between that cell and the table opening \\='\\begin\\=' macro
with its arguments, that string which should be in the first cell
may end up being parsed as an additional argument to the
\\='\\begin\\=' macro.

You avoid this danger if you set this variable to nil, but then
you should never have whitespace between the macro and its
arguments and between the arguments themselves."
  :type 'boolean)

(defcustom latex-table-wizard-warn-about-detached-args t
  "If t, warn about suspect cases of non-allowed detached arguments.

The warning will be echoed in the echo area any time that, while
parsing the table, cases in which a LaTeX macro and its
arguments, or two arguments of the same LaTeX macro might be
separated from its arguments by whitespace or comment are found.

Since the parser doesn't quite know what string preceded by an
unescaped backslash is a valid LaTeX macro and whether it accepts
what number of arguments, false positives are likely to be found.

If `latex-table-wizard-allow-detached-args' is non-nil, detached
arguments are allowed and so no warning will ever be issued
regardless of the value of this variable."
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
  "Name of macros that draw horizontal lines.

Each member of this list is a string that would be between the
\"\\\" and the arguments."
  :type '(repeat string))

(defcustom latex-table-wizard-new-environments-alist nil
  "Alist mapping environment names to property lists.

The environment name is a string, for example \"foo\" for an
environment like

  \\begin{foo}
      ...
  \\end{foo}

The cdr of each mapping is a property list with three keys:

   :col
   :row
   :lines

The values for :col and :row are two lists of strings.

The value for :lines is a list of strings just like is the case
for `latex-table-wizard-hline-macros', each of which is the name
of a macro that inserts some horizontal line.  For a macro
\"\\foo{}\", use string \"foo\"."
  :type '(alist :key-type (string :tag "Name of the environment:")
                :value-type (plist :key-type symbol
                                   :options (:col :row :lines)
                                   :value-type (repeat string)))
  :link '(variable-link latex-table-wizard-hline-macros))

;; Every time latex-table-wizard--parse-table is evaluated, the values
;; of the variables below are set:
(defvar latex-table-wizard--current-col-delims nil)
(defvar latex-table-wizard--current-row-delims nil)
(defvar latex-table-wizard--current-hline-macros nil)

(defun latex-table-wizard--set-current-values ()
  "Set temporary values that specify the syntax of the environment.

If the current environment is one that is mapped to something in
`latex-table-wizard-new-environments', set the values accordingly."
  (let* ((values (cdr (assoc (LaTeX-current-environment)
                             latex-table-wizard-new-environments-alist)))
         (col (plist-get values :col))
         (row (plist-get values :row))
         (lines (plist-get values :lines)))
    (if col
        (setq latex-table-wizard--current-col-delims col)
      (setq latex-table-wizard--current-col-delims
            latex-table-wizard-column-delimiters))
    (if row
        (setq latex-table-wizard--current-row-delims row)
      (setq latex-table-wizard--current-row-delims
            latex-table-wizard-row-delimiters))
    (if lines
        (setq latex-table-wizard--current-hline-macros lines)
      (setq latex-table-wizard--current-hline-macros
             latex-table-wizard-hline-macros))))

(defvar latex-table-wizard-after-table-modified-hook nil
  "Hook ran after table has been modified.

This hook is ran only after certain `latex-table-wizard'
interactive commands are called.")



;;; Parsing

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The central data structure is the CELL, which is a plist with     ;;
;; four keys:                                                        ;;
;;                                                                   ;;
;; + :column (column number, starting from 0 as the leftmost column) ;;
;; + :row (row number, starting from 0 as the top row)               ;;
;; + :start (marker, beginning of inside of the cell)                ;;
;; + :end (marker, end of inside of the cell)                        ;;
;;                                                                   ;;
;; A parse of a table is a list of all its cells represented as such ;;
;; plists.                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; this rx expression matches what can separate different arguments of
;; a (La)TeX macro: whitespace and comments.  If
;; latex-table-wizard-allow-detached-args is nil, this rx
;; expression will effectively never be used.
(defconst latex-table-wizard--blank-detach-arg-re
  (rx (seq (* space)
           (? (seq "%" (* not-newline)))
           (? "\n")
           (* (seq (* space) "%" (* not-newline) "\n"))
           (* space)))
  "Regexp matching what can separate a macro from its arguments.")

(defvar latex-table-wizard--detached nil)

(defun latex-table-wizard--warn-detached ()
  "Warn the user if suspected detached macros are found in table.

A macro is detached if there is any blank string separating the
macro from its arguments or one argument from the next.

Don't do anything if
`latex-table-wizard-allow-detached-args' is non-nil,
because it means that the user is aware of this and is taking the
measures needed for the parser not to be confused."
  (unless latex-table-wizard-allow-detached-args
    (let ((message-log-max 0))
      (message (concat "Warning: suspect detached macro found.\n"
                       "If the table isn't parsed correctly, "
                       "try not to separate arguments from macro,\n"
                       "or set latex-table-wizard-allow-detached-args"
                       "to t.")))))

(defun latex-table-wizard--macro-at-point (&optional pos bound detached-args)
  "Return data about LaTeX macro at point or at POS, if any.

If POS is nil, check whether point is currently on a LaTeX macro,
otherwise check if buffer position or marker POS is on one.

If BOUND is nil, look only as far back as
 `latex-table-wizard--table-begin' (if that is nil too, it
 defaults to the minimum available position in current buffer),
 otherwise stop at BOUND (a buffer position or marker).

Return value is a list that start with the buffer positions of
begin and end of the macro, and then the strings corresponding to
the name and each of its arguments.

This function knows nothing about the signature of the macro, so
it's greedy (it keeps eating up arguments at it finds them, but
it does not skip over an empty line).

If DETACHED-ARGS is non-nil, allow for arguments of the macro to
be separated by whitespace and one line break.

Should be rather costly, but robust."
  (let ((limit (or bound latex-table-wizard--table-begin (point-min)))
        (skip-some (lambda ()
                     (when detached-args
                       (skip-chars-forward " \t"))
                     (while (looking-at-p "%.*")
                       (goto-char (line-beginning-position 2)))
                     (when detached-args
                       (skip-chars-forward "\n" (line-end-position 2))
                       (skip-chars-forward " \t"))))
        (start (or pos (point)))
        guess b return e intermediate)
    (save-excursion
      (goto-char start)
      (when (and (not (TeX-escaped-p (1- (point))))
                 (looking-back "[\]}]" (line-beginning-position)))
        (forward-char -1))
      (setq guess (ignore-errors (LaTeX-what-macro limit)))
      (cond ((and (looking-back "\\\\[[:alpha:]]*" (line-beginning-position))
                  (not (TeX-escaped-p (match-beginning 0))))
             (goto-char (match-beginning 0)))
            ((and (not guess)
                  (looking-at-p "\\\\")
                  (not (TeX-escaped-p)))
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
      (setq b (point)
            intermediate (point))
      (when (looking-at "\\\\[^\[{\s]+")
        (goto-char (match-end 0)))
      (push (buffer-substring-no-properties intermediate (point))
            return)
      (funcall skip-some)
      (while (looking-at-p "[\[{]")
        (setq intermediate (point))
        (forward-sexp 1)
        (push (buffer-substring-no-properties intermediate (point))
              return)
        (funcall skip-some))
      (skip-chars-backward " \t\n")
      (setq e (point))
      (unless (>= start e)
        (cons b (cons e (nreverse (mapcar #'string-trim return))))))))

(defun latex-table-wizard--goto-end-of-macro (&optional pos names re)
  "If looking at unescaped macro named NAME, go to its end.

If POS is non-nil, it is a marker or buffer position, and it is
the position from which the macro whould be searched.  If nil, it
defaults to the current value of `point'.

If NAMES is nil, skip any LaTeX macro that point is looking at.
Otherwise, it is a list of strings, and this funcation will only
skip those macros whose name (without the backslash) is in NAME.

If RE is non-nil, it is a regular expression this function will
skip every macro whose name is matched by it (ignoring the value
passed as NAMES)

Call `latex-table-wizard--warn-detached' if the macro is
separated from its arguments, or any two successive arguments are
separated from each other."
  (when-let* ((macro
               (latex-table-wizard--macro-at-point
                pos nil latex-table-wizard-allow-detached-args))
              (mname (string-trim-left (nth 2 macro) "\\\\")))
    (when (or (and (not names) (not re))
              (member mname names)
              (when re (string-match re mname)))
      (goto-char (nth 1 macro)))))

(defun latex-table-wizard--get-out ()
  "If point is on an environment delimiting macro, move out.

If it is on an \\='end\\=' macro, move to its end, otherwise to
its beginning."
  (latex-table-wizard--set-current-values)
  (when-let* ((macro (latex-table-wizard--macro-at-point))
              (name (string-trim-left "\\\\" (nth 2 macro))))
    (cond ((equal name "begin")
           (goto-char (nth 0 macro)))
          ((equal name "end")
           (goto-char (nth 1 macro)))
          (t nil))))

(defun latex-table-wizard--skip-stuff (&optional bound)
  "Skip comments, blank space and hline macros.

Hline macros are LaTeX macros whose name is a string in
`latex-table-wizard--current-hline-macros'.

BOUND is a buffer position or marker: this function will not skip
beyond that point.  If it is nil, it defaults to the value of
`latex-table-wizard--table-end' (if that is nil too, it is the
maximum available position in current buffer)."
  (let ((lim (or bound
                 latex-table-wizard--table-end
                 (save-excursion
                   (goto-char (point-max))
                   (point-marker))))
        done new-start-of-line)
    (catch 'stop
      (while (and (not done) (<= (point) lim))
        (skip-syntax-forward " ")
        (let ((temp-pos (point)))
          (when (looking-at "\n\\|%")
            (forward-line)
            (setq new-start-of-line (point))
            (when (looking-at (concat
                               "[[:space:]]*"
                               (string-join
                                latex-table-wizard--current-col-delims
                                "\\|")))
              (throw 'stop nil)))
          (ignore-errors
            (latex-table-wizard--goto-end-of-macro
             nil latex-table-wizard--current-hline-macros))
          (when (looking-at "\n\\|%")
            (forward-line)
            (setq new-start-of-line (point)))
          (when (= (point) temp-pos)
            ;; we haven't moved since trying to skip whitespace, we
            ;; are done here.
            (setq done t)))))
    (when new-start-of-line (goto-char new-start-of-line))))

(defun latex-table-wizard--get-cell-boundaries (col-re
                                                row-re
                                                beginning limit)
  "Return boundaries of current cell (where point is).

What is returned is a list of the form

    (B E EOR)

where B and E are markers (beginning and end of the cell), and
EOR is t iff this cell is the rightmost cell in the current row,
nil otherwise.

COL-RE and ROW-RE are regular expressions matching column and row
delimiters respectively.

BEGINNING is a buffer position that is assumed to be where the
topmost point a cell left boundary can be.

LIMIT is a buffer position at which the parsing stops."
  (let ((beg (point-marker))
        end end-of-row)
    (latex-table-wizard--skip-stuff limit)
    (unless (string-blank-p (buffer-substring-no-properties beg (point)))
      (setq beg (point-marker)))
    (while (and (< (point) limit) (not end))
      (let ((macro (latex-table-wizard--macro-at-point
                    nil beginning latex-table-wizard-allow-detached-args)))
        (cond ((looking-at-p "[[:space:]]+%")
               (TeX-comment-forward 1))
              ((TeX-escaped-p)
               ;; whatever we are looking at is escaped so we just go
               ;; one step forward
               (forward-char 1))
              ((looking-at col-re)
               ;; a column delimiter: bingo
               (setq end (point-marker))
               (goto-char (match-end 0)))
              ((looking-at row-re)
               ;; a row delimiter: bingo
               (let ((after-del (save-excursion
                                  (goto-char (match-end 0))
                                  (point-marker)))
                     (end-of-previous-cell
                      (progn (goto-char (match-beginning 0))
                             (point-marker))))
                 (goto-char after-del)
                 (setq end end-of-previous-cell
                       end-of-row t)
                 (latex-table-wizard--skip-stuff limit)))
              ((looking-at "\\$\\|{")
               (unless (ignore-errors (forward-sexp))
                 (forward-char 1)))
              ((looking-at "\\\\(\\|\\\\\\[")
               (TeX-search-unescaped "\\\\)\\|\\\\\\]" 'forward t nil t))
              ((looking-at "[[:space:]]*\\\\\\(begin[\[{]\\)")
               (goto-char (match-beginning 1))
               (LaTeX-find-matching-end))
              (macro
               (goto-char (nth 1 macro)))
              (t (forward-char 1)))))
    `(,beg ,end ,end-of-row)))

(defsubst latex-table-wizard--get-env-ends (table)
  "Return leftmost and rightmost positions in TABLE.

TABLE is a list of cell plists.  The return type is a cons
cell (B . E) with B and E being markers.

Note that if TABLE is the list of all cells (i.e. the return
value of `latex-table-wizard--parse-table'), B and E will not
necessarily correspond to `latex-table-wizard--table-begin' and
`latex-table-wizard--table-end'.  These value should be equal
only if there are no hline macros at the beginning or at the end
of the table (which are part of the table environment but not of
any cell)."
  `(,(apply #'min (mapcar (lambda (x) (plist-get x :start)) table))
    .
    ,(apply #'max (mapcar (lambda (x) (plist-get x :end)) table))))

(defvar-local latex-table-wizard--parse nil
  "Data from a parsed table environment.

The value of this variable is a list of the form

    (H P)

where H is a hash string and P is a list of plists (that is, of
cell objects).  H is the sha256 of the corresponding buffer
substring and P is the parse of the the environment.")

(defvar-local latex-table-wizard--table-begin nil
  "Marker corresponding to the beginning of the inside of the table.

The value of this variable is set by
`latex-table-wizard--parse-table'.

Note that this is not the left boundary of the top left cell: the
value of this variable is a position preceding any hline macro
that is inside of the table environment but not considered part
of a cell.  If you need that value instead you need to get the
car of the result of applying `latex-table-wizard--get-env-ends'
to the list of all cells.")

(defvar-local latex-table-wizard--table-end nil
  "Marker corresponding to the end of the inside of the table.

The value of this variable is set by
`latex-table-wizard--parse-table'.

Note that this is not the right boundary of the bottom right
cell: the value of this variable is a position preceding any
hline macro that is inside of the table environment but not
considered part of a cell.  If you need that value instead you
need to get the cdr of the result of applying
`latex-table-wizard--get-env-ends' to the list of all cells.")

(defun latex-table-wizard--parse-table ()
  "Parse table(-like) environment point is in.

Return a list of plists, each of which is a cells and has the
form

    (:column C :row R :start S :end E).

Each value is an integer, S and E are markers.

If point is inside the table but between two cells, relocate it
to the one that precedes point."
  (setq latex-table-wizard--detached nil)
  (let* ((bl-rx (if latex-table-wizard-allow-detached-args
                    latex-table-wizard--blank-detach-arg-re
                  ""))
         (cells-list '())
         (col 0)
         (row 0)
         (env-beg (save-excursion
                    (LaTeX-find-matching-begin)
                    (latex-table-wizard--goto-end-of-macro (1+ (point)))
                    (ignore-errors
                      (latex-table-wizard--goto-end-of-macro
                       nil latex-table-wizard--current-hline-macros))
                    (point-marker)))
         (env-end (save-excursion
                    (LaTeX-find-matching-end)
                    (if-let ((end-macro
                              (latex-table-wizard--macro-at-point
                               (1- (point))
                               latex-table-wizard-allow-detached-args)))
                        (goto-char (car end-macro))
                      (TeX-search-unescaped (concat "\\\\end" bl-rx "[{\[]")
                                            'backward t env-beg t))
                    (re-search-backward "[^[:space:]]" nil t)
                    (while (TeX-in-comment)
                      (TeX-search-unescaped "%" 'backward t env-beg t)
                      (re-search-backward "[^[:space:]]" nil t))
                    (unless (eolp) (forward-char 1))
                    (point-marker)))
         (hash (secure-hash 'sha256
                            (buffer-substring-no-properties env-beg
                                                            env-end))))
    (save-excursion (goto-char env-beg)
                    (latex-table-wizard--set-current-values))
    (let ((col-re (regexp-opt latex-table-wizard--current-col-delims))
          (row-re (regexp-opt latex-table-wizard--current-row-delims)))
      (if (and (ignore-errors (<= latex-table-wizard--table-begin
                                  (point-marker)
                                  latex-table-wizard--table-end))
               (equal env-beg latex-table-wizard--table-begin)
               (equal env-end latex-table-wizard--table-end)
               (equal hash (car latex-table-wizard--parse)))
          (nth 1 latex-table-wizard--parse)
        (setq latex-table-wizard--table-begin env-beg
              latex-table-wizard--table-end env-end)
        (save-excursion
          (goto-char env-beg)
          ;; we need to make some space between the end of of the \begin
          ;; macro and the start of the (0,0) cell
          (if (looking-at-p "[[:space:]]")
              (forward-char 1)
            (insert " "))
          (TeX-comment-forward 1)
          (while (looking-at-p "[[:space:]]*%")
            (TeX-comment-forward 1))
          (skip-syntax-backward " ")
          (while (< (point) env-end)
            (let ((data (latex-table-wizard--get-cell-boundaries
                         col-re row-re env-beg env-end)))
              (push (list :column col
                          :row row
                          :start (nth 0 data)
                          :end (if (nth 1 data) (nth 1 data) env-end))
                    cells-list)
              (if (nth 2 data)         ; this was the last cell in the row
                  (setq row (1+ row)
                        col 0)
                (setq col (1+ col)))
              ;; if we just hit the end of a row and the next thing coming
              ;; is another row delimiter, skip that one (because you are
              ;; not in a cell)
              (while (and (nth 2 data)
                          (save-excursion
                            (skip-syntax-forward " ")
                            (looking-at-p row-re)))
                (re-search-forward row-re nil t)))))
        (setq latex-table-wizard--parse (list hash cells-list))
        (when latex-table-wizard--detached
          (latex-table-wizard--warn-detached))
        cells-list))))

(defun latex-table-wizard--get-cell-pos (table prop-val1
                                               &optional prop-val2)
  "Return the cell plist from TABLE at specific position.

The position is given by PROP-VAL1 and PROP-VAL2, each of which
is a cons cell of the form (P . V), where P is either
\\=':column\\=' or \\=':row\\=' and V is the corresponding value.

If prop-val2 is nil, it is assumed that TABLE is a list of cells
that only differ for the property in the car of PROP-VAL1 (in
other words, that TABLE is either a column or a row)"
  (catch 'cell
    (if prop-val2
        (dolist (x table)
          (when (and (= (cdr prop-val1) (plist-get x (car prop-val1)))
                     (= (cdr prop-val2) (plist-get x (car prop-val2))))
            (throw 'cell x)))
      (dolist (x table)
        (when (= (cdr prop-val1) (plist-get x (car prop-val1)))
          (throw 'cell x))))))

(defun latex-table-wizard--sort (table same-line dir)
  "Return a sorted table, column or row given TABLE.

TABLE is a list of cells (a list of plists) that includes
the cell point is in.

If SAME-LINE is non-nil, return sorted current column (if DIR is
either \\='next\\=' or \\='previous\\=') or current row (if
DIR is either \\='forward\\=' or \\='backward\\=').

If SAME-LINE is nil, return sorted table, so that given a table
like this:

  A & B & C \\
  D & E & F

if DIR is either \\='forward\\=' or \\='backward\\=', A follows
F, C precedes D and so on; and if DIR is either \\='next\\=' or
\\='previous\\=', A follows F, D precedes B and so on."
  (let* ((vert (memq dir '(next previous)))
         (prop (if vert :row :column))
         (thing (if vert
                    (latex-table-wizard--get-thing 'column table)
                  (latex-table-wizard--get-thing 'row table)))
         (copy-table (copy-sequence table)))
    (if (not same-line)
        (sort copy-table (lambda (x y)
                           (let ((rows (list (plist-get x :row)
                                             (plist-get y :row)))
                                 (cols (list (plist-get x :column)
                                             (plist-get y :column))))
                             (cond ((and vert (apply #'= cols))
                                    (apply #'< rows))
                                   (vert
                                    (apply #'< cols))
                                   ((apply #'= rows)
                                    (apply #'< cols))
                                   (t
                                    (apply #'< rows))))))
      (sort thing (lambda (x y) (< (plist-get x prop)
                                   (plist-get y prop)))))))



;;; Moving around

(defun latex-table-wizard--get-landing-index (now steps max-index
                                                  &optional min-index)
  "Move across indices of a sequence.

NOW is the index from which to start the movement.

STEPS, an integer, specifies how many steps to move forward or
backwards from index NOW (depending on whether it is a positive
or negative integer).

MAX-INDEX is the index at which the movement restarts from
MIN-INDEX (which if not specified defaults to 0)."
  (let* ((zero-index (or min-index 0))
         (floor (min zero-index max-index))
         (ceiling (max zero-index max-index))
         (count 0))
    (while (< count (abs steps))
      (let ((new (if (>= steps 0) (1+ now) (1- now))))
        (cond ((> new ceiling) (setq now floor
                                     count (1+ count)))
              ((< new floor) (setq now ceiling
                                   count (1+ count)))
              (t (setq now new
                       count (1+ count))))))
    now))

(defun latex-table-wizard--get-other-cell (dir same-line count table curr)
  "Return cell plist from TABLE.

The cell that is returned is the one found moving COUNT cells
from current cell CURR in direction DIR (either \\='forward\\=',
\\='backward\\=', \\='next\\=' or \\='previous\\=').

If SAME-LINE is non-nil, loop over the current row (if DIR is
\\='forward\\=' or \\='backward\\='), or column (if DIR is
\\='next\\=' or \\='previous\\=').  Otherwise continue search for
cell in a different row or column if no cell is left in the
current DIR."
  (let* ((steps (or count 1))
         (sorted (latex-table-wizard--sort table same-line dir))
         (cell-num (1- (length sorted)))
         (now (let ((ind 0)
                    (col (plist-get curr :column))
                    (row (plist-get curr :row)))
                (catch 'stop
                  (dolist (i sorted)
                    (when (and (= (plist-get i :column) col)
                               (= (plist-get i :row) row))
                      (throw 'stop t))
                    (setq ind (1+ ind))))
                ind))
         (land (if (memq dir '(next forward))
                   (latex-table-wizard--get-landing-index
                    now steps 0 cell-num)
                 (latex-table-wizard--get-landing-index
                  now (- 0 steps) 0 cell-num))))
    (nth land sorted)))

(defun latex-table-wizard--remove-overlays (&optional table beg end)
  "Remove table internal overlays.

These are the overlays that have a non-nil value for the name
property \\='table-inside-ol\\='.

Optional arguments BEG and END are passed, they are buffer
positions or markers indicating beginning and end of the table.

Optional arguments TABLE is a list of cell plists: if its not
given, a value is retrieved with
`latex-table-wizard--parse-table'."
  (if beg
      (remove-overlays beg end 'tabl-inside-ol t)
    (let* ((tab (or table (latex-table-wizard--parse-table)))
           (lims (latex-table-wizard--get-env-ends tab)))
      (remove-overlays (car lims) (cdr lims) 'tabl-inside-ol t))))

(defun latex-table-wizard--hl-cells (list-of-cells)
  "Highlight cells in LIST-OF-CELLS with an overlay.

The overlay has a non-nil value for the name property
\\='table-inside-ol\\='."
  (unless latex-table-wizard-no-highlight
    (let ((ols '()))
      (dolist (x list-of-cells)
        (push (make-overlay (plist-get x :start)
                            (plist-get x :end))
              ols))
      (dolist (x ols)
        (overlay-put x 'tabl-inside-ol t)
        (overlay-put x 'face 'latex-table-wizard-highlight)))))

(defvar-local latex-table-wizard--selection nil
  "Current selection, a list of cell objects.")

(defun latex-table-wizard--locate-point (pos &optional cells)
  "Return cell from CELLS in which position POS is in.

If CELLS is nil, it defaults to the value of
`latex-table-wizard--parse-table'.

POS is a buffer position or a marker.

If POS is not in a cell in CELLS, it means it's between two
cells: return the closest one after having moved point to its
beginning.."
  (let* ((table (or cells (latex-table-wizard--parse-table)))
         (ends (latex-table-wizard--get-env-ends table)))
    (cond ((< pos (car ends))
           (latex-table-wizard--get-cell-pos table
                                             '(:column . 0) '(:row . 0)))
          ((> pos (cdr ends))
           (car (seq-filter
                 (lambda (x) (= (plist-get x :end) (cdr ends)))
                 table)))
          (t
           (let (candidate)
             (catch 'found
               (dolist (c table)
                 (when (<= (plist-get c :start) pos (plist-get c :end))
                   (setq candidate c)
                   (throw 'found t))))
             (if candidate
                 candidate
               (let* ((end-pos
                       (thread-last
                         table
                         (seq-filter (lambda (x) (< (plist-get x :end) pos)))
                         (mapcar (lambda (x) (plist-get x :end)))
                         (apply #'max)))
                      (final (seq-find (lambda (x) (= end-pos
                                                      (plist-get x :end)))
                                       table)))
                 (goto-char (plist-get final :start))
                 final)))))))

(defun latex-table-wizard--get-thing (thing &optional table)
  "Return THING point is in.

THING can be either \\='cell\\=', \\='column\\=' or \\='row\\='.

TABLE is a list of cell plists.  If it is nil, evaluate
`latex-table-wizard--parse-table' to get a value.

If THING is \\='cell\\=', return one plist, else return a list of
plists."
  (let* ((pos (point))
         (cells-list (or table (latex-table-wizard--parse-table)))
         (curr (latex-table-wizard--locate-point pos)))
    (if (eq thing 'cell)
        curr
      (let* ((prop (if (eq thing 'row) :row :column))
             (other-prop (if (eq thing 'row) :column :row))
             (curr-value (plist-get curr prop)))
        (sort (seq-filter (lambda (x) (= curr-value (plist-get x prop)))
                          cells-list)
              (lambda (x y) (> (plist-get x other-prop)
                               (plist-get y other-prop))))))))

(defsubst latex-table-wizard--shift (dir cell table)
  "Given a CELL and a list of cells TABLE, return one of TABLE.

The cell returned is the one whose coordinates correspond to
having CELL shifted in direction DIR (whose value is either
\\='next\\=', \\='previous\\=', \\='forward\\=' or
\\='backward\\=').  If no such cell is found in TABLE, return
nil."
  (let (target                          ; a cons cell of (column . row)
        output)
    (cond ((eq dir 'next)
           (setq target (cons (plist-get cell :column)
                              (1+ (plist-get cell :row)))))
          ((eq dir 'previous)
           (setq target (cons (plist-get cell :column)
                              (1- (plist-get cell :row)))))
          ((eq dir 'forward)
           (setq target (cons (1+ (plist-get cell :column))
                              (plist-get cell :row))))
          ((eq dir 'backward)
           (setq target (cons (1- (plist-get cell :column))
                              (plist-get cell :row)))))
    (catch 'found
      (dolist (c table)
        (when (and (= (plist-get c :column) (car target))
                   (= (plist-get c :row) (cdr target)))
          (setq output c)
          (throw 'found t))))
    output))

(defun latex-table-wizard--jump (dir &optional absolute
                                     count same-line nocycle)
  "Move point to the beginning of a cell in the table.

DIR is either \\='next\\=', \\='previous\\=', \\='forward\\=' or
\\='backward\\=' and determines the direction of motion.  This
function assumes being evaluated with point inside of a
tabular-like environment.

With ABSOLUTE being t, move to the last or first cell in the row
or column (depending on the value of DIR) point is currently in.

COUNT is a positive integer that determines how many steps in
direction DIR to take.

If SAME-LINE is non-nil, never leave current column or row.

If NOCYCLE is non-nil, do not move and return nil in case the
jump would move point to a different column (if DIR is either
\\='forward\\=' or \\='backward\\=') or to a different row (if
DIR is either \\='next\\=', \\='previous\\=')."
  (when (latex-table-wizard--in-tabular-env-p)
    (latex-table-wizard--setup)
    (with-silent-modifications
      (let* ((message-log-max 0)
             (cells (latex-table-wizard--parse-table))
             (curr (latex-table-wizard--get-thing 'cell cells))
             (target (if (not absolute)
                         (latex-table-wizard--get-other-cell
                          dir same-line count cells curr)
                       (let ((sorted (latex-table-wizard--sort cells t dir)))
                         (if (memq dir '(previous backward))
                             (car sorted)
                           (car (last sorted))))))
             (stop (and nocycle (not (latex-table-wizard--shift dir curr cells)))))

        (latex-table-wizard--remove-overlays cells)
        (unless stop
          ;; (goto-char (plist-get curr :start))
          (goto-char (plist-get target :start))
          (latex-table-wizard--hl-cells `(,target))
          (latex-table-wizard--hl-cells latex-table-wizard--selection)
          (message "Col X Row (%d,%d)"
                   (plist-get target :column)
                   (plist-get target :row)))))))



;;; Swapping functions

(defun latex-table-wizard--swap-cells (x y)
  "Swap the content of two cells X and Y."
  (save-excursion
    (let ((x-string (concat
                     " "
                     (string-trim
                      (buffer-substring (plist-get x :start)
                                        (plist-get x :end)))
                     " "))
          (y-string (concat
                     " "
                     (string-trim
                      (buffer-substring (plist-get y :start)
                                        (plist-get y :end)))
                     " ")))
      (goto-char (plist-get x :end))
      (delete-region (plist-get x :start) (plist-get x :end))
      (insert y-string)
      (just-one-space)
      (goto-char (plist-get y :end))
      (delete-region (plist-get y :start) (plist-get y :end))
      (insert x-string)
      (just-one-space))))

(defun latex-table-wizard--type-of-selection (sel)
  "Return type of list of cells SEL.

Non-nil values that are returned are is either \\='cell\\=' (if
SEL only contains one cell), \\='column\\=' or \\='row\\='.

If SEL is a list of more than one cell such that not all the
cells have the same value for either :column or :row, it means
that this selection is neither a column or a row, and nil is
returned."
  (cond ((= 1 (length sel)) 'cell)
        ((not sel) (user-error "Empty selection"))
        ((apply #'= (mapcar (lambda (x) (plist-get x :column)) sel)) 'column)
        ((apply #'= (mapcar (lambda (x) (plist-get x :row)) sel)) 'row)
        (t nil)))

(defun latex-table-wizard--swap-line (type line1 line2)
  "Swap columns or rows LINE1 and LINE2.

TYPE is either \\='column\\=' or \\='row\\='."
  (save-excursion
    (let ((prop (if (eq type 'column) :row :column)))
      (dolist (x line1)
        (let ((other (latex-table-wizard--get-cell-pos
                      line2 `(,prop . ,(plist-get x prop)))))
          (latex-table-wizard--swap-cells x other))))))

(defun latex-table-wizard--swap-adjacent-line (dir type)
  "Swap current thing of type TYPE with the one in direction DIR.
DIR is either \\='forward\\=', \\='backward\\=', \\='next\\=' or
\\='previous\\='.
TYPE is either \\='cell\\=', \\='column\\=' or \\='row\\='."
  (latex-table-wizard--remove-overlays)
  (when (latex-table-wizard--in-tabular-env-p)
    (latex-table-wizard--setup)
    (cond ((eq type 'cell) (latex-table-wizard-select-deselect-cell t t))
          ((memq dir '(forward backward))
           (latex-table-wizard-select-column t))
          ((memq dir '(previous next))
           (latex-table-wizard-select-row t)))
    (setq latex-table-wizard--selection
          (seq-uniq latex-table-wizard--selection))
    (latex-table-wizard--jump dir nil 1 t)
    (latex-table-wizard-swap)
    (let ((new-table (latex-table-wizard--parse-table)))
      (if (eq type 'cell)
          (latex-table-wizard--hl-cells
           `(,(latex-table-wizard--get-thing type new-table)))
        (latex-table-wizard--hl-cells
         (latex-table-wizard--get-thing type new-table))))))

(defun latex-table-wizard--select-thing (thing &optional no-message)
  "Add THING point is at to list `latex-table-wizard--selection'.

THING is either \\='cell\\=', \\='column\\=' or \\='row\\='.

Don't print any message if NO-MESSAGE is non-nil."
  (let* ((table (latex-table-wizard--parse-table))
         (sel (latex-table-wizard--get-thing thing table))
         (message-log-max 0))
    (if (eq thing 'cell)
        (setq latex-table-wizard--selection
              (cons sel latex-table-wizard--selection))
      (setq latex-table-wizard--selection sel))
    (cond ((eq thing 'cell)
           (unless no-message
             (message "Cell (%s,%s) selected for swapping"
                      (plist-get sel :column)
                      (plist-get sel :row)))
           (latex-table-wizard--hl-cells (list sel)))
          ((eq thing 'row)
           (unless no-message
             (message "Row %s selected for swapping"
                      (plist-get (car sel) :row)))
           (latex-table-wizard--hl-cells sel))
          (t (unless no-message
               (message "Column %s selected for swapping"
                        (plist-get (car sel) :column)))
             (latex-table-wizard--hl-cells sel)))))



;;; Interactive functions

(defvar latex-table-wizard--align-status '(left center right compress))

(defun latex-table-wizard-align (&optional mode)
  "Align and format table at point.

Have every row start on its own line and vertically align column
delimiters.

There are five possible values for MODE:

- \\='left\\=': align left
- \\='center\\=': center text
- \\='right\\=': align right
- \\='compress\\=': remove extra whitespace at cell margins (no
    alignment)
- \\='nil\\=': cycle through the four above modes."
  (interactive)
  (when (latex-table-wizard--in-tabular-env-p)
    (latex-table-wizard--setup)
    (latex-table-wizard--remove-overlays)
    (unless (member last-command
                    '(latex-table-wizard-align
                      latex-table-wizard-align-left
                      latex-table-wizard-align-right
                      latex-table-wizard-align-center
                      latex-table-wizard-compress))
      ;; always start with aligning left, as per fountainer's
      ;; suggestion
      ;; https://github.com/enricoflor/latex-table-wizard/issues/2#issue-1748301074
      (setq latex-table-wizard--align-status '(left center right compress)))
    (save-excursion
      (let ((message-log-max 0)
            (md (or mode (car latex-table-wizard--align-status)))
            (max-col (thread-last (latex-table-wizard--parse-table)
                                  (mapcar (lambda (x) (plist-get x :column)))
                                  (delete-dups)
                                  (apply #'max))))
        (setq latex-table-wizard--align-status
              (append (cdr latex-table-wizard--align-status)
                      (list (car latex-table-wizard--align-status))))
        (dolist (x (seq-filter (lambda (x) (= 0 (plist-get x :column)))
                               (latex-table-wizard--parse-table)))
          (goto-char (plist-get x :start))
          (unless (looking-back "^[[:space:]]*" (line-beginning-position))
            (insert "\n")))
        (whitespace-cleanup-region latex-table-wizard--table-begin
                                   latex-table-wizard--table-end)
        (dolist (x (flatten-list (mapcar (lambda (x) `(,(plist-get x :start)
                                                       ,(plist-get x :end)))
                                         (latex-table-wizard--parse-table))))
          (goto-char x)
          (just-one-space))
        (if (eq md 'compress)
            (message "Table compressed")
          (let ((count 0))
            (while (<= count max-col)
              (let ((line (seq-filter (lambda (x) (= count
                                                     (plist-get x :column)))
                                      (latex-table-wizard--parse-table)))
                    (col-pos '()))
                (dolist (cell line)
                  (goto-char (plist-get cell :end))
                  (push (current-column) col-pos))
                (let ((longest (apply #'max col-pos)))
                  (dolist (cell line)
                    (goto-char (plist-get cell :end))
                    (when (< (current-column) longest)
                      (let* ((tot (- longest (current-column)))
                             (pre (/ tot 2))
                             (post (- tot pre)))
                        (cond ((eq md 'left)
                               (insert (make-string tot
                                                    (string-to-char " ")))
                               (message "Table content aligned left"))
                              ((eq md 'right)
                               (goto-char (plist-get cell :start))
                               (insert (make-string tot
                                                    (string-to-char " ")))
                               (message "Table content aligned right"))
                              ((eq md 'center)
                               (insert (make-string post
                                                    (string-to-char " ")))
                               (goto-char (plist-get cell :start))
                               (insert (make-string pre
                                                    (string-to-char " ")))
                               (message "Table content centered"))))))))
              (setq count (1+ count)))))))
    (run-hooks 'latex-table-wizard-after-table-modified-hook)))

(defun latex-table-wizard-align-left ()
  "Align text in table to the left.

Make every row start on a new line."
  (interactive)
  (latex-table-wizard-align 'left))

(defun latex-table-wizard-align-right ()
  "Align text in table to the right.

Make every row start on a new line."
  (interactive)
  (latex-table-wizard-align 'right))

(defun latex-table-wizard-center ()
  "Center text in cells.

Make every row start on a new line."
  (interactive)
  (latex-table-wizard-align 'center))

(defun latex-table-wizard-compress ()
  "Remove extra whitespace from cell margins.

Make every row start on a new line."
  (interactive)
  (latex-table-wizard-align 'compress))

(defun latex-table-wizard-right (&optional n nocycle)
  "Move point N cells to the right.

Leave point at the beginning of the cell.

If N is nil, move one cell to the right.

If there is no cell to the right of where point is, and NOCYCLE
is nil, move to the leftmost cell of the row below where point
is.  If NOCYCLE is non-nil, do not move and return nil in that
case."
  (interactive "p")
  (latex-table-wizard--jump 'forward nil n nil nocycle))

(defun latex-table-wizard-left (&optional n nocycle)
  "Move point N cells to the left.

Leave point at the beginning of the cell.

If N is nil, move one cell to the left.

If there is no cell to the left of where point is, and NOCYCLE is
nil, move to the rightmost cell of the row above where point is.
If NOCYCLE is non-nil, do not move and return nil in that case."
  (interactive "p")
  (latex-table-wizard--jump 'backward nil n nil nocycle))

(defun latex-table-wizard-down (&optional n nocycle)
  "Move point N cells down.

Leave point at the beginning of the cell.

If N is nil, move one row down.

If there is no row below where point is, and NOCYCLE is nil, move
to the top cell of the column to the right of where point is.  If
NOCYCLE is non-nil, do not move and return nil in that case."
  (interactive "p")
  (latex-table-wizard--jump 'next nil n nil nocycle))

(defun latex-table-wizard-up (&optional n nocycle)
  "Move point N cells up.

Leave point at the beginning of the cell.

If N is nil, move one row up.

If there is no row above where point is, and NOCYCLE is nil, move
to the bottom cell of the column to the left of where point is.
If NOCYCLE is non-nil, do not move and return nil in that case."
  (interactive "p")
  (latex-table-wizard--jump 'previous nil n nil nocycle))

(defun latex-table-wizard-end-of-row ()
  "Move point to the rightmost cell in current row."
  (interactive)
  (latex-table-wizard--jump 'forward t))

(defun latex-table-wizard-beginning-of-row ()
  "Move point to the leftmost cell in current row."
  (interactive)
  (latex-table-wizard--jump 'backward t))

(defun latex-table-wizard-bottom ()
  "Move point to the bottom cell in current column."
  (interactive)
  (latex-table-wizard--jump 'next t))

(defun latex-table-wizard-top ()
  "Move point to the top cell in current column."
  (interactive)
  (latex-table-wizard--jump 'previous t))

(defun latex-table-wizard-end-of-cell ()
  "Move point to the end of the current cell."
  (interactive)
  (let ((cell (latex-table-wizard--get-thing 'cell)))
    (goto-char (plist-get cell :end))))

(defun latex-table-wizard-beginning-of-cell ()
  "Move point to the beginning of the current cell."
  (interactive)
  (let ((cell (latex-table-wizard--get-thing 'cell)))
    (goto-char (plist-get cell :start))))

(defun latex-table-wizard-mark-cell ()
  "Mark current cell.

TABLE is a list of cell plists.  If it is nil, evaluate
`latex-table-wizard--parse-table' to get a value."
  (interactive)
  (when (latex-table-wizard--in-tabular-env-p)
    (latex-table-wizard--setup)
    (let* ((table (latex-table-wizard--parse-table))
           (cell (latex-table-wizard--get-thing 'cell table)))
      (push-mark (plist-get cell :start) nil t)
      (goto-char (plist-get cell :end)))))

(defun latex-table-wizard-swap-column-right ()
  "Swap current column and the one to the right."
  (interactive)
  (latex-table-wizard--swap-adjacent-line 'forward 'column))

(defun latex-table-wizard-swap-column-left ()
  "Swap current column and the one to the left."
  (interactive)
  (latex-table-wizard--swap-adjacent-line 'backward 'column))

(defun latex-table-wizard-swap-row-up ()
  "Swap current row and the one above."
  (interactive)
  (latex-table-wizard--swap-adjacent-line 'previous 'row))

(defun latex-table-wizard-swap-row-down ()
  "Swap current row and the one below."
  (interactive)
  (latex-table-wizard--swap-adjacent-line 'next 'row))

(defun latex-table-wizard-swap-cell-right ()
  "Swap content of current cell and the one to the right."
  (interactive)
  (latex-table-wizard--swap-adjacent-line 'forward 'cell))

(defun latex-table-wizard-swap-cell-left ()
  "Swap content of current cell and the one to the left."
  (interactive)
  (latex-table-wizard--swap-adjacent-line 'backward 'cell))

(defun latex-table-wizard-swap-cell-down ()
  "Swap content of current cell and the one below."
  (interactive)
  (latex-table-wizard--swap-adjacent-line 'next 'cell))

(defun latex-table-wizard-swap-cell-up ()
  "Swap content of current cell and the one above."
  (interactive)
  (latex-table-wizard--swap-adjacent-line 'previous 'cell))

(defun latex-table-wizard-insert-column ()
  "Insert empty column to the right of the one at point."
  (interactive)
  (when (latex-table-wizard--in-tabular-env-p)
    (latex-table-wizard--setup)
    (save-excursion
      (let* ((table (latex-table-wizard--parse-table))
             (current-column (latex-table-wizard--get-thing 'column table))
             (col-del (car latex-table-wizard--current-col-delims)))
        (dolist (x current-column)
          (goto-char (plist-get x :end))
          (insert " " col-del " "))))
    (run-hooks 'latex-table-wizard-after-table-modified-hook)))

(defun latex-table-wizard-delete-column ()
  "Delete current column.

Unlike `latex-table-wizard-kill-column-content', this function
modifies the structure of the table (it comments out a delimiter
for each cells too)."
  (interactive)
  (when (latex-table-wizard--in-tabular-env-p)
    (latex-table-wizard--setup)
    (save-excursion
      (let* ((table (latex-table-wizard--parse-table))
             (current-column (latex-table-wizard--get-thing 'column table))
             (ind (plist-get (car current-column) :column))
             (re (regexp-opt latex-table-wizard--current-col-delims))
             (fun (if (= ind 0)
                      (lambda (c)
                        (goto-char (plist-get c :end))
                        (re-search-forward re nil t)
                        (delete-region (plist-get c :end) (point-marker)))
                    (lambda (c)
                      (goto-char (plist-get c :start))
                      (re-search-backward re nil t)
                      (delete-region (point-marker) (plist-get c :start)))))
             kills poss)
        (dolist (x current-column)
          (funcall fun x)                 ; get rid of delimiter
          (let* ((b (plist-get x :start))
                 (e (plist-get x :end)))
            (push (buffer-substring b e) kills)
            (push (cons b e) poss)))
        (dolist (p poss)
          (delete-region (car p) (cdr p)))
        (run-hooks 'latex-table-wizard-after-table-modified-hook)
        (message "Column %s deleted" ind)))))

(defalias 'latex-table-wizard-kill-column
  'latex-table-wizard-kill-column-content)

(defun latex-table-wizard-kill-column-content ()
  "Kill content of column at point.  Leave delimiters in place."
  (interactive)
  (when (latex-table-wizard--in-tabular-env-p)
    (latex-table-wizard--setup)
    (save-excursion
      (let* ((table (latex-table-wizard--parse-table))
             (current-column (latex-table-wizard--get-thing 'column table))
             kills poss)
        (dolist (x current-column)
          (let* ((b (plist-get x :start))
                 (e (plist-get x :end)))
            (push (buffer-substring b e) kills)
            (push (cons b e) poss)))
        (dolist (p poss)
          (delete-region (car p) (cdr p)))
        (kill-new (string-join (nreverse kills) "\n"))
        (run-hooks 'latex-table-wizard-after-table-modified-hook)
        (message "Content of column %s added to kill ring"
                 (plist-get (car current-column) :column))))))

(defun latex-table-wizard-insert-row ()
  "Insert empty row below the one at point."
  (interactive)
  (when (latex-table-wizard--in-tabular-env-p)
    (latex-table-wizard--setup)
    (save-excursion
      (let* ((table (latex-table-wizard--parse-table))
             (end-table (make-marker))
             (current-row (latex-table-wizard--sort table t 'forward))
             (row-del (car latex-table-wizard--current-row-delims))
             (col-del (car latex-table-wizard--current-col-delims)))
        (set-marker end-table (cdr (latex-table-wizard--get-env-ends table)))
        (goto-char (plist-get (car (last current-row)) :end))
        (if (looking-at
             (concat "[[:space:]]*"
                     (regexp-opt latex-table-wizard--current-row-delims)))
            (progn (goto-char (match-end 0))
                   (latex-table-wizard--skip-stuff end-table))
          (insert row-del "\n"))
        (let ((how-many (length current-row)))
          (dotimes (i (1- how-many))
            (ignore i)
            (insert " " col-del))
          (insert " " row-del "\n")))))
  (run-hooks 'latex-table-wizard-after-table-modified-hook))

(defalias 'latex-table-wizard-kill-row
  'latex-table-wizard-kill-row-content)

(defun latex-table-wizard-kill-row-content ()
  "Kill content of row at point.  Leave delimiters in place."
  (interactive)
  (when (latex-table-wizard--in-tabular-env-p)
    (latex-table-wizard--setup)
    (save-excursion
      (let* ((table (latex-table-wizard--parse-table))
             (current-row (latex-table-wizard--get-thing 'row table))
             kills poss)
        (dolist (x current-row)
          (let ((b (plist-get x :start))
                (e (plist-get x :end)))
            (push (buffer-substring b e) kills)
            (push (cons b e) poss)))
        (dolist (p poss)
          (let ((repl (make-string (- (cdr p) (car p)) 32)))
            (delete-region (car p) (cdr p))
            (goto-char (car p))
            (insert repl)))
        (run-hooks 'latex-table-wizard-after-table-modified-hook)
        (kill-new (string-join (nreverse kills) " "))))))

(defun latex-table-wizard-delete-row ()
  "Kill row at point."
  (interactive)
  (when (latex-table-wizard--in-tabular-env-p)
    (latex-table-wizard--setup)
    (save-excursion
      (let* ((table (latex-table-wizard--parse-table))
             (b-e (latex-table-wizard--get-env-ends
                   (latex-table-wizard--get-thing 'row table)))
             (end (save-excursion
                    (goto-char (cdr b-e))
                    (when (looking-at
                           (regexp-opt latex-table-wizard--current-row-delims))
                      (goto-char (match-end 0)))
                    (point))))
        (kill-region (car b-e) end)
        (run-hooks 'latex-table-wizard-after-table-modified-hook)))))

(defun latex-table-wizard--echo-selection ()
  "Print status of selection in the echo area."
  (let ((sel (latex-table-wizard--sort latex-table-wizard--selection
                                       nil 'forward))
        (message-log-max 0))
    (if (not sel)
        (message "Nothing is selected")
      (let ((str (mapcar (lambda (x) (format "(%d,%d)"
                                             (plist-get x :column)
                                             (plist-get x :row)))
                         sel)))
        (message "Current selection: %s"
                 (string-join str ", "))))))

(defun latex-table-wizard-select-row (&optional no-message)
  "Add row at point to selection for swapping.

If NO-MESSAGE is non-nil, do not print anything in the echo area."
  (interactive)
  (when (latex-table-wizard--in-tabular-env-p)
    (latex-table-wizard--setup)
    (latex-table-wizard--select-thing 'row no-message)
    (latex-table-wizard--echo-selection)))

(defun latex-table-wizard-select-column (&optional no-message)
  "Add column at point to selection for swapping.

If NO-MESSAGE is non-nil, do not print anything in the echo area."
  (interactive)
  (when (latex-table-wizard--in-tabular-env-p)
    (latex-table-wizard--setup)
    (latex-table-wizard--select-thing 'column no-message)
    (latex-table-wizard--echo-selection)))

(defun latex-table-wizard--deselect-cell ()
  "Remove cell at point from selection for swapping."
  (interactive)
  (when (latex-table-wizard--in-tabular-env-p)
    (latex-table-wizard--setup)
    (let* ((table (latex-table-wizard--parse-table))
           (curr-cell (latex-table-wizard--get-thing 'cell table)))
      (setq latex-table-wizard--selection
            (remove curr-cell latex-table-wizard--selection))
      (latex-table-wizard--remove-overlays nil
                                           (plist-get curr-cell :start)
                                           (plist-get curr-cell :end))
      (setq latex-table-wizard--selection
            (remove curr-cell latex-table-wizard--selection)))))

(defun latex-table-wizard-select-deselect-cell (&optional no-message select)
  "Add or remove cell at point to selection for swapping.

If NO-MESSAGE is non-nil, do not print anything in the echo area.

If SELECT is non-nil, add the cell."
  (interactive)
  (when (latex-table-wizard--in-tabular-env-p)
    (latex-table-wizard--setup)
    (let* ((table (latex-table-wizard--parse-table))
           (curr (latex-table-wizard--get-thing 'cell table)))
      (if (and (member curr latex-table-wizard--selection) (not select))
          (latex-table-wizard--deselect-cell)
        (latex-table-wizard--select-thing 'cell no-message))
      (latex-table-wizard--echo-selection))))

(defun latex-table-wizard-deselect-all ()
  "Remove all selected cells from selection."
  (interactive)
  (when (latex-table-wizard--in-tabular-env-p)
    (latex-table-wizard--setup)
    (latex-table-wizard--remove-overlays)
    (setq latex-table-wizard--selection nil)
    (latex-table-wizard--echo-selection)))

(defun latex-table-wizard--comment-thing (start end)
  "Comment out text between markers START and END."
  (save-excursion
    (unless (markerp end)
      (goto-char end)
      (setq end (point-marker)))
    (goto-char start)
    (unless (TeX-in-comment)
      (comment-region start end))))

(defun latex-table-wizard-comment-out-content ()
  "Comment out the content of the selected cells.

If this command is called while no cell is selected, it defaults
to current cell (the cell point is in)."
  (interactive)
  (when (latex-table-wizard--in-tabular-env-p)
    (latex-table-wizard--setup)
    (let* ((table (latex-table-wizard--parse-table))
           (cells (or latex-table-wizard--selection
                      (list (latex-table-wizard--get-thing 'cell table)))))
      (dolist (c cells)
        (latex-table-wizard--comment-thing (plist-get c :start)
                                           (plist-get c :end)))
      (run-hooks 'latex-table-wizard-after-table-modified-hook)
      (message "Content of %s cells commmented out" (length cells)))))

(defun latex-table-wizard-comment-out ()
  "Comment out the the selected cells.

If this command is called while no cell is selected, it defaults
to current cell (the cell point is in).

Unlike `latex-table-wizard-comment-out-content', this function
modifies the structure of the table (it comments out a delimiter
for each cells too)."
  (interactive)
  (when (latex-table-wizard--in-tabular-env-p)
    (latex-table-wizard--setup)
    (let* ((cells (or latex-table-wizard--selection
                      (list (latex-table-wizard--get-thing 'cell))))
           (re (regexp-opt latex-table-wizard--current-col-delims))
           (fun (lambda (c ind)
                  (if (= ind 0)
                      (progn
                        (goto-char (plist-get c :end))
                        (re-search-forward re nil t)
                        (point-marker))
                    (goto-char (plist-get c :start))
                    (re-search-backward re nil t)
                    (point-marker)))))
      (dolist (c cells)
        (let* ((ind (plist-get c :column))
               (start (if (= ind 0)
                          (plist-get c :start)
                        (funcall fun c ind)))
               (end (if (= ind 0)
                        (funcall fun c ind)
                      (plist-get c :end))))
          (latex-table-wizard--comment-thing start end)))
      (run-hooks 'latex-table-wizard-after-table-modified-hook)
      (message "%s cells commented out" (length cells)))))

(defun latex-table-wizard-swap ()
  "Swap selection and thing at point.

Selection is the current value of
`latex-table-wizard--selection'.  Depending on whether it is a
cell, a column or a row, swap that with the cell, column or row
at point.  If it is none of those object, return nil."
  (interactive)
  (when (latex-table-wizard--in-tabular-env-p)
    (latex-table-wizard--setup)
    (let ((message-log-max 0))
      (unless latex-table-wizard--selection
        (message "Select thing to swap first"))
      (let* ((table (latex-table-wizard--parse-table))
             ;; we need to remove the current cell from the selection,
             ;; if it's there, because the general case is that swapping
             ;; is between the selected cells and the "thing" at point
             ;; (what this thing is depends on what the selection is).
             ;; If current cell is selected there will be an attempt of
             ;; swapping the current cell with itself and this would
             ;; cause a bug when you are just swapping two cells.
             (other (remove (latex-table-wizard--get-thing 'cell table)
                            latex-table-wizard--selection))
             (type (latex-table-wizard--type-of-selection other))
             (current (latex-table-wizard--get-thing type table)))
        (cond ((not type)
               (latex-table-wizard--cleanup)
               (setq latex-table-wizard--selection nil)
               (message "Invalid selection"))
              ((eq type 'cell)
               (latex-table-wizard--swap-cells (car other) current))
              (t
               (latex-table-wizard--swap-line type other current)))
        (latex-table-wizard--remove-overlays table)
        (latex-table-wizard--hl-cells other)
        (setq latex-table-wizard--selection nil)
        (run-hooks 'latex-table-wizard-after-table-modified-hook)))))

(defun latex-table-wizard--fit-string (str len)
  "If lenght of string STR is <= than number LEN, trim STR.

That means, apply `string-trim' to STR.  Otherwise, pad STR left
and right with equal amount of whitespace for it to reach length
LEN."
  (let ((nstr (string-trim str)))
    (if (> (length nstr) len)
      nstr
      (let* ((diff (- len (length nstr)))

             (right (make-string (/ diff 2) ?\s))
             (left (make-string (- diff (/ diff 2)) ?\s)))
        (concat left nstr right)))))

(defun latex-table-wizard-edit-cell ()
  "Edit cell at point."
  (interactive)
  (let* ((cell (latex-table-wizard--get-thing 'cell))
         (current-text (buffer-substring (plist-get cell :start)
                                         (plist-get cell :end)))
         (len (length current-text))
         (new-text (read-string "" (string-trim current-text) nil nil t)))
    (delete-region (plist-get cell :start) (plist-get cell :end))
    (goto-char (plist-get cell :start))
    (insert " " (latex-table-wizard--fit-string new-text len) " ")))

(defvar-local latex-table-wizard--copied-cell-content nil
  "The return value of `latex-table-wizard-copy-cell-content'.

It replaces the content of current cell upon calling
`latex-table-wizard-yank-cell-content'.")

(defun latex-table-wizard--get-cell-content (&optional kill)
  "Get content of cell at point.

Add it to the `kill-ring' and as the value of
`latex-table-wizard--copied-cell-content'.

If KILL is non-nil, also remove that content from the cell. "
  (let* ((cell (latex-table-wizard--get-thing 'cell))
         (cont  (buffer-substring (plist-get cell :start)
                                  (plist-get cell :end))))
    (when kill
      (delete-region (plist-get cell :start) (plist-get cell :end))
      (insert " "))
    (kill-new (string-trim cont))
    (setq latex-table-wizard--copied-cell-content cont)
    (message "Content of cell (%s,%s) %s"
             (plist-get cell :column) (plist-get cell :row)
             (if kill "killed" "copied"))))

(defun latex-table-wizard-copy-cell-content ()
  "Add content of current cell to the `kill-ring'.

Also set the value of `latex-table-wizard--copied-cell-content'
to it."
  (interactive)
  (funcall #'latex-table-wizard--get-cell-content))

(defun latex-table-wizard-yank-cell-content ()
  "Replace content of current cell with a previously copied one.

That is whatever the current value of
`latex-table-wizard--copied-cell-content'."
  (interactive)
  (if (not latex-table-wizard--copied-cell-content)
      (user-error "No cell content copied")
    (let* ((cell (latex-table-wizard--get-thing 'cell))
           (len (length (buffer-substring (plist-get cell :start)
                                          (plist-get cell :end)))))
      (delete-region (plist-get cell :start) (plist-get cell :end))
      (goto-char (plist-get cell :start))
      (insert (latex-table-wizard--fit-string
               latex-table-wizard--copied-cell-content
               len)))))

(defun latex-table-wizard-kill-cell-content ()
  "Kill content of current cell and add it to the `kill-ring'.

Also set the value of `latex-table-wizard--copied-cell-content'
to it."
  (interactive)
  (funcall #'latex-table-wizard--get-cell-content t))



;;; Transient

(defconst latex-table-wizard-default-transient-keys
  '((latex-table-wizard-copy-cell-content    "w" "copy cell content")
    (latex-table-wizard-yank-cell-content    "y" "yank cell content")
    (latex-table-wizard-edit-cell            "." "edit cell")
    (toggle-truncate-lines                   "t" "toggle truncate-lines")
    (latex-table-wizard-kill-cell-content    "k k" "kill cell content")
    (latex-table-wizard-kill-row-content     "k r" "kill row content")
    (latex-table-wizard-kill-column-content  "k c" "kill column content")
    (latex-table-wizard-delete-row           "D r" "delete row")
    (latex-table-wizard-delete-column        "D c" "delete column")
    (latex-table-wizard-comment-out          "; ;" "comment out")
    (latex-table-wizard-comment-out-content  "; c" "comment out content")
    (latex-table-wizard-insert-row           "i r" "insert row below")
    (latex-table-wizard-insert-column        "i c" "insert column right")
    (latex-table-wizard-swap                 "s" "swap selection")
    (latex-table-wizard-deselect-all         "d" "deselect all")
    (latex-table-wizard-select-row           "r" "select row")
    (latex-table-wizard-select-column        "c" "select column")
    (latex-table-wizard-select-deselect-cell "SPC" "select/deselect cell")
    (transient-quit-all                      "C-g" "done")
    (universal-argument                      "u" "universal argument")
    (undo                                    "/" "undo")
    (latex-table-wizard-align                "TAB" "cycle alignment")
    (latex-table-wizard-swap-row-down        "M-n" "swap row down")
    (latex-table-wizard-swap-row-up          "M-p" "swap row up")
    (latex-table-wizard-swap-column-left     "M-b" "swap column left")
    (latex-table-wizard-swap-column-right    "M-f" "swap column right")
    (latex-table-wizard-swap-cell-down       "C-n" "swap cell down")
    (latex-table-wizard-swap-cell-up         "C-p" "swap cell up")
    (latex-table-wizard-swap-cell-left       "C-b" "swap cell left")
    (latex-table-wizard-swap-cell-right      "C-f" "swap cell right")
    (exchange-point-and-mark                 "x" "exchange point and mark")
    (latex-table-wizard-mark-cell            "m c" "mark cell")
    (latex-table-wizard-end-of-cell          "e" "end of cell")
    (latex-table-wizard-beginning-of-cell    "a" "beginning of cell")
    (latex-table-wizard-bottom               "N" "bottom")
    (latex-table-wizard-top                  "P" "top")
    (latex-table-wizard-beginning-of-row     "B" "beginning of row")
    (latex-table-wizard-end-of-row           "F" "end of row")
    (latex-table-wizard-down                 "n" "move up")
    (latex-table-wizard-up                   "p" "move down")
    (latex-table-wizard-left                 "b" "move left")
    (latex-table-wizard-right                "f" "move right"))
  "Default values for constructing `latex-table-wizard-prefix'.

Each member of this list is a list of the form

    (C K D)

where C is the name of a command, K is a key description
string (in the syntax of `kbd'), and D is a string that acts as
description of the command.

See Info node `(transient) Suffix Specifications' for more
information.")

(defconst latex-table-wizard--interactive-commands
  (seq-concatenate
   'list
   '(latex-table-wizard-align-left
     latex-table-wizard-align-right
     latex-table-wizard-center
     latex-table-wizard-compress
     latex-table-wizard
     latex-table-wizard-do)
   (mapcar #'car latex-table-wizard-default-transient-keys))
  "List of interactive commands for \\='latex-table-wizard\\='.

This consists of the commands with a default binding for the
transient prefix as specified in
`latex-table-wizard-default-transient-keys', and adds to them
those that are not there specified.

The value of this list is used in `latex-table-wizard--setup' and
`latex-table-wizard--cleanup'.")

(defcustom latex-table-wizard-transient-keys
  (mapcar (lambda (x) (cons (nth 0 x) (nth 1 x)))
          latex-table-wizard-default-transient-keys)
  "Alist mapping command symbols to key description strings.

Each time the value of this variable is changed, a new transient
prefix `latex-table-wizard-prefix' is defined, based on the
values stored here.

Each cons cell in this alist has the form (C . K), where C is the
name of a command and C is a key description string (in the
syntax of `kbd').  These, together with the description strings
in `latex-table-wizard-default-transient-keys', determine what
the transient prefix will look like.

The transient prefix will only have suffixes for the commands
that are mapped to a key in this alist.  As usual with alists,
you can just shadow default values by adding a new mapping to the
top of the list, but be sure that no key ends up bound to more
than one command, otherwise you will experience loss of
functionality of the transient interface.

A safer way would be to replace the default values you don't like
with other.

See Info node `(transient) Suffix Specifications' for more
information about how transient suffixes are defined (that is,
how the data stored in this variable and in
`latex-table-wizard-default-transient-keys' contributes to the
definition of the transient prefix)."
  :type '(alist :key-type
                (symbol :tag "Command:"
                        :options ,(mapcar #'car
                                          latex-table-wizard-default-transient-keys))
                :value-type string)
  :group 'latex-table-wizard)

(defun latex-table-wizard--make-suffix (symbol)
  "Return a transient suffix for command SYMBOL.

Retrieve the value of the description string from
`latex-table-wizard-default-transient-keys', and the value of the key
description string from `latex-table-wizard-transient-keys'.

See Info node `(transient) Suffix Specifications' for more
information."
  (let ((descr (nth 2 (assq symbol latex-table-wizard-default-transient-keys)))
        (custom-key (cdr (assq symbol latex-table-wizard-transient-keys))))
    (when custom-key
      `(,custom-key ,descr ,symbol :transient t))))

(defun latex-table-wizard--make-prefix ()
  "Redefine transient prefix `latex-table-wizard-prefix'.

This is done through the macro `transient-define-prefix' with the
suffixes provided by evaluating `latex-table-wizard--make-suffix'."
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
        ,(latex-table-wizard--make-suffix 'latex-table-wizard-delete-row)
        ]
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

(defun latex-table-wizard--selection-in-current-table-p (&optional table)
  "Return non-nil if current selection is in current table.

If non-nil TABLE is a list of cells."
  (and latex-table-wizard--selection
       (let* ((tab (or table (latex-table-wizard--parse-table)))
              (envs (latex-table-wizard--get-env-ends tab))
              (out t))
         (dolist (c latex-table-wizard--selection)
           (unless (<= (car envs) (plist-get c :start) (cdr envs))
             (setq out nil)))
         out)))

(defvar latex-table-wizard--environments
  (nconc (mapcar #'car latex-table-wizard-new-environments-alist)
         (mapcar #'car
                 (seq-filter (lambda (c) (eq (nth 1 c) 'LaTeX-indent-tabular))
                             LaTeX-indent-environment-list)))
  "LaTeX environments where `latex-table-wizard' is useful.

This list contains both the user defined environments from
`latex-table-wizard-new-environments-alist' and the environments
AuCTeX indents as tabular according to the specification in
`LaTeX-indent-environment-list'.")

(defun latex-table-wizard--in-tabular-env-p (&optional pos)
  "Non-nil if POS is inside a tabular-like environment.

That is, if POS is in an envionment among those in
`latex-table-wizard--environments'.  If POS is nil, it defaults
to the value of (point)."
  (let* ((p (or pos (point)))
         (env (save-excursion
                (goto-char p)
                (LaTeX-current-environment))))
    (member env latex-table-wizard--environments)))

(defun latex-table-wizard--setup ()
  "Prepare for an operation on the table.

These preparations are only needed before the first of a chain of
\\='latex-table-wizard\\=' commands is used, hence do nothing if
`last-command' is in `latex-table-wizard--interactive-commands'.

Preparations mean:

  - deactivate the mark
  - activate `latex-table-wizard-mode' if needed
  - move the point inside the closest cell
  - put the overlays if appropriate."
  (unless (memq last-command latex-table-wizard--interactive-commands)
    (when (region-active-p) (deactivate-mark))
    (let ((orig-point (point)))
      (if latex-table-wizard-mode
          (latex-table-wizard--make-prefix)
        (latex-table-wizard-mode 1))
      (latex-table-wizard--get-out)
      (latex-table-wizard--hide-rest)
      (let ((cell (latex-table-wizard--locate-point
                   orig-point
                   (latex-table-wizard--parse-table))))
        (unless (<= (plist-get cell :start)
                    orig-point
                    (plist-get cell :end))
          (goto-char (plist-get cell :start)))
        (latex-table-wizard--hl-cells (list cell))))
    (if (latex-table-wizard--selection-in-current-table-p)
        (latex-table-wizard--echo-selection)
      (setq latex-table-wizard--selection nil))))

(defalias 'latex-table-wizard-do 'latex-table-wizard)

;;;###autoload
(defun latex-table-wizard ()
  "Edit table-like environment with a transient interface."
  (interactive)
  (when (latex-table-wizard--in-tabular-env-p)
    (latex-table-wizard--setup)
    (call-interactively #'latex-table-wizard-prefix)))

;;; Aesthetics

(defcustom latex-table-wizard-no-highlight nil
  "Whether or not current or selected cells are highlighted."
  :type 'boolean
  :group 'latex-table-wizard)

(defcustom latex-table-wizard-no-focus nil
  "Whether or not the outside of the table is greyed out.

If this is nil, upon calling `latex-table-wizard' the face
`latex-table-wizard-background' is applied on the portions of the
buffer before and after the table-like environment.  By default,
this means they are greyed out, but you can set the value of
`latex-table-wizard-background' to whatever face you prefer.

See Info node `(emacs) Face Customization' for more information."
  :type 'boolean
  :group 'latex-table-wizard)

(defgroup latex-table-wizard-faces nil
  "Faces used by \\='latex-table-wizard\\='."
  :group 'latex-table-wizard
  :group 'faces)

(defface latex-table-wizard-background
  '((nil (:foreground "gray40")))
  "Face for hiding non-table buffer content.

See Info node `(emacs) Face Customization' for more information
about customizing faces and `list-faces-display' for a list of
all defined faces."
  :group 'latex-table-wizard)

(defface latex-table-wizard-highlight
  '((nil :inherit region))
  "Face for highlighting current or selected cells.

See Info node `(emacs) Face Customization' for more information
about customizing faces and `list-faces-display' for a list of
all defined faces."
  :group 'latex-table-wizard)

(defun latex-table-wizard--hide-rest ()
  "Apply face `latex-table-wizard-background' outside of table."
  (unless latex-table-wizard-no-focus
    (latex-table-wizard--parse-table)
    (let* ((tab-b latex-table-wizard--table-begin)
           (tab-e (1+ latex-table-wizard--table-end))
           (ols `(,(make-overlay (point-min) tab-b)
                  ,(make-overlay tab-e (point-max)))))
      (dolist (x ols)
        (overlay-put x 'tabl-outside-ol t)
        (overlay-put x 'face 'latex-table-wizard-background)))))

(defun latex-table-wizard--cleanup (&optional if-not-in-chain)
  "Remove all overlays created by \\='latex-table-wizard\\='.

Only remove them in current buffer.

Remove unconditionally if IF-NOT-IN-CHAIN is nil, otherwise only
remove if `last-command' but not `this-command' is in
`latex-table-wizard--interactive-commands'."
  (let* ((this-comm-wiz (memq this-command
                              latex-table-wizard--interactive-commands))
         (exited (and (memq last-command
                           latex-table-wizard--interactive-commands)
                      (not this-comm-wiz))))
    ;; now we want to remove stuff either if this function was called
    ;; "unconditionally" of if it seems like we exited a chain of
    ;; latex-table-wizard operations
    (when (or (not if-not-in-chain) exited (not this-comm-wiz))
      (remove-overlays (point-min) (point-max) 'tabl-inside-ol t)
      (remove-overlays (point-min) (point-max) 'tabl-outside-ol t))))

(defsubst latex-table-wizard--clear-parse ()
  "Set value of `latex-table-wizard--parse' to nil.

This function is meant to be added to
`latex-table-wizard-after-table-modified-hook'."
  (setq latex-table-wizard--parse nil
        latex-table-wizard--table-begin nil
        latex-table-wizard--table-end nil))

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
                  #'latex-table-wizard--clear-parse nil)
        (add-hook 'before-save-hook #'latex-table-wizard--cleanup nil t)
        (add-hook 'transient-exit-hook #'latex-table-wizard--cleanup nil t)
        (add-hook 'pre-command-hook
                  (apply-partially #'latex-table-wizard--cleanup t) t))
    (remove-hook 'latex-table-wizard-after-table-modified-hook
                  #'latex-table-wizard--clear-parse t)
    (remove-hook 'before-save-hook #'latex-table-wizard--cleanup t)
    (remove-hook 'transient-exit-hook #'latex-table-wizard--cleanup t)
    (remove-hook 'pre-command-hook #'latex-table-wizard--cleanup t)))

;;;###autoload
(defun latex-table-wizard-customize ()
  "Access customization interface for \\='latex-table-wizard\\='."
  (interactive)
  (customize-browse 'latex-table-wizard))

(provide 'latex-table-wizard)
;;; latex-table-wizard.el ends here
