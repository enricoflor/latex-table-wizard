;;; latex-table-wizard.el --- Magic editing of LaTeX tables  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Enrico Flor

;; Author: Enrico Flor <enrico@eflor.net>
;; Maintainer: Enrico Flor <enrico@eflor.net>
;; URL: https://github.com/enricoflor/latex-table-wizard
;; Version: 1.0.3
;; Keywords: convenience

;; Package-Requires: ((emacs "27.1") (auctex "12.1") (transient "0.3.7"))

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

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

;; + navigate "logically" (that is, move by cells);
;; + insert or kill rows or column;
;; + move arbitrary cells or groups of cells around;
;; + align the table in different ways (however alignment is not needed
;;   for the functionalities above).

;; Standard LaTeX2e table environments are supported out of the box, but
;; you can define additional ones.  The entry point for customization is

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

;;  \begin{mytable}
;;      ...
;;  \end{mytable}

;; For latex-table-wizard to handle this table, just add the following
;; cons cell to latex-table-wizard-new-environments-alist:

;;  '("mytable" . (:col '("\\COL") :row '("\\ROW") :lines '("myhline")))

;; Each value is a list of strings to allow for more than one macro to
;; have the same function.

;; See the Info page for a complete overview of the package.

;;; Code:

;;; Dependencies

(require 'latex)
(require 'seq)
(eval-when-compile (require 'rx))
(require 'regexp-opt)
(eval-when-compile (require 'subr-x))
(require 'transient)

(defgroup latex-table-wizard nil
  "LaTeX table wizard configuration options."
  :prefix "latex-table-wizard"
  :group 'convenience)

;;; Regular expressions and configuration options

(defconst latex-table-wizard--macro-args-re
  (rx (seq (zero-or-more                    ; obligatory arguments
            (seq "{" (*? anything) "}"))
           (zero-or-more                    ; optional arguments
            (seq "[" (*? anything) "]" ))
           (zero-or-more                    ; obligatory arguments
            (seq "{" (*? anything) "}"))))
  "Regexp matching argument part of LaTeX macros.")

(defconst latex-table-wizard--macro-re
  (concat "\\\\"
          (rx (group-n 1 (one-or-more alnum)))
          latex-table-wizard--macro-args-re)
  "Regexp matching unescaped LaTeX macros.

Capture group 1 matches the name of the macro.")

(defcustom latex-table-wizard-column-delimiters '("&")
  "List of strings that are column delimiters if unescaped."
  :type '(repeat string))

(defcustom latex-table-wizard-row-delimiters '("\\\\\\\\")
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
                                   :value-type (repeat string))))

(defun latex-table-wizard--unescaped-p (&optional position)
  "Return t if LaTeX macro starting at POSITION is not escaped.

If POSITION is nil, use the value of `point'.

A macro is escaped if it is preceded by a single \\='\\\\='."
  (let ((p (or position (point))))
    (save-excursion
      (goto-char p)
      (save-match-data
        (looking-back "\\\\*" (line-beginning-position) t)
        (let ((len (length (match-string-no-properties 0))))
          (when (eq (logand len 1) 0) t))))))

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
      (setq  latex-table-wizard--current-hline-macros
             latex-table-wizard-hline-macros))))



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

(defun latex-table-wizard--end-of-macro (&optional name)
  "If looking at unescaped macro named NAME, go to its end.

If NAME is nil, skip any LaTeX macro that point is looking at."
  (save-excursion
    (let* ((n (concat "\\(?1:" (or name (rx (one-or-more alnum))) "\\)"))
           (macro-re (concat "\\\\" n latex-table-wizard--macro-args-re)))
      ;; this trouble is to deal with problematic arguments to the
      ;; environment being macro like:
      ;;    \begin{tabular}{@{} llllllll}
      (when (and (latex-table-wizard--unescaped-p)
                 (looking-at macro-re))
        (goto-char (match-end 1))         ; goto end of name
        (while (looking-at-p "{\\|\\[") (forward-sexp))
        (point)))))

(defun latex-table-wizard--skip-stuff (limit)
  "Skip comments, blank space and hline macros.

Hline macros are LaTeX macros whose name is a string in
`latex-table-wizard--current-hline-macros'.

Stop the skipping at LIMIT (a buffer position or a marker)."
  (let ((done)
        (new-start-of-line))
    (catch 'stop
      (while (and (not done) (<= (point) limit))
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
            (goto-char (latex-table-wizard--end-of-macro
                        (concat
                         (regexp-opt
                          latex-table-wizard--current-hline-macros)
                         latex-table-wizard--macro-args-re))))
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
                                                &optional limit)
  "Return boundaries of current cell (where point is).

What is returned is a list of the form

    (B E EOR)

where B and E are markers (beginning and end of the cell), and
EOR is t iff this cell is the rightmost cell in the current row,
nil otherwise.

COL-RE and ROW-RE are regular expressions matching column and row
delimiters respectively.

LIMIT is a buffer position at which the parsing stops, and
defaults to `point-max' if nothing else is passed as the
argument."
  (let ((lim (or limit (point-max)))
        (beg (point-marker))
        (end)
        (end-of-row))
    (while (and (< (point) lim) (not end))
      (cond ((and (looking-at "%")
                  (latex-table-wizard--unescaped-p))
             ;; the first step is important to avoid being fooled by
             ;; column or row delimiters in comments!
             (forward-line))
            ((looking-at-p "[[:space:]]+")
             (skip-syntax-forward " "))
            ((looking-at (concat "\\\\begin" latex-table-wizard--macro-args-re))
             (forward-char 1)
             (LaTeX-find-matching-end))
            ((and (latex-table-wizard--unescaped-p)
                  (looking-at latex-table-wizard--macro-re))
             (goto-char (latex-table-wizard--end-of-macro
                         (match-string-no-properties 1))))
            ((and (looking-at col-re)
                  (latex-table-wizard--unescaped-p))
             ;; a column delimiter: bingo
             (setq end (point-marker))
             (goto-char (match-end 0)))
            ((and (looking-at row-re)
                  (latex-table-wizard--unescaped-p))
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
               (latex-table-wizard--skip-stuff lim)))
            (t
             ;; nothing special, just go one step forward
             (forward-char 1))))
    `(,beg ,end ,end-of-row)))

(defsubst latex-table-wizard--get-env-ends (table)
  "Given TABLE, return beginning and end of the environment.

TABLE is a list of cell plists.  The return type is a cons
cell (B . E) with B and E being markers."
  `(,(apply #'min (mapcar (lambda (x) (plist-get x :start)) table))
    .
    ,(apply #'max (mapcar (lambda (x) (plist-get x :end)) table))))

(defvar-local latex-table-wizard--parse nil
  "Data from a parsed table environment.

The value of this variable is a list of the form

    ((B . E) H P)

where B and E are buffer positions or markers, H is a hash string
and P is a list of plists (that is, of cell objects).

B and E are the beginning and end of a tabular environment, H is
the sha256 of the corresponding buffer substring and P is the
parse of the the environment.")

(defun latex-table-wizard--parse-table ()
  "Parse table(-like) environment point is in.

Return a list of plists, each of which is a cells and has the
form

    (:column C :row R :start S :end E).

Each value is an integer, S and E are markers."
  (latex-table-wizard--set-current-values)
  (let* ((cells-list '())
         (col 0)
         (row 0)
         (env-beg (save-excursion
                    (LaTeX-find-matching-begin)
                    (goto-char (latex-table-wizard--end-of-macro))
                    (point-marker)))
         (env-end (save-excursion
                    (LaTeX-find-matching-end)
                    (re-search-backward (concat
                                         "\\\\"
                                         (rx (one-or-more alnum))
                                         latex-table-wizard--macro-args-re)
                                        nil t)
                    (forward-char -1)
                    (point-marker)))
         (hash (secure-hash 'sha256
                            (buffer-substring-no-properties env-beg env-end)))
         (col-re (string-join latex-table-wizard--current-col-delims "\\|"))
         (row-re (string-join latex-table-wizard--current-row-delims "\\|")))
    (if (and (equal `(,env-beg . ,env-end) (nth 0 latex-table-wizard--parse))
             (equal hash (nth 1 latex-table-wizard--parse)))
        (nth 2 latex-table-wizard--parse)
      (save-excursion
        (goto-char env-beg)
        ;; we need to make some space between the end of of the \begin
        ;; macro and the start of the (0,0) cell
        (if (looking-at-p "[[:space:]]")
            (forward-char 1)
          (insert " "))
        (while (< (point) env-end)
          (when (looking-at-p "[[:space:]]*\\($\\|%\\)")
            ;; nothing interesting left between point and eol
            (forward-line))
          (let ((data (latex-table-wizard--get-cell-boundaries
                       col-re row-re env-end)))
            (push `( :column ,col
                     :row ,row
                     :start ,(nth 0 data)
                     :end ,(if (nth 1 data) (nth 1 data) env-end))
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
      (setq latex-table-wizard--parse
            `((,env-beg . ,env-end) ,hash ,cells-list))
      cells-list)))

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

(defun latex-table-wizard--point-on-regexp-p (regexp
                                              &optional capture-group
                                              search-beginning-pos)
  "Return non-nil if point is on a substring matched by REGEXP.

If CAPTURE-GROUP is non-nil, limit the condition to the substring
matched by the corresponding capture group.  If CAPTURE-GROUP is
nil, it defaults to 0.

What is returned is a list of the form

    (S B E)

where S is the substring matched, and B and E are the buffer
position corresponding to the beginning and the end of such
substring.

Start the search from SEARCH-BEGINNING-POS (a buffer position or
marker): if this argument is nil, start the search from the
beginning of the available portion of the buffer."
  (let ((position (point))
        (group (or capture-group 0))
        (search-b (or search-beginning-pos (point-min))))
    (save-match-data
      (save-excursion
        (goto-char search-b)
        (catch 'found
          (while (re-search-forward regexp nil t)
            (when (<= (match-beginning group) position (match-end group))
              (throw 'found (list (match-string-no-properties group)
                                  (match-beginning group)
                                  (match-end group))))))))))



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
         (now (seq-position sorted curr))
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
  (let ((ols '()))
    (dolist (x list-of-cells)
      (push (make-overlay (plist-get x :start)
                          (plist-get x :end))
            ols))
    (dolist (x ols)
      (overlay-put x 'tabl-inside-ol t)
      (overlay-put x 'face 'latex-table-wizard-highlight))))

(defvar-local latex-table-wizard--selection nil
  "Current selection, a list of cell objects.")

(defun latex-table-wizard--locate-point (pos table)
  "Return cell from TABLE in which position POS is in.

POS is a buffer position or a marker.

If POS is not in a cell in TABLE, it means it's between two
cells: return the closest one."
  (let ((candidate (car (seq-filter
                         (lambda (x) (<= (plist-get x :start)
                                         pos
                                         (plist-get x :end)))
                         table)))
        (ends (latex-table-wizard--get-env-ends table)))
    (cond (candidate
           candidate)
          ((< pos (car ends))
           (latex-table-wizard--get-cell-pos table '(:column . 0) '(:row . 0)))
          ((> pos (cdr ends))
           (car (seq-filter
                 (lambda (x) (= (plist-get x :end) (cdr ends)))
                 table)))
          (t (goto-char (apply #'max
                               (mapcar (lambda (x) (plist-get x :start))
                                       (seq-filter
                                        (lambda (x) (< (plist-get x :end) pos))
                                        table))))))))

(defun latex-table-wizard--get-thing (thing &optional table)
  "Return THING point is in.

THING can be either \\='cell\\=', \\='column\\=' or \\='row\\='.

TABLE is a list of cell plists.  If it is nil, evaluate
`latex-table-wizard--parse-table' to get a value.

If THING is \\='cell\\=', return one plist, else return a list of
plists."
  (let* ((pos (point))
         (cells-list (or table (latex-table-wizard--parse-table)))
         (curr (latex-table-wizard--locate-point pos cells-list)))
    (if (eq thing 'cell)
        curr
      (let* ((prop (if (eq thing 'row) :row :column))
             (other-prop (if (eq thing 'row) :column :row))
             (curr-value (plist-get curr prop)))
        (sort (seq-filter (lambda (x) (= curr-value (plist-get x prop)))
                          cells-list)
              (lambda (x y) (> (plist-get x other-prop)
                               (plist-get y other-prop))))))))

(defun latex-table-wizard--jump (dir &optional absolute count same-line)
  "Move point to the beginning of a cell in the table.

DIR is either \\='next\\=', \\='previous\\=', \\='forward\\=' or
\\='backward\\=' and determines the direction of motion.  This
function assumes being evaluated with point inside of a
tabular-like environment.

With ABSOLUTE being t, move to the last or first cell in the row
or column (depending on the value of DIR) point is currently in.

COUNT is a positive integer that determines how many steps in
direction DIR to take.

If SAME-LINE is non-nil, never leave current column or row."
  (unless (ignore-errors (save-excursion (LaTeX-find-matching-begin)))
    (user-error "Not in a LaTeX environment"))
  (when-let ((macro-at-point
              (latex-table-wizard--point-on-regexp-p
               latex-table-wizard--macro-re
               0 (line-beginning-position))))
    (cond ((string-prefix-p "\\begin" (nth 0 macro-at-point))
           (goto-char (nth 1 macro-at-point)))
          ((string-prefix-p "\\end" (nth 0 macro-at-point))
           (goto-char (nth 2 macro-at-point)))))
  (let* ((message-log-max 0)
         (cells (latex-table-wizard--parse-table))
         (curr (latex-table-wizard--get-thing 'cell cells))
         (target (if (not absolute)
                     (latex-table-wizard--get-other-cell
                      dir same-line count cells curr)
                   (let ((sorted (latex-table-wizard--sort cells t dir)))
                     (if (memq dir '(previous backward))
                         (car sorted)
                       (car (last sorted)))))))
    (latex-table-wizard--remove-overlays cells)
    (goto-char (plist-get target :start))
    (latex-table-wizard--hl-cells `(,target))
    (latex-table-wizard--hl-cells latex-table-wizard--selection)
    (message "Col X Row (%d,%d)"
             (plist-get target :column)
             (plist-get target :row))))



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

(defun latex-table-wizard--swap-adjacent-line (dir &optional type)
  "Swap current thing of type TYPE with the one in direction DIR.
DIR is either \\='forward\\=', \\='backward\\=', \\='next\\=' or
\\='previous\\='.
TYPE is either \\='cell\\=', \\='column\\=' or \\='row\\='."
  (latex-table-wizard--remove-overlays)
  (cond ((eq type 'cell) (latex-table-wizard-select-deselect-cell t))
        ((memq dir '(forward backward))
         (latex-table-wizard-select-column t))
        ((memq dir '(previous next))
         (latex-table-wizard-select-row t)))
  (latex-table-wizard--jump dir nil 1 t)
  (latex-table-wizard-swap)
  (let ((new-table (latex-table-wizard--parse-table)))
    (if (eq type 'cell)
        (latex-table-wizard--hl-cells
         `(,(latex-table-wizard--get-thing type new-table)))
      (latex-table-wizard--hl-cells
       (latex-table-wizard--get-thing type new-table)))))

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

(defun latex-table-wizard-align ()
  "Align and format table at point.

Have every row start on its own line and vertically align column
delimiters.

Cycle through three modes of alignment for the text in the cells:
align left, center, right and no alignment (minimize space at
cell borders)."
  (interactive)
  (latex-table-wizard--remove-overlays)
  (save-excursion
    (let ((message-log-max 0)
          (mode (car latex-table-wizard--align-status))
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
      (whitespace-cleanup-region (car (car latex-table-wizard--parse))
                                 (cdr (car latex-table-wizard--parse)))
      (dolist (x (flatten-list (mapcar (lambda (x) `(,(plist-get x :start)
                                                     ,(plist-get x :end)))
                                       (latex-table-wizard--parse-table))))
        (goto-char x)
        (just-one-space))
      (if (eq mode 'compress)
          (message "Table compressed")
        (let ((count 0))
          (while (<= count max-col)
            (let ((line (seq-filter (lambda (x) (= count (plist-get x :column)))
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
                      (cond ((eq mode 'left)
                             (insert (make-string tot
                                                  (string-to-char " ")))
                             (message "Table content aligned left"))
                            ((eq mode 'right)
                             (goto-char (plist-get cell :start))
                             (insert (make-string tot
                                                  (string-to-char " ")))
                             (message "Table content aligned right"))
                            ((eq mode 'center)
                             (insert (make-string post
                                                  (string-to-char " ")))
                             (goto-char (plist-get cell :start))
                             (insert (make-string pre
                                                  (string-to-char " ")))
                             (message "Table content centered"))))))))
            (setq count (1+ count))))))))

(defun latex-table-wizard-right (&optional n)
  "Move point N cells to the right.

Leave point at the beginning of the cell.

If N is nil, move one cell to the right.

If there is no cell to the right of where point is, move to the
leftmost cell of the row below where point is."
  (interactive "p")
  (latex-table-wizard--jump 'forward nil n))

(defun latex-table-wizard-left (&optional n)
  "Move point N cells to the left.

Leave point at the beginning of the cell.

If N is nil, move one cell to the left.

If there is no cell to the left of where point is, move to the
rightmost cell of the row above where point is."
  (interactive "p")
  (latex-table-wizard--jump 'backward nil n))

(defun latex-table-wizard-down (&optional n)
  "Move point N cells down.

Leave point at the beginning of the cell.

If N is nil, move one row down.

If there is no row below where point is, move to the top cell of
the column to the right of where point is."
  (interactive "p")
  (latex-table-wizard--jump 'next nil n))

(defun latex-table-wizard-up (&optional n)
  "Move point N cells up.

Leave point at the beginning of the cell.

If N is nil, move one row up.

If there is no row above where point is, move to the bottom cell
of the column to the left of where point is."
  (interactive "p")
  (latex-table-wizard--jump 'previous nil n))

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
  (let* ((table (latex-table-wizard--parse-table))
         (cell (latex-table-wizard--get-thing 'cell table)))
    (push-mark (plist-get cell :start) nil t)
    (goto-char (plist-get cell :end))))

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
  (save-excursion
    (let* ((table (latex-table-wizard--parse-table))
           (current-column (latex-table-wizard--get-thing 'column table))
           (col-del (car latex-table-wizard--current-col-delims)))
      (dolist (x current-column)
        (goto-char (plist-get x :end))
        (insert " " col-del " ")))))

(defun latex-table-wizard-kill-column ()
  "Kill content of column at point."
  (interactive)
  (save-excursion
    (let* ((table (latex-table-wizard--parse-table))
           (current-column (latex-table-wizard--get-thing 'column table))
           (kills '()))
      (dolist (x current-column)
        (let* ((b (plist-get x :start))
               (next (latex-table-wizard--get-other-cell 'forward t 1 table x))
               (e (plist-get next :start)))
          (push (buffer-substring b e) kills)
          (delete-region b e)))
      (kill-new (string-join (nreverse kills) "\n")))))

(defun latex-table-wizard-insert-row ()
  "Insert empty row below the one at point."
  (interactive)
  (save-excursion
    (let* ((table (latex-table-wizard--parse-table))
           (end-table (cdr (latex-table-wizard--get-env-ends table)))
           (current-row (latex-table-wizard--sort table t 'forward))
           (row-del (car latex-table-wizard--current-row-delims))
           (col-del (car latex-table-wizard--current-col-delims)))
      (goto-char (plist-get (car (last current-row)) :end))
      (if (looking-at (concat "[[:space:]]*" row-del))
          (progn (goto-char (match-end 0))
                 (latex-table-wizard--skip-stuff end-table))
        (insert row-del "\n"))
      (let ((how-many (length current-row)))
        (dotimes (i (1- how-many))
          (ignore i)
          (insert " " col-del))
        (insert " " row-del "\n")))))

(defun latex-table-wizard-kill-row ()
  "Kill row at point."
  (interactive)
  (save-excursion
    (let* ((table (latex-table-wizard--parse-table))
           (b-e (latex-table-wizard--get-env-ends
                 (latex-table-wizard--get-thing 'row table))))
      (kill-region (car b-e) (cdr b-e)))))

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
  (latex-table-wizard--select-thing 'row no-message)
  (latex-table-wizard--echo-selection))

(defun latex-table-wizard-select-column (&optional no-message)
  "Add column at point to selection for swapping.

If NO-MESSAGE is non-nil, do not print anything in the echo area."
  (interactive)
  (latex-table-wizard--select-thing 'column no-message)
  (latex-table-wizard--echo-selection))

(defun latex-table-wizard--deselect-cell ()
  "Remove cell at point from selection for swapping."
  (interactive)
  (let* ((table (latex-table-wizard--parse-table))
         (curr-cell (latex-table-wizard--get-thing 'cell table)))
    (setq latex-table-wizard--selection
          (remove curr-cell latex-table-wizard--selection))
    (latex-table-wizard--remove-overlays nil
                                         (plist-get curr-cell :start)
                                         (plist-get curr-cell :end))
    (setq latex-table-wizard--selection
          (remove curr-cell latex-table-wizard--selection))))

(defun latex-table-wizard-select-deselect-cell (&optional no-message)
  "Add or remove cell at point to selection for swapping.

If NO-MESSAGE is non-nil, do not print anything in the echo area."
  (interactive)
  (let* ((table (latex-table-wizard--parse-table))
         (curr (latex-table-wizard--get-thing 'cell table)))
    (if (member curr latex-table-wizard--selection)
        (latex-table-wizard--deselect-cell)
      (latex-table-wizard--select-thing 'cell no-message)))
  (latex-table-wizard--echo-selection))

(defun latex-table-wizard-deselect-all ()
  "Remove all selected cells from selection."
  (interactive)
  (latex-table-wizard--remove-overlays)
  (setq latex-table-wizard--selection nil)
  (latex-table-wizard--echo-selection))

(defun latex-table-wizard-swap ()
  "Swap selection and thing at point.

Selection is the current value of
`latex-table-wizard--selection'.  Depending on whether it is a
cell, a column or a row, swap that with the cell, column or row
at point.  If it is none of those object, return nil."
  (interactive)
  (unless latex-table-wizard--selection
    (user-error "Select thing to swap first"))
  (let* ((table (latex-table-wizard--parse-table))
         (other latex-table-wizard--selection)
         (type (latex-table-wizard--type-of-selection other))
         (current (latex-table-wizard--get-thing type table)))
    (cond ((not type)
           (latex-table-wizard--cleanup)
           (setq latex-table-wizard--selection nil))
          ((eq type 'cell)
           (latex-table-wizard--swap-cells (car other) current))
          (t
           (latex-table-wizard--swap-line type other current)))
    (latex-table-wizard--remove-overlays table)
    (latex-table-wizard--hl-cells other)
    (setq latex-table-wizard--selection nil)))



;;; Transient

(defconst latex-table-wizard-default-keys
  '((toggle-truncate-lines                   "t" "toggle truncate-lines")
    (latex-table-wizard-kill-row             "k r" "kill current row")
    (latex-table-wizard-kill-column          "k c" "kill current column")
    (latex-table-wizard-insert-row           "i r" "insert row below")
    (latex-table-wizard-insert-column        "i c" "insert column right")
    (latex-table-wizard-swap                 "s" "swap selection")
    (latex-table-wizard-deselect-all         "d" "deselect all")
    (latex-table-wizard-select-row           "r" "select row")
    (latex-table-wizard-select-column        "c" "select column")
    (latex-table-wizard-select-deselect-cell "SPC" "select/deselect cell")
    (transient-quit-all                      "RET" "done")
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

(defcustom latex-table-wizard-transient-keys
  (mapcar (lambda (x) (cons (nth 0 x) (nth 1 x)))
          latex-table-wizard-default-keys)
  "Alist mapping command symbols to key description strings.

Each time the value of this variable is changed, a new transient
prefix `latex-table-wizard-prefix' is defined, based on the
values stored here.

Each cons cell in this alist has the form (C . K), where C is the
name of a command and C is a key description string (in the
syntax of `kbd').  These, together with the description strings
in `latex-table-wizard-default-keys', determine what the
transient prefix will look like.

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
`latex-table-wizard-default-keys' contributes to the definition
of the transient prefix)."
  :type '(alist :key-type
                (symbol :tag "Command:"
                        :options ,(mapcar #'car latex-table-wizard-default-keys))
                :value-type string)
  :group 'latex-table-wizard)

(defun latex-table-wizard--make-suffix (symbol)
  "Return a transient suffix for command SYMBOL.

Retrieve the value of the description string from
`latex-table-wizard-default-keys', and the value of the key
description string from `latex-table-wizard-transient-keys'.

See Info node `(transient) Suffix Specifications' for more
information."
  (let ((descr (nth 2 (assq symbol latex-table-wizard-default-keys)))
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
        ,(latex-table-wizard--make-suffix 'latex-table-wizard-end-of-cell)]
       [,(latex-table-wizard--make-suffix 'latex-table-wizard-swap-cell-right)
        ,(latex-table-wizard--make-suffix 'latex-table-wizard-swap-cell-left)
        ,(latex-table-wizard--make-suffix 'latex-table-wizard-swap-cell-up)
        ,(latex-table-wizard--make-suffix 'latex-table-wizard-swap-cell-down)
        ,(latex-table-wizard--make-suffix 'latex-table-wizard-swap-column-right)
        ,(latex-table-wizard--make-suffix 'latex-table-wizard-swap-column-left)
        ,(latex-table-wizard--make-suffix 'latex-table-wizard-swap-row-up)
        ,(latex-table-wizard--make-suffix 'latex-table-wizard-swap-row-down)
        ""
        ,(latex-table-wizard--make-suffix 'toggle-truncate-lines)
        ,(latex-table-wizard--make-suffix 'latex-table-wizard-align)
        ,(latex-table-wizard--make-suffix 'undo)
        ,(latex-table-wizard--make-suffix 'transient-quit-all)]
       [,(latex-table-wizard--make-suffix 'latex-table-wizard-select-deselect-cell)
        ,(latex-table-wizard--make-suffix 'latex-table-wizard-select-column)
        ,(latex-table-wizard--make-suffix 'latex-table-wizard-select-row)
        ,(latex-table-wizard--make-suffix 'latex-table-wizard-deselect-all)
        ""
        ,(latex-table-wizard--make-suffix 'latex-table-wizard-swap)
        ""
        ,(latex-table-wizard--make-suffix 'latex-table-wizard-insert-column)
        ,(latex-table-wizard--make-suffix 'latex-table-wizard-insert-row)
        ,(latex-table-wizard--make-suffix 'latex-table-wizard-kill-column)
        ,(latex-table-wizard--make-suffix 'latex-table-wizard-kill-row)
        ,(latex-table-wizard--make-suffix 'latex-table-wizard-mark-cell)
        ,(latex-table-wizard--make-suffix 'exchange-point-and-mark)]])))

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
  (latex-table-wizard--parse-table)
  (let* ((tab-b (car (car latex-table-wizard--parse)))
         (tab-e (cdr (car latex-table-wizard--parse)))
         (ols `(,(make-overlay (point-min) tab-b)
                ,(make-overlay tab-e (point-max)))))
    (dolist (x ols)
      (overlay-put x 'tabl-outside-ol t)
      (overlay-put x 'face 'latex-table-wizard-background))))

(defun latex-table-wizard--cleanup ()
  "Remove all overlays created by \\='latex-table-wizard\\='.

Only remove them in current buffer."
  (setq latex-table-wizard--selection nil)
  (when-let ((lims (car latex-table-wizard--parse)))
    (remove-overlays (point-min) (point-max) 'tabl-inside-ol t)
    (remove-overlays (point-min) (point-max) 'tabl-outside-ol t)))

(defun latex-table-wizard--get-out ()
  "If point is on column or row delimiter, move to its beginning."
  (latex-table-wizard--set-current-values)
  (when-let ((macro (latex-table-wizard--point-on-regexp-p
                     (string-join
                      `(,(regexp-opt
                          (append latex-table-wizard--current-row-delims
                                  latex-table-wizard--current-col-delims))
                        ,latex-table-wizard--macro-re)
                      "\\|")
                     0 (line-beginning-position))))
    (thread-last macro
                 (nth 1)
                 (1-)
                 (goto-char))))

(define-minor-mode latex-table-wizard-mode
  "Minor mode for editing LaTeX table-like environments."
  :init-value nil
  :global nil
  :lighter " ltw"
  :group 'convenience
  (if latex-table-wizard-mode
      (progn
        (latex-table-wizard--make-prefix)
        (add-hook 'before-save-hook #'latex-table-wizard--cleanup nil t)
        (add-hook 'transient-exit-hook #'latex-table-wizard--cleanup nil t))
    (remove-hook 'before-save-hook #'latex-table-wizard--cleanup t)
    (remove-hook 'transient-exit-hook #'latex-table-wizard--cleanup t)))

(defalias 'latex-table-wizard-do 'latex-table-wizard)

;;;###autoload
(defun latex-table-wizard ()
  "Edit table-like environment with a transient interface."
  (interactive)
  (when (region-active-p) (deactivate-mark))
  (if latex-table-wizard-mode
      (latex-table-wizard--make-prefix)
    (latex-table-wizard-mode 1))
  (latex-table-wizard--get-out)
  (latex-table-wizard--hide-rest)
  (call-interactively #'latex-table-wizard-prefix))

;;;###autoload
(defun latex-table-wizard-customize ()
  "Access customization interface for \\='latex-table-wizard\\='."
  (interactive)
  (customize-browse 'latex-table-wizard))

(provide 'latex-table-wizard)

;;; _
;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; latex-table-wizard.el ends here
