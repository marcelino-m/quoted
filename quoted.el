;;; quoted.el --- Manage quoted block of text

;; Author: Marcelo Muñoz Araya. <ma.munoz.araya@gmail.com>
;; URL: https://github.com/marcelino-m/quoted
;; Keywords: convenience
;; Version: 0.1.0
;; Package-Requires: ((s "1.10.0"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 's)

(defun q/quote-current-line (col-1 col-2 &optional padding)
  "Quote current line, quote start en column COL-1 and end in column COL-2"
  (let ((padding (or padding 0)))
    (save-excursion
      (move-to-column col-2 t)
      (insert (concat (s-repeat padding " ") "\"")) )
      (move-to-column col-1 t)
      (insert (concat "\"" (s-repeat padding " ")))
    ))



(defun q/quote-start-position ()
  "Return the start position of the string at point."
  (nth 8 (syntax-ppss)))

(defun q/quote-end-position ()
  "Return the end position of the string at point."
  (let ((beg (q/quote-start-position)))
    (save-excursion
      (goto-char beg)
      (forward-sexp 1)
      (skip-syntax-backward "^\"")
      (point))))


(defun q/unquote-current-line ()
  "Unquote line at pos  if line is quoted"
  (save-excursion
    (end-of-line)
    (let ((eol (point))
          endq
          startq)
      (beginning-of-line)
      (unless (= eol (skip-syntax-forward "^\"" eol))
        (forward-char)
        (when (q/line-quoted-at-point-p)
          (setq endq (q/quote-end-position))
          (goto-char endq)
          (delete-char -1)
          (setq startq (q/quote-start-position))
          (goto-char startq)
          (delete-char 1))))))

(defun q/quote-region (rbeg rend &optional padding)
  "Quote region"
  (interactive "r\nP")
  (let ((col-left  (q/mincol-no-blanc-in-region rbeg rend))
        (col-right (q/maxcol-no-blanc-in-region rbeg rend))
        (last-line (line-number-at-pos rend))
        (padd      nil))
    (when (and col-left col-right)
      (save-excursion
        (and
         padding
         (not (consp padding))
         (setq padd (prefix-numeric-value padding)))

        (goto-char rend)

        (when (not (q/empty-before-point-in-line-p rend))
          (if (> col-right (current-column))
              (q/quote-current-line col-left (current-column) padd)
            (q/quote-current-line col-left col-right padd)))

        (goto-char rbeg)

        (when (not (q/empty-after-point-in-line-p rbeg))
          (if (< col-left (current-column))
              (q/quote-current-line (current-column) col-right  padd)
            (q/quote-current-line col-left col-right  padd)))

        (while (and (forward-line 1)
                    (< (line-number-at-pos) last-line))
          (if (not (q/current-line-empty-p))
              (q/quote-current-line col-left col-right  padd)))))))


(defun q/mincol-no-blanc-in-region (rbeg rend)
  "Get min column of no blank chars in region, empty lines in
region not count, if region contain only blank chars return nil"
  (let ((max-line         (line-number-at-pos rend))
        (min-col          (point-max)))
    (save-excursion
      (goto-char rbeg)
      (while
          (progn
            (back-to-indentation)
            (if (and (> min-col (current-column))
                     (not (q/current-line-empty-p)))
                (setq min-col (current-column)))
            (and (zerop (forward-line))
                 (<= (line-number-at-pos) max-line)))))
    (if (= min-col (point-max))
        nil
      min-col)))


(defun q/maxcol-no-blanc-in-region (rbeg rend)
  "Get min column of no blank chars in region, empty lines in
region not count, if region contain only blank chars return nil"
  (let ((max-line         (line-number-at-pos rend))
        (max-col          (point-min)))
    (save-excursion
      (goto-char rbeg)
      (while
          (progn
            (end-of-line 1)
            (skip-syntax-backward " ")
            (if (< max-col (current-column))
                (setq max-col (current-column)))
            (and (zerop (forward-line 1))
                 (<= (line-number-at-pos) max-line)))))
    (if (= max-col (point-min))
        nil
      max-col)))



(defun q/current-line-empty-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at "[[:space:]]*$")))


(defun q/empty-after-point-in-line-p (arg)
  (save-excursion
    (goto-char arg)
    (looking-at "[[:space:]]*$")))


(defun q/empty-before-point-in-line-p (arg)
  (save-excursion
    (goto-char arg)
    (looking-back "^[[:space:]]*")))


(defun q/line-quoted-p (&optional pt)
  "Check if line in position PT is quoted"
  (interactive)
  (let ((p          (or pt (point)))
        (quote-char nil))
      (save-excursion
        (goto-char p)
        (back-to-indentation)
        (setq quote-char (char-after))
        (unless (q/current-line-empty-p)
          (and
           (progn
             (or
              (char-equal ?'  quote-char)
              (char-equal ?\" quote-char)))
           (progn
             (end-of-line 1)
             (skip-syntax-backward " ")
             (message "%s" (char-equal quote-char  (char-before)))
             (char-equal quote-char  (char-before))))))))

;;; quoted.el ends here’
