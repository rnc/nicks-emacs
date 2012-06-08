;;; glyph-highlight.el --- Highlight special characters, such as control chars

;; Copyright (C) 2001 Samuel Padgett

;; Author:	Samuel Padgett <samuel.padgett@gte.net>
;; Created:	March 5, 2001
;; Version:	0.4
;; Time-stamp:	<2001/03/27 01:01:48 est -- Samuel Padgett>
;; Keywords:	faces

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the GNU
;; General Public License for more details.

;;; Commentary:

;; This packages highlights various special characters with faces.
;; The intent is to make it easy to distinguish between ordinary
;; characters in a buffer and the glyphs Emacs uses when displaying
;; control characters and octal codes and the like.

;; To use, simply put glyph-highlight.el in your `load-path' and the
;; following lines in your .emacs:

;; (require 'glyph-highlight)
;; (glyph-highlight-do-it)

;; The package also defines `glyph-highlight-show-tabs', which
;; displays tab characters as `\t'.  This is similar to Vi's `:set
;; list' (although it does not change the display of newline
;; characters).	 Calling `glyph-highlight-show-tabs' again toggles off
;; this display.

;; You can customize the different faces used with M-x customize-group
;; RET glyph-highlight RET.

;; Special thanks to Eli Zaretskii who helped me understand glyphs and
;; display tables.

;;; Change Log:

;; Changes from 0.3 to 0.4

;; * Call `redraw-display' at the end of `glyph-highlight-show-tabs'.

;; Changes from 0.2 to 0.3

;; * Added `glyph-highlight-show-tabs'.
;; * Initialize the standard display table if it doesn't yet exist.
;;   (Thanks Valeriy E. Ushakov, <uwe@ptc.spbu.ru>.)
;; * Made `glyph-highlight' customization group a subgroup of `Faces'.

;; Changes from 0.1 to 0.2

;; * Changed name from special-char.el to glyph-highlight.el
;; * Defined `glyph-highlight' group

;;; TODO:

;; * Allow glyph characters used to be customized.
;; * Highlight the entire control sequence or octal code.  (For
;;   instance, highlight ^L instead of simply ^.)  Can this be done
;;   easily?
;; * Port to XEmacs.

;;; Code:

(defgroup glyph-highlight nil
  "Highlights special characters, such as control characters."
  :group 'faces)

(defface truncated-line-face
  '((t (:foreground "Tan")))
  "Face used for the glyph indicating that a line of text is truncated."
  :group 'glyph-highlight)

(defface wrapped-line-face
  '((t (:foreground "Tan")))
  "Face used for the glyph indicating that a line of text is wrapped."
  :group 'glyph-highlight)

(defface octal-code-face
  '((t (:background "Blue" :foreground "White")))
  "Face used for the glyph indicating the presence of an octal code character."
  :group 'glyph-highlight)

(defface ctrl-char-face
  '((t (:background "Coral" :foreground "White")))
  "Face used for the glyph indicating the presence of a control character."
  :group 'glyph-highlight)

(defface invisible-lines-face
  '((t (:background "LightGrey" :foreground "Black")))
  "Face used for the glyph indicating the presence of invisible lines."
  :group 'glyph-highlight)

(defface tab-char-face
  '((t (:foreground "Firebrick" :bold t)))
  "Face used to display tab characters."
  :group 'glyph-highlight)

(defvar glyph-highlight-showing-tabs-p nil)

(defun glyph-highlight-create-glyph (char face)
  "Make a glyph of character CHAR with face FACE.
For instance, (glyph-highlight-create-glyph ?^ 'ctrl-char-face) creates a
glyph of `^' with face `ctrl-char-face'."
  (if window-system
      (+ char (lsh (face-id face) 19))
    char))

;;;###autoload
(defun glyph-highlight-do-it ()
  "Frob the `standard-display-table' to highlight special characters."
  (interactive)
  (unless standard-display-table
    (setq standard-display-table (make-display-table)))
  (set-display-table-slot
   standard-display-table 'truncation
   (glyph-highlight-create-glyph ?$ 'truncated-line-face))
  (set-display-table-slot
   standard-display-table 'wrap
   (glyph-highlight-create-glyph ?\\ 'wrapped-line-face))
  (set-display-table-slot
   standard-display-table 'escape
   (glyph-highlight-create-glyph ?\\ 'octal-code-face))
  (set-display-table-slot
   standard-display-table 'control
   (glyph-highlight-create-glyph ?^ 'ctrl-char-face))
  (set-display-table-slot
   standard-display-table 'selective-display
   (make-vector 3 (glyph-highlight-create-glyph ?. 'invisible-lines-face))))

;;;###autoload
(defun glyph-highlight-show-tabs (&optional arg)
  "Toggle the display of tabs.
When enabled, tabs are shown as `\t' and colored with `tab-char-face'.
With arg, turn tab display on iff arg is positive."
  (interactive "P")
  (setq glyph-highlight-showing-tabs-p
	(not (or (and (null arg) glyph-highlight-showing-tabs-p)
		 (<= (prefix-numeric-value arg) 0))))
  (if glyph-highlight-showing-tabs-p
      (progn
	(unless standard-display-table
	  (setq standard-display-table (make-display-table)))
	(aset standard-display-table ?\t
 	      (vector (glyph-highlight-create-glyph ?\\ 'tab-char-face)
 		      (glyph-highlight-create-glyph ?t 'tab-char-face))))
    (aset standard-display-table ?\t nil))
  (redraw-display))

(provide 'glyph-highlight)

;;; glyph-highlight.el ends here
