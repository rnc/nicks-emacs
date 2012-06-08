;;; findbugs.el --- Major mode for pluging FindBugs into Emacs.

;; Author: Nick Cross
;; Based on pmd.el written by:
;; John Russell <drjimmy42 at yahoo.com>
;; Maintainer: Nascif A. Abousalh-Neto <nascif at acm.org>
;; Created: 11/10/2005
;; Version: $Revision: 1.1 $
;; Keywords: FindBugs major-mode

;; Copyright (C) 2002 John Russell
;; Copyright (C) 2003 Nascif A. Abousalh-Neto
;; Copyright (C) 2005 Nick Cross

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;; Installation:
;;  1) Install the core FindBugs application (not part of this download, get it
;;     from http://findbugs.sourceforge.net/);
;;  2) Make sure the directory of findbugs.el is part of your Emacs load-path;
;; 	3) Add this line to your .emacs file:
;; 	    (autoload 'findbugs-current-buffer "findbugs" "FindBugs Mode" t)
;; 	    (autoload 'findbugs-current-dir "findbugs" "FindBugs Mode" t)
;;  4) Load findbugs.el
;;  5) Configure the location of the Java executable and the FindBugs installation
;;     directory using the command findbugs-customize.
;;
;; 	Once this is done, you can call the findbugs-current-buffer and findbugs-current-dir
;;  functions using M-x <function name> or by bind them to key sequences.

;; Configuration:
;; 	Besides the variables described above, you can also compile the rulesets used by FindBugs.
;;  Type:
;; 	    M-x findbugs-customize
;; 		--or--
;; 	    M-x customize-group <RET>
;; 	    findbugs <RET>

;; Description:
;; 	This mode uses a compilation buffer to display the output of FindBugs
;; (http://findbugs.sourceforge.net/).
;; It provides two commands, findbugs-current-buffer and findbugs-current-dir, which as you
;; guessed run FindBugs on the contents of the current buffer (must be a .java file)
;; or on all .java files in the directory associated with the current buffer.
;; You can defining the FindBugs rulesets by customizing the variable findbugs-ruleset-list.

;; Change History

;; 10:40 Wed 12 Oct 2005 - 0.1
;; First version of FindBugs for Emacs


(defgroup findbugs nil "FindBugs"
  :group 'emacs)

(defcustom findbugs-java-home "/usr/local/bin/java"
  "Java binary to run FindBugs with."
  :type 'file
  :group 'findbugs )

(defcustom findbugs-home "~/findbugs"
  "Directory where FindBugs is installed."
  :type 'directory
  :group 'findbugs)

(defcustom findbugs-aux-classpath "findbugs-classpath"
  "Auxilliary classpath for findbugs (Defaults to findbugs-classpath)"
  :type 'directory
  :group 'findbugs)


(defcustom findbugs-ruleset-list "rulesets/basic.xml"
  "A list of Rulesets to apply. Rulesets are specified in XML files inside the \"rulesets\" subdirectory of the main FindBugs jar file."
  :type 'file
  :group 'findbugs)

;;-------------------------
;;Inner workings
;;-------------------------

(defconst findbugs-xemacsp (string-match "XEmacs" (emacs-version)))

(defun findbugs-help ()
  "Help for `findbugs-mode'."
  (interactive)
  (describe-function 'findbugs-mode))

;;-------------------------
;; Main functions
;;-------------------------

;;;###autoload
(defun findbugs-customize ()
  "Customization of group `findbugs' for findbugs-mode."
  (interactive)
  (customize-group "findbugs"))

;;;###autoload
(defun findbugs-current-buffer ()
  "Run FindBugs on the contents of the current buffer."
  (interactive)
  (if (string-equal (file-name-extension
                        (buffer-file-name)) "java")
         (findbugs-file-or-dir (buffer-file-name))
    (message "Current buffer does not contain a Java file.  Aborting.")))

(defun findbugs-current-dir ()
  "Run FindBugs on the contents of the current directory (recursively)."
  (interactive)
  (findbugs-file-or-dir (file-name-directory (buffer-file-name))))

(defun findbugs-classpath ()
  (let* ((path-separator (if (eq system-type 'windows-nt) ";" ":"))
         (path-slash     (if (eq system-type 'windows-nt) "\\" "/"))
         (findbugs-lib     (concat findbugs-home path-slash "lib" path-slash)))
    (concat "\'"
            (mapconcat
             (lambda (path)
               path)
             (directory-files findbugs-lib t "\\.jar$")
             path-separator)
            "\'")))


(defun my-compilation-parse-errors-filename-function (filename)
  "Post processes FindBugs results to remove path information - as FindBugs only prints a partial path (Emacs requires either full or just a filename"
  (file-name-nondirectory filename)
  )


(defun findbugs-file-or-dir (target)
  "Run FindBugs on the given target (file or dir)"

  (let
      ((findbugs-command
         (concat findbugs-java-home " -Xbootclasspath/p:" findbugs-aux-classpath " -classpath " (findbugs-classpath) " -Dfindbugs.home=" findbugs-home " edu.umd.cs.findbugs.FindBugs2 -auxclasspath " findbugs-aux-classpath  " -emacs -effort:max -medium -exclude " (concat findbugs-ruleset-list) " " (replace-regexp-in-string "/src/" "/classes/" (replace-regexp-in-string "\.java$" ".class" target)))))

    (setq compilation-parse-errors-filename-function 'my-compilation-parse-errors-filename-function)
    (add-hook 'kill-buffer-hook 'fb-kill-buffer-hook)

    ;; Force save-some-buffers to use the minibuffer
    ;; to query user about whether to save modified buffers.
    (if (and (eq system-type 'windows-nt)
             (not findbugs-xemacsp))
        (let ((temp last-nonmenu-event))
          ;; The next line makes emacs think that jde-jalopy
          ;; was invoked from the minibuffer, even when it
          ;; is actually invoked from the menu-bar.
          (setq last-nonmenu-event t)
          (save-some-buffers (not compilation-ask-about-save) nil)
          (setq last-nonmenu-event temp))
      (save-some-buffers (not compilation-ask-about-save) nil))
    (compile-internal findbugs-command "No more errors" "FindBugs")
    )
)

;;
;; Ok this is grim but I don't know a better way of doing it. Basically after we have
;; finished with the FindBugs buffer it will reset compilation-parse-errors-filename-function
;; This won't work when we use a grep and findbugs buffer at the same time.
(defun fb-kill-buffer-hook ()
  (if (string-match (buffer-name) "*findbugs*")
      (setq-default compilation-parse-errors-filename-function 'nil))
  )

(provide 'findbugs)
