(message (concat "START emacs_main:" (format-time-string "%H:%M:%S %3Nms")))

;;
;;
;; R.N.Cross
;;
;;
;;
;; Main File - Separate for byte compilation
;;
;;
;;


;;*************************;;
;; Emacs Built In Settings ;;
;;*************************;;

;; Emacs 21
(if (not (> emacs-major-version 22))
    (progn
      ;; As Emacs 23 doesn't work well with this moving it to older versions
      ;; Move by screen lines
      ;; Other options to use here were screenline.el by Joshua E. Buhl or
      ;; screen-lines.el by Yuji 'bmonkey' Minejima. Both of these are minor
      ;; modes and may work quite well but, then, so does next-screen-line so
      ;; am sticking with that for the time being.
      (require 'next-screen-line)

      ;; Highlight selection
      (setq-default transient-mark-mode t)

      ;; Tag special characters with faces
      (progn
        (require 'glyph-highlight)
        (eval-after-load "glyph-highlight"
          '(progn
             (glyph-highlight-show-tabs)
             (glyph-highlight-do-it)
             )
          )
        )
      )
  ;; Else Emacs 23
  (progn
    ;; For emacs 23 use whitespace-mode.

    ;; make whitespace-mode use “¶” for newline and “▷” for tab.
    ;; together with the rest of its defaults
    (setq whitespace-display-mappings
          '(
            (space-mark 32 [183] [46]) ; normal space
            (space-mark 160 [164] [95])
            (space-mark 2208 [2212] [95])
            (space-mark 2336 [2340] [95])
            (space-mark 3616 [3620] [95])
            (space-mark 3872 [3876] [95])
            (newline-mark 10 [182 10]) ; newline
            (tab-mark 9 [9655 9] [92 9]) ; tab
            ))
    (setq-default whitespace-style '(tabs trailing lines newline empty tab-mark newline-mark))
    )
  )

;; Make this emacs instance run as a server
(require 'server)
(unless (server-running-p)
    (server-start))

;; Put backups into autosaved directory
(setq-default backup-directory-alist '(("." . "~/.emacs.d/autosaved")))

;; Switch off the toolbar.
(tool-bar-mode 0)

;; Switch off the scroll bar and load sml-modeline instead
(scroll-bar-mode -1)
(require 'sml-modeline)
(sml-modeline-mode 1)
;; Note : sml-modeline face customisation in 'color' section below.

(setq-default default-indicate-buffer-boundaries 'right)

(global-set-key (kbd "M-<RET>") 'keyboard-quit)
(setq-default indicate-empty-lines 't)
(setq-default auto-image-file-mode 't)

;; Using my own printing menu so get rid of the ones off File
(define-key global-map [menu-bar files print-buffer] nil)
(define-key global-map [menu-bar files print-region] nil)
(define-key global-map [menu-bar files ps-print-buffer-faces] nil)
(define-key global-map [menu-bar files ps-print-region] nil)
(define-key global-map [menu-bar files separator-print] nil)
(define-key global-map [menu-bar files ps-print-buffer] nil)
(define-key global-map [menu-bar files ps-print-region-faces] nil)


;; Use C-h w to locate keybinding
;;
(if (symbol-value 'rnc_load_keys_reset)
    (progn
      ;; Move Quit and vc-toggle-read-only - because I keep hitting
      ;; C-xc by mistake and quitting which is bloody annoying!
      ;; toggle-read-only now takes quits place. vc-toggle takes an unused.
      (global-unset-key "\C-x\C-c")     ; Was save-buffers-kill-emacs
      (global-unset-key "\C-x\C-q")     ; Was toggle-read-only
      (global-unset-key "\C-x\C-z")     ; Was suspend-frame (iconify)
      (global-unset-key "\C-z")         ; Was suspend-frame (iconify)
      (global-set-key (kbd "C-x C-c") 'toggle-read-only) ; Was save-buffers-kill-emacs
      (global-set-key (kbd "C-x C-q") 'save-buffers-kill-emacs) ; Was vc-toggle-read-only
      ))


(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-c C-i") 'indent-region)
(global-unset-key (kbd "M-b")) ; Was backward-word
(global-unset-key (kbd "M-f")) ; Was forward-word

;; Windows key - s : revert buffer
(global-set-key (kbd "s-b") 'revert-buffer)

;; Couple of useful functions to set the machine name.
(defvar machine-name (system-name) "Machine name for this system.")
(defun machine-name ()
  "Return the name of the machine you are running on, as a string.
Same as (system-name) up to the first '.'"
  (concat machine-name))
(let
  ((i 1) (s (system-name)) (sl (length (system-name))))
  (while (< i sl)
    (if
        (string-equal "." (substring s i (1+ i)))
        (setq i (length s))  ;; bail out
        (progn ;; keep looping
          (setq machine-name (substring s 0 (setq i (1+ i))))
          )
        )
    )
)


;; after mouse selection in X11, you can paste by `yank' in emacs
(setq x-select-enable-primary t)
;; Set the default file creation to u=rw, o=r, g=r -> 420
;; Set the default file creation to u=rw, o=r, g=r, uog+x for directories
(set-default-file-modes 493)
;; Only suggest binding for one second
(setq-default suggest-key-bindings 1)
;; Set frame title format (See http://emacs-fu.blogspot.co.uk/2011/01/setting-frame-title.html)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))
;; Amount of messages in log
(setq-default message-log-max '200)
;; Wrap long lines
(setq-default truncate-partial-width-windows 'nil)
;; Display line numbers with no limit
(setq line-number-display-limit 'nil)
;; Toggle column mode
(setq-default column-number-mode t)
;; Window System doesn't change focus when I move the mouse
(setq-default focus-follows-mouse 'nil)
;; Handle case sensitive / insensitive searching
(setq-default case-fold-search 'nil)
;; Preserve case in replacements
(setq-default case-replace t)
;; Auto-compression mode
(auto-compression-mode t)
;; Imenu sort by name
(setq-default imenu-sort-function 'imenu--sort-by-name)
;; Mouse menu group buffers by mode
(setq-default mouse-buffer-menu-mode-mult 2)
;; Limit on number of Lisp variable bindings & unwind-protects
(setq-default max-specpdl-size 3000)
;; Increase size of recursion limit
(setq-default max-lisp-eval-depth 3000)
;; Ensure the below are enabled
(put 'narrow-to-region 'disabled 'nil)
(put 'downcase-region 'disabled 'nil)
(put 'upcase-region 'disabled 'nil)
;; Replace yes by y
(fset 'yes-or-no-p 'y-or-n-p)
;; Avoid visiting the same file in two buffers under different names.
(setq-default find-file-existing-other-name t)
;; Fill column for text modes (use fci-mode).
(setq-default fill-column 80)
(require 'fill-column-indicator)
(setq-default fci-handle-truncate-lines 'nil)
(setq-default fci-rule-color "dark orange")
(setq-default fci-rule-use-dashes t)
(if (and (string-equal window-system "x")
         (not (string-equal system-type "windows-nt"))
         )
    ;; When pointer is over 'clickable text' turn it into a hand
    (setq-default x-sensitive-text-pointer-shape x-pointer-hand2)
  )
;; Get rid of bell
;; Could set visible-bell variable to true but has horrible side-effects on
;; minibuffer. Therefore just use this to remove it.
(setq ring-bell-function '(lambda () nil))
;; Mouse-wheel scroll window mouse is over
(setq-default mouse-wheel-follow-mouse t)

;; Dired-recursive-deletes
(setq-default dired-recursive-deletes 'top)

(add-hook 'dired-load-hook
          (lambda () (require 'dired-sort-menu)))


(defun delete-this-buffer-and-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))


;;
;;*****************;;
;;                 ;;
;; EXTRA FUNCTIONS ;;
;;                 ;;
;;*****************;;
;;


;;*************;;
;; Screen      ;;
;;*************;;

;; Proper scrolling
;; Meta arrow is one screen line at a time.
(setq-default scroll-preserve-screen-position t)
(setq-default scroll-conservatively 10)
(defun page-up-one () "Move up one line" (interactive) (scroll-up 1))
(defun page-down-one () "Move down one line" (interactive) (scroll-down 1))
(global-set-key (kbd "M-<up>") 'page-down-one) ; Was Undefined
(global-set-key (kbd "M-<down>") 'page-up-one) ; Was Undefined

;; Active Matching Parens
;; Using mic-paren to provide additional facilities e.g. offscreen matching
(require 'mic-paren)
(paren-activate)
(setq-default blink-matching-paren 'nil)
(setq-default blink-matching-paren-distance 'nil)
(setq-default paren-ding-unmatched t)
(setq-default paren-dont-load-timer t)
(setq-default paren-highlight-offscreen t)

;; Kill completion buffer when we exit the mini-buffer
(add-hook 'minibuffer-exit-hook
          (lambda ()
            (and (get-buffer "*Completions*")
                 (progn (kill-buffer (get-buffer "*Completions*"))
                        (message "Killed 'Completion' Buffer")))
            ));; End of add hook
(add-hook 'minibuffer-exit-hook
          (lambda ()
            (and (get-buffer "*Buffer Completions*")
                 (progn (kill-buffer (get-buffer "*Buffer Completions*"))
                        (message "Killed 'Buffer Completion' Buffer")))
            ));; End of add hook


;; MSB
(require 'msb)
(setq msb-menu-cond
      (cons '((string-match "\\.properties\\(<.*>\\)*$" (buffer-name))
              600 "Properties Files (%d)")
            msb-menu-cond))
;; Perl files
(setq msb-menu-cond
      (cons '((eq major-mode 'perl-mode) 590 "Perl Files (%d)")
            msb-menu-cond))
;; HTML files
(setq msb-menu-cond
      (cons '((and (eq major-mode 'html-mode) 'no-multi)
              580 "HTML Files (%d)")
            msb-menu-cond))
;; JSP files
(setq msb-menu-cond
      (cons '((string-match "\\.jsp\\(<.*>\\)*$" (buffer-name))
              570 "JSP Files (%d)")
            msb-menu-cond))
;; Java files
(setq msb-menu-cond
      (cons '((and (memq major-mode '(java-mode)) 'no-multi)
              500 "Java Files (%d)")
            msb-menu-cond))
;; C++ Source files
(setq msb-menu-cond
      (cons '((memq major-mode '(c-mode c++-mode))
              520 "C++ Source Files (%d)")
            msb-menu-cond))
;; C++ Header files (must come after C++ Source files)
(setq msb-menu-cond
      (cons '((string-match "\\.h\\(<.*>\\)*$" (buffer-name))
              510 "C++ Header Files (%d)")
            msb-menu-cond))
;; IDL files
(setq msb-menu-cond
      (cons '((eq major-mode 'idl-mode) 530 "IDL Files (%d)")
            msb-menu-cond))
;; Config files
(setq msb-menu-cond
      (cons '((and
               (string-match "\\.\\(ini\\|cfg\\)\\(<.*>\\)*$"
                             (buffer-name))
               'no-multi)
              540 "Config Files (%d)")
            msb-menu-cond))
;; Makefile files
(setq msb-menu-cond
      (cons '((eq major-mode 'makefile-mode) 550 "Makefiles (%d)")
            msb-menu-cond))
;; Shell scripts
(setq msb-menu-cond
      (cons '((eq major-mode 'sh-mode) 560 "Shell Scripts (%d)")
            msb-menu-cond))
;; Other options...
(msb-mode)
(setq-default msb-files-by-directory t)
(setq-default msb-max-menu-items 25)
(setq-default msb-max-file-menu-items 1)



;;*************;;
;; FILES       ;;
;;*************;;

;; Add menu item to do end of line conversion
(require 'eol-conversion)

(defun unix-file ()
  "Change the current buffer to Latin 1 with Unix line-ends."
  (interactive)
  (set-buffer-file-coding-system 'iso-latin-1-unix t))

(defun dos-file ()
  "Change the current buffer to Latin 1 with DOS line-ends."
  (interactive)
  (set-buffer-file-coding-system 'iso-latin-1-dos t))

(defun mac-file ()
  "Change the current buffer to Latin 1 with Mac line-ends."
  (interactive)
  (set-buffer-file-coding-system 'iso-latin-1-mac t))

;; Strips trailing white space from ends of lines
(autoload 'nuke-trailing-whitespace "whitespace-nuke" 'nil t)
(add-hook 'write-file-hooks 'nuke-trailing-whitespace)

;; Resync All Loaded Files
(autoload 'resync-files "resync" "Resync All Loaded Files" t)
(global-set-key (kbd "M-<f20>") 'resync-files)

;; Autosaves (defaults to on)
(setq-default auto-save-default t)
(setq-default auto-save-interval 100)
(setq-default auto-save-timeout 20)     ;; 20 sec inactivity triggers auto-save
;; Backups
(setq-default make-backup-files t)
(setq-default backup-by-copying t)
(setq-default backup-by-copying-when-mismatch t)
(setq-default vc-make-backup-files t)   ;; backup even cvs/rcs files
(setq-default vc-follow-symlinks t)     ;; follow symbolic links to cvs files
(setq-default vc-cvs-stay-local 'nil)   ;; stay local
(setq-default version-control 'nil)     ;; do not make version numbers for backup files

;;
;; Printing enhancements
;;
(require 'printing)
(defun pr-ps-file (&optional filename)
  (pr-dosify-file-name (or filename
                           (make-temp-file
                            (convert-standard-filename
                             (expand-file-name pr-ps-temp-file pr-temp-dir)))))
  )


;;*******************;;
;; Regions/ BUFFERS  ;;
;;*******************;;

;; Smart region selection via Ctrl-Tab
(autoload 'id-select-and-kill-thing    "id-select"
  "Kill syntactical region selection" t)
(autoload 'id-select-and-copy-thing    "id-select"
  "Select and copy syntactical region" t)
(autoload 'id-select-double-click-hook "id-select"
  "Double mouse click syntactical region selection" 'nil)
(autoload 'id-select-thing             "id-select"
  "Keyboard-driven syntactical region selection" t)
(autoload 'id-select-thing-with-mouse  "id-select"
  "Single mouse click syntactical region selection" t)
(global-set-key (kbd "C-<tab>") 'id-select-thing)

;; Substring Matching of Buffers for Quick Change
(autoload 'iswitchb-buffer "iswitchb" "Substring Matching between buffers" t)
(iswitchb-mode 1)
(setq read-buffer-function 'iswitchb-read-buffer)
(setq-default iswitchb-case t)
(setq-default iswitchb-default-method 'samewindow)

;; Uniquify buffer name
(require 'uniquify)
(setq-default uniquify-buffer-name-style 'post-forward)
(defadvice iswitchb-kill-buffer (after rescan-after-kill activate)
  "*Regenerate the list of matching buffer names after a kill.
    Necessary if using `uniquify' with `uniquify-after-kill-buffer-p'
    set to non-nil."
  (setq iswitchb-buflist iswitchb-matches)
  (iswitchb-rescan))

(defun iswitchb-rescan ()
  "*Regenerate the list of matching buffer names."
  (interactive)
  (iswitchb-make-buflist iswitchb-default)
  (setq iswitchb-rescan t))


;; Autocirculate buffers with C-, C-. C-<, C->
(autoload 'cycle-buffer "cycle-buffer" "Cycle forward." t)
(autoload 'cycle-buffer-backward "cycle-buffer" "Cycle backward." t)
(autoload 'cycle-buffer-permissive "cycle-buffer" "Cycle forward allowing *buffers*." t)
(autoload 'cycle-buffer-backward-backward-permissive "cycle-buffer" "Cycle backward allowing *buffers*." t)
(global-set-key (kbd "<mouse-9>") 'cycle-buffer)
(global-set-key (kbd "<mouse-8>") 'cycle-buffer-backward)
(global-set-key (kbd "C-.") 'cycle-buffer)
(global-set-key (kbd "C-,") 'cycle-buffer-backward)
(global-set-key (kbd "C->") 'cycle-buffer-permissive)
(global-set-key (kbd "C-<") 'cycle-buffer-backward-permissive)
(setq-default cycle-buffer-allow-visible t)
(setq-default cycle-buffer-reset-after 2)
;; Modify cycle-buffer-message to font-lock on cycle buffer.
(eval-after-load "cycle-buffer" '(defun cycle-buffer-message (str)
                                   "Show STR but don't log it on the message log."
                                   ;; Prevent a byte compiler warning
                                   ;;       (if (fboundp 'display-message)
                                   ;;           ;; XEmacs way of preventing log messages.
                                   ;;           (display-message 'no-log str)
                                     (let ((message-log-max nil))
                                       (progn
                                         (if (> (length str) 0)
                                             (progn
                                               (string-match "\\[[^]]*\\]" str)
                                               (put-text-property (match-beginning 0)
                                                                  (match-end 0)
                                                                  'face 'highlight str)
                                               ))
                                         (message "%s" str)))
                                   ;;  )
                                   ))

(defun forward-kill-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times."
  (interactive "*p")
  (delete-region (point) (save-excursion (forward-word arg) (point))))

(defun forward-kill-line (arg)
  "Delete to end of the line. With argument, do this that many times."
  (interactive "*p")
  (delete-region (point) (save-excursion (end-of-line arg) (point))))

(defun backward-kill-line (arg)
  "Delete to start of line. With argument, do this that many times."
  (interactive "*p")
  (delete-region (point) (save-excursion (beginning-of-line arg) (point))))

(global-set-key [(control delete)] 'forward-kill-word)
(global-set-key [(control backspace)] 'backward-kill-word)
(global-set-key [(meta backspace)] 'backward-kill-line)
(global-set-key [(meta delete)] 'forward-kill-line)

(if (string-equal system-type "windows-nt")
    (progn
      (message "Disabling cut/paste to NT clipboard. Use toggle-use-x-clipboard to enable")
      (setq-default x-select-enable-clipboard nil)

      (defun toggle-use-x-clipboard ()
        (interactive)
        (setq-default x-select-enable-clipboard (not x-select-enable-clipboard))
        (message "x-select-enable-clipboard set to %s" x-select-enable-clipboard))
      )
  )

(defvar indirect-mode-name nil
  "Mode to set for indirect buffers.")
(make-variable-buffer-local 'indirect-mode-name)

(defun indirect-region (start end)
  "Edit the current region in another buffer.
    If the buffer-local variable `indirect-mode-name' is not set, prompt
    for mode name to choose for the indirect buffer interactively.
    Otherwise, use the value of said variable as argument to a funcall."
  (interactive "r")
  (let ((buffer-name (generate-new-buffer-name "*indirect*"))
        (mode
         (if (not indirect-mode-name)
             (setq indirect-mode-name
                   (intern
                    (completing-read
                     "Mode: "
                     (mapcar (lambda (e)
                               (list (symbol-name e)))
                             (apropos-internal "-mode$" 'commandp))
                     nil t)))
           indirect-mode-name)))
    (pop-to-buffer (make-indirect-buffer (current-buffer) buffer-name))
    (funcall mode)
    (narrow-to-region start end)
    (goto-char (point-min))
    (shrink-window-if-larger-than-buffer)))


;;*************;;
;; MODES       ;;
;;*************;;

;; Markdown
(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files." t)
(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))

;; AWK
(autoload 'awk-mode "cc-mode" nil t)

;;; use groovy-mode when file ends in .groovy or has #!/bin/groovy at start
(autoload 'groovy-mode "groovy-mode" "Major mode for editing Groovy code." t)
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))

;; make Groovy mode electric by default.
(add-hook 'groovy-mode-hook
          '(lambda ()
             (require 'groovy-electric)
             (groovy-electric-mode)))

;; HTML Mode
(autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)
(setq auto-mode-alist (cons '("\\.html$" . html-helper-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.htm$" . html-helper-mode) auto-mode-alist))
(setq-default html-helper-use-expert-menu 't)
(setq-default html-helper-basic-offset 4)
(setq-default html-helper-item-continue-indent 4)
(setq-default html-helper-mode-uses-JDE 'nil)


;; HTMLize
(autoload 'htmlize-buffer "htmlize" "HTMLize Buffer" t)
(define-key-after (lookup-key global-map [menu-bar tools]) [rnc_html] '("HTMLize Buffer" . htmlize-buffer) 'gdb)


;; Latex
(autoload 'latex-mode "auctex" "LaTeX editing mode" t nil)
(add-to-list 'auto-mode-alist '("\\.tex\\'" . latex-mode))
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (eval-after-load "auctex"
              '(progn (TeX-fold-mode 1)
                   (define-key TeX-mode-map (kbd "<C-return>") 'TeX-complete-symbol)))))
(setq-default reftex-plug-into-AUCTeX t)
(setq-default TeX-master nil)
(setq-default TeX-PDF-mode t)
(setq-default TeX-view-program-selection '(((output-dvi style-pstricks) "dvips and gv") (output-dvi "xdvi") (output-pdf "xdg-open") (output-html "xdg-open")))
(setq-default TeX-parse-self t)

;; XML-Lite
;; Commented out 24/11/2011: nxml mode is default now.

;; Diff Mode
(autoload 'diff-mode "diff-mode" "Diff major mode" t)
(add-to-list 'auto-mode-alist '("\\.\\(diffs?\\|patch\\|rej\\)\\'" . diff-mode))

;; Fill Adapt mode
(require 'filladapt)
(setq-default filladapt-mode-line-string "")
(add-hook 'text-mode-hook 'turn-on-filladapt-mode)

;; HideShow Mode
(require 'hideshow)
(add-to-list 'hs-special-modes-alist
             '(nxml-mode
               "<!--\\|<[^/>]>\\|<[^/][^>]*[^/]>"
               ""
               "<!--" ;; won't work on its own; uses syntax table
               (lambda (arg) (my-nxml-forward-element))
               nil))

(defun my-nxml-forward-element ()
  (let ((nxml-sexp-element-flag))
    (eval-after-load "nxml-mode"
      '(progn
         (setq nxml-sexp-element-flag (not (looking-at "<!--")))
         (unless (looking-at outline-regexp)
           (condition-case nil
               (nxml-forward-balanced-item 1)
             (error nil)))))))


(defun my-nxml-mode-hook () "Functions to run when in nxml mode."
  (eval-after-load "nxml-mode"
    '(progn
       (hs-minor-mode 1)
       (setq-default nxml-outline-child-indent 4)
       (setq-default nxml-child-indent 4)
       (setq-default nxml-slash-auto-complete-flag t)
       (define-key nxml-mode-map (kbd "<C-return>") 'nxml-complete)
       )))
(eval-after-load "rng-loc"
  '(progn
     ;; Used http://tech.groups.yahoo.com/group/emacs-nxml-mode/message/1399 to customise the grammar.
     (add-to-list 'rng-schema-locating-files (concat rnc_emacs_home "Schemas/schemas.xml"))
     )
)
(add-to-list 'auto-mode-alist '("\\.pom$" . nxml-mode))

(add-hook 'c-mode-common-hook 'hs-minor-mode)
(add-hook 'conf-mode-hook 'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'lisp-mode-hook 'hs-minor-mode)
(add-hook 'nxml-mode-hook 'my-nxml-mode-hook)
(add-hook 'python-mode-hook 'hs-minor-mode)
(add-hook 'sh-mode-hook 'hs-minor-mode)

(add-to-list 'hs-special-modes-alist
             '(conf-unix-mode
               "#{{{"
               "#}}}"
               "#"
              (lambda (arg) (search-forward-regexp "#}}}")) nil))

;; Folding for modes where we can't detect it automatically.
;;(require 'folding)
;;(folding-add-to-marks-list 'conf-unix-mode "#{{{" "#}}}" nil t)
;;(add-hook 'conf-mode-hook 'folding-mode)

;; Fix for commenting in xml - see http://www.emacswiki.org/emacs/NxmlMode
(require 'mz-comment-fix)
(add-to-list 'comment-strip-start-length (cons 'xml-mode 3))
(add-to-list 'comment-strip-start-length (cons 'nxml-mode 3))

(require 'hideshow-fringe)

(require 'fold-dwim)
(global-set-key (kbd "M-f") 'fold-dwim-toggle)

;; Live Mode - equivilant to Unix tail command
(autoload 'live-mode "live-mode"
  "Automatically update buffer akin to unix tail -f" t)
(setq-default live-interval 1)


;; To-Do Mode
(autoload 'todo-mode "todo-mode" "Major mode for editing TODO lists." t)
(autoload 'todo-show "todo-mode" "Show TODO items." t)
(autoload 'todo-insert-item "todo-mode" "Add TODO item." t)
;; Not using todo mode at the moment - will leave the autoloads but remove key shortcut.
;; (global-set-key (kbd "C-c t") 'todo-show) ; Switch to todo buffer


;; Align mode
(autoload 'align "align" "Align Code" t)
(add-hook 'align-load-hook 'my-align-load-hook)
(defun my-align-load-hook ()
  (eval-after-load "align"
    '(progn
       (setq-default align-c++-modes (cons 'java-mode align-c++-modes))
       )
    )
  )

;; Tabs
;;
;; Don't ever insert tabs
(setq-default indent-tabs-mode 'nil)


;; Shell Script
(setq-default sh-indentation 4)
(setq-default sh-basic-offset 4)
(setq-default sh-indent-for-then 0)
(setq-default sh-indent-comment t)

(setq-default sh-indent-for-do 0)
(setq-default sh-indent-after-do 4)
(setq-default sh-indent-after-if 4)
(setq-default sh-indent-for-case-label '*)
(setq-default sh-indent-for-case-alt '+)


;;
;; Calculator
;;
(autoload 'calculator "calculator" "A simple pocket calculator for Emacs." t)
;; Possible to find wierd keyboard bindings by using the trick in the
;; emacs FAQ question 104
;; (global-set-key [C-kp-add] 'calculator)


;; Extra Modes for generic-mode
(require 'generic-x)
(setq-default generic-define-mswindows-modes t)
(setq auto-mode-alist
      (append
       '(("\\.properties\\'" . java-properties-generic-mode)
         )
       auto-mode-alist))


;; ExtraFunctions contains four functions -
;; dos-to-unix - removes any ^M from a file
;; check-parentheses - check for missing brackets in a file
;; toggle-case-fold-search - toggles case sensitivity when searching;
;;                           default is 'insensitive'
;; printenv - print an environment variable
(load "extraFunctions" nil t)
(autoload 'dos-to-unix "extraFunctions" t)
(autoload 'check-parentheses "extraFunctions" t)
(global-set-key (kbd "C-c C-p") 'check-parentheses)
(autoload 'toggle-case-fold-search "extraFunctions" t)
(autoload 'print-env "extraFunctions" t)
(global-set-key (kbd "C-c C-d") 'insert-date-and-time)

;;
;; CVS Enhancements
(autoload (quote cvs-flags-query) "pcvs" "Autoload cvs-flags-query for key shortcut")
(setq-default log-edit-confirm t)
(setq-default cvs-auto-remove-handled 'delayed)
(setq-default cvs-find-file-and-jump t)

;;
;; Makefile mode (Makefile.*)
(add-to-list 'auto-mode-alist '("Makefile.*" . makefile-gmake-mode))


(defun my-pcl-cvs-jacorb ()
  "Automatically examine JACORB_HOME CVS directory"
  (interactive)
  (if (getenv "JACORB_HOME")
      (progn
        (cvs-examine (getenv "JACORB_HOME")
                     (cvs-flags-query 'cvs-update-flags "cvs -n update flags"))
        (save-excursion
          (progn
            (set-buffer (get-buffer "*cvs*"))
            (rename-buffer "*cvs-jacorb*")
            )
          )
        (setq truncate-lines nil)
        )
    (message "JACORB_HOME not set!")
    )
  )

(defun kill-changelog-buffers ()
  "Kill all ChangeLog buffers"
  (interactive)
  (let ((buffers (buffer-list))
        buffer)
    (while buffers
      (setq buffer (car buffers)
            name (buffer-name buffer)
            buffers (cdr buffers))
      (and (string-match "^ChangeLog" name)
           (kill-buffer buffer)))))
(defun kill-cvsignore-buffers ()
  "Kill all cvsignore buffers"
  (interactive)
  (let ((buffers (buffer-list))
        buffer)
    (while buffers
      (setq buffer (car buffers)
            name (buffer-name buffer)
            buffers (cdr buffers))
      (and (string-match "^.cvsignore" name)
           (kill-buffer buffer)))))
(global-set-key (kbd "C-x C-<pause>") 'kill-cvsignore-buffers)
(global-set-key (kbd "M-<pause>") 'kill-changelog-buffers)
(global-set-key (kbd "C-<pause>") 'my-pcl-cvs-jacorb)


(if (not (boundp 'rnc_disable_ecb_load))
    (progn
      (x-popup-dialog t '("Unknown config variable rnc_disable_ecb_load. Upgrade .emacs file" ("Ok" . nil)))
      (setq-default rnc_disable_ecb_load (symbol-value 'nil))
    )
  )
(if (and (> emacs-major-version 22)
         (not (symbol-value 'rnc_disable_ecb_load)))
    (progn
      ;; Cedet
      (require 'cedet)
      (if (>= emacs-major-version 23)
          (semantic-mode 1)
        (if (fboundp 'semantic-load-enable-minimum-features)
            (semantic-load-enable-minimum-features)
          )
        )
      ;; ECB
      (require 'ecb)
;;      (setq-default ecb-activate-hook nil)
      (setq-default ecb-layout-name "left10")
      (setq-default ecb-minor-mode-text "")
      (setq-default ecb-tip-of-the-day nil)
      (setq-default ecb-history-sort-method 'nil)
      (setq-default ecb-vc-enable-support t)
      (setq-default ecb-tree-buffer-style 'image)

;;      (setq-default ecb-token-display-function '((default . ecb-uml-concise-prototype-nonterminal)))

      (setq-default ecb-show-tags '((default (include collapsed nil) (parent collapsed nil) (type flattened nil) (variable collapsed access) (function flattened access) (label hidden nil) (t collapsed nil)) (c++-mode (include collapsed nil) (parent collapsed nil) (type flattened nil) (variable collapsed access) (function flattened access) (function collapsed access) (label hidden nil) (t collapsed nil)) (c-mode (include collapsed nil) (parent collapsed nil) (type flattened nil) (variable collapsed access) (function flattened access) (function collapsed access) (label hidden nil) (t collapsed nil)) (bovine-grammar-mode (keyword collapsed name) (token collapsed name) (nonterminal flattened name) (rule flattened name) (t collapsed nil)) (wisent-grammar-mode (keyword collapsed name) (token collapsed name) (nonterminal flattened name) (rule flattened name) (t collapsed nil)) (texinfo-mode (section flattened nil) (def collapsed name) (t collapsed nil)) (java-mode (include collapsed nil) (parent collapsed nil) (type flattened nil) (variable collapsed access) (function flattened name) (label collapsed nil) (t collapsed nil))))

       (setq-default ecb-advice-window-functions '(other-window delete-window delete-other-windows delete-windows-on split-window-horizontally split-window-vertically split-window switch-to-buffer switch-to-buffer-other-window other-window-for-scrolling))

       (setq-default ecb-token-visit-post-actions '((default ecb-token-visit-smart-token-start ecb-token-visit-highlight-token-header) (java-mode ecb-token-visit-goto-doc-start ecb-token-visit-recenter-top) ))

      ;; (setq-default ecb-methods-general-face 'ecb-methods-general-face)
      ;; (setq-default ecb-history-general-face 'ecb-history-general-face)
      ;; (setq-default ecb-sources-general-face 'ecb-sources-general-face)

      (if (> emacs-major-version 20)
          (progn
            (set-face-attribute 'ecb-default-general-face 'nil :height 0.9)
            ;; (set-face-attribute 'ecb-methods-general-face 'nil :family "helvetica"    :height 0.7)
            ;; (set-face-attribute 'ecb-history-general-face 'nil :family "trebuchet ms" :height 1.0 :foreground "gray20")
            ;; (set-face-attribute 'ecb-sources-general-face 'nil :family "trebuchet ms" :height 1.0 :foreground "gray20")
            ;; (set-face-attribute 'ecb-directories-general-face 'nil :family "trebuchet ms" :height 1.0 :foreground "gray20")
             ))

      (setq-default global-semantic-stickyfunc-mode nil)
      (setq-default global-senator-minor-mode nil)
      )
  )


;; PMD
(autoload 'pmd-current-buffer "pmd" "PMD Mode" t)
(autoload 'pmd-current-dir "pmd" "PMD Mode" t)
;; Try PCS_ROOT_DIR first
(if (getenv "JACORB_HOME")
    (setq-default pmd-ruleset-list (concat (getenv "JACORB_HOME") "/etc/pmd-ruleset.xml")))

;; FindBugs
(autoload 'findbugs-current-buffer "findbugs" "FindBugs Mode" t)
(autoload 'findbugs-current-dir "findbugs" "FindBugs Mode" t)
(if (getenv "JACORB_HOME")
   (progn
      (setq-default findbugs-ruleset-list  (concat (getenv "JACORB_HOME") "/etc/findbugs-ruleset.xml"))
      (setq-default findbugs-aux-classpath (concat (getenv "JACORB_HOME") "/classes:" (getenv "V4_ROOT_DIR") "/jars/junit-3.8.1/junit.jar"))
      ))
(setq-default findbugs-java-home "/usr/local/jdk1.6.0/bin/java")

;; Bookmarks
(global-set-key (kbd "<f12>") 'bookmark-bmenu-list)
(eval-after-load "bookmark"
  '(progn
     (setq-default bookmark-save-flag 1)
     ))

;; GIT
(autoload 'git-status "git" "GIT Mode" 't)
(eval-after-load "git"
  '(progn
     (define-key git-status-mode-map "="   'my-git-diff-file)
     (define-key git-status-mode-map "\M-="   'git-diff-file)
     (defun my-git-diff-file ()
       "Diff the marked file(s) against HEAD *ignoring* whitespace."
       (interactive)
       (let ((files (git-marked-files)))
         (git-setup-diff-buffer
          (apply #'git-run-command-buffer "*git-diff*" "diff-index" "-w" "-p" "-M" "HEAD" "--" (git-get-filenames files)))))
     ))
(autoload 'git-blame-mode "git-blame" "Minor mode for incremental blame for Git." t)

;; Subversion
(autoload 'svn-status "psvn" "PSVN Mode" 't)
(autoload 'svn-examine "psvn" "PSVN Mode" 't)
(setq-default svn-log-edit-use-log-edit-mode nil)

(eval-after-load "vc-git"
  '(defun vc-git-print-log (files buffer &optional shortlog start-revision limit)
  "Get change log associated with FILES.
Note that using SHORTLOG requires at least Git version 1.5.6,
for the --graph option."
  (let ((coding-system-for-read vc-git-commits-coding-system))
    ;; `vc-do-command' creates the buffer, but we need it before running
    ;; the command.
    (vc-setup-buffer buffer)
    ;; If the buffer exists from a previous invocation it might be
    ;; read-only.
    (let ((inhibit-read-only t))
      (with-current-buffer
          buffer
        (apply 'vc-git-command buffer
               'async files
               (append
                '("log" "--follow" "--no-color")
                (when shortlog
                  `("--graph" "--decorate" "--date=short"
                    ,(format "--pretty=tformat:%s"
                             (car vc-git-root-log-format))
                    "--abbrev-commit"))
                (when limit (list "-n" (format "%s" limit)))
                (when start-revision (list start-revision))
                '("--")))))))
  )

;;*************;;
;; COLOUR      ;;
;;*************;;
;;
;; Font-lock all modes
(global-font-lock-mode t)
(setq-default font-lock-verbose nil)
;; Enable loads of decorations!
(setq-default font-lock-maximum-decoration 't)
;; Fontify up to 512k size
(setq-default font-lock-maximum-size 512000)

;; C/C++
(add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'c++-mode-hook 'my-c-mode-hook)
(add-hook 'ctypes-load-hook 'my-ctypes-load-hook)

(defun my-c-mode-hook ()
  (progn
    ;; C/C++ font lock enhancments
    (font-lock-add-keywords 'c++-mode '("\\<\\(c\\(lass\\|onst\\)\\|extern\\|inline\\|p\\(ublic\\|rivate\\|rotected\\)\\|t\\(emplate\\|ypedef\\)\\|friend\\|st\\(atic\\|ruct\\)\\|v\\(irtual\\|olatile\\)\\)\\>"))
    (font-lock-add-keywords 'c-mode '("\\<\\(const\\|extern\\|typedef\\|st\\(atic\\|ruct\\)\\)\\>"))
    (require 'ctypes)
    )
  )

(defun my-ctypes-load-hook ()
  (eval-after-load "ctypes"
    '(progn
       (ctypes-read-file (concat rnc_emacs_home ".ctypes") 'nil t t)
       (ctypes-auto-parse-mode 1)
       )
    )
  )

;; Use java-mode for Groovy, jjtree, javaCC input files & ORBLayer files
(setq auto-mode-alist (cons '("\\.jj\\'" . java-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.jjt\\'" . java-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.visi\\'" . java-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.orbix\\'" . java-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.openorb\\'" . java-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.orbacus\\'" . java-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.jacorb\\'" . java-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.jdk\\'" . java-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.eorb\\'" . java-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.cup\\'" . java-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.sun\\'" . java-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.ibm\\'" . java-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.j9\\'" . java-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.real\\'" . java-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.dummy\\'" . java-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.ofj\\'" . java-mode) auto-mode-alist))


;; IDL
(setq-default auto-mode-alist
              (append '(("\\.idl\\'" . idl-mode)) auto-mode-alist))
(require 'idl-font-lock)

(if (symbol-value 'rnc_load_colours)
    (progn

      (if (not (>= emacs-major-version 24))
          (progn
            ;; Enable color theme mode
            (require 'color-theme)
            ;; (require 'color-theme-solarized 'nil 'noerror)
            (require 'zenburn-theme 'nil 'noerror)

            ;; (color-theme-solarized-dark)
            ;; (color-theme-solarized-light)
            (if (fboundp 'color-theme-zenburn)
                (color-theme-zenburn))
            )
        (progn
          (condition-case nil
              ;;(load-theme 'solarized-light t)
              (load-theme 'zenburn t)
            (error
             (message "Unable to find zenburn theme to load. Setup custom-theme-load-path")
             ))
          )
        )

      ;; Set colours
      (setq-default default-frame-alist
                    (add-to-list 'default-frame-alist
                                 '(cursor-color . "MediumVioletRed")))
      (setq-default default-frame-alist
                    (add-to-list 'default-frame-alist
                                 '(mouse-color  . "MediumVioletRed")))

      ;; Fringe face to stand out.
      (set-face-foreground 'fringe "Red")

      ;; Change cursor color according to mode. To add read-only detection
      ;; add " (if buffer-read-only "white" " to the below if block.
      (defvar hcz-set-cursor-color-color "")
      (defvar hcz-set-cursor-color-buffer "")
      (defun hcz-set-cursor-color-according-to-mode ()
        "change cursor color according to some minor modes."
        ;; set-cursor-color is somewhat costly, so we only call it when needed:
        (let ((color
               (if overwrite-mode "red"
                 "yellow")))
          (unless (and
                   (string= color hcz-set-cursor-color-color)
                   (string= (buffer-name) hcz-set-cursor-color-buffer))
            (set-cursor-color (setq hcz-set-cursor-color-color color))
            (setq hcz-set-cursor-color-buffer (buffer-name)))))
      (add-hook 'post-command-hook 'hcz-set-cursor-color-according-to-mode)

      (set-face-italic-p 'font-lock-comment-face t)
      (set-face-bold-p 'font-lock-keyword-face t)
      (set-face-underline-p 'font-lock-constant-face t)

      (set-face-foreground 'paren-face-match "Dim Gray")
      (set-face-background 'paren-face-match "SeaGreen")
      (set-face-foreground 'paren-face-mismatch "White")
      (set-face-background 'paren-face-mismatch "Red")
      (set-face-foreground 'paren-face-no-match "Yellow")
      (set-face-background 'paren-face-no-match "Red")

      (modify-face 'region "black" "khaki" nil nil nil nil nil nil)
      (modify-face 'lazy-highlight "#f0dfaf" "#5f5f5f" nil nil nil nil t nil)

      (set-face-attribute 'sml-modeline-end-face 'nil :inherit 'modeline :width 'condensed)
      (set-face-attribute 'sml-modeline-vis-face 'nil :inherit 'region)
      )
  )



;; C++ / C

;; Get the correct indentation style.
(defconst default-coding-style
  '(
    ;; Tab key always reindents. CC-mode means this always inserts spaces
    (c-tab-always-indent . t)
    ;; Offset of 4
    (c-basic-offset . 4)
    ;; Indent comments as well
    (c-indent-comments-syntactically-p . t)
    ;; Ensure newlines are correctly inserted after braces
    (c-hanging-braces-alist . ((brace-list-open)
                               (brace-entry-open)
                               (substatement-open before after)
                               (block-close . c-snug-do-while)
                               (extern-lang-open after)
                               (inexpr-class-open after)
                               (inexpr-class-close before)
                               ))
    ;; Cleanup
    (c-cleanup-list . (defun-close-semi
                        scope-operator
                        space-before-funcall))
    ;; Corrections for our coding style
    (c-offsets-alist . ((inline-open . 0)
                        (substatement-open . 0)
                        (inexpr-class . 0)
                        (statement-case-intro . 0)
                        (case-label . +)
                        ))
    )
  "Default Coding Style"
  )


(defconst sun-coding-style
  '(
    ;; Tab key always reindents. CC-mode means this always inserts spaces
    (c-tab-always-indent . t)
    ;; Offset of 4
    (c-basic-offset . 4)
    ;; Indent comments as well
    (c-indent-comments-syntactically-p . t)
    ;; Ensure newlines are correctly inserted after braces
    (c-hanging-braces-alist . ((brace-list-open)
                               (brace-entry-open)
                               (substatement-open before after)
                               (block-close . c-snug-do-while)
                               (extern-lang-open after)
                               (inexpr-class-open after)
                               (inexpr-class-close before)
                               ))
    ;; Cleanup
    (c-cleanup-list . (defun-close-semi
                        scope-operator))
    ;; Corrections for our coding style
    (c-offsets-alist . ((inline-open . 0)
                        (substatement-open . 0)
                        (inexpr-class . 0)
                        (statement-case-intro . 0)
                        (case-label . +)
                        ))
    )
  "Sun (or new JacORB) Coding Style"
  )

;; Hook run on ObjC/Java/C/C++/IDL/Pike modes
(defun my-c-mode-common-hook ()
  ;; HideShow Mode
  (hs-minor-mode)
  ;; Auto-newline
  (c-toggle-auto-newline 1)
  ;; Add my styles
  (c-add-style "Default Coding Style" default-coding-style t)
  (c-add-style "JacORB Coding Style" sun-coding-style t)
  ;; The `other' symbol says that CC Mode should use "my-style" in all
  ;; modes not explicitly listed. Since there's nothing else on the list
  ;; this causes "my-style" to be used in every mode.
  (setq c-default-style '((other . "Default Coding Style")))
  ;; Force correct indentation style (if we are in the JacORB tree use that style).
  (if (and (buffer-file-name) (string-match "org\/jacorb" (buffer-file-name)))
      (c-set-style "JacORB Coding Style")
    (c-set-style "Default Coding Style")
    )

  ;; Keybindings for all supported languages.  We can put these in
  ;; c-mode-base-map because c-mode-map, c++-mode-map, objc-mode-map,
  ;; java-mode-map, idl-mode-map, and pike-mode-map inherit from it.
  (define-key c-mode-base-map (kbd "<RET>") 'c-context-line-break)

  ;; Cleanup after ;
  (setq c-hanging-semi&comma-criteria
        (append '(c-semi&comma-no-newlines-before-nonblanks)
                '(c-semi&comma-inside-parenlist)
                '(c-semi&comma-no-newlines-for-oneline-inliners)))
  ;; Line up the current argument line under the first argument
  (c-set-offset 'arglist-cont-nonempty 'c-lineup-arglist)
  ;; Indent one line block
  (c-set-offset 'statement-cont 'c-indent-one-line-block)
  (c-set-offset 'topmost-intro-cont 'c-indent-one-line-block)
  ;; Line up C++ stream operators (i.e. `<<' and `>>').
  (c-set-offset 'stream-op 'c-lineup-streamop)
  ;; This one lines up close parentheses under the open.
  (c-set-offset 'arglist-close 'c-lineup-arglist-close-under-paren)
  ;; Line up Java throws declarations
  (c-set-offset 'func-decl-cont 'c-lineup-java-throws)
  ;; Currently not using.
  ;; Line up the classes in C++ multiple inheritance clauses under each other.
  ;; Line up Java implements and extends declarations
  ;; (c-set-offset 'inher-cont '((c-lineup-java-inher)
  ;;                             (c-lineup-multi-inher)))
  (c-setup-filladapt)
  (filladapt-mode 1)
  (paren-toggle-open-paren-context 1)
  ;; Don't display auto-newline/hungry-delete designation on the modeline
  (setq-default c-auto-hungry-string "")
  ;; Get rid of abbrev-mode as don't use it.
  (abbrev-mode -1)
  )

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)


;;**********;;
;; BUFFERS  ;;
;;**********;;

;; Ibuffer - replaces buffer-menu. Has to be after global-font-lock-mode
(require 'ibuffer)
(fset 'buffer-menu 'ibuffer)

;;**********;;
;; Printing ;;
;;**********;;

(setq-default ps-lpr-switches '("-h"))
(setq-default ps-font-size 7.5)
(setq-default ps-header-title-font-size 12)
(setq-default ps-header-font-size 9)
(setq-default ps-line-number t)
(setq-default ps-spool-duplex t)

(setq-default pr-file-duplex t)
(setq-default ps-print-color-p 'black-white)
(setq-default ps-default-bg "white")

(setq-default ps-black-white-faces '((font-lock-builtin-face nil nil bold) (font-lock-comment-face "gray20" nil italic) (font-lock-constant-face nil nil underline) (font-lock-function-name-face nil nil bold) (font-lock-keyword-face nil nil bold) (font-lock-string-face nil nil italic) (font-lock-type-face nil nil bold italic) (font-lock-variable-name-face nil nil) (font-lock-warning-face nil nil bold italic) ))

(defun toggle-print-line-numbers ()
  "Toggle Printing of line numbers"
  (interactive)
  (set-variable 'ps-line-number (not ps-line-number))
  (message "ps-line-number set to %s" ps-line-number))
(global-set-key (kbd "C-c C-t") 'toggle-print-line-numbers)



;;*************;;
;; DESKTOP     ;;
;;*************;;

(if (symbol-value 'rnc_desktop_enable)
    (progn
      ;; Do this last once everything else has succeeded
      ;; Load Desktop Standard Library
      (if (or (not (getenv "NODESK"))
              (not (string-equal (downcase (getenv "NODESK")) "true"))
              )
          (progn
            ;; Desktop Menu
            (autoload 'desktop-menu "desktop-menu" "Desktop Menu" t)
            (global-set-key (kbd "<pause>") 'desktop-menu)
            (setq-default desktop-load-locked-desktop 't)
            (setq-default desktop-missing-file-warning 't)

            (add-hook 'desktop-save-hook
                      '(lambda ()
                         (desktop-truncate search-ring 2)
                         (desktop-truncate regexp-search-ring 2)))

            (setq-default desktop-menu-autosave 3600)
            )
        )
      )
  )


;;*************;;
;; HELP MENU   ;;
;;*************;;
(defun rnc_describe_me ()
  "Describe Nick Cross' emacs customisations."
  (interactive)
  (progn
    (if (get-buffer "Extension Help")
        (switch-to-buffer "Extension Help")
      (progn
          (get-buffer-create "Extension Help")
          (switch-to-buffer "Extension Help")
          (insert-file-contents (concat rnc_emacs_home "Documentation/ExtensionHelp"))
          (set-buffer-modified-p 'nil)
          (toggle-read-only t)
          )
      )
    )
  )

(define-key-after (lookup-key global-map [menu-bar help-menu]) [rnc_extensions] '("Nick Cross Extensions" . rnc_describe_me) 'describe-no-warranty)
(define-key-after (lookup-key global-map [menu-bar help-menu]) [rnc_separator] '("--") 'describe-no-warranty)


(message (concat "DONE emacs_main:" (format-time-string "%H:%M:%S %3Nms")))
