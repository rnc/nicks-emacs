;;
;; This next function deletes those annoying ^M characters
;; left over from some DOS files or after a DOS ftp operation
;; Type M-x dos-to-unix to get rid of them.
;;

;(defun dos-to-unix ()
;  "Replace \r\n with \n"
;  (interactive)
;  (save-excursion
;    (goto-char (point-min))
;    ( replace-string "\r\n" "\n" )))

;; This version works in read-only files as well.
;; (defun dos-to-unix ()
;;   "CRLF -> LF"
;;   (interactive)
;;   (let ((ro buffer-read-only))
;;     (if ro (toggle-read-only))
;;     (save-excursion
;;       (goto-char (point-min))
;;       (goto-char (point-min))
;;       (while (search-forward "" (point-max) 't)
;;         (goto-char (match-beginning 0))
;;         (delete-char 1))
;;       (if ro (progn (toggle-read-only)
;;                     (set-buffer-modified-p nil))
;;         (set-buffer-modified-p 't)))))


;; (defun unix-to-dos ()
;;   "Replace \n with \r\n"
;;   (interactive)
;;   (save-excursion
;;     (goto-char (point-min))
;;     ( replace-string "\n" "\r\n" )))



;; Bob Wiener
(defun check-parentheses ()
  "Check the buffer for unbalanced parentheses.  Stops at any that are unbalanced."
  (interactive)
  (let ((start-point (point)))
    (goto-char (point-min))
    (condition-case e
        (while (/= (point) (point-max))
          (forward-sexp))
      (error
       ;; If this is an extra left paren error, we have to scan backwards to
       ;; find the exact left paren in error
       (cond ((and (eq (car e) 'error)
                   (string-equal (car (cdr e)) "Unbalanced parentheses"))
              ;; left paren error
              (goto-char (point-max))
              (while (/= (point) (point-min))
                (condition-case e (backward-sexp)
                  (error
                   (error "Probably an extra left parenthesis here.")))))
             (t
              (error "Probably an extra right parenthesis here.")))))
    (goto-char start-point)
    (message "All parentheses appear balanced.")))





;;
;; Toggle Case in searches
;;
(defun toggle-case-fold-search ()
  "Toggle the variable case-fold-search."
  (interactive)
  (set-variable 'case-fold-search (not case-fold-search))
  (message "case-fold-search set to %s" case-fold-search))




;;
;; Print value of enviroment variable
;;
(defun printenv(var)
  "Returns value of environment variable"
  (interactive
     (list
       (progn
         (require 'env)
         (read-envvar-name "Enviroment variable: "))))
  (let ((var-name (getenv var)))
    (if (null var-name)
      (error "%s does not exist" var)
    (message var-name))))



;;
;; Insert Date and Time
;;
;;
;; Redefinable Variable for format
(defvar insert-date-time-format "%R%P %a %e %b %Y"
  "*Format for \\[insert-date-and-time] (c.f. 'format-time-string' for how to format).")
;%l:%M %p %a %b %e %Y
;%M:%H %d %b %Y
;%Y-%m-%d %H:%M:%S

(defun insert-date-and-time ()
  "Insert the date and time into the current buffer. See variable insert-date-time-format."
  (interactive)
  (insert (format-time-string insert-date-time-format (current-time))))


;; Insert the ascii table
(defun ascii-table (&optional limit)
  "Print the ascii table (up to char 127).

Given the optional argument LIMIT, print the characters up to char
LIMIT.  Try 254 for example."
  (interactive "P")
  (if (null limit)
      (setq limit 127))
  (switch-to-buffer "*ASCII*")
  (setq buffer-read-only nil)
  (erase-buffer)
  (insert (format "ASCII characters up to number %d.\n" limit))
  (let ((i 0))
    (while (< i limit)
      (setq i (+ i 1))
      (insert (format "%6d%5s" i (single-key-description i)))
      (if (= 0 (% i 6))
          (insert "\n"))))
  (beginning-of-buffer)
  (setq buffer-read-only t))
