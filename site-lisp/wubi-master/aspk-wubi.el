;;; aspk-wubi.el --- extention code to the wubi by astropeak -*- coding: utf-8; -*-
;;; Implemented a function to add new phrases quickly.

(setq wubi-word-to-key (make-hash-table :test 'equal))
(cl-loop for x in wubi-rules 
         do
         ;; (message "y: %s, x: %s"  y (aref (nth 1 x) 0))
         (cl-loop with key = (nth 0 x)
                  for word across (nth 1 x)
                  do
                  ;; (message "y2 : %s, x: %s"  y (nth 0 x))
                  (when (> (length key) (length (gethash word wubi-word-to-key)))
                    (puthash word key wubi-word-to-key))
                  ))

(defun wubi-get-word-key (word)
  "Get the wubi key for the given `word'"
  (gethash word wubi-word-to-key))

;; (wubi-get-word-key "虎")


;; http://stackoverflow.com/questions/2321904/elisp-how-to-save-data-in-a-file
(defun aspk-dump-var (varlist filename)
  "simplistic dumping of variables in VARLIST to a file FILENAME"
  (save-excursion
    (let ((buf (find-file-noselect filename)))
      (set-buffer buf) (erase-buffer)
      ;; aspk-util-dump 
      (funcall
       (lambda (varlist buffer)
         "insert into buffer the setq statement to recreate the variables in VARLIST"
         (loop for var in varlist do
               (print (list 'setq var (list 'quote (symbol-value var))) buffer)))
       varlist buf)
      (save-buffer) (kill-buffer))))

;; (aspk-dump-var (list 'wubi-local-phrases) "aaa")

(defun aspk-wubi-calculate-key (phrase)
  "Calculate the wubi key for the given `phrase'"
  (cond ((equal (length phrase) 1)
         (wubi-get-word-key phrase))
        ((equal (length phrase) 2)
         (concat
          (substring (wubi-get-word-key (substring phrase 0 1)) 0 2)
          (substring (wubi-get-word-key (substring phrase 1 2)) 0 2)))
        ((equal (length phrase) 3)
         (concat
          (substring (wubi-get-word-key (substring phrase 0 1)) 0 1)
          (substring (wubi-get-word-key (substring phrase 1 2)) 0 1)
          (substring (wubi-get-word-key (substring phrase 2 3)) 0 2)))
        ((>= (length phrase) 4)
         (concat
          (substring (wubi-get-word-key (substring phrase 0 1)) 0 1)
          (substring (wubi-get-word-key (substring phrase 1 2)) 0 1)
          (substring (wubi-get-word-key (substring phrase 2 3)) 0 1)
          (substring (wubi-get-word-key (substring phrase
                                                   (- (length phrase) 1)
                                                   (length phrase))) 0 1)))
        (t
         (signal 'error-symbol (format "can't get key for phrase '%s'" phrase))
         )))


;; (aspk-wubi-calculate-key "演示")
;; (aspk-wubi-calculate-key "中华人民共和国")
;; (aspk-wubi-calculate-key "车")


(defun aspk-wubi-add-local-phrase (&optional phrase)
  ""
  (interactive)
  (unless phrase
    (setq phrase (if mark-active (buffer-substring-no-properties (region-beginning) (region-end)) "")))
  ;; (message "%S" phrase)
  (add-to-list 'wubi-local-phrases (list (aspk-wubi-calculate-key phrase) (vector phrase)))
  (aspk-wubi-store-local-phrases)
  (wubi-load-local-phrases)
  (message "%s added" phrase)
  )

(defun aspk-wubi-store-local-phrases ()
  (interactive)
  (aspk-dump-var (list 'wubi-local-phrases) wubi-phrases-file)
  )


;; (aspk-wubi-add-local-phrase "财报")


(provide 'aspk-wubi)
