;;; literate-clojure-mode.el --- Tools for literate clojure in org-mode

;; Copyright (C) 2019 Rakuten Ready

;; Author: Alexandre Gariepy <alexandre.gariepy@rakuten.com>
;; Created: 8 Jul 2019
;; Version: 0.1
;; Keywords: literate programming, clojure, CIDER

;;; Commentary:
;; Tools to facilitate clojure literate programming in org mode.

;;; Code:

(require 'org)
(require 'dash)

;; Helper functions

(defun litclj--get-block-tangle-property ()
  (cdr
   (assoc :tangle
          (nth 2 (org-babel-get-src-block-info)))))

(defun litclj--get-tangle-path ()
  (let ((path (litclj--get-block-tangle-property)))
    (if (string-equal path "no")
        (progn (message "This code block is not tangled")
               nil)
      path)))

(defun litclj--tangled-clojure-block? ()
  (and (string-equal "clojure"
                     (nth 0 (org-babel-get-src-block-info)))
       (not (string-equal "no"
                          (litclj--get-block-tangle-property)))))

(defun litclj--previous-heading-point ()
  (save-excursion
    (progn
      (org-back-to-heading)
      (point))))

(defun litclj--previous-heading-name ()
  (nth 4 (org-heading-components)))

(defun litclj--count-code-blocks-recur (n stop)
  (condition-case nil
      (if (and (> (org-previous-block 1) stop))
          (let ((count? (litclj--tangled-clojure-block?)))
            (if count?
                (litclj--count-code-blocks-recur (+ 1 n) stop)
              (litclj--count-code-blocks-recur n stop)))
        n)
    (error n)))

(defun litclj--count-code-blocks ()
  (save-excursion
    (forward-line 1)
    (litclj--count-code-blocks-recur 0 (litclj--previous-heading-point))))

(defun litclj--subtree-name-and-number ()
  (let ((n (litclj--count-code-blocks)))
    (concat (litclj--previous-heading-name)
            ":"
            (number-to-string n))))

(defun litclj--current-block-name ()
  (or (nth 4 (org-babel-get-src-block-info))
      (litclj--subtree-name-and-number)))

(defun litclj--block-name-to-point-assoc-list-recur (block-list)
  (condition-case nil
      (progn
        (org-next-block nil)
        (cons `(,(litclj--current-block-name) . ,(point))
              (litclj--block-name-to-point-assoc-list-recur block-list)))
    (error block-list)))

(defun litclj--block-name-to-point-assoc-list ()
  (save-excursion
    (goto-char (point-min))
    (litclj--block-name-to-point-assoc-list-recur '())))

(defun litclj--block-name-regex (name)
  (concat ";;\s\\[.+\\["
          name
          "\\]\\]"))

(defun litclj--current-line-empty-p ()
  (= 0 (string-match-p "^\\s-*$" (thing-at-point 'line))))

(defun litclj--next-non-empty-line ()
  (progn
    (forward-line)
    (while (litclj--current-line-empty-p)
      (forward-line))))

(defun litclj--block-point-position ()
  (save-excursion
    (let* ((point-pos (point)))
      (progn
        (org-previous-block 1)
        (litclj--next-non-empty-line)
        (- point-pos (point))))))

(defun litclj--org-file-and-id ()
    (save-excursion
      (re-search-backward org-bracket-link-analytic-regexp nil t)
      (let* ((full-path (match-string 3))
             (block-name (match-string 5))
             (path (progn (string-match "::" full-path)
                        (substring full-path 0 (match-beginning 0)))))
        `(,path ,block-name))))

(defun litclj--in-tangled-block? ()
  (save-excursion
    (-let [(_ block-name) (litclj--org-file-and-id)]
      (re-search-forward (concat " " (regexp-quote block-name)
                                 " ends here")
                         nil t))))

;; Minor mode functions

(defun litclj-goto-tangle ()
  (interactive)
  (when-let ((tangle-file (litclj--get-tangle-path)))
    (let* ((block-name (litclj--current-block-name))
           (point-pos (litclj--block-point-position)))
      (find-file tangle-file)
      (goto-char (point-min))
      (re-search-forward (litclj--block-name-regex block-name))
      (beginning-of-line)
      (forward-line)
      (forward-char point-pos))))

(defun litclj-tangle-goto-org ()
  (interactive)
  (if (litclj--in-tangled-block?)
      (-let [(path block-name) (litclj--org-file-and-id)]
        (find-file-other-window path)
        (let ((block-point (cdr (assoc block-name (litclj--block-name-to-point-assoc-list)))))
          (org-overview)
          (goto-char block-point)
          (outline-show-subtree)
          (goto-char block-point)))
    (error "Not in tangled code")))

;;;###autoload
(define-minor-mode literate-clojure-mode
  "Tools for literate clojure in org-mode"
  :keymap
  `((,(kbd "C-c g") . litclj-go-to-tangle)))

(provide 'literate-clojure-mode)

;;; literate-clojure-mode.el ends here
