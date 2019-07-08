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
      (if (and (> (search-backward "#+BEGIN_SRC") stop))
          (let ((count? (litclj--tangled-clojure-block?)))
            (if count?
                (litclj--count-code-blocks-recur (+ 1 n) stop)
              (litclj--count-code-blocks-recur n stop)))
        n)
    (search-failed n)))

(defun litclj--count-code-blocks ()
  (save-excursion
    (litclj--count-code-blocks-recur 0 (litclj--previous-heading-point))))

(defun litclj--subtree-name-and-number ()
  (let ((n (litclj--count-code-blocks)))
    (concat (litclj--previous-heading-name)
            ":"
            (number-to-string n))))

(defun litclj--current-block-name ()
  (or (nth 4 (org-babel-get-src-block-info))
      (litclj--subtree-name-and-number)))

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


;; Minor mode functions

(defun litclj-go-to-tangle ()
  (interactive)
  (when-let ((tangle-file (litclj--get-tangle-path)))
    (let* ((block-name (litclj--current-block-name))
           (point-pos (litclj--block-point-position)))
      (progn
        (find-file tangle-file)
        (goto-char (point-min))
        (re-search-forward (litclj--block-name-regex block-name))
        (beginning-of-line)
        (forward-line)
        (forward-char point-pos)))))

;;;###autoload
(define-minor-mode literate-clojure-mode
  "Tools for literate clojure in org-mode"
  :keymap
  `((,(kbd "C-c g") . litclj-go-to-tangle)))

(provide 'literate-clojure-mode)

;;; literate-clojure-mode.el ends here
