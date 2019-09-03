;; -*- lexical-binding: t -*-
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

;; Variables

(defcustom litclj-auto-detangle-delay "500 millisec"
  "Time delay after which a modified code block is detangled")

(defvar litclj-follow-last-block-infos nil)
(defvar litclj-follow-file-blocks-cache '())
(defvar litclj-detangle-timers '())

;; Helper functions

(defun litclj--strip-text-properties(txt)
  (set-text-properties 0 (length txt) nil txt)
  txt)

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

(defun litclj--tangled-block? ()
  (not (string-equal "no" (litclj--get-block-tangle-property))))

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
          (let ((count? (litclj--tangled-block?)))
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
  (let* ((current-file (buffer-file-name))
         (name-to-point-assoc-list (assoc current-file litclj-follow-file-blocks-cache)))
    (if name-to-point-assoc-list
        name-to-point-assoc-list
      (let ((new-list (save-excursion
                        (goto-char (point-min))
                        (litclj--block-name-to-point-assoc-list-recur '()))))
        (setq litclj-follow-file-blocks-cache (cons `(,current-file . ,new-list) litclj-follow-file-blocks-cache))
        new-list))))

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

(defun litclj--code-block-infos ()
  "Returns the origin file of a code block, its name and its point in the tangled file."
  (save-excursion
    (let* ((point (re-search-backward org-bracket-link-analytic-regexp nil t))
           (full-path (match-string 3))
           (block-name (match-string 5))
           (path (progn (string-match "::" full-path)
                        (substring full-path 0 (match-beginning 0)))))
      `(,point ,path ,block-name))))

(defun litclj--search-block-end (block-name)
  (re-search-forward (concat " " (regexp-quote block-name)
                             " ends here") nil t))

(defun litclj--in-tangled-block? ()
  (save-excursion
    (-let [(_ _ block-name) (litclj--code-block-infos)]

      (litclj--search-block-end block-name))))

(defun litclj--tangled-block-content ()
  (-let* (((begin-header-point _ block-name) (litclj--code-block-infos))
          (end-footer-point (save-excursion (litclj--search-block-end block-name))))
    (buffer-substring-no-properties
     (save-excursion (goto-char begin-header-point)
                     (line-beginning-position 2))
     (save-excursion (goto-char end-footer-point)
                     (line-end-position 0)))))

(defun litclj--goto-point-subheading (point)
  "Show the content of the subheading at point."
  (outline-show-all)
  (goto-char point)
  (outline-hide-other))

;; Callable functions

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
      (-let [(_ path block-name) (litclj--code-block-infos)]
        (find-file-other-window path)
        (let ((block-point (cdr (assoc block-name (litclj--block-name-to-point-assoc-list)))))
          (litclj--goto-point-subheading block-point)
          (recenter)
          (evil-scroll-line-to-bottom (line-number-at-pos))
          (scroll-up-line)))
    (error "Not in tangled code")))

(defun litclj-detangle-current-block ()
  (interactive)
  (let ((b (current-buffer))
        (content (litclj--tangled-block-content)))
    (litclj-tangle-goto-org)
    (org-babel-update-block-body content)
    (switch-to-buffer-other-window b)))

(defun litclj-follow ()
  (interactive)
  (ignore-errors
    (let ((block-infos (litclj--code-block-infos)))
      (unless (string= (nth 2 block-infos)
                       (nth 2 litclj-follow-last-block-infos))
        (let ((b (current-buffer)))
          (litclj-tangle-goto-org)
          (switch-to-buffer-other-window b)
          (setq litclj-follow-last-block-infos block-infos))))))

(defun litclj--auto-detangle-current-block (begin end lenght)
  (-let [(_ file name) (litclj--code-block-infos)]
    (when-let (timer (assoc (litclj--strip-text-properties name) litclj-detangle-timers))
      (cancel-timer (cdr timer))
      (setq litclj-detangle-timers (delq timer litclj-detangle-timers)))
    (let ((current-point (point))
          (b (current-buffer)))
      (setq litclj-detangle-timers
            (cons `(,(litclj--strip-text-properties name) .
                    ,(run-at-time litclj-auto-detangle-delay
                                  nil
                                  (lambda ()
                                    (with-current-buffer b
                                      (save-excursion
                                        (goto-char current-point)
                                        (litclj-detangle-current-block))))))
                  litclj-detangle-timers)))))

(defun litclj--invalid-cache-on-tangle ()
  (setq litclj-follow-file-blocks-cache '()))

;;;###autoload
(define-minor-mode literate-clojure-mode
  "Tools for literate clojure in org-mode"
  :keymap
  `((,(kbd "C-c g") . litclj-go-to-tangle)))

(define-minor-mode literate-clojure-follow-mode
  "Automatically follow org mode files from tangles clojure files."
  :init nil
  (if literate-clojure-follow-mode
      (progn
        (add-hook 'post-command-hook 'litclj-follow nil t)
        (add-hook 'org-babel-post-tangle-hook 'litclj--invalid-cache-on-tangle))
    (progn
      (remove-hook 'post-command-hook 'litclj-follow t)
      (remove-hook 'org-babel-post-tangle-hook 'litclj--invalid-cache-on-tangle))))

(define-minor-mode literate-clojure-auto-detangle-mode
  "Automatically detangles clojure files on modification"
  :init nil
  (if literate-clojure-auto-detangle-mode
      (add-hook 'after-change-functions 'litclj--auto-detangle-current-block nil t)
    (remove-hook 'after-change-functions 'litclj--auto-detangle-current-block t)))


(provide 'literate-clojure-mode)
;;; literate-clojure-mode.el ends here
