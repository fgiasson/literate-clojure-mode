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
(defcustom litclj-auto-detangle-delay-sec 1
  "Time in seconds delay which a modified code block is detangled")

(defvar litclj-follow-last-block-infos nil)
(defvar litclj-follow-file-blocks-cache '())
(defvar litclj-detangle-timers '())
(defvar litclj-tangled-block-header "\\[\\[file:\\(.+\\)::.+\\]\\[\\(.+\\)\\]\\]")

;; helper functions
(defun litclj--invalid-cache-on-tangle ()
  (setq litclj-follow-file-blocks-cache '()))
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
          (if (org-babel-get-src-block-info) ;; This filters out #+BEGIN_EXAMPLE blocks
              (litclj--count-code-blocks-recur (+ 1 n) stop)
            (litclj--count-code-blocks-recur n stop))
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

(defun litclj--block-name-to-point-assoc-list (org-filepath)
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

(defun litclj--code-block-infos ()
  "Returns the origin file of a code block, its name and its point in the tangled file."
  (save-excursion
    (let* ((point (re-search-backward litclj-tangled-block-header nil t))
           (path (match-string 1))
           (block-name (match-string 2)))
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

(defun litclj--block-name-to-point-assoc-list-cached (org-filepath)
  (let* ((name-to-point-assoc-list (assoc org-filepath litclj-follow-file-blocks-cache)))
    (if name-to-point-assoc-list
        name-to-point-assoc-list
      (let ((new-list (litclj--block-name-to-point-assoc-list org-filepath)))
        (setq litclj-follow-file-blocks-cache (cons `(,org-filepath . ,new-list) litclj-follow-file-blocks-cache))
        new-list))))

(defun litclj--org-block-point (file block-name)
  (cdr (assoc block-name (litclj--block-name-to-point-assoc-list-cached file))))

(defun litclj-tangle-goto-org ()
  (interactive)
  (if (litclj--in-tangled-block?)
      (-let [(_ path block-name) (litclj--code-block-infos)]
        (find-file-other-window path)
        (let ((block-point (litclj--org-block-point path block-name)))
          (litclj--goto-point-subheading block-point)
          (recenter)
          (evil-scroll-line-to-bottom (line-number-at-pos))
          (scroll-up-line)))
    (error "Not in tangled code")))

(defun litclj--current-block-size ()
  (save-window-excursion
    (litclj-tangle-goto-org)
    (let ((p (point)))
      (re-search-forward "#\\+\\(END_SRC\\|end_src\\)")
      (- (point) p))))

(defun litclj--update-follow-file-blocks-cache (file after-point increment)
  "When detangling, if we add x characters to the block at point y, we have to increment
   the point of all blocks after y by x characters"
  (when-let ((name-to-point-alist (assoc file litclj-follow-file-blocks-cache)))
    (setcdr
     (assoc (litclj--strip-text-properties file) litclj-follow-file-blocks-cache)
     (mapcar (lambda (name-point-elem)
              (let ((name (car name-point-elem))
                    (point (cdr name-point-elem)))
                (if (> point after-point)
                    `(,name . ,(+ point increment))
                  name-point-elem)))
            (cdr name-to-point-alist)))))

(defun litclj-detangle-current-block ()
  (interactive)
  (let ((old-size (litclj--current-block-size))
        org-file-point)
    (save-window-excursion
      (let ((content (litclj--tangled-block-content)))
        (litclj-tangle-goto-org)
        (setq org-file-point (point))
        (org-babel-update-block-body content)))
    (-let ((new-size (litclj--current-block-size))
           ((_ block-file block-name) (litclj--code-block-infos)))
      (litclj--update-follow-file-blocks-cache block-file org-file-point (- new-size old-size)))))

(defun litclj-detangle-all ()
  (interactive)
  (save-excursion
    (goto-char 0)
    (forward-line)
    (litclj-detangle-current-block)
    (while (re-search-forward litclj-tangled-block-header nil t)
      (beginning-of-line 2)
      (litclj-detangle-current-block))))

(defun litclj-follow (&optional force-follow)
  (interactive)
  (save-match-data
    (ignore-errors
      (let ((block-infos (litclj--code-block-infos)))
        (when (or force-follow
                  (not (string= (nth 2 block-infos)
                                (nth 2 litclj-follow-last-block-infos))))
          (let ((b (current-buffer)))
            (litclj-tangle-goto-org)
            (switch-to-buffer-other-window b)
            (setq litclj-follow-last-block-infos block-infos)))))))

(defun litclj--remove-detangle-timer (key)
  (when-let (timer (assoc key litclj-detangle-timers))
    (cancel-timer (cdr timer))
    (setq litclj-detangle-timers (delq timer litclj-detangle-timers))))

(defun litclj--auto-detangle-all (begin end lenght)
  (save-match-data
    (let ((key (buffer-file-name)))
      (when (and key ;; temp and special buffers are not associated with a file
                 (not (assoc key litclj-detangle-timers)))
        (let ((timer (run-with-idle-timer litclj-auto-detangle-delay-sec
                                          nil
                                          (lambda (b key)
                                            (save-window-excursion
                                              (save-excursion
                                                (with-current-buffer b
                                                  (litclj-detangle-all)
                                                  (litclj-follow t))))
                                            (litclj--remove-detangle-timer key))
                                          (current-buffer)
                                          key)))
          (setq litclj-detangle-timers (cons `(,key . ,timer) litclj-detangle-timers)))))))

(defun litclj--cleanup-auto-detangle ()
  (mapc (lambda (timer)
          (cancel-timer (cdr timer)))
        litclj-detangle-timers)
  (setq litclj-detangle-timers '()))

(defun litclj-validate-code-block-name-uniqueness ()
  (interactive)
  (let ((seen-names '())
        (invalid-names '()))
    (save-excursion
      (dolist (current (litclj--block-name-to-point-assoc-list (buffer-file-name)))
        (goto-char (cdr current))
        (forward-line)
        (let ((tanled-property (litclj--get-block-tangle-property)))
          (when (and (member (car current) seen-names)
                     tanled-property
                     (not (string= "no" tanled-property)))
            (setq invalid-names (cons (car current) invalid-names))))
        (setq seen-names (cons (car current) seen-names))))
    (when invalid-names
      (message-box (concat "The following names are used for multiple code blocks: \n"
                           (mapconcat 'identity invalid-names ", ")
                           "\nThis may break tooling.")))))

;;;###autoload
(define-minor-mode literate-clojure-mode
  "Tools for literate clojure in org-mode"
  :init nil
  :keymap
  `((,(kbd "C-c g") . litclj-go-to-tangle)))

(define-minor-mode literate-clojure-validate-mode
  "Validate that a org mode buffer does not contain any duplicate code block names"
  :init nil
  (if literate-clojure-validate-mode
      (progn
        (litclj-validate-code-block-name-uniqueness)
        (add-hook 'after-save-hook 'litclj-validate-code-block-name-uniqueness))
    (remove-hook 'after-save-hook 'litclj-validate-code-block-name-uniqueness)))

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
      (progn
        (add-hook 'org-babel-pre-tangle-hook 'litclj--cleanup-auto-detangle)
        (add-hook 'after-change-functions 'litclj--auto-detangle-all nil t))
    (progn
      (litclj--cleanup-auto-detangle)
      (remove-hook 'org-babel-pre-tangle-hook 'litclj--cleanup-auto-detangle)
      (remove-hook 'after-change-functions 'litclj--auto-detangle-all t))))


(provide 'literate-clojure-mode)
;;; literate-clojure-mode.el ends here
