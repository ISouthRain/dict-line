;;; dict-line --- View dict in Emacs.  -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.
;; License: GPL-3.0-or-later

;; Author: ISouthRain
;; Version: 0.7.0
;; Package-Requires: ((emacs "24.2"))
;; Keywords: dict
;; URL: https://github.com/ISouthRain/dict-line

;;; Commentary:
;;
;; This package is quickly view word dictionary.

;;; Code:

(defgroup dict-line nil
  "Emacs dictionary lookup on cursor movement."
  :group 'tools)

(defcustom dict-line-dict-directory "~/my-dict/"
  "The directory where .ts dictionary files are stored."
  :type 'directory
  :group 'dict-line)

(defcustom dict-line-dict-personal-file "~/my-dict/my-dict.ts"
  "Personal dict file."
  :type 'string
  :group 'dict-line)

(defcustom dict-line-audio nil
  "Toggle play audio file."
  :type 'boolean
  :group 'dict-line)

(defcustom dict-line-audio-root-dir "~/my-dict/my-audio/"
  "The directory where audio files are stored."
  :type 'directory
  :group 'dict-line)

(defcustom dict-line-cache-file
  (expand-file-name ".dict-line-cache.el" user-emacs-directory)
  "File to persist dict-line cache across Emacs sessions."
  :type 'file
  :group 'dict-line)

(defcustom dict-line-idle-time 0.5
  "Idle time in seconds before triggering dictionary lookup."
  :type 'number
  :group 'dict-line)

(defcustom dict-line-audio-play-program "mplayer"
  "Play audio file program."
  :type 'string
  :group 'dict-line)

(defcustom dict-line-audio-play-program-arg ""
  "Audio play program arguments."
  :type 'string
  :group 'dict-line)

(defcustom dict-line-posframe-location #'posframe-poshandler-point-bottom-left-corner
  "The location function for displaying the dict-line posframe.
Option `posframe-show' =>  (2) POSHANDLER"
  :type '(choice (const nil) function)
  :group 'dict-line)

(defcustom dict-line-display #'dict-line--message
  "dict-line display function."
  :type '(choice (const nil) function)
  :group 'dict-line)

(defvar dict-line--posframe-buffer "*dict-line-posframe*")

(defvar dict-line-word nil
  "Recently searched words.
From `dict-line--extract-word'")
(defvar dict-line-dict nil
  "Recent dictionary lookup results.
From `dict-line--search-word'")

;; Ensure only one load.
(defvar dict-line--cache-loaded-p nil
  "Non-nil if dict-line cache has been loaded in this Emacs session.
For `dict-line--load-cache'")

(defvar dict-line--cache (make-hash-table :test 'equal)
  "Hash table cache: word → definition.
For `dict-line--build-dict-cache'")

(defvar dict-line--audio-cache (make-hash-table :test 'equal)
  "Hash table cache: word → audio-path.
For `dict-line--build-audio-cache'")


;; ----------------------------
;; Display relevant
;; ----------------------------
(defun dict-line--dict-convert ()
  "dict-line convert dict txt to display."
  (setq dict-line-dict (replace-regexp-in-string "\\\\\\\\n" "\n" dict-line-dict))
  (setq dict-line-dict (replace-regexp-in-string "\"," "\" " dict-line-dict))
  (setq dict-line-dict (concat dict-line-word "\n" dict-line-dict))
  ;; (setq dict-line-dict (substring dict-line-dict 1 -2))
  )

(defun dict-line--message ()
  "Display translation with `message'."
  (dict-line--dict-convert)
  (when dict-line-dict
    (message "%s" dict-line-dict)))

(defun dict-line--posframe ()
  "Show translation in posframe."
  (dict-line--dict-convert)
  (when (and dict-line-dict (posframe-workable-p))
    (posframe-show dict-line--posframe-buffer
                   :string dict-line-dict
                   :max-width 50
                   :left-fringe 5
                   :right-fringe 5
                   :poshandler dict-line-posframe-location
                   :border-width 3
                   :border-color "#ed98cc")))

(defun dict-line--posframe-delete ()
  "Delete posframe."
  (when (eq dict-line-display #'dict-line--posframe)
    (if (fboundp 'posframe-hide)
        (posframe-hide dict-line--posframe-buffer))))

;; ----------------------------
;; Cache build/load
;; ----------------------------
(defun dict-line--build-dict-cache ()
  "Load all dictionary files into `dict-line--cache'.
Keys are stored in lowercase for case-insensitive matching."
  (clrhash dict-line--cache)
  (let ((files (directory-files dict-line-dict-directory t "\\.ts$")))
    (dolist (file files)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        ;; Exact match "word":"definition"
        (while (re-search-forward "\"\\([^\"[:space:]]+\\)\":\"\\([^\"]+\\)\"" nil t)
          (let ((word (downcase (match-string 1)))
                (def (match-string 2)))
            (puthash word def dict-line--cache)))))))

(defun dict-line--build-audio-cache ()
  "Scan audio root directory and build `dict-line--audio-cache'.
Keys are stored in lowercase for case-insensitive matching."
  (clrhash dict-line--audio-cache)
  (when (file-directory-p dict-line-audio-root-dir)
    (dolist (f (directory-files-recursively dict-line-audio-root-dir "\\.mp3$"))
      (let* ((word (downcase (file-name-base f))))
        (puthash word f dict-line--audio-cache)))))

(defun dict-line--save-cache ()
  "Persist cache to `dict-line-cache-file'."
  (with-temp-file dict-line-cache-file
    (insert ";; Auto-generated dict-line cache\n\n")
    (prin1 `(setq dict-line--cache ',dict-line--cache) (current-buffer))
    (insert "\n\n")
    (prin1 `(setq dict-line--audio-cache ',dict-line--audio-cache) (current-buffer))))

(defun dict-line--load-cache ()
  "Load cache from `dict-line-cache-file' if it exists.
Set `dict-line--cache-loaded-p' to t after successful load."
  (when (and (file-exists-p dict-line-cache-file)
             (not dict-line--cache-loaded-p))
    (load-file dict-line-cache-file)
    (setq dict-line--cache-loaded-p t)
    (message "[dict-line] Cache loaded from %s" dict-line-cache-file)))

(defun dict-line--ensure-cache-loaded ()
  "Ensure dictionary and audio caches are loaded.
Load once per session if cache file exists."
  (unless (or dict-line--cache-loaded-p
              (and (> (hash-table-count dict-line--cache) 0)
                   (> (hash-table-count dict-line--audio-cache) 0)))
    (if (file-exists-p dict-line-cache-file)
        (dict-line--load-cache)
      (message "[dict-line] No cache found. Please run `dict-line-build-cache`"))))

;;;###autoload
(defun dict-line-build-cache ()
  "Build dictionary and audio caches, then persist them."
  (interactive)
  (message "[dict-line] Cache building, please wait.....")
  (dict-line--build-dict-cache)
  (dict-line--build-audio-cache)
  (dict-line--save-cache)
  (message "[dict-line] Cache build successful. Saved to %s" dict-line-cache-file))


;; ----------------------------
;; Search/Play word/dict
;; ----------------------------
(defun dict-line--extract-word ()
  "Extract word or region."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (thing-at-point 'word t)))

(defun dict-line--search-word (word)
  "Lookup WORD in cache (case-insensitive, exact match)."
  (when word
    (setq dict-line-word word)
    (dict-line--ensure-cache-loaded)
    ;; Case-insensitive search
    (let ((key (downcase word)))
      (setq dict-line-dict (gethash key dict-line--cache)))
    (when dict-line-dict
      (funcall dict-line-display))
    (when dict-line-audio
      (dict-line--play-audio word))))

(defun dict-line--play-audio (word)
  "Play word audio if available (case-insensitive)."
  (let* ((key (downcase word))
         (file (gethash key dict-line--audio-cache)))
    (when (and file (file-exists-p file))
      (let ((args (append (split-string dict-line-audio-play-program-arg)
                          (list file))))
        ;; BUG: Windows playback audio can cause infinite looping, so kill after 1 second.
        (if (and (eq system-type 'windows-nt))
            (let ((proc (apply #'start-process "dict-line-audio" nil
                               dict-line-audio-play-program args)))
              (run-at-time "1 sec" nil (lambda (p) (when (process-live-p p) (kill-process p))) proc))
          (apply #'start-process "dict-line-audio" nil
                 dict-line-audio-play-program args))))))

;;;###autoload
(defun dict-line-get-dict ()
  "Main entry: lookup word at point."
  (interactive)
  (let ((word (dict-line--extract-word)))
    (when (and word (not (minibufferp)))
      (dict-line--search-word word))))

;; ----------------------------
;; Save personal dictionary.
;; ----------------------------
;;;###autoload
(defun dict-line-word-save-from-echo ()
  "Save word under cursor to personal dict file.
Save to `dict-line-dict-personal-file'"
  (interactive)
  (let* ((word (thing-at-point 'word t))
         (input (read-string (format "Enter information for '%s': " word)))
         (entry (format "\"%s\":\"%s\"," word input)))
    (when (and word input)
      (with-temp-buffer
        (insert-file-contents dict-line-dict-personal-file)
        (goto-char (point-max))
        (insert "\n" entry)
        (write-region (point-min) (point-max) dict-line-dict-personal-file))
      ;; save the word to cache
      (puthash word input dict-line--cache)
      (message "Saved %s" entry))))

;; ----------------------------
;; Minor mode
;; ----------------------------
(defvar-local dict-line--schedule-idle-timer nil)

(defun dict-line--schedule-search ()
  "Schedule dictionary lookup."
  (when (timerp dict-line--schedule-idle-timer)
    (cancel-timer dict-line--schedule-idle-timer))
  (setq dict-line--schedule-idle-timer
        (run-with-idle-timer dict-line-idle-time nil
                             #'dict-line-get-dict)))

;;;###autoload
(define-minor-mode dict-line-mode
  "Minor mode to look up words under the cursor."
  :lighter " Dict"
  :group 'dict-line
  (if dict-line-mode
      (progn
        (dict-line--ensure-cache-loaded)
        (add-hook 'post-command-hook #'dict-line--schedule-search nil t)
        (add-hook 'post-command-hook #'dict-line--posframe-delete nil t))
    (remove-hook 'post-command-hook #'dict-line--schedule-search t)
    (remove-hook 'post-command-hook #'dict-line--posframe-delete t)
    (when (timerp dict-line--schedule-idle-timer)
      (cancel-timer dict-line--schedule-idle-timer)
      (setq dict-line--schedule-idle-timer nil))))

(defun dict-line--enable-if-eligible ()
  (unless (minibufferp)
    (dict-line-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-dict-line-mode
  dict-line-mode dict-line--enable-if-eligible
  :group 'dict-line)

(provide 'dict-line)

