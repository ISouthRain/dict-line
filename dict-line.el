;;; dict-line.el --- View dict in Emacs.  -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.
;; License: GPL-3.0-or-later

;; Author: ISouthRain
;; Version: 0.7.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: dict
;; URL: https://github.com/ISouthRain/dict-line

;;; Commentary:
;;
;; This package is quickly view word dictionary.

;;; Code:

(defgroup dict-line nil
  "Emacs dictionary lookup on cursor movement."
  :group 'tools)

(defcustom dict-line-dict-directory
  (expand-file-name "dict/" (file-name-directory load-file-name))
  "The directory where .ts dictionary files are stored."
  :type 'directory
  :group 'dict-line)

(defcustom dict-line-dict-personal-file
  (expand-file-name "dict/Mydict.ts" (file-name-directory load-file-name))
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
  (expand-file-name ".dict-line-cache.sqlite" user-emacs-directory)
  "SQLite database file used to persist dict-line cache across Emacs sessions."
  :type 'file
  :group 'dict-line)

(defcustom dict-line-idle-time 0.5
  "Idle time in seconds before triggering dictionary lookup."
  :type 'number
  :group 'dict-line)

(defcustom dict-line-audio-play-program (or (executable-find "mplayer")
                                            (executable-find "mpv")
                                            (executable-find "mpg123"))
  "Play audio file program."
  :type 'string
  :group 'dict-line)

(defcustom dict-line-audio-play-program-arg ""
  "Audio play program arguments."
  :type 'string
  :group 'dict-line)

(defcustom dict-line-display #'dict-line--message
  "dict-line display function."
  :type '(choice (const nil) function)
  :group 'dict-line)

(defcustom dict-line-posframe-location #'posframe-poshandler-point-bottom-left-corner
  "The location function for displaying the dict-line posframe.
Option `posframe-show' => (2) POSHANDLER"
  :type '(choice (const nil) function)
  :group 'dict-line)

(defvar-local dict-line--timer nil)

(defvar dict-line--posframe-buffer "*dict-line-posframe*")

(defvar dict-line-word nil
  "Recently searched words.
From `dict-line--extract-word'")

(defvar dict-line-dict nil
  "Recent dictionary lookup results.
From `dict-line--search-word'")

(defvar dict-line--cache-db nil
  "SQLite connection used by dict-line cache.")

(defconst dict-line--sqlite-batch-size 500
  "Number of rows written per sqlite batch during cache rebuilds.")


(defun dict-line--dict-convert ()
  "dict-line convert dict txt to display."
  (setq dict-line-dict (replace-regexp-in-string "\\\\\\\\n" "\n" dict-line-dict))
  (setq dict-line-dict (replace-regexp-in-string "\"," "\" " dict-line-dict))
  (setq dict-line-dict (concat dict-line-word "\n" dict-line-dict)))

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

(defun dict-line--display-delete ()
  "Delete posframe."
  (when (eq dict-line-display #'dict-line--posframe)
    (if (fboundp 'posframe-hide)
        (posframe-hide dict-line--posframe-buffer))))

(defun dict-line--sqlite-available-check ()
  "Signal an error when Emacs SQLite support is unavailable."
  (unless (sqlite-available-p)
    (user-error "[dict-line] Current Emacs was built without sqlite support")))

(defun dict-line--ensure-cache-db ()
  "Open sqlite cache and ensure required tables exist."
  (dict-line--sqlite-available-check)
  (unless (sqlitep dict-line--cache-db)
    (make-directory (file-name-directory dict-line-cache-file) t)
    (setq dict-line--cache-db (sqlite-open dict-line-cache-file))
    (sqlite-execute
     dict-line--cache-db
     "CREATE TABLE IF NOT EXISTS dict_cache (
        word TEXT PRIMARY KEY,
        definition TEXT NOT NULL
      )")
    (sqlite-execute
     dict-line--cache-db
     "CREATE TABLE IF NOT EXISTS audio_cache (
        word TEXT PRIMARY KEY,
        path TEXT NOT NULL
      )"))
  dict-line--cache-db)

(defun dict-line--close-cache-db ()
  "Close sqlite cache connection."
  (when (sqlitep dict-line--cache-db)
    (sqlite-close dict-line--cache-db)
    (setq dict-line--cache-db nil)))

(defun dict-line--cache-exists-p ()
  "Return non-nil when the sqlite cache file exists."
  (file-exists-p dict-line-cache-file))

(defun dict-line--sqlite-first-column (rows)
  "Return the first column from ROWS."
  (let ((row (car rows)))
    (cond
     ((vectorp row) (aref row 0))
     ((consp row) (car row))
     (t nil))))

(defun dict-line--sqlite-batch-values (rows)
  "Flatten ROWS into a single vector for sqlite parameter binding."
  (apply #'vconcat rows))

(defun dict-line--sqlite-flush-batch (db table value-column rows)
  "Insert ROWS into TABLE using VALUE-COLUMN in a single statement."
  (when rows
    (let ((placeholders (mapconcat (lambda (_) "(?, ?)") rows ", ")))
      (sqlite-execute
       db
       (format "INSERT OR REPLACE INTO %s(word, %s) VALUES %s"
               table value-column placeholders)
       (dict-line--sqlite-batch-values rows)))))

(defun dict-line--sqlite-call-with-fast-bulk-mode (db fn)
  "Call FN with DB configured for bulk writes in a single transaction."
  (let* ((old-synchronous (dict-line--sqlite-first-column
                           (sqlite-select db "PRAGMA synchronous")))
         (old-temp-store (dict-line--sqlite-first-column
                          (sqlite-select db "PRAGMA temp_store")))
         (committed nil))
    (unwind-protect
        (progn
          (sqlite-execute db "PRAGMA synchronous = OFF")
          (sqlite-execute db "PRAGMA temp_store = MEMORY")
          (sqlite-execute db "BEGIN IMMEDIATE")
          (funcall fn)
          (sqlite-execute db "COMMIT")
          (setq committed t))
      (unless committed
        (ignore-errors (sqlite-execute db "ROLLBACK")))
      (ignore-errors
        (sqlite-execute db (format "PRAGMA synchronous = %s" old-synchronous)))
      (ignore-errors
        (sqlite-execute db (format "PRAGMA temp_store = %s" old-temp-store))))))

(defun dict-line--sqlite-get-dict (word)
  "Return definition for WORD from sqlite cache."
  (dict-line--sqlite-first-column
   (sqlite-select
    (dict-line--ensure-cache-db)
    "SELECT definition FROM dict_cache WHERE word = ?"
    (vector word))))

(defun dict-line--sqlite-get-audio (word)
  "Return audio path for WORD from sqlite cache."
  (dict-line--sqlite-first-column
   (sqlite-select
    (dict-line--ensure-cache-db)
    "SELECT path FROM audio_cache WHERE word = ?"
    (vector word))))

(defun dict-line--build-dict-cache ()
  "Load all dictionary files into sqlite cache.
Keys are stored in lowercase for case-insensitive matching."
  (let ((db (dict-line--ensure-cache-db)))
    (dict-line--sqlite-call-with-fast-bulk-mode
     db
     (lambda ()
       (sqlite-execute db "DELETE FROM dict_cache")
       (let ((files (directory-files dict-line-dict-directory t "\\.ts$"))
             (rows nil)
             (row-count 0))
         (dolist (file files)
           (with-temp-buffer
             (insert-file-contents file)
             (goto-char (point-min))
             ;; Exact match "word":"definition"
             (while (re-search-forward "\"\\([^\"[:space:]]+\\)\":\"\\([^\"]+\\)\"" nil t)
               (push (vector (downcase (match-string 1))
                             (match-string 2))
                     rows)
               (setq row-count (1+ row-count))
               (when (>= row-count dict-line--sqlite-batch-size)
                 (dict-line--sqlite-flush-batch
                  db "dict_cache" "definition" (nreverse rows))
                 (setq rows nil
                       row-count 0)))))
         (dict-line--sqlite-flush-batch
          db "dict_cache" "definition" (nreverse rows)))))))

(defun dict-line--build-audio-cache ()
  "Scan audio root directory and build sqlite audio cache.
Keys are stored in lowercase for case-insensitive matching."
  (let ((db (dict-line--ensure-cache-db)))
    (dict-line--sqlite-call-with-fast-bulk-mode
     db
     (lambda ()
       (sqlite-execute db "DELETE FROM audio_cache")
       (when (file-directory-p dict-line-audio-root-dir)
         (let ((rows nil)
               (row-count 0))
           (dolist (file (directory-files-recursively dict-line-audio-root-dir "\\.mp3$"))
             (push (vector (downcase (file-name-base file)) file) rows)
             (setq row-count (1+ row-count))
             (when (>= row-count dict-line--sqlite-batch-size)
               (dict-line--sqlite-flush-batch
                db "audio_cache" "path" (nreverse rows))
               (setq rows nil
                     row-count 0)))
           (dict-line--sqlite-flush-batch
            db "audio_cache" "path" (nreverse rows))))))))

(defun dict-line--save-cache ()
  "Ensure the sqlite cache database is initialized."
  (dict-line--ensure-cache-db))

(defun dict-line--load-cache ()
  "Open cache from `dict-line-cache-file' if it exists."
  (when (dict-line--cache-exists-p)
    (dict-line--ensure-cache-db)
    (message "[dict-line] Cache loaded from %s" dict-line-cache-file)))

(defun dict-line--cache-loaded-check ()
  "Ensure sqlite cache is ready for lookups."
  (unless (sqlitep dict-line--cache-db)
    (if (dict-line--cache-exists-p)
        (dict-line--load-cache)
      (message "[dict-line] No cache found. Please run `dict-line-build-cache`"))))

;;;###autoload
(defun dict-line-build-cache ()
  "Build dictionary and audio caches, then persist them."
  (interactive)
  (message "[dict-line] Dict Cache building, please wait.....")
  (dict-line--build-dict-cache)
  (dict-line--build-audio-cache)
  (dict-line--save-cache)
  (message "[dict-line] Dict Cache build successful. Saved to %s" dict-line-cache-file))

(defun dict-line--extract-word ()
  "Extract word or region."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (thing-at-point 'word t)))

(defun dict-line--search-word (word)
  "Lookup WORD in cache (case-insensitive, exact match)."
  (when word
    (setq dict-line-word word)
    (dict-line--cache-loaded-check)
    (setq dict-line-dict
          (and (sqlitep dict-line--cache-db)
               (dict-line--sqlite-get-dict (downcase word))))
    (when dict-line-dict
      (funcall dict-line-display))
    (when dict-line-audio
      (dict-line--play-audio word))))

(defun dict-line--play-audio (word)
  "Play word audio if available (case-insensitive)."
  (let ((file (and (sqlitep dict-line--cache-db)
                   (dict-line--sqlite-get-audio (downcase word)))))
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
      (sqlite-execute
       (dict-line--ensure-cache-db)
       "INSERT OR REPLACE INTO dict_cache(word, definition) VALUES (?, ?)"
       (vector (downcase word) input))
      (message "Saved %s" entry))))

(defun dict-line--post-command-hook ()
  "Schedule dictionary lookup after idle."
  (when dict-line--timer
    (cancel-timer dict-line--timer))
  (setq dict-line--timer
        (run-with-idle-timer
         dict-line-idle-time nil
         #'dict-line-get-dict)))

;;;###autoload
(define-minor-mode dict-line-mode
  "Minor mode to look up words under the cursor."
  :lighter " Dict"
  :group 'dict-line
  (if dict-line-mode
      (if (minibufferp)
          (setq dict-line-mode nil)
        (dict-line--cache-loaded-check)
        (add-hook 'post-command-hook #'dict-line--post-command-hook nil t)
        (add-hook 'pre-command-hook #'dict-line--display-delete nil t))
    (remove-hook 'post-command-hook #'dict-line--post-command-hook t)
    (remove-hook 'pre-command-hook #'dict-line--display-delete t)
    (when (timerp dict-line--timer)
      (cancel-timer dict-line--timer)
      (setq dict-line--timer nil))))

;;;###autoload
(define-globalized-minor-mode global-dict-line-mode
  dict-line-mode
  dict-line-mode
  :group 'dict-line)

(add-hook 'kill-emacs-hook #'dict-line--close-cache-db)

(provide 'dict-line)
