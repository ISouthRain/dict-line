;; -*- coding: utf-8; -*-

;;; dict-line --- View dict in Emacs.  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.
;; License: GPL-3.0-or-later

;; Author: ISouthRain
;; Version: 0.4
;; Package-Requires: ((emacs "24.2") (async "1.8"))
;; Keywords: dict sdcv
;; URL: https://github.com/ISouthRain/dict-line

;;; Commentary:
;;
;; This package is quickly view git blame information of the current file line in Emacs in real time.

;;; Code:
(require 'async)

(defgroup dict-line nil
  "Emacs dictionary lookup on cursor movement."
  :group 'tools)

(defcustom dict-line-dict-directory "~/my-dict/"
  "The directory where .ts dictionary files are stored."
  :type 'directory
  :group 'dict-line)

(defcustom dict-line-dict-personal-file "~/my-dict/my-dict.ts"
  "Personal dict file"
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

(defcustom dict-line-audio-play-program "mplayer"
  "Play audio file program.
List: `mplayer`, `mpg123`, `mpv`"
  :type 'string
  :group 'dict-line)

(defcustom dict-line-idle-time 0.5
  "Idle time in seconds before triggering dictionary lookup."
  :type 'number
  :group 'dict-line)

(defvar dict-line-word nil
  "dict-line point word.")

(defvar dict-line-dict nil
  "dict-line result dict txt.")

(defcustom dict-line-display #'dict-line--message
  "emsg-blame to display function."
  :type '(choice (const nil)
                 function)
  :group 'dict-line)

(defun dict-line--message ()
  "dict-line display function."
  (dict-line--dict-convert)
  (message dict-line-dict))

(defun dict-line--posframe ()
  "Show translation in the posframe"
  (dict-line--dict-convert)
  (when (posframe-workable-p)
    (posframe-show "*dict-line-posframe*"
                   :string dict-line-dict
                   :timeout 5
                   :max-width 30
                   :left-fringe 5
                   :right-fringe 5
                   :position (point)
                   :poshandler #'posframe-poshandler-frame-bottom-right-corner
                   :border-width 5;; 外边框大小
                   :border-color "#ed98cc" ;; 边框颜色
                   )
    )
  )

(defun dict-line--dict-convert ()
  "dict-line convert dict txt."
  (setq dict-line-dict (replace-regexp-in-string "\\\\\\\\n" "\n" dict-line-dict))
  (setq dict-line-dict (replace-regexp-in-string "\"," "\" " dict-line-dict))
  (setq dict-line-dict (substring dict-line-dict 1 -2))
  )

(defun dict-line--get-dict-async ()
  "Check the word under cursor and look it up in the dictionary asynchronously."
  (let ((word (if (use-region-p) ;; Check if there is a selected area
                  (buffer-substring-no-properties (region-beginning) (region-end)) ;; Use selected text
                (thing-at-point 'word t))) ;; Otherwise use the word under the cursor
        (dir dict-line-dict-directory)) ;; Extract dictionary directory
    (setq dict-line-word word)
    (when (and word (not (minibufferp)))
      (async-start
       `(lambda ()
          (let ((dict-files (directory-files ,dir t "\\.ts$"))
                (dicts nil))
            (while (and dict-files (not dicts))
              (with-temp-buffer
                (insert-file-contents (car dict-files))
                (goto-char (point-min))
                (when (search-forward (concat "\"" ,word "\":") nil t)
                  (setq dicts (buffer-substring-no-properties (point) (line-end-position)))))
              (setq dict-files (cdr dict-files)))
            dicts))
       ;; Callback
       (lambda (dicts)
         (when dicts
           (setq dict-line-dict dicts)
           (when (functionp dict-line-display)
             (funcall dict-line-display)))
         ;; Play audio
         (when dict-line-audio
           (let* ((first-letter (upcase (substring dict-line-word 0 1))) ;; Get the first letter of a word
                  (audio-file (concat dict-line-audio-root-dir first-letter "/" dict-line-word ".mp3"))
                  (program dict-line-audio-play-program))
             (when (file-exists-p audio-file)
               (let ((process (start-process "dict-line" nil program audio-file)))
                 ;; Automatically terminate playback after x seconds
                 (run-at-time "1 sec" nil #'kill-process process))))
           )
         ))
      ))
  )

;;;###autoload
(defun dict-line-word-save-from-echo ()
  "Extract the word under the cursor, prompt the user to enter information, and then save 'word': 'Input information' to the last line of the specified file."
  (interactive)
  (let* ((word (thing-at-point 'word t))
         (input (read-string (format "Enter information for '%s': " word)))
         (entry (format "\"%s\":\"%s\"," word input)))
    (when (and word input)
      (with-temp-buffer
        (insert-file-contents dict-line-dict-personal-file)
        (goto-char (point-max))
        (insert (concat "\n" entry))
        (write-region (point-min) (point-max) dict-line-dict-personal-file))
      (message "Save %s to %s" entry dict-line-dict-personal-file))))

;; TODO not completed
;;;###autoload
(define-minor-mode dict-line-mode
  "Minor mode to look up words under the cursor asynchronously."
  :lighter " "
  :group 'dict-line
  (if dict-line-mode
      (run-with-idle-timer dict-line-idle-time t 'dict-line--get-dict-async)
    (cancel-function-timers 'dict-line--get-dict-async)))

(provide 'dict-line)
