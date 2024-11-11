;; -*- coding: utf-8; -*-

;;; dict-line --- View dict in Emacs.  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.
;; License: GPL-3.0-or-later

;; Author: ISouthRain
;; Version: 0.6
;; Package-Requires: ((emacs "24.2") (async "1.8") (posframe "1.0.0"))
;; Keywords: dict sdcv
;; URL: https://github.com/ISouthRain/dict-line

;;; Commentary:
;;
;; This package is quickly view git blame information of the current file line in Emacs in real time.

;;; Code:
(require 'async)
(require 'posframe)

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

(defvar dict-line--current-buffer nil
  "dict-line word current buffer name.")

(defvar dict-line--posframe-buffer "*dict-line-posframe*"
  "dict-line show dict txt buffer.")

(defcustom dict-line-posframe-location #'posframe-poshandler-point-bottom-left-corner
  "The location function for displaying the dict-line posframe.
Choose from a list of `posframe` position handlers to control where
the posframe appears relative to the frame, window, or point.
Sourcy for `posframe-show` (2) POSHANDLER:
1.  posframe-poshandler-frame-center
2.  posframe-poshandler-frame-top-center
3.  posframe-poshandler-frame-top-left-corner
4.  posframe-poshandler-frame-top-right-corner
5.  posframe-poshandler-frame-top-left-or-right-other-corner
6.  posframe-poshandler-frame-bottom-center
7.  posframe-poshandler-frame-bottom-left-corner
8.  posframe-poshandler-frame-bottom-right-corner
9.  posframe-poshandler-window-center
10. posframe-poshandler-window-top-center
11. posframe-poshandler-window-top-left-corner
12. posframe-poshandler-window-top-right-corner
13. posframe-poshandler-window-bottom-center
14. posframe-poshandler-window-bottom-left-corner
15. posframe-poshandler-window-bottom-right-corner
16. posframe-poshandler-point-top-left-corner
17. posframe-poshandler-point-bottom-left-corner
18. posframe-poshandler-point-bottom-left-corner-upward
19. posframe-poshandler-point-window-center
20. posframe-poshandler-point-frame-center"
  :type '(choice (const nil)
                 function)
  :group 'dict-line)


(defcustom dict-line-display #'dict-line--message
  "dict-line to display function."
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
    (posframe-show dict-line--posframe-buffer
                   :string dict-line-dict
                   :max-width 30
                   :left-fringe 5
                   :right-fringe 5
                   :poshandler dict-line-posframe-location
                   :border-width 5;; 外边框大小
                   :border-color "#ed98cc" ;; 边框颜色
                   )
    )
  )

(defun dict-line--posframe-delete ()
  "Delete the posframe associated with BUFFER if it exists."
  (when (eq dict-line-display #'dict-line--posframe)
    (posframe-hide dict-line--posframe-buffer))
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
        (buffer (get-buffer (buffer-name)))
        (dir dict-line-dict-directory)) ;; Extract dictionary directory
    (setq dict-line-word word)
    (setq dict-line--current-buffer buffer) ;; Need to query the word buffer
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
           (with-current-buffer (get-buffer-create dict-line--current-buffer)
             (when (functionp dict-line-display)
               (funcall dict-line-display)))
           )
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
      (progn
      (run-with-idle-timer dict-line-idle-time t 'dict-line--get-dict-async)
      (add-hook 'post-command-hook #'dict-line--posframe-delete nil nil)
      )
    (cancel-function-timers 'dict-line--get-dict-async)
    (remove-hook 'post-command-hook #'dict-line--posframe-delete nil nil)))

(provide 'dict-line)
