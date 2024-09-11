;; -*- coding: utf-8; -*-

(require 'async)

(defgroup dict-line nil
  "Emacs dictionary lookup on cursor movement."
  :group 'tools)

(defcustom dict-line-dict-directory "~/my-dict/"
  "The directory where .ts dictionary files are stored."
  :type 'directory
  :group 'dict-line)

(defcustom dict-line-audio nil
  "Toggle play audio file."
  :type 'boolean
  :group 'dict-line)

(defcustom dict-line-audio-root-dir "~/my-dict/my-audio/"
  "The directory where audio files are stored."
  :type 'directory
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

(defun dict-line--audio-play-async (word )
  "Play the audio file for WORD if it exists."
  (let* ((first-letter (upcase (substring word 0 1))) ;; Extract the first letter of word and convert to uppercase
         (audio-file (concat dict-line-audio-root-dir first-letter "/" word ".mp3")))
    (when (file-exists-p audio-file)
      (async-start
       `(lambda ()
          (call-process "mplayer" nil nil nil ,audio-file))
       ;; Processing after asynchronous tasks are completed
       (lambda (result)
         ;; (message "Played audio for: %s" dict-line-word)
         )
       ))))

(defun dict-line--get-dict-async ()
  "Check the word under cursor and look it up in the dictionary."
  (let ((word (thing-at-point 'word t))
        (dir dict-line-dict-directory)) ;; Extract dictionary directory
    (setq dict-line-word word)
    (when (and word (not (minibufferp)))
      (async-start
       `(lambda ()
          (let ((dict-files (directory-files ,dir t "\\.ts$"))
                (dicts nil))
            ;; Loop through dictionary files to find dicts for words
            (while (and dict-files (not dicts))
              (with-temp-buffer
                (insert-file-contents (car dict-files))
                (goto-char (point-min))
                (when (search-forward (concat "\"" ,word "\":") nil t)
                  (setq dicts (buffer-substring-no-properties (point) (line-end-position)))))
              (setq dict-files (cdr dict-files)))
            dicts)) ;; Return to dicts
       (lambda (dicts)
         (when dicts
           (setq dict-line-dict dicts)
           ;; To display
           (when (functionp dict-line-display)
             (funcall dict-line-display))
           ))
       )
      ;; Play audio
      (when dict-line-audio
        (dict-line--audio-play-async dict-line-word))
      )
    )
  )

(define-minor-mode dict-line-mode
  "Minor mode to look up words under the cursor asynchronously."
  :lighter " Dict Line"
  :group 'dict-line
  (if dict-line-mode
      (run-with-idle-timer dict-line-idle-time t 'dict-line--get-dict-async)
    (cancel-function-timers 'dict-line--get-dict-async)))

(provide 'dict-line)
