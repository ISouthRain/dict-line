;; -*- coding: utf-8; -*-

(defgroup dict-line nil
  "Show word translation in mode line."
  :group 'convenience)

(defcustom dict-line-dict-directory "~/dict"
  "The directory of dictionary files."
  :type 'directory
  :group 'dict-line)

(defcustom dict-line-idle-delay 1
  "The number of seconds of idle time to wait before showing translation."
  :type 'number
  :group 'dict-line)

(defcustom dict-line-icon "📚:"
  "Show icon"
  :type 'string
  :group 'dict-line)

(defcustom dict-line-more-icon " ↘️ "
  "And more Dict icon."
  :type 'string
  :group 'dict-line)

(defcustom dict-line-no-results-string "No Results"
  "No Results Dict."
  :type 'string
  :group 'dict-line)

(defcustom dict-line-dict-extension "ts"
  "Dict file extension name."
  :type 'string
  :group 'dict-line)

(defvar dict-line--timer nil
  "Timer that is set up when the last command finished.")

(defvar dict-line--translation nil
  "The translation to be shown in the modeline.")

(defun dict-line-cancel-timer ()
  "Cancel the current idle timer."
  (when dict-line--timer
    (cancel-timer dict-line--timer)
    (setq dict-line--timer nil)))

(defun dict-line-show-translation ()
  "Show translation in the modeline."
  (unless (minibufferp)
    (let* ((word (thing-at-point 'word t))
           (translation (when word (dict-line-get-translation word))))
      (if translation
          (progn
            ;; 筛检, 减少 \\n 出现
            (setq dict-line--translation (concat dict-line-icon (replace-regexp-in-string "\\\\\\\\n" dict-line-more-icon translation)))
            ;; TODO 继续筛检,  减少 \ 符号的出现
            (setq dict-line--translation (replace-regexp-in-string "\\\\" "" dict-line--translation))
            (force-mode-line-update))
        (setq dict-line--translation (concat dict-line-icon dict-line-no-results-string))
        (force-mode-line-update))
      )))

(defun dict-line-get-translation (word)
  "Get translation of the WORD from dictionary files."
  (let* ((dict-files (directory-files dict-line-dict-directory t (concat "\\." dict-line-dict-extension "$")))
         (translation nil))
    (while (and dict-files (not translation))
      (setq translation (dict-line-search-dict (car dict-files) word))
      (setq dict-files (cdr dict-files)))
    translation))

(defun dict-line-search-dict (dict-file word)
  "Search the WORD in the DICT-FILE and return its translation."
  (with-temp-buffer
    (insert-file-contents dict-file)
    (goto-char (point-min))
    (when (search-forward (concat "\"" word "\":") nil t)
      (buffer-substring-no-properties (point) (line-end-position)))))

(defun dict-line-set-timer ()
  "Set a timer to show translation when idle."
  (dict-line-cancel-timer)
  (setq dict-line--timer (run-with-idle-timer dict-line-idle-delay nil 'dict-line-show-translation)))

;;;###autoload
(define-minor-mode dict-line-mode
  "Minor mode to show word translation in mode line."
  :lighter " DictLine"
  :global nil
  (if dict-line-mode
      (progn
        (add-hook 'post-command-hook 'dict-line-set-timer nil t)
        (unless (memq 'dict-line--translation global-mode-string)
          (setq global-mode-string (append global-mode-string '("")))  ;; 添加一个空字符串
          (setq global-mode-string (append global-mode-string '(dict-line--translation)))))
    (remove-hook 'post-command-hook 'dict-line-set-timer t)
    (setq global-mode-string (remq 'dict-line--translation global-mode-string))))
;;;###autoload
(define-globalized-minor-mode global-dict-line-mode
  dict-line-mode
  (lambda () (dict-line-mode 1)))

(provide 'dict-line)
