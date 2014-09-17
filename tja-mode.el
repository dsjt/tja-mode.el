;;; tja-mode.el --- define tja-mode for editting taiko_sanjiro score

;; Copyright (C) 2014  

;; Author:  dsjt
;; Keywords: faces

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This program provides a major mode for editting ".tja" file. ".tja"
;; file is for taiko sanjiro.

;;; Install

;; (require 'tja-mode)

;; Attention

;; key-chordモードを使用している場合、tja-traceモードは正常に機能しません。なので、(key-chord-mode -1) を評価するなどして、モードを切ってください。
;; 

;;; Code:

(require 'cl)

(defvar tja-bpm-count-start-time nil
  "BPM")
(defvar tja-bpm-counting-num nil)
(defvar tja-trace-list nil)
(defvar tja-bpm nil)
(defvar tja-timer nil)
(defvar tja-trace-division 16
  "")
(defvar tja-trace-rhythm 4
  "")
(defvar tja-trace-progress-flag nil
  "現在、トレースが進行中であるかどうかを示します。")
(defvar tja-trace-bar-num 0
  "現在トレースしている小節番号")
(defvar tja-trace-bar nil)
(defvar tja-trace-bar-time nil)
(defvar tja-trace-gap nil)
(defvar tja-trace-conf-flag nil
  "適切なBPMの設定など、トレースの設定が整っているかどうかを示します。")
(defvar tja-hist nil)
(defvar tja-bpm-init nil)
(defvar tja-forward-num)
(defvar tja-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-l") 'tja-partition-line)
    (define-key map (kbd "C-c C-h") 'tja-partition-buffer)
    (define-key map (kbd "C-c C-j") 'tja-trace-mode)
    (define-key map (kbd "C-c C-q") 'tja-fill-region)
    (define-key map (kbd "C-c C-M-p") 'tja-start)
    (define-key map (kbd "C-c C-n") ' tja-numbering-buffer)
    map))
(defvar tja-trace-mode-map

  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "j") 'tja-trace-dong)
    (define-key map (kbd "f") 'tja-trace-dong)
    (define-key map (kbd "k") 'tja-trace-ka)
    (define-key map (kbd "d") 'tja-trace-ka)
    (define-key map (kbd "q") 'tja-trace-quit)
    (define-key map (kbd "y") 'tja-bpm-count)
    (define-key map (kbd "c") 'tja-confirm-bpm)
    map))
(defvar taiko-program ""
  "taikojiro.exeのアドレスを指定。\"~/Taiko/taikojiro/exe\"など。")

(defvar tja-comment-prefix "//"
  "Tja comment prefix.")

(defun tja-partition-line (&optional rhythm)
  "現在行を整列する。"
  (interactive "P")
  (if rhythm
      (setq tja-trace-rhythm rhythm)
    (setq rhythm tja-trace-rhythm))
  (save-excursion
    (let ((str-beg (point-at-bol))
          (str-end (search-forward ",")))
      (let* ((str
              (replace-regexp-in-string "[ \s]" ""
                                        (buffer-substring-no-properties str-beg
                                                                        str-end)))
             (sp-num (floor (/ (1- (length str)) rhythm))))
        (delete-region str-beg str-end)
        (insert str)
        (if (not (= sp-num 0))
            (progn
              (goto-char str-beg)
              (loop repeat (1- rhythm) do
                    (progn (goto-char (+ sp-num (point)))
                           (insert " ")))))))))

(defun tja-partition-buffer (&optional rhythm)
  "現在のバッファ全体を整列する"
  (interactive "P")
  (setq rhythm (or rhythm 4))
  (save-excursion
    (goto-char (point-min))
    (while (setq tja-forward-num (string-match "^[0123456789 ]+," (buffer-substring (point) (point-max))))
      (forward-char tja-forward-num)
      (tja-partition-line tja-trace-rhythm)
      (goto-char (point-at-eol)))))

(defun tja-fill-region (beg end)
  (interactive "r")
  (save-excursion
    (let ((str (buffer-substring-no-properties beg end)))
      (kill-region beg end)
      (insert (replace-regexp-in-string "\\([^a-z]\\) \\([0-9,]\\)" "\\1\\2" str)))))

(defun tja-numbering-buffer (&optional interval)
  (interactive "P")
  (if (null interval)
      (setq interval tja-numbering-interval)
    (setq tja-numbering-interval interval))
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "#START")
    (let ((counter 1)
          flag)
      (while (re-search-forward "[0-9 ]*,[ \s]*" nil t)
        (if (equal "//" (thing-at-point 'symbol))
            (progn
              (kill-region (point) (point-at-eol))
              (just-one-space)))
        (if (and (eolp) flag)
            (progn
              (insert (format "// %d" counter))
              (setq flag nil))
          (and (= (% counter interval) (1- interval)) (setq flag t)))
        (setq counter (1+ counter))))))

(defun tja-goto-bar (&optional bar)
  (interactive "N:")
  (goto-char (point-min))
  (loop repeat bar do
        (re-search-forward "^[0-9 \n]+,"))
  (forward-line 0))

(defvar tja-numbering-interval 4)

(defun tja-set-timer ()
  (setq tja-timer
        (run-with-timer (- tja-trace-bar tja-trace-gap)
                        tja-trace-bar
                        ;; nil
                        'tja-trace)))

(defun tja-trace ()
  (let ((end (current-time))
        (tl tja-trace-list))
    (setq tja-trace-list nil)
    (let ((beg (subtract-time end tja-trace-bar-time)))
      (tja-trace-insert (tja-trace-calc beg end tl)))))

(defun tja-trace-insert (list)
  (dolist (x list)
    (insert (number-to-string x)))
  (insert ",\n"))

(defun tja-trace-start ()
  (if tja-trace-conf-flag
      (progn (setq tja-trace-progress-flag t)
             (tja-set-timer))
    (message "不適切なBPMです。 BPM %s" tja-bpm)))

(defun tja-trace-calc (beg end tl)
  "return vect"
  (let ((output ())
        (tl (nreverse (cons (cons end 0) tl))) ;最初に(end . 0)を
                                        ;くっつけて、反転
        (last-x (cons beg 0))) 
    (dolist (x tl)
      (let ((sub (- (float-time (car x))
                    (float-time (car last-x)))))
        (cond ((< sub 0) t)
              ((= (cdr last-x) 0) (push (cdr x) output)) ;(cons beg 0)への対応
              ((= (cdr x) 0)
               (loop for x from 1 to (- tja-trace-division (length output))
                     do (push 0 output))) ;(cond end 0)への対応
              ((< sub (* tja-trace-gap 1))
               (let ((x-note (cdr x))
                     (l-note (cdr last-x)))
                 (cond ((and (= x-note l-note) (= x-note 1))
                        (setq output (cons 3 (cdr output))))
                       ((and (= x-note l-note) (= x-note 2))
                        (setq output (cons 4 (cdr output))))
                       (t t))                ;後々、連打とみなすべきかも
                 ))
              (t
               (let ((num (floor (/ (- sub (* tja-trace-gap 1)) (* tja-trace-gap 2.0)))))
                 (loop for x from 1 to num
                       do (push 0 output))
                 (push (cdr x) output))))) 
      (setq last-x x))
    (reverse output)))

(defun tja-trace-order-num (beg end time)
  (let ((bar (float-time (subtract-time end beg)))
        (pro (float-time (subtract-time time beg))))
    (floor (* (/ pro bar) tja-trace-division))))

(defun tja-trace-quit ()
  (interactive)
  (and tja-timer (cancel-timer tja-timer))
  (setq tja-trace-list nil)
  (if tja-trace-progress-flag
      (progn (setq tja-trace-progress-flag nil)
             (setq tja-trace-bar-num 0))
    (tja-trace-mode -1)))

(defun tja-trace-dong ()
  (interactive)
  (or tja-trace-progress-flag (tja-trace-start))
  (add-to-list 'tja-trace-list (cons (current-time) 1)))

(defun tja-trace-ka ()
  (interactive)
  (or tja-trace-progress-flag (tja-trace-start))
  (add-to-list 'tja-trace-list (cons (current-time) 2)))

(defun tja-confirm-bpm ()
  (interactive)
  (tja-auto-bpm-conf)
  (setq tja-bpm (string-to-number (read-string "BPM:" tja-bpm-init 'tja-hist nil)))
  (if (> tja-bpm 0)
      (tja-trace-conf)
      (progn (setq tja-trace-conf-flag nil)
           (message "不適切なBPMです。"))))

(defun tja-auto-bpm-conf ()
  (save-excursion
    (goto-char (point-min))
    (and (re-search-forward "\\(^BPM:\\)\\([ 0-9]+$\\)")
         (setq tja-bpm-init (match-string 2))
         (setq tja-bpm (string-to-number tja-bpm-init))
         (tja-trace-conf))))

(defun tja-trace-conf ()
  (setq tja-trace-conf-flag t
        tja-trace-bar (* (/ 60.0 tja-bpm) tja-trace-rhythm)
        tja-trace-bar-time (seconds-to-time tja-trace-bar)
        tja-trace-gap (/ (/ tja-trace-bar tja-trace-division) 2.0)))

(defun tja-bpm-count ()
  "BPMの計測を行うコマンド。

yを1拍子1打打つと、ミニバッファにBPMの予想値が表示される"
  (interactive)
  (cond ((eq last-command this-command)
         (progn (setq tja-bpm-counting-num (1+ tja-bpm-counting-num))
                (let* ((ct (current-time))
                       (sec (- (nth 1 ct) (nth 1 tja-bpm-count-start-time)))
                       (msec (/ (- (nth 2 ct) (nth 2 tja-bpm-count-start-time))
                                1000000.0))
                       (bpm (* 60.0 (/ tja-bpm-counting-num (+ sec msec)))))
                  (message (format "BPM: %f " bpm)))))
        (t
         (progn (message "START")
                (setq tja-bpm-count-start-time (current-time)
                      tja-bpm-counting-num 0)))))

(defun tja-start ()
  "現在の譜面で太鼓さん次郎を起動する。
使用するには、使用するtaikojiro.exeのアドレスをtaiko-sanjiro-programに格納する必要があります。"
  (interactive)
  (start-process "taiko" "*taiko*" taiko-program (replace-regexp-in-string "/" "\\\\" buffer-file-name)))

;; define mode

(define-derived-mode tja-mode nil "Tja" "tjaモード"
  (set (make-local-variable 'font-lock-multiline) t)
  (font-lock-add-keywords
   nil
   '(
     ("\\(SUBTITLE\\|TITLE\\|LEVEL\\|BPM\\|WAVE\\|OFFSET\\|BALLOON\\|SONGVOL\\|SEVOL\\|SCOREINIT\\|SCOREDIFF\\|COURSE\\|STYLE\\|GAME\\|LIFE\\|DEMOSTART\\|SIDE\\|SCOREMODE\\)\\(:\\)\\(.+\\)"
      (1 'font-lock-constant-face nil)
      (2 'default nil)
      (3 'default nil))
     ("\\(#BPMCHANGE\\|#MEASURE\\|#SCROLL\\|#DELAY\\) ??\\(.+\\)"
      (1 'font-lock-constant-face t)
      (2 'tja-change-number-face t))
     ("#\\(START\\|END\\|GOGOSTART\\|GOGOEND\\|BMSCROLL\\|HBSCROLL\\)" . font-lock-keyword-face)
     ("[13]" . 'tja-dong-face)
     ("[24]" . 'tja-ka-face)
     ("\\([567][0 ]*,?\n*[0 ]*\\)\\(8\\)"
      (1 'tja-renda-face)
      (2 'tja-renda-end-face))
     ("[0,]" . 'tja-rest-face)
     ("\\(//.*\\)"
      (0 'font-lock-comment-face t))))
  (setq comment-start tja-comment-prefix))

(define-minor-mode tja-trace-mode
  ""
  :keymap tja-trace-mode-map
  :init-value nil
  :lighter " trace"
  (setq tja-trace-progress-flag nil)
  (setq tja-trace-list nil)
  (if tja-trace-mode
      (or tja-bpm (tja-auto-bpm-conf))))

;; face
(defface tja-dong-face
  '((t (:foreground "orangered")))
  "face of dong sign"
  :group 'tja)
(defface tja-ka-face
  '((t (:foreground "aquamarine")))
  "face of ka sign"
  :group 'tja)
(defface tja-rest-face
  '((t (:foreground "darkgray")))
  "face of ka sign"
  :group 'tja)
(defface tja-change-number-face
  '((t (:foreground "yellow")))
  "face of ka sign"
  :group 'tja)
(defface tja-renda-face
  '((t (:foreground "red")))
  "face of ka sign"
  :group 'tja)
(defface tja-renda-end-face
  '((t (:foreground "maroon")))
  "face of ka sign"
  :group 'tja)

(provide 'tja-mode)
;;; tja-mode.el ends here


