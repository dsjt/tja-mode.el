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

;;; Code:


(defvar tja-bpm-count-start-time nil
  "BPM")
(defvar tja-bpm-counting-num nil)

(define-derived-mode tja-mode nil "Tja" "tjaモード"
  (set (make-local-variable 'font-lock-multiline) t)
  (font-lock-add-keywords
   nil
   '(("\\(SUBTITLE\\|TITLE\\|LEVEL\\|BPM\\|WAVE\\|OFFSET\\|BALLOON\\|SONGVOL\\|SEVOL\\|SCOREINIT\\|SCOREDIFF\\|COURSE\\|STYLE\\|GAME\\|LIFE\\|DEMOSTART\\|SIDE\\)\\(:\\)\\(.+\\)"
      (1 'font-lock-constant-face t)
      (2 'default t)
      (3 'default t))
     ("\\(#BPMCHANGE\\|#MEASURE\\|#SCROLL\\|#DELAY\\) \\(.+\\)"
      (1 'font-lock-constant-face t)
      (2 'tja-change-number-face t))
     ("#\\(START\\|END\\|GOGOSTART\\|GOGOEND\\|BMSCROLL\\|HBSCROLL\\)" . font-lock-keyword-face)
     ("//.*" . font-lock-comment-face)
     ("[13]" . 'tja-dong-face)
     ("[24]" . 'tja-ka-face)
     ("\\([567][0 ]+\\)\\(8\\)"
      (1 'tja-renda-face)
      (2 'tja-renda-end-face))
     ("[0,]" . 'tja-rest-face)
     ))
  (define-key tja-mode-map (kbd "C-c C-l") 'tja-format-line)
  (define-key tja-mode-map (kbd "C-c C-h") 'tja-format-buffer)
  (define-key tja-mode-map (kbd "C-c C-j") 'tja-jfkd-mode))

(defun tja-format-line (&optional rhythm)
  "現在行を整列する。"
  (interactive "P")
  (or rhythm (setq rhythm 4))
  (save-excursion
    (let* ((cur-str (replace-regexp-in-string "[\s]" "" (buffer-substring (point-at-bol) (point-at-eol))))
           (split-num (ceiling (/ (1- (length cur-str)) (float rhythm))))
           (time 1)
           (base-str (substring cur-str 0 split-num))
           (start-char split-num))
      (when (string-match "^[0-9].+," cur-str)
        (setq cur-str (substring cur-str 0 (1- (length cur-str))))
        (while (< time rhythm)
          (setq base-str (concat base-str
                                 " "
                                 (substring cur-str
                                            (* split-num time)
                                            (min (length cur-str) (* split-num (+ time 1)))))
                time (1+ time)))
        (delete-region (point-at-bol) (point-at-eol))
        (insert (concat base-str ","))))))

(defun tja-format-buffer (&optional rhythm)
  "現在のバッファ全体を整列する"
  (interactive "P")
  (setq rhythm (or rhythm 4))
  (save-excursion
    (goto-char (point-min))
    (while (setq forward-num (string-match "^[0123456789 ]+," (buffer-substring (point) (point-max))))
      (forward-char forward-num)
      (tja-format-line rhythm)
      (goto-char (point-at-eol)))))

(defvar tja-jfkd-mode-map (make-sparse-keymap))
(define-minor-mode tja-jfkd-mode
  "tja-mode内でjfkdで入力を行うモード

jfkdで1と2の入力を行うことができ、yでBPMの計測ができる"
  :init-value nil
  :lighter " jfkd"
  :keymap 'tja-jfkd-mode-map
  (define-key tja-jfkd-mode-map "j" 'tja-insert-dong)
  (define-key tja-jfkd-mode-map "f" 'tja-insert-dong)
  (define-key tja-jfkd-mode-map "k" 'tja-insert-ka)
  (define-key tja-jfkd-mode-map "d" 'tja-insert-ka)
  (define-key tja-jfkd-mode-map "q" 'tja-jfkd-mode)
  (define-key tja-jfkd-mode-map "i" 'tja-format-buffer)
  (define-key tja-jfkd-mode-map "y" 'tja-bpm-count))

(defun tja-insert-dong ()
  (interactive)
  (insert "1"))
(defun tja-insert-ka ()
  (interactive)
  (insert "2"))


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


