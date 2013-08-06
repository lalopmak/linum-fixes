
;;  Copyright (C) 2013

;;  This program is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.

;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.

;;  You should have received a copy of the GNU General Public License
;;  along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;;;;fixes some linum bugs
(defun linum-update-window (win)
  "Update line numbers for the portion visible in window WIN."
  ;;bugfix for "first line updating"
  (goto-char (let ((first-char (window-start win)))
               (if (> first-char (point-min))
                   (1- first-char)
                 first-char)))
  (let ((line (line-number-at-pos))
        ;;sometimes window-end is buggy and only goes to half the window.
        ;; accounts for that by always going 2x ahead
        (limit (let* ((last-char (window-end win t))
                      (lines-in-window (count-lines (window-start win) last-char)))
                 (save-excursion (goto-char last-char)
                                 (forward-line lines-in-window)
                                 (point))))
        (fmt (cond ((stringp linum-format) linum-format)
                   ((eq linum-format 'dynamic)
                    (let ((w (length (number-to-string
                                      (count-lines (point-min) (point-max))))))
                      (concat "%" (number-to-string w) "d")))))
        (width 0))
    (run-hooks 'linum-before-numbering-hook)
    ;; Create an overlay (or reuse an existing one) for each
    ;; line visible in this window, if necessary.
    (while (and (not (eobp)) (<= (point) limit))
      (let* ((str (if fmt
                      (propertize (format fmt line) 'face 'linum)
                    (funcall linum-format line)))
             (visited (catch 'visited
                        (dolist (o (overlays-in (point) (point)))
                          (when (string= (overlay-get o 'linum-str) str)
                            (unless (memq o linum-overlays)
                              (push o linum-overlays))
                            (setq linum-available (delete o linum-available))
                            (throw 'visited t))))))
        (setq width (max width (length str)))
        (unless visited
          (let ((ov (if (null linum-available)
                        (make-overlay (point) (point))
                      (move-overlay (pop linum-available) (point) (point)))))
            (push ov linum-overlays)
            (overlay-put ov 'before-string
                         (propertize " " 'display `((margin left-margin) ,str)))
            (overlay-put ov 'linum-str str))))
      (forward-line)
      (setq line (1+ line)))
    (set-window-margins win width)))

(provide 'linum-fixes)
