;;; jstestmacs.el --- Front-end for JsTestDriver

;; Copyright (C) 2012 by Yuta Yamada

;; Author: Yuta Yamada <yamada@clear-code.com>
;; Version: 0.0.1

;;; Licence:
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; this program is frontend for JsTestDriver, execute test simply from Emacs.

;;;Usage
;; execute below command line
;; % java -jar JsTestDriver.jar --port 9876 --captureConsole --browser firefox
;; put below configuration to your .emacs
;; (add-to-list 'load-path "path/to/jstestmacs")
;; (require 'jstestmacs)
;; (setq jste-driver-dir "path/to/JsTestDriver.jar")
;; (add-hook 'js2-mode-hook
;;           '(lambda ()
;;              (local-set-key "\C-ct" 'jste-dwim)))
;;
;; types M-x jste-dwim, it's command that execute current file
;;
;; note:
;; It is necessary to put jsTestDriver.conf in directory on the test than

(eval-when-compile
  (require 'cl))

(defvar jste-driver-dir nil)
(defvar jste-test-name nil)
(defvar jste-config-path nil)
(defvar jste-output-buffer "*JsTestDriver Output*")

(defvar jste-command-alist
  '((?q . :quit)
    (?n . :next-line)
    (?p . :previous-line)))

(defun jste-control-command ()
  (setq other-window-scroll-buffer jste-output-buffer)
  (read-event)
  (case (assoc-default last-input-event jste-command-alist)
    (:next-line
     (jste-scroll-other-window :up))
    (:previous-line
     (jste-scroll-other-window :down))
    (:quit
     (kill-buffer jste-output-buffer))))

(defun jste-scroll-other-window (ctrl)
  (case ctrl
    (:up (scroll-other-window 1))
    (:down (scroll-other-window-down 1)))
  (jste-control-command))

(defun jste-dwim ()
  (interactive)
  (setq jste-config-path (jste-decide-conf-directory))
  (when (null jste-driver-dir)
    (jste-query :driverDir))
  (when (or (null jste-test-name) current-prefix-arg)
    (jste-query :testName))
  (jste-send-current-test))

(defun jste-decide-conf-directory ()
  (interactive)
  (let*
      ((dir-names (split-string buffer-file-truename "/"))
       config-path)
    (loop repeat (length dir-names) do
          (setq dir-names (jste-pop dir-names)
                config-path (jste-make-config-path dir-names))
          (if (file-exists-p config-path)
              (return config-path)))))

(defun jste-make-config-path (directorys)
  (concat (mapconcat 'identity directorys "/") "/jsTestDriver.conf"))

(defun jste-pop (list)
  (reverse (cdr (reverse list))))

(defun jste-from-symbol-to-string (symbol)
  (replace-regexp-in-string ":" "" (symbol-name symbol)))

(defun jste-query (&optional type)
  (let* ((header (jste-from-symbol-to-string type))
         (input (read-string (concat header " here: "))))
    (case type
      (:driverDir (setq jste-driver-dir input))
      (:testName (setq jste-test-name input)))))

(defun jste-send-current-test ()
  (let* ((file-name (jste-current-file-name))
         test-name result)
    (string-match "^test-\\([a-zA-Z-]+\\)\\.js$" file-name)
    (setq test-name (or jste-test-name (match-string 1 file-name))
          result (jste-fetch-result jste-driver-dir test-name))
    (jste-make-buffer result)
    (jste-control-command)))

(defun jste-current-file-name ()
  (let* ((split-dir-and-file-names (split-string buffer-file-truename "/")))
    (mapconcat 'identity (last split-dir-and-file-names) "")))

(defun jste-fetch-result (driver-dir test-name)
  (shell-command-to-string
   (concat "\\cd " driver-dir
           "; java -jar JsTestDriver.jar --tests "
           test-name " --verbose --captureConsole --config "
           jste-config-path)))

(defun jste-make-buffer(content)
  (let* ((basebuffer (current-buffer)))
    (with-temp-buffer
      (switch-to-buffer (get-buffer-create jste-output-buffer))
      (erase-buffer) ;;initialize
      (insert content))
    (switch-to-buffer basebuffer)
    (cond
     ((require 'popwin nil t)
      (popwin:popup-buffer
       (get-buffer-create "*JsTestDriver Output*")
       :noselect t :stick t :height 20 :position :top) t)
     (t (pop-to-buffer (get-buffer-create jste-output-buffer))))))

(provide 'jstestmacs)
