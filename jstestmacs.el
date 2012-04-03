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
      ((file-name (split-string buffer-file-truename "/"))
       (dir-name file-name)
       (to-string '(lambda (name) (mapconcat 'identity name "/")))
       (assign-config-path '(lambda (directorys)
                              (concat (mapconcat 'identity directorys "/")
                                      "/jsTestDriver.conf")))
       jsTestConfPath)
    (loop repeat (length dir-name) do
          (setq dir-name (jste-pop dir-name)
                jsTestConfPath (funcall assign-config-path dir-name))
          (if (file-exists-p jsTestConfPath)
              (return jsTestConfPath)))))

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
  (interactive)
  (let* ((to-string '(lambda (name) (mapconcat 'identity name "")))
         (filename (funcall to-string (last
                                       (split-string buffer-file-truename "/"))))
         testname command result)
    (string-match "^test-\\([a-zA-Z-]+\\)\\.js$" filename)
    (setq testname (or jste-test-name (match-string 1 filename))
          command
          (concat "\\cd " jste-driver-dir
                  "; java -jar JsTestDriver.jar --tests "
                  testname " --verbose --captureConsole --config "
                  jste-config-path)
          result (shell-command-to-string command))
    (jste-make-buffer result)
    (jste-control-command)))

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
