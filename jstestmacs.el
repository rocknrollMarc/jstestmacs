;;; jstestmacs.el --- Front-end for JsTestDriver

;; Copyright (C) 2012 by Yuta Yamada

;; Author: Yuta Yamada <cokesboy@gmail.com>
;; URL: https://github.com/yuutayamada/jstestmacs
;; Version: 0.3.0
;; Package-Requires:  ((popwin "20120529"))
;; Keywords: test, javascript

;;; License:
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
;; This program is frontend for JsTestDriver, It can be execute test
;; simply from Emacs.

;;; Usage
;; Before setup your .emacs, set below:
;; (add-to-list 'load-path "path/to/jstestmacs")
;; (setq jstestmacs-driver-dir "~/directory where there are JsTestDriver.jar/")
;; (require 'jstestmacs)
;; (add-hook 'js2-mode-hook
;;           '(lambda ()
;;              (local-set-key "\C-ct" 'jstestmacs-dwim)))
;;
;; After the end of the preparation, you can execute JsTestDriver with push
;; "C-c t" when js2-mode.
;; Of course, require JsTestdriver and browser. :)
;; Note:
;; jsTestDriver.conf is detected automatically by recursive traverse
;; directory to above.

(eval-when-compile
  (require 'cl))

(defcustom jstestmacs-test-browser "firefox"
  "assign your browser"
  :group 'jstestmacs
  :type 'string)

(defcustom jstestmacs-port "9876"
  "port number"
  :group 'jstestmacs
  :type 'string)

(defcustom jstestmacs-driver-dir "~/lib/"
  "set directory where there are JsTestDriver.jar"
  :group 'jstestmacs
  :type 'string)

(defvar jstestmacs-test-name nil)
(defvar jstestmacs-config-path nil)
(defvar jstestmacs-output-buffer "*JsTestDriver Output*")

(defvar jstestmacs-command-alist
  '((?q . :quit)
    (?n . :next-line)
    (?p . :previous-line)))

(defun jstestmacs-control-command ()
  (setq other-window-scroll-buffer jstestmacs-output-buffer)
  (read-event)
  (case (assoc-default last-input-event jstestmacs-command-alist)
    (:next-line
     (jstestmacs-scroll-other-window :up))
    (:previous-line
     (jstestmacs-scroll-other-window :down))
    (:quit
     (kill-buffer jstestmacs-output-buffer))))

(defun jstestmacs-scroll-other-window (manipulation)
  (case manipulation
    (:up (scroll-other-window 1))
    (:down (scroll-other-window-down 1)))
  (jstestmacs-control-command))

(defun jstestmacs-driver-live-p ()
  (let ((result (shell-command-to-string
                 "\\ps -fU`whoami` --forest | grep JsTestDriver")))
    (if (string-match "java -jar JsTestDriver" result)
        t
      nil)))

(defun jstestmacs-boot-driver ()
  (let* ((command (concat
                   "\\cd " jstestmacs-driver-dir ";"
                   "java -jar JsTestDriver.jar "
                   "--port " jstestmacs-port
                   " --captureConsole --browser "
                   jstestmacs-test-browser " &")))
    (shell-command command)
    (sleep-for 3)))

(defun jstestmacs-dwim ()
  (interactive)
  (setq jstestmacs-config-path (jstestmacs-decide-conf-directory))
  (when (null jstestmacs-driver-dir)
    (jstestmacs-query :driverDir))
  (unless (jstestmacs-driver-live-p)
    (jstestmacs-boot-driver))
  (when (or (null jstestmacs-test-name) current-prefix-arg)
    (jstestmacs-query :testName))
  (jstestmacs-send-current-test))

(defun jstestmacs-decide-conf-directory ()
  (interactive)
  (let*
      ((dir-names (split-string buffer-file-truename "/"))
       config-path)
    (loop repeat (length dir-names) do
          (setq dir-names (jstestmacs-pop dir-names)
                config-path (jstestmacs-make-config-path dir-names))
          (if (file-exists-p config-path)
              (return config-path)))))

(defun jstestmacs-make-config-path (directorys)
  (concat (mapconcat 'identity directorys "/") "/jsTestDriver.conf"))

(defun jstestmacs-pop (list)
  (reverse (cdr (reverse list))))

(defun jstestmacs-from-symbol-to-string (symbol)
  (replace-regexp-in-string ":" "" (symbol-name symbol)))

(defun jstestmacs-query (&optional type)
  (let* ((header (jstestmacs-from-symbol-to-string type))
         (input (read-string (concat header " here: "))))
    (case type
      (:driverDir (setq jstestmacs-driver-dir input))
      (:testName (setq jstestmacs-test-name input)))))

(defun jstestmacs-send-current-test ()
  (let* ((file-name (jstestmacs-current-file-name))
         test-name result)
    (string-match "^test-\\([a-zA-Z-]+\\)\\.js$" file-name)
    (setq test-name (or jstestmacs-test-name (match-string 1 file-name))
          result (jstestmacs-fetch-result jstestmacs-driver-dir test-name))
    (jstestmacs-make-buffer result)
    (jstestmacs-control-command)))

(defun jstestmacs-current-file-name ()
  (let* ((split-dir-and-file-names (split-string buffer-file-truename "/")))
    (mapconcat 'identity (last split-dir-and-file-names) "")))

(defun jstestmacs-fetch-result (driver-dir test-name)
  (shell-command-to-string
   (concat "\\cd " driver-dir
           "; java -jar JsTestDriver.jar --tests "
           test-name " --verbose --captureConsole --config "
           jstestmacs-config-path " --reset")))

(defun jstestmacs-make-buffer(content)
  (let* ((basebuffer (current-buffer)))
    (with-temp-buffer
      (switch-to-buffer (get-buffer-create jstestmacs-output-buffer))
      (erase-buffer) ;;initialize
      (insert content))
    (switch-to-buffer basebuffer)
    (cond
     ((require 'popwin nil t)
      (popwin:popup-buffer
       (get-buffer-create "*JsTestDriver Output*")
       :noselect t :stick t :height 20 :position :top) t)
     (t (pop-to-buffer (get-buffer-create jstestmacs-output-buffer))))))

(provide 'jstestmacs)

;;; jstestmacs.el ends here
