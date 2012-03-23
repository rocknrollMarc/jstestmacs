# Commentary:
this program is frontend for JsTestDriver, it can be that execute test simply from Emacs.

# Usage
shell: execute below command line
% java -jar JsTestDriver.jar --port 9876 --captureConsole --browser firefox

    emacs: put below configuration to your .emacs
    (add-to-list 'load-path "path/to/jstestmacs")
    (require 'jstestmacs)
    (setq jste-driver-dir "path/to/JsTestDriver.jar")
    (add-hook 'js2-mode-hook
             '(lambda ()
              (local-set-key "\C-ct" 'jste-dwim)))

types M-x jste-dwim, it's command that execute current file
note:
this program require what put jsTestDriver.conf to directory on than the test
