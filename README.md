## Description
This program is frontend for JsTestDriver, it can be that execute test simply from Emacs.

## Usage

Before setup your .emacs, set below:

    (add-to-list 'load-path "path/to/jstestmacs")
    (setq jstestmacs-driver-dir "~/directory where there are JsTestDriver.jar/"
          jstestmacs-test-browser "firefox"
          jstestmacs--port "9876")
    (require 'jstestmacs)
    (add-hook 'js2-mode-hook
             '(lambda ()
                (local-set-key "\C-ct" 'jstestmacs-dwim)))

After the end of the preparation, you can execute JsTestDriver with types
"C-c t" where there are suffix *.js when js2-mode(case with js2-mode by add-hook).
Of course, require JsTestdriver and browser. :)

### Note
jsTestDriver.conf is detected automatically by recursive traverse directory to above.
