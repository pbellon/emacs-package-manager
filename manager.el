;;; manager.el --- pseudo dependency manager based on use-package  -*- lexical-binding: t -*-
;;; Commentary:

;;; The idea is to get a simple way to add & manage dependencies like following:
;; (setq manager/dependencies-config-dir "~/.emacs.d/packages")
;; (manager/setup) and it will load every .el file listed under ~/.emac.d/packages/
;; 
;; Another functionnality would be to add with command similar as package-install
;; with =M-x manager/add RET <dependency name>= that will add it to the
;; dependency file located at manager/dependency-config-file.
(defcustom manager/dependencies-config-dir nil
  "The folder to use for dependencies configuration"
  :type 'string
  :group 'manager)

(defun m-fname (name)
  "Return the filname of the dependency"
  (format "%s/%s.el" manager/dependencies-config-dir name))

(defun m-reload-dep (name)
  "Reload a single dependency, creates a <name>.el with a (user-package ...) template"
  (message "Loading %s depency" name) 
  (load-file (m-fname name)))

(defun m-create-new (name)
  "Creates a new dependency"
  (unless (file-exists-p (m-fname name))
    (write-region
     (format "(use-package %s\n  :ensure t)" name)
     nil
     (m-fname name))
    (message "Dependency already exists, doing nothing")))

(defun m-list-dependencies ()
  (message "Going to list %s folder" manager/dependencies-config-dir)
  (let (dependencies)
    (dolist (el (directory-files manager/dependencies-config-dir nil "[\w-]*.el$") dependencies)
      (message "found %s" el)
      (setq dependencies (cons (file-name-sans-extension el) dependencies)))))

(defun m-reload-deps (deps)
  (dolist (el deps)
    (m-reload-dep el)))

(defun manager/setup ()
  "Trigger dependency initialization, will create files if it does not exist"
  (interactive)
  (m-reload-deps (m-list-dependencies)))

(defun manager/reload-dependencies ()
    "Load all defined dependencies"
    (interactive)
    (message "[manager] - Reloading dependencies")
    (m-reload-deps))

(defun manager/add ()
  "Create a new dependency file in the dependencies config dir, see =manager/dependencies-config-dir="
  (interactive)
  (let ((name (read-string "[manager] - Dependency name?\n> ")))
    (m-create-new name)
    (m-reload-dep name)))

(provide 'manager)
;;; manager.el ends here

