;;; manager.el --- pseudo dependency manager based on use-package  -*- lexical-binding: t -*-
;; Author: Pierre BELLON <bellon.pierre@gmail.com>
;;; Commentary:

;;; The idea is to get a simple way to add & manage dependencies like following:
;; (setq manager/dependencies-config-dir "~/.emacs.d/packages")
;; (manager/load-dependencies) and it will load every .el file listed under ~/.emac.d/packages/
;; 
;; Another functionnality would be to add with command similar as package-install
;; with =MANAGER/x manager/add RET <dependency name>= that will add it to the
;; dependency file located at manager/dependency-config-file.
(defcustom manager/dependencies-config-dir nil
  "The folder to use for dependencies configuration"
  :type 'string
  :group 'manager)

(defcustom manager/dependency-template "(use-package %s\n  :ensure t)"
  "The template to use to create new dependencies lisp files"
  :type 'string
  :group 'manager)

(defun manager/fname (name)
  "Return the filname of the dependency"
  (format "%s/%s.el" manager/dependencies-config-dir name))

(defun manager/load-dependency (name)
  "Reload a single dependency, creates a <name>.el with a (user-package ...) template"
  (message "Loading %s depency" name) 
  (load-file (manager/fname name)))

(defun manager/open (name)
  (find-file (manager/fname name)))

(defun manager/create-new (name)
  "Creates a new dependency"
  (unless (file-exists-p (manager/fname name))
    (write-region
     (format manager/dependency-template name)
     nil
     (manager/fname name))
    (message "Dependency already exists, doing nothing")))

(defun manager/list-dependencies ()
  (message "Going to list %s folder" manager/dependencies-config-dir)
  (let (dependencies)
    (dolist (el (directory-files manager/dependencies-config-dir nil "[\w-]*.el$") dependencies)
      (message "found %s" el)
      (setq dependencies (cons (file-name-sans-extension el) dependencies)))))

(defun manager/load-dependencies-list (deps)
  (dolist (el deps)
    (manager/load-dependency el)))

(defun manager/load-dependencies ()
    "Load all defined dependencies"
    (interactive)
    (message "Reloading dependencies")
    (manager/load-dependencies-list (manager/list-dependencies)))

(defun manager/add ()
  "Create a new dependency file in the dependencies config dir, see =manager/dependencies-config-dir="
  (interactive)
  (let ((name (read-string "Dependency name?\n> ")))
    (manager/create-new name)
    (manager/open name)))

(provide 'manager)
;;; manager.el ends here

