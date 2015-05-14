;;; cask-package-toolset.el --- Toolsettize your package

;; Copyright (C) 2015  Adrien Becchis

;; Author: Adrien Becchis <adriean.khisbe@live.fr>
;; Created:  2015-05-14
;; Version: 0.1
;; Keywords: convenience, tools
;; Url: http://github.com/AdrieanKhisbe/cask-package-toolset.el
;; Package-Requires: ((s "1.6.1") (dash "1.8.0") (f "0.10.0") (commander "0.2.0") (ansi "0.1.0") (shut-up "0.1.0") (magit "1.4.0"))


;; This file is not part of GNU Emacs.

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

;; In building project to help you start or tool up your emacs package project.

;;; Code:

(require 'cl-lib)
(require 's)
(require 'dash)
(require 'f)
(require 'commander)
(require 'ansi)
(require 'shut-up)
(shut-up (require 'magit))

(when noninteractive
  (shut-up-silence-emacs))

(defvar cask-package-toolset-templates '("Makefile" ".gitignore" ".travis.yml")
  "List of templates supported by `cask-package-toolset'.")
;; §maybe: later replace with alist struct: option, category


(defun cask-package-toolset-install-template (template-name)
  "Install provided TEMPLATE-NAME."
  (if (cask-package-toolset-template-present-p template-name)
      (progn
        (warn "File %s is already existing. Skipping" template-name)
        nil)
    (progn (cask-package-toolset-copy-template template-name)
           t)))

(defvar cask-package-toolset-template-dir (f-expand "templates")
  "Folder holding the package templates.")

(defun cask-package-toolset--template-path (template-name)
  "Return path for TEMPLATE-NAME."
  (f-expand template-name cask-package-toolset-template-dir))

(defun cask-package-toolset-template-present-p (template-name)
  "Return t if TEMPLATE-NAME is already present in current-dir."
  (f-exists? (f-expand template-name)))

(defun cask-package-toolset-copy-template (template-name &optional subfolder)
  "Copy specified TEMPLATE-NAME in current folder or specified SUBFOLDER."
  ;; §MAYBE: add force param?
  (let ((template-file (cask-package-toolset--template-path template-name))
        (destination-file (f-expand template-name)))
    (unless (f-exists? template-file)
      (error "Template %s not found" template-name)) ; §todo: use real error.
    (if (f-exists? destination-file)
        (error "File already existing %s" destination-file)
      (f-copy template-file destination-file))))

(defun cask-package-toolset-get-github-repositery-name()
  (let ((remote-name (magit-get "remote" "github" "url")))
    ;; §todo: relace origin with variable
    (when (s-contains? "github" remote-name)
      (s-chop-suffix
       ".git"
       (if (s-starts-with? "git" remote-name)
           (nth 1 (s-split ":" remote-name)) ; git protocol
         (nth 1 (s-split ".com/" remote-name))))))) ; http
;;§todo: mock for test


;; commands
(defun cask-package-toolset-install-all-templates ()
  "Install all the packages. (from `cask-package-toolset-templates')"
  (-each cask-package-toolset-templates
    (lambda (template) (cask-package-toolset-install-template template))))

(defun cask-package-toolset-usage ()
  "Print Help for package toolset."
  (message "Help yourself, we'll help you."))

(defun cask-package-toolset-print-github-remote()
  (message "%s" (cask-package-toolset-get-github-repositery-name)))

(commander
 (name "cask-package-toolset")
 (description "Toolsettize your emacs package")
 (config ".cask-package-toolset")

 (default cask-package-toolset-usage)

 (option "--help, -h" cask-package-toolset-usage)

 (command "install" cask-package-toolset-install-all-templates)
 (command "git" cask-package-toolset-print-github-remote)
 (command "help" cask-package-toolset-usage))


(provide 'cask-package-toolset)
;;; cask-package-toolset.el ends here
