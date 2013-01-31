;;; vc-browse.el --- Browse to the current file on GitHub

;; Copyright (C) 2013  Ian Eure

;; Author: Ian Eure <ian.eure@gmail.com>
;; Keywords:

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

;;

;;; Code:

(require 'vc-git)
(eval-when-compile
  (require 'cl))

(defun vc-git-make-github-transmuter (&optional host)
  (lexical-let ((target-host (or host "github.com")))
    (lambda (remote branch)
      (concat (replace-regexp-in-string
               (concat "^.*" (regexp-quote target-host) ".")
               (concat "https://" target-host "/")
               (replace-regexp-in-string "\\.git$" "" remote))
              "/tree/" branch "/" (vc-git-file-path) "#L" (number-to-string
                                                           (line-number-at-pos))))))

(defconst vc-git-transmutations
  (list (cons "github\\.com" (vc-git-make-github-transmuter))))

(defun vc-git-file-path ()
  (substring
   (expand-file-name (buffer-file-name))
   (length (expand-file-name
            (locate-dominating-file (buffer-file-name)".git")))))

(defun vc-git-browse-tracking ()
  (let ((branch (car (vc-git-branches))))
    (split-string
     (substring
      (shell-command-to-string
       (format "%s for-each-ref --format='%%(upstream:short)' refs/heads/%s"
               vc-git-program branch)) 0 -1)  "/")))

(defun vc-git-browse-remote-url (remote)
  (substring (shell-command-to-string
              (format "%s config --get remote.%s.url"
                      vc-git-program remote)) 0 -1))

(defun vc-git-transmuter (remote-url)
  (let* ((muters vc-git-transmutations)
         (found) (transmuter))
    (while (and muters (not found))
      (if (not (string-match (caar muters) remote-url))
          (setq muters (cdr muters))
        (setq found t)
        (setq transmuter (cdar muters))))
    transmuter))

(defun vc-git-transmute-url (remote-url branch)
  (funcall (vc-git-transmuter remote-url) remote-url branch))

(defun vc-git-browse-url ()
  (let* ((remote-branch (vc-git-browse-tracking))
         (remote-url (vc-git-browse-remote-url (car remote-branch)))
         (branch (cadr remote-branch)))
  (vc-git-transmute-url remote-url branch)))

(defun vc-git-browse ()
  (interactive)
  (browse-url (vc-git-browse-url)))

(defun vc-git-browse-kill ()
  (interactive)
  (kill-new (vc-git-browse-url)))

(provide 'vc-browse)
;;; vc-browse.el ends here
