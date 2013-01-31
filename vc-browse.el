(require 'vc-git)

(defun vc-git-transmute-github (remote branch)
  (concat (replace-regexp-in-string
           "^.*github\\.com."
           "https://github.com/"
           (replace-regexp-in-string "\\.git$" "" remote))
          "/tree/" branch "/" (vc-git-file-path) "#L" (number-to-string 
                                                       (line-number-at-pos))))

(defconst vc-git-transmutations
  '(("github\\.com" . vc-git-transmute-github)))

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
  ;; FIXME - this should iterate vc-git-transmutations and return the
  ;; car of the match
  'vc-git-transmute-github)

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
