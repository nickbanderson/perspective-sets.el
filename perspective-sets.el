;;; perspective-sets.el --- Organize separate sets of perspective.el perspectives -*-lexical-binding: t-*-

;;; Commentary:

;; Perspective sets (AKA psets) organize perspectives (AKA persps) together.
;; pset/* functions simply provide one layer of abstraction over calls to
;; persp-* functions in order to divide them into sets.
;;
;; Code naming/vocabulary definitions
;; - for (persp-current-name) of "epic-workspace/crappy-repo"
;; - => full-persp-name := "epic-workspace/crappy-repo"
;; - => persp-name := "crappy-repo"
;; - => delimiter := "/"
;; - => pset-name := "epic-workspace"
;;
;; Example usage:
;; 1. Initialize with full-persp-name == "main/main", a common default
;; 2. Create new persp: M-x psets/switch-persp blah RET
;;    -> full-persp-name == "main/blah"
;; 3. Create new pset: M-x psets/switch-pset test RET
;;    -> full-persp-name == "test/main"
;;
;; TODO
;; - through 'marginalia, show persps within each pset when showing a completing-read over psets

;;; Code:

(require 'perspective)  ; heavy integration
(require 'consult)  ; #'consult--read

(defcustom psets/default-pset-name "main"
  "Name of the default pset."
  :type 'string
  :group 'perspective-sets)

(defcustom psets/delimiter "/"
  "String separating the string of a pset and a persp (i.e. \"/\" -> my-pset/my-persp)"
  :type 'string
  :group 'perspective-sets)

(defcustom psets/persp-dir-aliases-alist '()
  "Alist of aliases to use as persp names in place of matching directory names when
opening a directory as a persp."
  :type '(alist :key-type string :value-type string)
  :group 'perspective-sets)

(defcustom psets/remote-host-persp-name-setter
  (lambda (original-persp-name remote-hostname)
    (s-concat original-persp-name (when remote-hostname (s-concat "-" remote-hostname))))
  "When a persp is created from a directory on a remote host, mutate
that new perp's name with this function (e.g. add the remote hostname
as suffix: mycodebase on myserver -> mycodebase-myserver)."
  :type 'function  ; parameter list: (original-persp-name remote-hostname
  :group 'perspective-sets)

(defcustom psets/default-search-dir (concat (getenv "HOME") "/")
  "Directory to use as default when not provided one for `psets/open-prompted-dir-as-persp'."
  :type 'directory
  :group 'perspective-sets)

(defcustom psets/setup-persp-from-opened-dir-hook nil
  "Hook to run when a new persp is created from an opened dir
(i.e. `psets/open-dir-as-pset' and `psets/open-dir-as-persp'. The
sole argument passed to these function(s) is the full path of the opened dir."
  :type 'hook  ; parameter list: (dir)
  :group 'perspective-sets)

(defun psets/full-persp-name (pset-name persp-name)
  "Defines how to map the name of a persp and its pset into a true, unique persp name."
  (concat pset-name psets/delimiter persp-name))

(defun psets/try-to-rename-initial-persp ()
  "If it's safe, rename initial persp to be included in the default pset.
This should be ran right after loading this package, `perspective-sets'."
  ;; safeguard in case the current persp is not the initial persp
  (when (string= (persp-current-name)
                 persp-initial-frame-name)
    (persp-rename (psets/full-persp-name psets/default-pset-name
                                         persp-initial-frame-name))))

(defun psets/extract-pset-from-full-persp-name (&optional full-persp-name)
  "Return pset name from FULL-PERSP-NAME or current perspective."
  (interactive "i")
  (unless full-persp-name (setq full-persp-name (persp-current-name)))
  (cl-assert (stringp full-persp-name))
  (substring full-persp-name
             0
             (string-match
              psets/delimiter
              full-persp-name)))

(defun psets/extract-persp-from-full-persp-name (&optional full-persp-name)
  "Return persp-name from FULL-PERSP-NAME or current perspective."
  (interactive "i")
  (unless full-persp-name (setq full-persp-name (persp-current-name)))
  (cl-assert (stringp full-persp-name))
  (substring full-persp-name
             (1+ (string-match
                  psets/delimiter
                  full-persp-name))))

(defun psets/list-persps-in-pset (&optional pset-name)
  "Return list of persp-names in PSET-NAME or current pset."
  (unless pset-name (setq pset-name (psets/extract-pset-from-full-persp-name)))
  (cl-assert (stringp pset-name))
  (mapcar
   #'psets/extract-persp-from-full-persp-name
   (cl-remove-if-not (lambda (persp-name)
                       (string-match
                        (concat pset-name psets/delimiter)
                        persp-name))
                     (persp-names))))

(defun psets/list-psets ()
  "Return list of psets."
  (cl-delete-duplicates
   (mapcar #'psets/extract-pset-from-full-persp-name
           (persp-names))
   :test #'string=))

(defun psets/create-pset (pset-name &optional persp-names)
  "Create a new pset PSET-NAME with optional PERSP-NAMES using `psets/create-persp'."
  (unless persp-names (setq persp-names (list persp-initial-frame-name)))
  (cl-assert (stringp pset-name))
  (cl-assert (and (listp persp-names) (-every? #'stringp persp-names)))
  (mapcar (lambda (persp-name) (psets/create-persp persp-name pset-name)) persp-names))

(defun psets/create-persp (persp-name &optional pset-name)
  "Create persp PERSP-NAME in pset PSET-NAME or current pset."
  (unless pset-name (setq pset-name (psets/extract-pset-from-full-persp-name)))
  (cl-assert (and (stringp persp-name) (stringp pset-name)))
  (persp-switch (psets/full-persp-name pset-name persp-name)))

(defun psets/switch-pset (pset-name)
  "Switch to pset PSET-NAME (creating if necessary)."
  (interactive "i")
  (unless pset-name
    (setq pset-name (completing-read "Switch to pset: " (psets/list-psets))))
  (cl-assert (stringp pset-name))
  (if (not (member pset-name (psets/list-psets)))
      (persp-switch (psets/full-persp-name pset-name persp-initial-frame-name))
    (let ((persp-name (completing-read "Switch to persp: " (psets/list-persps-in-pset pset-name))))
      (persp-switch (psets/full-persp-name pset-name persp-name)))))

(defun psets/switch-persp (persp-name &optional pset-name)
  "Switch to persp PERSP-NAME (creating if necessary) in pset PSET-NAME or current pset."
  (interactive "i")
  (unless persp-name
    (setq persp-name (completing-read "Switch to persp: " (psets/list-persps-in-pset))))
  (unless pset-name
    (setq pset-name (psets/extract-pset-from-full-persp-name)))
  (cl-assert (and (stringp persp-name) (stringp pset-name)))
  (persp-switch (psets/full-persp-name pset-name persp-name)))

(defun psets/kill-persp (persp-name &optional pset-name)
  "Remove persp PERSP-NAME in pset PSET-NAME or current pset."
  (interactive "i")
  (unless persp-name
    (setq persp-name (completing-read "Kill persp: " (psets/list-persps-in-pset) nil t)))
  (unless pset-name
    (setq pset-name (psets/extract-pset-from-full-persp-name)))
  (cl-assert (and (stringp persp-name) (stringp pset-name)))
  (persp-kill (psets/full-persp-name pset-name persp-name)))

(defun psets/kill-pset (pset-name)
  "Remove pset PSET-NAME or current pset."
  (interactive "i")
  (unless pset-name
    (setq pset-name
          (completing-read "Kill pset: " (psets/list-psets) nil t)))
  (cl-assert (stringp pset-name))
  (dolist (persp-name (psets/list-persps-in-pset pset-name))
    (persp-kill (psets/full-persp-name pset-name persp-name))))

(defun psets/rename-persp (&optional new-persp-name)
  "Pset-aware wrapper around `persp-rename'."
  (interactive "i")
  (let* ((new-persp-name (or new-persp-name
                             (read-from-minibuffer
                              (concat "Rename persp from \""
                                      (psets/extract-persp-from-full-persp-name)
                                      "\": ")
                              (psets/extract-persp-from-full-persp-name))))
         (new-full-persp-name (psets/full-persp-name
                               (psets/extract-pset-from-full-persp-name)
                               new-persp-name)))
    (unless (string= (persp-current-name) new-full-persp-name)
      (persp-rename new-full-persp-name))))

(defun psets/rename-pset (&optional new-pset-name)
  "Rename a pset, which renames its underlying full-persp-names."
  (interactive "i")
  (let* ((old-pset-name (psets/extract-pset-from-full-persp-name))
         (new-pset-name
          (or new-pset-name
              (read-from-minibuffer
               (concat "Rename pset from \"" old-pset-name "\": ")
               old-pset-name))))
    (dolist (persp-name (psets/list-persps-in-pset))
      (let ((old-full-persp-name (psets/full-persp-name old-pset-name persp-name))
            (new-full-persp-name (psets/full-persp-name new-pset-name persp-name)))
        (persp-switch old-full-persp-name t)
        (persp-rename new-full-persp-name)))))

(defun psets/move-persp-to-pset (&optional target-pset-name)
  (interactive "i")
  (let* ((target-pset-name
          (or target-pset-name
              (consult--read (remove (psets/extract-pset-from-full-persp-name)
                                     (psets/list-psets))
                             :prompt (concat "Move persp \""
                                             (persp-current-name)
                                             "\" to new pset: ")
                             ))))
    (persp-rename (concat target-pset-name
                          psets/delimiter
                          (psets/extract-persp-from-full-persp-name)))))

(defun psets/open-dir-as-persp (&optional dir search-dir post-hook)
  "If called with nil DIR, prompt for dir to open as a persp in the
current pset starting from SEARCH-DIR if non-nil or
`psets/default-search-dir'. If POST-HOOK is non-nil, call it after
opening the new persp with one argument: DIR.

Example invocations:
- (psets/open-dir-as-persp)
- (psets/open-dir-as-persp nil \"~/code\")
- (psets/open-dir-as-persp \"~/code/tem\")
- (psets/open-dir-as-persp \"~/code/tem\" nil
                           #'(lambda (dir)
                             (message \"hi from %s\" dir)))
"
  (interactive)
  (cl-assert (file-directory-p
              (or search-dir
                  (setq search-dir psets/default-search-dir))))
  (cl-assert (file-directory-p
              (or dir
                  (setq dir (read-directory-name "Open dir as persp: " search-dir nil t)))))
  (let* ((dir (psets//trim-dir-trailing-slash dir))
         (persp-name (file-name-nondirectory dir))
         (preferred-name (or (cdr (assoc persp-name psets/persp-dir-aliases-alist)) persp-name))
         (remote-hostname (when (file-remote-p dir)
                              ;; works at least for "/ssh:myremoteserver:/some/path"
                              (substring (file-remote-p dir) (length "/ssh:") -1)))
         (final-name (funcall psets/remote-host-persp-name-setter
                              preferred-name
                              remote-hostname)))
    (psets/switch-persp final-name (psets/extract-pset-from-full-persp-name))
    (run-hook-with-args 'psets/setup-persp-from-opened-dir-hook dir)
    (when (functionp post-hook)
      (apply post-hook (list dir)))))

(defun psets/open-dir-as-pset (&optional dir search-dir post-hook)
  "See `psets/open-dir-as-persp' for close-enough coverage of
arguments and usage. This function creates a new pset then opens and
bootstraps a persp for each dir within DIR instead of opening DIR as a
persp directly. POST-HOOK and `psets/new-persp-from-opened-dir-hook'
are ran for each persp and are passed that persp's directory as the
only argument."
  (interactive)
  (cl-assert (file-directory-p
              (or search-dir
                  (setq search-dir psets/default-search-dir))))
  (cl-assert (file-directory-p
              (or dir
                  (setq dir (read-directory-name "Open dir as pset: " search-dir nil t)))))
  (let* ((pset-dir (psets//trim-dir-trailing-slash dir))
         (pset-name (file-name-nondirectory pset-dir)))
    (dolist (persp-dir (directory-files pset-dir t "^[^.]"))  ; for now, hardcode exclusion on hidden dirs
      (when (file-directory-p persp-dir)
        (let* ((persp-name (file-name-nondirectory persp-dir))
               (preferred-name (cdr (assoc persp-name psets/persp-dir-aliases-alist))))
          (psets/switch-persp (or preferred-name persp-name) pset-name)
          (run-hook-with-args 'psets/setup-persp-from-opened-dir-hook persp-dir)
          (when (functionp post-hook)
            (apply post-hook (list dir))))))))

(defun psets//trim-dir-trailing-slash (dir)
  "If present, removes a trailing slash from a directory path.
/path/to/thing/ => /path/to/thing
/another/path => /another/path
/ => /  ; special case

Warning: doesn't do much verification :)"
  (if (> (length dir) 1)  ; protect root dir "/" from turning into ""
      (s-chop-suffix "/" dir)
    dir))

(provide 'perspective-sets)
