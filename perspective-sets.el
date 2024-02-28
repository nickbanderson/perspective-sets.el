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

;;; Code:

(require 'perspective)

(defcustom psets/default-pset-name "main"
  "Name of the default pset."
  :type 'string
  :group 'perspective-sets)

(defcustom psets/delimiter "/"
  "String separating the string of a pset and a persp (i.e. \"/\" -> my-pset/my-persp)"
  :type 'string
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

;; TODO not sound yet, failed on se-rs
(defun psets/rename-pset (&optional new-pset-name)
  "Rename a pset, which renames its underlying full-persp-names."
  (interactive "i")
  (let* ((new-pset-name (or new-pset-name
                            (read-from-minibuffer
                             (concat "Rename pset from \""
                                     (psets/extract-pset-from-full-persp-name)
                                     "\": ")
                             (psets/extract-pset-from-full-persp-name)))))
    (mapc
     (lambda (persp-name)
       (psets/switch-persp persp-name)
       (let ((new-full-persp-name (psets/full-persp-name
                                   new-pset-name
                                   (psets/extract-persp-from-full-persp-name))))
         (persp-rename new-full-persp-name)))
     (psets/list-persps-in-pset))))

(provide 'perspective-sets)
