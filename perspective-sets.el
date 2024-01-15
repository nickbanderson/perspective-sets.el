;;; perspective-sets.el --- Organize separate sets of perspective.el perspectives

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
This should be ran right after loading this package, perspective-sets."
  ;; Safeguard in case the current persp is not the initial persp
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
  "Return persp name from FULL-PERSP-NAME or current perspective."
  (interactive "i")
  (unless full-persp-name (setq full-persp-name (persp-current-name)))
  (cl-assert (stringp full-persp-name))
  (substring full-persp-name
             (1+ (string-match
                  psets/delimiter
                  full-persp-name))))

(defun psets/list-persps-in-pset (&optional pset-name)
  "Return list of persps in PSET-NAME or current pset."
  (unless pset-name (setq pset-name (psets/extract-pset-from-full-persp-name)))
  (cl-assert (stringp pset-name))
  (mapcar
   #'psets/extract-persp-from-full-persp-name
   (cl-remove-if-not (lambda (persp-name)
                       (string-match
                        (concat pset-name psets/delimiter)
                        persp-name))
                     (persp-names))))

(defun psets/list-psets (&optional pset-name)
  "Return list of psets."
  (mapcar )
  )

(defun psets/create-pset (pset-name &optional persp-names)
  "Create a new pset PSET-NAME with optional PERSP-NAMES.
Automatically prepends PSET-NAME before each of PERSP-NAMES to attain uniqueness."
  (unless persp-names (setq persp-names (list persp-initial-frame-name)))
  (cl-assert (stringp pset-name))
  (cl-assert (and (listp persp-names) (-every? #'stringp persp-names)))
  (mapcar (lambda (persp-name) (psets/create-persp persp-name pset-name)) persp-names))

(defun psets/create-persp (persp-name &optional pset-name)
  "Create persp PERSP-NAME in pset PSET-NAME or current pset."
  (unless pset-name (setq pset-name (psets/extract-pset-from-full-persp-name)))
  (cl-assert (and (stringp persp-name) (stringp pset-name)))
  (persp-switch (psets/full-persp-name pset-name persp-name)))

(defun psets/switch-pset (&optional pset-name)
  "Switch to pset PSET-NAME (creating if necessary)."
  (interactive "i")
  (unless pset-name
    (setq pset-name
          (completing-read "Switch to pset: " (psets/list-persps-in-pset))))
  (unless pset-name (setq pset-name (psets/extract-pset-from-full-persp-name)))
  (cl-assert (and (stringp pset-name) (stringp pset-name)))
  (persp-switch (psets/full-pset-name pset-name pset-name)))

(defun psets/switch-persp (persp-name &optional pset-name)
  "Switch to persp PERSP-NAME (creating if necessary) in pset PSET-NAME or current pset."
  (interactive "i")
  (unless persp-name
    (setq persp-name
          (completing-read "Switch to persp: " (psets/list-persps-in-pset))))
  (unless pset-name (setq pset-name (psets/extract-pset-from-full-persp-name)))
  (cl-assert (and (stringp persp-name) (stringp pset-name)))
  (persp-switch (psets/full-persp-name pset-name persp-name)))

(defun psets/kill-persp (persp-name &optional pset-name)
  "Remove persp PERSP-NAME in pset PSET-NAME or current pset."
  (interactive "i")
  (unless persp-name
    (setq persp-name
          (completing-read "Kill persp: " (psets/list-persps-in-pset) nil t)))
  (unless pset-name (setq pset-name (psets/extract-pset-from-full-persp-name)))
  (cl-assert (and (stringp persp-name) (stringp pset-name)))
  (message "killing: %s / %s" pset-name persp-name)
  (let ((full-persp-name
         (if (string-match psets/delimiter persp-name)
             persp-name
           (psets/full-persp-name pset-name persp-name))))
    (persp-kill full-persp-name)))

(defun psets/rename-persp (&optional new-persp-name)
  "Wrapper around #'persp-rename with a nicer prompt."
  (interactive "i")
  (setq new-persp-name
        (read-from-minibuffer (concat "Rename persp from \""
                                      (persp-current-name)
                                      "\"): ")
                              (persp-current-name)))
  (cl-assert (stringp new-persp-name))
  (unless (string= (persp-current-name) new-persp-name)
    (persp-rename new-persp-name)))

(provide 'perspective-sets)
