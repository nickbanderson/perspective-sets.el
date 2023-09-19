;;; perspective-sets.el --- Organize separate sets of perspective.el perspectives

;;; Commentary:

;; Perspective sets (AKA psets) organize perspectives (AKA persps) together.
;;
;; Perspective names (in the context of perspective.el) are the concatenation of the set name,
;; '/' (or 'psets-separator-of-set-and-persp), and the set entry's name. This is done to ensure
;; that perspective names are unique.
;;
;; Instead of using persp-* functions to interact with perspective.el directly, use psets-* functions which wrap them.

;;; Code:

(defcustom psets-default-set-name "main"
  "Name of the default pset."
  :type 'string
  :group 'perspective-sets)

(defcustom psets-separator-of-set-and-persp "/"
  "String separating the string of a pset and a persp (i.e. \"/\" -> my-pset/my-persp)"
  :type 'string
  :group 'perspective-sets)

(defun psets-full-persp-name (pset-name persp-name)
  "Defines how to map the name of a persp and its pset into a true, unique persp name."
  (concat pset-name psets-separator-of-set-and-persp persp-name))

(defun psets-active-pset ()
  "Return pset name of active persp."
  (let* ((full-persp-name (persp-name (persp-curr)))
        (pset-name (substring full-persp-name 0 (string-match psets-separator-of-set-and-persp full-persp-name))))
    pset-name))

(defvar psets--psets
  (let ((sets (make-hash-table :test 'equal)))
    (puthash psets-default-set-name (list persp-initial-frame-name) sets)
    sets)
  "Hash table mapping psets to their contained persps.
(key: value) = (pset name string: list of persp name strings belonging to that set)
Example: (\"juicer-set\": '(\"p1\" \"p2\"))")

  ;; (mapcar #'make-persp (psets-full-persp-names pset-name persp-names))
  ;; (mapcar (lambda (full-persp-name) (make-persp :name full-persp-name)) (psets-full-persp-names pset-name persp-names))

(defun psets-create-pset (pset-name &optional persp-names)
  "Create a new pset PSET-NAME with optional PERSP-NAMES.
Automatically prepends PSET-NAME before each of PERSP-NAMES to promote uniqueness."
  (unless persp-names (setq persp-names (list persp-initial-frame-name)))
  (cl-assert (stringp pset-name))
  (cl-assert (and (listp persp-names) (-every? #'stringp persp-names)))
  (puthash pset-name nil psets--sets)
  (mapcar (lambda (persp-name) (psets-create-persp persp-name pset-name)) perp-names))

(defun psets-create-persp (persp-name &optional pset-name)
  "Create persp PERSP-NAME in pset PSET-NAME (default: current pset)."
  (unless pset-name (setq pset-name (psets-active-pset)))
  (cl-assert (and (stringp persp-name) (stringp pset-name)))
  ;; TODO this prompts for a name instead of accepting my argument
  (make-persp (psets-full-persp-name pset-name persp-name))
  (push persp-name (gethash pset-name psets--psets)))

(defun psets-kill-persp (full-persp-name &optional pset-name)
  "Remove persp PERSP-NAME in pset PSET-NAME (default: current pset)."
  (unless pset-name (setq pset-name (psets-active-pset)))
  (cl-assert (and (stringp persp-name) (stringp pset-name)))
  (persp-kill persp-name)
  (unless (member persp-name persp-names))
  (remhash pset-name psets--psets)
  ;; (setq perspective-sets-alist
  ;;       (delete '(assoc set-name 'perspective-sets-alist) 'perspective-sets-alist)))
  )

(provide 'perspective-sets)
