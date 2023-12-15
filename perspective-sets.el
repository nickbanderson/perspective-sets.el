;;; perspective-sets.el --- Organize separate sets of perspective.el perspectives

;;; Commentary:

;; Perspective sets (AKA psets) organize perspectives (AKA persps) together.
;;
;; Perspective.el already manages sets of persps (and related data) through frame
;; parameters so you can have independent persps per frame. Perspective-sets manages
;; these frame parameters to allow smooth pset swappage within one frame.

;;; Code:

(defcustom psets-default-pset-name "main"
  "Name of the default pset."
  :type 'string
  :group 'perspective-sets)

(defvar psets-active-pset-name psets-default-pset-name
  "The name (string) of the currently active pset.")

(defun psets-active-pset ()
  "Returns the currently active pset by accessing frame parameters used by perspective.el."
  ;; WAIT use cl-defstruct! tagged vector!!! lol
  (list
   (frame-parameter nil 'persp--hash)
   (frame-parameter nil 'persp--curr)
   (frame-parameter nil 'persp--last)
   (frame-parameter nil 'persp--recursive)
   (frame-parameter nil 'persp--modestring)))
     ;; '((persp--hash) (persp--curr) (persp--last) (persp--recursive) (persp--modestring)))

(defvar psets--psets
  (let ((psets (make-hash-table :test 'equal)))
    (puthash psets-default-pset-name (psets-active-pset) psets)
    psets)
  "Hash table mapping each pset name (string) to its associated pset.")

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
  (unless pset-name (setq pset-name psets-active-pset-name))
  (cl-assert (and (stringp persp-name) (stringp pset-name)))
  ;; TODO this prompts for a name instead of accepting my argument
  (make-persp (psets-full-persp-name pset-name persp-name))
  (push persp-name (gethash pset-name psets--psets)))

(defun psets-kill-persp (full-persp-name &optional pset-name)
  "Remove persp PERSP-NAME in pset PSET-NAME (default: current pset)."
  (unless pset-name (setq pset-name psets-active-pset-name))
  (cl-assert (and (stringp persp-name) (stringp pset-name)))
  (persp-kill persp-name)
  (unless (member persp-name persp-names))
  (remhash pset-name psets--psets)
  ;; (setq perspective-sets-alist
  ;;       (delete '(assoc set-name 'perspective-sets-alist) 'perspective-sets-alist)))
  )

(provide 'perspective-sets)
