;;; chronometrist-backend.el --- backend-related definitions for Chronometrist

;;; Commentary:
;;

(require 'cl)
(require 'eieio)

(defclass chronometrist-backend ()
  ((name
    :initarg :name
    :type string
    :documentation "The name of the backend."
    :initform "")
   (ext
    :initarg :ext
    :type string
    :documentation
    "The extension used by a file of this backend, without a leading period."
    :initform "")))

(defvar chronometrist-backends nil
  "List of enabled backends.")

;; # Migration #
(cl-defgeneric chronometrist-to-hash (backend table)
  "Clear TABLE and fill it using BACKEND.")

;; If the file for BACKEND exists and is not empty, signal an error or
;; prompt the user?
(cl-defgeneric chronometrist-from-hash (backend table)
  "Fill BACKEND using TABLE.")

;; # Queries #
(cl-defgeneric chronometrist-open-file (backend)
  "Open the storage file associated with BACKEND.")

(cl-defgeneric chronometrist-last (backend)
  "Return the latest interval from BACKEND.")

(cl-defgeneric chronometrist-current-task (backend)
  "Return the name of the currently clocked-in task, or nil if not clocked in.")

(cl-defgeneric chronometrist-populate (backend table)
  "Read data from BACKEND into hash table TABLE.
Return final number of intervals read from file, or nil if there
were none.")

;; # Modifications #
(cl-defgeneric chronometrist-create-file (backend)
  "Create BACKEND file if it does not already exist.")

(cl-defgeneric chronometrist-new (backend plist)
  "Use PLIST to add a new interval to BACKEND.")

(cl-defgeneric chronometrist-replace-last (backend plist)
  "Replace the latest record in BACKEND with PLIST.")

(provide 'chronometrist-backend)

;;; chronometrist-backend.el ends here
