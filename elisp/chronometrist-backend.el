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

(defvar chronometrist-backend-current nil
  "The current backend in use.")

(defvar chronometrist-backends nil
  "List of enabled backends.")

;; # Migration #
(cl-defgeneric chronometrist-backend-to-hash (backend table)
  "Clear TABLE and fill it using BACKEND.
Return final number of intervals read from backend, or nil if
there were none.")

;; If the file for BACKEND exists and is not empty, signal an error or
;; prompt the user?
(cl-defgeneric chronometrist-backend-from-hash (backend table)
  "Fill BACKEND using TABLE.")

;; # Queries #
(cl-defgeneric chronometrist-backend-open-file (backend)
  "Open the storage file associated with BACKEND.")

(cl-defgeneric chronometrist-backend-latest-record (backend)
  "Return the latest record from BACKEND.")

(cl-defgeneric chronometrist-backend-current-task (backend)
  "Return the name of the currently clocked-in task, or nil if not clocked in.")

;; # Modifications #
(cl-defgeneric chronometrist-backend-create-file (backend)
  "Create BACKEND file if it does not already exist.")

(cl-defgeneric chronometrist-backend-add-new (backend plist)
  "Use PLIST to add a new interval to BACKEND.")

(cl-defgeneric chronometrist-backend-replace-last (backend plist)
  "Replace the latest record in BACKEND with PLIST.")

(provide 'chronometrist-backend)

;;; chronometrist-backend.el ends here
