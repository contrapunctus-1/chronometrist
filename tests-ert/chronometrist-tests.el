;; -*- lexical-binding: t; -*-
(require 'chronometrist)

(defvar chronometrist-test-file
  (concat (file-name-directory (buffer-file-name))
          "test.sexp"))

(ert-deftest task-list ()
  (let ((task-list (chronometrist-task-list)))
    (should (listp task-list))
    (should (seq-every-p #'stringp task-list))))

(ert-deftest file-hash ()
  (-let* ((chronometrist-file chronometrist-test-file)
          ((last-start last-end)
           (chronometrist-file-hash :before-last nil))
          ((rest-start rest-end rest-hash)
           (chronometrist-file-hash nil :before-last t)))
    (should (= 1 rest-start))
    (should (= 1247 rest-end))
    (should (= 1249 last-start))
    (should (= 1419 last-end))))

(defmacro chronometrist-tests--change-type-and-update (state)
  `(prog1 (chronometrist-file-change-type ,state)
     (setq ,state
           (list :last (chronometrist-file-hash :before-last nil)
                 :rest (chronometrist-file-hash nil :before-last t)))))

;; ;; TODO
;; add newline after last expression and save => nil
;; remove newline after last expession and save => nil

(ert-deftest file-change-type ()
  (let* ((chronometrist-file            chronometrist-test-file)
         (test-contents    (with-current-buffer (find-file-noselect chronometrist-file)
                             (buffer-substring (point-min) (point-max))))
         (chronometrist--file-state-old chronometrist--file-state)
         (chronometrist--file-state     (list :last (chronometrist-file-hash :before-last nil)
                                 :rest (chronometrist-file-hash nil :before-last t)))
         (chronometrist-events-old      chronometrist-events))
    (chronometrist-events-populate)
    (unwind-protect
        (progn
          (should (eq nil (chronometrist-file-change-type chronometrist--file-state)))
          (should (eq :append
                      (progn
                        (chronometrist-sexp-new
                         '(:name "Append Test"
                                 :start "2021-02-01T13:06:46+0530"
                                 :stop "2021-02-01T13:06:49+0530"))
                        (chronometrist-tests--change-type-and-update chronometrist--file-state))))
          (should (eq :modify
                      (progn
                        (chronometrist-sexp-replace-last
                         '(:name "Modify Test"
                                 :tags (some tags)
                                 :start "2021-02-01T13:06:46+0530"
                                 :stop "2021-02-01T13:06:49+0530"))
                        (chronometrist-tests--change-type-and-update chronometrist--file-state))))
          (should (eq :remove
                      (progn
                        (chronometrist-sexp-in-file chronometrist-file
                          (goto-char (point-max))
                          (backward-list 1)
                          (chronometrist-sexp-delete-list 1)
                          (save-buffer))
                        (chronometrist-tests--change-type-and-update chronometrist--file-state))))
          (should (eq t
                      (progn
                        (chronometrist-sexp-in-file chronometrist-file
                          (goto-char (point-min))
                          (chronometrist-plist-pp '(:name "Other Change Test"
                                             :start "2021-02-02T17:39:40+0530"
                                             :stop "2021-02-02T17:39:44+0530")
                                     (current-buffer))
                          (save-buffer))
                        (chronometrist-tests--change-type-and-update chronometrist--file-state)))))
      (with-current-buffer (find-file-noselect chronometrist-file)
        (delete-region (point-min) (point-max))
        (insert test-contents)
        (save-buffer))
      (setq chronometrist--file-state chronometrist--file-state-old
            chronometrist-events chronometrist-events-old))))

;; Local Variables:
;; nameless-current-name: "chronometrist"
;; End:
