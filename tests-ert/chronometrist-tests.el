;; -*- lexical-binding: t; -*-
(require 'chronometrist)

(ert-deftest file-change-type ()
  (let* ((chronometrist-file            (concat default-directory "test.sexp"))
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
                        (prog1 (chronometrist-file-change-type chronometrist--file-state)
                          (setq chronometrist--file-state
                                (list :last (chronometrist-file-hash :before-last nil)
                                      :rest (chronometrist-file-hash nil :before-last t)))))))
          (should (eq :modify
                      (progn
                        (chronometrist-sexp-replace-last
                         '(:name "Modify Test"
                                 :tags (some tags)
                                 :start "2021-02-01T13:06:46+0530"
                                 :stop "2021-02-01T13:06:49+0530"))
                        (prog1 (chronometrist-file-change-type chronometrist--file-state)
                          (setq chronometrist--file-state
                                (list :last (chronometrist-file-hash :before-last nil)
                                      :rest (chronometrist-file-hash nil :before-last t)))))))
          (should (eq :remove
                      (progn
                        (chronometrist-sexp-in-file chronometrist-file
                          (goto-char (point-max))
                          (backward-list 1)
                          (chronometrist-sexp-delete-list 1)
                          (save-buffer))
                        (prog1 (chronometrist-file-change-type chronometrist--file-state)
                          (setq chronometrist--file-state
                                (list :last (chronometrist-file-hash :before-last nil)
                                      :rest (chronometrist-file-hash nil :before-last t)))))))
          (should (eq t
                      (progn
                        (chronometrist-sexp-in-file chronometrist-file
                          (goto-char (point-min))
                          (chronometrist-plist-pp '(:name "Other Change Test"
                                             :start "2021-02-02T17:39:40+0530"
                                             :stop "2021-02-02T17:39:44+0530")
                                     (current-buffer))
                          (save-buffer))
                        (chronometrist-file-change-type chronometrist--file-state)))))
      (with-current-buffer (find-file-noselect chronometrist-file)
        (delete-region (point-min) (point-max))
        (insert test-contents)
        (save-buffer))
      (setq chronometrist--file-state chronometrist--file-state-old
            chronometrist-events chronometrist-events-old))))

;; Local Variables:
;; nameless-current-name: "chronometrist"
;; End:
