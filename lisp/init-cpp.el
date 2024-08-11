;;; init-cpp.el --- for c++
;;; Commentary:
;;; Code:
;; Quickrun
(quickrun-add-command "c++/c1z"
  '((:command . "clang++")
    (:exec    . ("%c -std=c++1z %o -o %e %s"
		 "%e %a"))
    (:remove  . ("%e")))
  :default "c++")

(provide 'init-cpp)
;;; init-cpp.el ends here
