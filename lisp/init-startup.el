;;; init-startup.el -- when starting
;;; Commentary:
;;; Code:
(tool-bar-mode -1) ;; 关闭任务栏
(set-scroll-bar-mode nil) ;; 关闭滚动条
(electric-pair-mode t) ;; 自动生成对应括号
(global-auto-revert-mode t)
(delete-selection-mode t)

(mouse-wheel-mode -1) ;; 禁用滚轮
(mac-auto-operator-composition-mode t) ;; operator composition
(pixel-scroll-mode t) ;; 支持像素滚动
(pixel-scroll-precision-mode t)

(setq make-backup-files nil) ;; 不自动备份文件
(setq inhibit-startup-message t) ;; 禁用初始欢迎界面
(setq-default cursor-type 'bar) ;; 设置光标为竖线

;; 忽略某些 message
(let ((inhibit-message t))
  (message "Wrong type argument: number-or-marker-p, nil"))

;;; 全屏启动，且可使用状态栏与程序坞
(set-frame-parameter nil 'fullscreen 'fullboth)
;; Inhibit resizing Puremacs frame
(setq frame-inhibit-implied-resize t)
;; To suppress flashing at startup
(setq-default inhibit-redisplay t
              inhibit-message t)
(defun reset-inhibit-vars ()
  (setq-default inhibit-redisplay nil
                inhibit-message nil)
  (redraw-frame))
(add-hook 'window-setup-hook #'reset-inhibit-vars)
(define-advice startup--load-user-init-file (:after (&rest _) reset-inhibit-vars)
  (and init-file-had-error (reset-inhibit-vars)))


(add-hook 'prog-mode-hook #'show-paren-mode)
(add-hook 'prog-mode-hook #'hs-minor-mode)

;; 垃圾回收
(setq gc-cons-threshold 100000000)

(setq read-process-output-max (* 1024 1024))

(setq package-native-compile t)

;; 将 load-file 的内容加载到 custom.el 中
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load-file custom-file))

(provide 'init-startup)
;;; init-startup.el ends here
