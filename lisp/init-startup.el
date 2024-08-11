;;; init-startup.el -- when starting
;;; Commentary:
;;; Code:
(tool-bar-mode -1) ;; 关闭任务栏
(set-scroll-bar-mode nil) ;; 关闭滚动条
(electric-pair-mode t) ;; 自动生成对应括号
(global-auto-revert-mode t)
(delete-selection-mode t)
(global-hl-line-mode t) ;; 高亮当前行

(setq mac-mouse-wheel-smooth-scroll t) ;; 平滑滚动，对于 mac 只有这个起作用

(mac-auto-operator-composition-mode t) ;; Ligature for mac port

(setq make-backup-files nil) ;; 不自动备份文件
(setq inhibit-startup-message t) ;; 禁用初始欢迎界面
(setq frame-title-format "Emacs") ;; frame 标题
(setq-default cursor-type 'bar) ;; 设置光标为竖线

;;; 位置
(setq calendar-location-name "Beijing, CN")
(setq calendar-latitude 39.9042)
(setq calendar-longitude 116.4074)

;; Inhibit resizing Puremacs frame
(setq frame-inhibit-implied-resize t)
;; To suppress flashing at startup
(setq-default inhibit-redisplay t
              inhibit-message t)
(defun reset-inhibit-vars ()
  "Reset inhibit vars."
  (setq-default inhibit-redisplay nil
                inhibit-message nil)
  (redraw-frame))
(add-hook 'window-setup-hook #'reset-inhibit-vars)
(define-advice startup--load-user-init-file (:after (&rest _) reset-inhibit-vars)
  (and init-file-had-error (reset-inhibit-vars)))

;;; 默认 ssh
(setq tramp-default-method "ssh")

(add-hook 'prog-mode-hook #'show-paren-mode)
(add-hook 'prog-mode-hook #'hs-minor-mode)

(setq package-native-compile t)

;; 将 load-file 的内容加载到 custom.el 中
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load-file custom-file))

(provide 'init-startup)
;;; init-startup.el ends here
