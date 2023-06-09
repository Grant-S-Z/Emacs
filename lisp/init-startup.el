;;; init-startup.el -- when starting
;;; Commentary:
;;; Code:

;; (set-frame-position (selected-frame) 720 0) ;; 设置启动位置
;; (set-frame-width (selected-frame) 88) ;; 设置启动宽度

(tool-bar-mode -1) ;; 关闭任务栏
(set-scroll-bar-mode nil) ;; 关闭滚动条
(electric-pair-mode t) ;; 自动生成对应括号
(global-auto-revert-mode t)
(delete-selection-mode t)

(mac-auto-operator-composition-mode) ;; fira code, you know

(setq make-backup-files nil) ;; 不自动备份文件
(setq inhibit-startup-message t) ;; 禁用初始欢迎界面
(setq-default cursor-type 'bar) ;; 设置光标为竖线

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
