;;; Early init --- Configuration for emacs.;

;;; Commentary:

;; By Jonathan Villemaire-Krajden
;; Inspired by lots of online sources.

;;; Code:


(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t
      inhibit-startup-screen +1
      initial-scratch-message nil
      frame-inhibit-implied-resize t
      column-number-mode t
      frame-resize-pixelwise t
      default-frame-alist '((left . 0)
                            (fullscreen . fullheight)
                            (fullscreen-restore . fullheight)
                            (width . 0.5))
      display-buffer-alist
      '(("\\*\\(xref\\|log\\|help\\).*\\*"
         (display-buffer-reuse-window display-buffer-in-previous-window display-buffer-in-side-window)
         (window-width . 0.25)
         (side . right)
         (reusable-frames . visible))))

;; Full frame
(add-hook
 'emacs-startup-hook
 (lambda ()
   (progn
     (if (<= (display-pixel-width) 1920) (set-frame-parameter nil 'fullscreen 'maximized)
   (setq
    window-min-height (- (/ (window-body-height) 3) 1)
    window-min-width (- (/ (window-body-width) 3) 1))))))

(provide 'early-init)
;;; early-init.el ends here
