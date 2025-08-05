;;; ai.el --- Configure ai tools
;;; Commentary:
;;; Code:

(use-package gptel
  :ensure t)

(use-package aidermacs
  :vc (:url "https://github.com/odontomachus/aidermacs"
       :rev "feat/enable-custom-lambda-arguments-for-aider")
  :ensure t
  :bind (("C-c a" . aidermacs-transient-menu))
  :custom
  (aidermacs-custom-args '(lambda () (list "--mount" (projectile-project-root))))
  (aidermacs-default-chat-mode 'architect)
  (aidermacs-default-model "gemini/gemini-2.5-pro")
  (aidermacs-weak-model "gemini/gemini-2.5-flash"))
;;; https://ai.google.dev/gemini-api/docs/models#model-versions

(gptel-make-gemini "Gemini pro"
   :key (secrets-get-secret "kdewallet" "api-keys/gemini-api-key")
   :stream t)

(setq
 gptel-model 'gemini-2.5-flash
 gptel-backend (gptel-make-gemini "Gemini flash"
                 :key (secrets-get-secret "kdewallet" "api-keys/gemini-api-key")
                 :stream t))

(provide 'ai)
;;; ai.el ends here
