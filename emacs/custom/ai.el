;;; ai.el --- Configure ai tools
;;; Commentary:
;;; Code:

(use-package gptel
  :ensure t)

(gptel-make-gemini "Gemini pro"
   :key (secrets-get-secret "kdewallet" "api-keys/gemini-api-key")
   :stream t)

;; (setq
;;  gptel-model 'gemini-2.5-flash
;;  gptel-backend (gptel-make-gemini "Gemini flash"
;;                  :key (secrets-get-secret "kdewallet" "api-keys/gemini-api-key")
;;                  :stream t))

(setq
 gptel-model 'mistral-small
 gptel-backend
 (gptel-make-openai "Mistral-small"  ;Any name you want
   :host "api.mistral.ai"
   :endpoint "/v1/chat/completions"
   :protocol "https"
   :key  (secrets-get-secret "kdewallet" "work-api-keys/mistral")
   :models '("mistral-small")))

(gptel-make-openai "Mistral-medium"  ;Any name you want
   :host "api.mistral.ai"
   :endpoint "/v1/chat/completions"
   :protocol "https"
   :key  (secrets-get-secret "kdewallet" "work-api-keys/mistral")
   :models '("magistral-medium-latest"))

(use-package vterm :ensure t)

(provide 'ai)
;;; ai.el ends here
