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
  (aidermacs-default-model "mistral/devstral-medium-2507")
  (aidermacs-weak-model "mistral/devstral-medium-2507"))
;;; https://ai.google.dev/gemini-api/docs/models#model-versions



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

(setq claude-code-terminal-backend 'vterm)

(use-package claude-code :ensure t
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
  ;; :config
  ;; ;; optional IDE integration with Monet
  ;; (add-hook 'claude-code-process-environment-functions #'monet-start-server-function)
  ;; (monet-mode 1)

  (claude-code-mode)
  :bind-keymap ("C-c c" . claude-code-command-map)

  ;; Optionally define a repeat map so that "M" will cycle thru Claude auto-accept/plan/confirm modes after invoking claude-code-cycle-mode / C-c M.
  :bind
  (:repeat-map my-claude-code-map ("M" . claude-code-cycle-mode)))

(provide 'ai)
;;; ai.el ends here
