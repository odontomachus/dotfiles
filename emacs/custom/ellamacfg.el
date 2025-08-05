;;; ellamacfg --- Setup ellama

;;; Commentary:
;;;  configure ellama using ollama local models

;;; Code:
(use-package ellama
  :ensure t
  :bind ("C-c e" . ellama-transient-main-menu)
  :init
  ;; setup key bindings
  ;; (setopt ellama-keymap-prefix "C-c e")
  ;; language you want ellama to translate to
  (setopt ellama-language "English")
  ;; could be llm-openai for example
  (require 'llm-ollama)
  (setopt ellama-provider
	  (make-llm-ollama
	   ;; this model should be pulled to use it
	   ;; value should be the same as you print in terminal during pull
	   :chat-model "qwen3:14b"
	   :embedding-model "granite-embedding:30m"
	   :default-chat-non-standard-params '(("num_ctx" . 65536))))
  (setopt ellama-summarization-provider
	  (make-llm-ollama
	   :chat-model "granite3.1-dense:8b"
	   :embedding-model "granite-embedding:30m"
	   :default-chat-non-standard-params '(("num_ctx" . 32768))))
  (setopt ellama-coding-provider
	  (make-llm-ollama
	   :chat-model "granite3.1-dense:8b"
	   :embedding-model "granite-embedding:30m"
	   :default-chat-non-standard-params '(("num_ctx" . 32768))))
  ;; Naming new sessions with llm
  (setopt ellama-naming-provider
	  (make-llm-ollama
	   :chat-model "llama3:8b-instruct-q8_0"
	   :embedding-model "nomic-embed-text"
	   :default-chat-non-standard-params '(("stop" . ("\n")))))
  (setopt ellama-naming-scheme 'ellama-generate-name-by-llm)
  ;; Translation llm provider
  (setopt ellama-translation-provider
	  (make-llm-ollama
	   :chat-model "llama3:8b-instruct-q8_0"
	   :embedding-model "nomic-embed-text"
	   :default-chat-non-standard-params
	   '(("num_ctx" . 32768))))
  ;; customize display buffer behaviour
  ;; See ~(info "(elisp) Buffer Display Action Functions")~
  (setopt ellama-chat-display-action-function #'display-buffer-full-frame)
  (setopt ellama-instant-display-action-function #'display-buffer-at-bottom)
  :config
  ;; send last message in chat buffer with C-c C-c
  (add-hook 'org-ctrl-c-ctrl-c-hook #'ellama-chat-send-last-message))

(provide 'ellamacfg)
;;; ellamacfg.el ends here
