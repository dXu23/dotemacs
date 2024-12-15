(require 'gptel)

(load-library "core/secrets.el.gpg")

(setopt gptel-model "gpt-3.5-turbo"
        gptel-playback t
        gpt-default-mode 'org-mode
        gptel-api-key #'my/read-openai-key)
