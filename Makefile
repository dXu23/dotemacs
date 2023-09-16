DRONES_DIR = $(shell git config "borg.drones-directory" || echo "lib")

-include $(DRONES_DIR)/borg/borg.mk

lisp/loaddefs.el: lisp/hooks.el
	cd lisp && $(EMACS) -Q -batch -f loaddefs-generate-batch loaddefs.el config

bootstrap-borg:
	@git submodule--helper clone --name borg --path $(DRONES_DIR)/borg \
	--url git@github.com:emacscollective/borg.git
	@cd $(DRONES_DIR)/borg; git symbolic-ref HEAD refs/heads/main
	@cd $(DRONES_DIR)/borg; git reset --hard HEAD

clean-loaddefs:
	rm lisp/loaddefs.el
