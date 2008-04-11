# Requires GNU make.

EMACS=emacs -q --no-site-file

ELS = $(shell ls -1 *.el)
ELCS = $(ELS:.el=.elc)
all: $(ELCS)
	make $(ELCS)

.el.elc:
	$(EMACS) -batch -L . \
		-eval "(setq max-lisp-eval-depth 1500 max-specpdl-size 3000)" \
		-eval "(mapc (lambda (dir) (add-to-list 'load-path dir)) (parse-colon-path (getenv \"LOAD_PATH\")))" \
		-f batch-byte-compile $*.el
