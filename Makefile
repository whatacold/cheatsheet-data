all:
	mkdir -p ./data/
	emacs -Q --script gen-elisp-api-data.el
