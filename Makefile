# Build the docker
docker:=sudo docker
emacs:=~/emacs/bin/emacs


marmalade: build
	$(docker) push nicferrier/elmarmalade

build: test
	$(emacs) -batch --eval '(progn(package-initialize)(elpakit-make-multi "." "builds"))'
	$(docker) build --no-cache -t nicferrier/elmarmalade .

test:
	[ -x $(emacs) ] || exit 1 # missing emacs?
	$(docker) --help 2> /dev/null || exit 1 # missing docker?

# End
