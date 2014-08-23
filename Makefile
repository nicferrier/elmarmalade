# Build the docker
docker:=sudo docker

marmalade: build
	$(docker) push nicferrier/elmarmalade

build:
	$(docker) build --no-cache -t nicferrier/elmarmalade .

# End
