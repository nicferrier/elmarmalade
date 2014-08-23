# A Docker file to marmalade in a docker
FROM nicferrier/elnode
MAINTAINER nic@ferrier.me.uk
USER root
ADD Dockerfile-deploy.el /tmp/Dockerfile-deploy.el
ADD boot.el /home/emacs/boot.el
RUN chown emacs /tmp/Dockerfile-deploy.el
RUN chown emacs /home/emacs/boot.el
USER emacs
WORKDIR /home/emacs
ENV HOME /home/emacs
RUN mkdir /home/emacs/marmalade
VOLUME /home/emacs/marmalade/
RUN /usr/local/emacs/bin/emacs -daemon -l /tmp/Dockerfile-deploy.el
EXPOSE 8005
CMD /usr/local/emacs/bin/emacs -daemon -l  /home/emacs/boot.el ; tail -f /dev/null
