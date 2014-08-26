# A Docker file to marmalade in a docker
FROM nicferrier/elnode
MAINTAINER nic@ferrier.me.uk
USER root
ADD Dockerfile-marmalade-deploy.el /tmp/Dockerfile-marmalade-deploy.el
ADD builds/ /home/emacs/builds
ADD boot.el /home/emacs/boot.el
RUN chown -R emacs /home/emacs/builds
RUN chown emacs /tmp/Dockerfile-marmalade-deploy.el
RUN chown emacs /home/emacs/boot.el
USER emacs
WORKDIR /home/emacs
ENV HOME /home/emacs
RUN mkdir /home/emacs/marmalade
VOLUME /home/emacs/marmalade/
RUN /usr/local/emacs/bin/emacs -daemon -l /tmp/Dockerfile-marmalade-deploy.el
EXPOSE 8005
CMD /usr/local/emacs/bin/emacs -daemon -l  /home/emacs/boot.el ; tail -f /dev/null
