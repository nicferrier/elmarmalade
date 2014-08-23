# A Docker file to marmalade in a docker
FROM nicferrier/elnode
MAINTAINER nic@ferrier.me.uk
USER root
ADD deploy.el /tmp/deploy.el
RUN chown emacs /tmp/deploy.el
USER emacs
WORKDIR /home/emacs
ENV HOME /home/emacs
RUN mkdir /home/emacs/marmalade
VOLUME /home/emacs/marmalade/
RUN /usr/local/emacs/bin/emacs -daemon -l /tmp/deploy.el
EXPOSE 8005
#CMD /usr/local/emacs/bin/emacs -daemon ; tail -f /dev/null
