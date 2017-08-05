# Base image
#
# VERSION   0.2
FROM        python:3
MAINTAINER  Paul R. Tagliamonte <paultag@hylang.org>

ADD . /opt/hylang/hyhy
RUN pip3 install -e /opt/hylang/hyhy

CMD ["hyhy"]
