FROM ubuntu:14.04
RUN apt-get update
# stack dependencies
RUN apt-get install g++ gcc libc6-dev libffi-dev libgmp-dev make xz-utils \
zlib1g-dev git gnupg -y

# stack itself
RUN apt-get install wget
RUN wget -qO- https://get.haskellstack.org/ | sh

# add stack to path
RUN export PATH=$PATH:/usr/local/bin

# copy files and get dependencies
RUN stack update

WORKDIR /sharons_app
COPY sharons-app.cabal .
COPY stack.yaml .

RUN stack setup

COPY app/*.hs app/
COPY src/*.hs src/
COPY test/*.hs test/
COPY auctions.json .

#build
RUN stack build

# run executable
ENTRYPOINT ["stack"]
CMD ["exec", "sharons-exe"]
