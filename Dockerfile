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

# cache stack stuff
RUN stack update

# add files to build
WORKDIR /sharons_app
COPY app/*.hs app/
COPY sharons-app.cabal .
COPY src/*.hs src/
COPY stack.yaml .
COPY test/*.hs test/
COPY auctions.json .

#build
RUN stack setup
RUN stack build

# run executable
ENTRYPOINT ["stack"]
CMD ["exec", "sharons-exe"]
