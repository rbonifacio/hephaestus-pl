FROM ubuntu:14.04

RUN apt-get update

RUN apt-get install -qy    \
        haskell-platform   \
        build-essential    \
        libghc-ncurses-dev \
        ncurses-dev        \
        git        

RUN cabal update

RUN cabal install bimap        \
                  bitset       \
                  parse-dimacs \
                  hxt-relaxng

RUN cabal install hatt     \
                  fgl      \
                  graphviz

RUN cabal install syb      \
                  MissingH

RUN git clone https://github.com/rbonifacio/hephaestus-pl.git
RUN cd hephaestus-pl

WORKDIR /hephaestus-pl

# to build as a Docker image
# docker build -t hephaestus
# docker run -it hephaestus /bin/bash
    