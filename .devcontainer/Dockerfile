FROM mcr.microsoft.com/devcontainers/universal

RUN cd /root && \
        curl -L -o autoconf.tar.gz https://ftp.gnu.org/gnu/autoconf/autoconf-2.71.tar.gz && \
        tar fxz autoconf.tar.gz && \
        cd autoconf-2.71 && \
        ./configure --prefix=/usr && \
        make && \
        make install && \
        cd .. && \
        rm -rf autoconf-2.71 autoconf.tar.gz

ARG USERNAME=vscode
ARG USER_UID=1000
ARG USER_GID=$USER_UID

RUN apt-get update && \
        apt-get install -y opam && \
        apt-get clean && \
        rm -rf /var/lib/apt/lists/*

RUN opam init -a --yes && \
        opam switch create 5.2.0+flambda2 --yes \
        --repos "with-extensions=git+https://github.com/janestreet/opam-repository.git#with-extensions,default" && \
        eval $(opam env --switch 5.2.0+flambda2) && \
        opam install --yes ocamlformat merlin ocaml-lsp-server utop
