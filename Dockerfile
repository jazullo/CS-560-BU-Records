# Ubuntu 22
ARG VARIANT="jammy" 
ARG DEBIAN_FRONTEND=noninteractive
FROM mcr.microsoft.com/vscode/devcontainers/base:0-${VARIANT} as base
ENV USERNAME=vscode

RUN apt-get update
RUN apt-get --yes --force-yes install opam
RUN opam init -y --disable-sandboxing
RUN eval $(opam env)
RUN opam install -y dune menhir batteries
RUN git clone https://github.com/jazullo/CS-560-BU-Records.git
