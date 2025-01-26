FROM haskell:9.10.1-slim-bullseye AS build

# upgrade cabal
RUN cabal update
RUN cabal install cabal-install

WORKDIR /opt/dagensord
COPY ./dagensord.cabal ./
# Download and build deps
RUN cabal build --dependencies-only -j
COPY . ./
RUN cabal install --install-method=copy --installdir=./out

FROM debian:bullseye-slim
WORKDIR /opt/dagensord
COPY --from=build /opt/dagensord/out/dagensord ./dagensord
COPY ./public ./public
COPY ./words.json ./words.json
RUN chmod +x ./dagensord
ENTRYPOINT [ "./dagensord" ]
