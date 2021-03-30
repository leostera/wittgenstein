FROM elixir:1.10-alpine

ENV MIX_ENV=prod

RUN apk add git make g++

WORKDIR /factdb

RUN mix local.hex --force && mix local.rebar --force
COPY mix.lock /factdb/mix.lock
COPY mix.exs /factdb/mix.exs
RUN mix deps.get
RUN mix compile

COPY src /factdb/src
COPY lib /factdb/lib
COPY config /factdb/config

RUN mix release --force --overwrite --path /release

CMD ["/bin/sh"]
