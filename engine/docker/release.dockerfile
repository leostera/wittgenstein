FROM alpine:3.12

RUN apk --update add ncurses g++ && rm -rf /var/cache/apk/*

WORKDIR factdb

COPY _build/docker/release/ /factdb/
COPY docker/entrypoint.sh /entrypoint.sh

ENTRYPOINT ["/entrypoint.sh"]
