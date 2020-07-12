#!/bin/sh -xe

ENV=$(env | sort -u)

echo "================== Environment ====================="
echo ${ENV}
echo "===================================================="

/factdb/bin/factdb daemon

mkdir -p /factdb/tmp/log

sleep 1

tail -f /factdb/tmp/log/*
