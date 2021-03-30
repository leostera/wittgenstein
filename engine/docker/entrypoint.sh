#!/bin/sh


echo "================== Environment ====================="
env | sort -u
echo "===================================================="

exec /factdb/bin/factdb start
