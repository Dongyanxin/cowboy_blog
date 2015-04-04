#!/bin/sh

erl -pa ../ebin \
    +K true \
    -name ecm@127.0.0.1 \
    -s ecm_app startup 
