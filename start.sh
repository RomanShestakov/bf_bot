#!/bin/sh
cd `dirname $0`
exec erl -pa $PWD/ebin -pa $PWD/deps/*/ebin -name bf_bot@127.0.0.1 -setcookie rs -s bf_bot_app -bf_bot marketId 102873784 
