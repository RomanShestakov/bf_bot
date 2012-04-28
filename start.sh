#!/bin/sh

# need an argument, MarketId , e.g.102873784
EXPECTED_ARGS=1
marketId=$1
cd `dirname $0`
exec erl -pa $PWD/ebin -pa $PWD/deps/*/ebin -name $marketId@127.0.0.1 -setcookie rs -config ebin/bf_bot -s bf_bot_app -bf_bot marketId $marketId
