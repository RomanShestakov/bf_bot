bf_bot - betfair client application 
==============================================

client for bf_gateway, allows to watch betfair market qoutes for a given market, which bf_gateway pushes to 0MX

## Dependencies

1. log4erl for logging
2. erlzmq - erlang bindings for 0MQ

## Building

bf_bot uses rebar for building and wraps it in a Makefile for convenience.

First clone from GitHub:

    $ git clone git://github.com/romanshestakov/bf_bot.git

Then change into the newly created directory:

    $ cd bf_bot

then build:

    $ make

Rebar will first pull in dependencies from GitHub, attempt to build them all, then build bf_bot.

you need to have bf_gateway built and running to use bf_bot

start bf_bot:

./start.sh <MarketID>

e.g. to subsribe to market 102873783:
./start.sh 102873783


if everything is ok, you should see market quotes appearing on the console


License
=======
