-module(producer).
-export([start/0, produce/3]).
-define(NSLOTS, 1000).
-define(DELAY, 100).


pause(Factor) ->
    timer:sleep(round(rand:uniform(Factor))).

produce(undefined, undefined, idle) ->
    receive
        {start, Buffer, Consumer} -> io:format("initiated by ~p~n", [Buffer]),Buffer ! {produce, {producer, node()}}, pause(?DELAY), produce(Buffer, Consumer, producing)
    end;

produce(Buffer, Consumer, producing) ->
    receive
        ack -> io:format("producing~n"),Buffer ! {produce, {producer, node()}}, pause(?DELAY),produce(Buffer, Consumer, producing);
        full -> io:format("sleeping~n"),Consumer ! wake, pause(?DELAY),produce(Buffer, Consumer, sleeping)        
    end;
produce(Buffer, Consumer, sleeping) ->
    receive
        wake -> io:format("waking up~n"), Buffer ! {produce, {producer, node()}}, pause(?DELAY),produce(Buffer, Consumer, producing)
    end.

start() -> 
    io:format("starting...~n"),
    P=spawn(?MODULE, produce, [undefined, undefined, idle]),
    register(producer, P),
    {ok, producer}.