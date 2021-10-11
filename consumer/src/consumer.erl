-module(consumer).
-export([start/0, consume/3,pause/1]).
-define(NSLOTS, 1000).
-define(DELAY, 300).

pause(Factor) ->
    timer:sleep(round(rand:uniform(Factor))).

consume(undefined, undefined, idle) ->
    receive
        {start, Buffer, Producer} -> io:format("initiated by ~p~n", [Buffer]), Buffer ! {consume, {consumer, node()}}, pause(?DELAY),consume(Buffer, Producer, consuming)
    end;

consume(Buffer, Producer, consuming) ->
    receive
        ack -> io:format("consuming~n"), Buffer ! {consume, {consumer, node()}}, pause(?DELAY),consume(Buffer, Producer, consuming);
        empty -> Producer ! wake, io:format("sleeping~n"),pause(?DELAY),consume(Buffer, Producer, sleeping)
    end;

consume(Buffer, Producer, sleeping) ->
    receive
        wake -> io:format("waking up~n"),Buffer ! {consume, {consumer, node()}}, pause(?DELAY),consume(Buffer, Producer, consuming)
    end.




start() -> 
    io:format("starting...~n"),
    P=spawn(?MODULE, consume, [undefined, undefined, idle]),
    register(consumer, P),
    {ok, consumer}.
