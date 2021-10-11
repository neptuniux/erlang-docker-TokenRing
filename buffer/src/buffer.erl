-module(buffer).
-export([start/1, balance/1]).
-define(FULL, 100).
-define(BALANCE, 0).

balance(Count) ->
    receive
        {produce, Producer} -> 
            if
                Count < ?FULL ->
                    Producer ! ack,
                    balance(Count+1);
                true ->
                    io:format("buffer full~n"),
                    Producer ! full,
                    balance(Count)
            end;
        {consume, Consumer} -> 
            if
                Count > 0 ->
                    Consumer ! ack,
                    balance(Count-1);
                true ->
                    io:format("buffer empty~n"),
                    Consumer ! empty,
                    balance(Count)
            end
    end,
    unregister(buffer).


start([ConsumerNode, ProducerNode]) ->
    
    From = {buffer, node()},
    net_adm:ping(ConsumerNode),
    net_adm:ping(ProducerNode),

    Consumer = {consumer, ConsumerNode},
    Producer = {producer, ProducerNode},
    try Consumer ! {start, From, Producer} of
        _ -> 
            try  Producer ! {start, From, Consumer} of 
                _ -> 
                io:format("starting...~n"),
                P = spawn(?MODULE, balance, [?BALANCE]),
                register(buffer, P)
            catch 
                error:E -> timer:sleep(timer:seconds(1)),io:format("producer ~p not found: error ~p~n", [Producer, E]), start([Consumer, Producer])
            end
    catch
        error:E -> timer:sleep(timer:seconds(1)), io:format("consumer ~p not found: error ~p~n", [Consumer, E]), start([Consumer, Producer])
    end,
    {ok, buffer}.

