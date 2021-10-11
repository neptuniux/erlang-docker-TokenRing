FROM erlang:alpine

RUN mkdir /app
WORKDIR /app
ENV hostname token_ring@token_ring.com
COPY src/ /app
RUN erlc token_ring.erl
ENTRYPOINT erl -name ${hostname} -setcookie cookie -noshell -run token_ring start [3,3]