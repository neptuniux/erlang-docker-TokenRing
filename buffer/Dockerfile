FROM erlang:alpine

RUN mkdir /app
WORKDIR /app
ENV consumer udf
ENV producer udf
ENV hostname buffer@buffer.com
COPY src/ /app
RUN erlc buffer.erl
ENTRYPOINT erl -name ${hostname} -setcookie cookie -noshell -s buffer start ${consumer} ${producer}