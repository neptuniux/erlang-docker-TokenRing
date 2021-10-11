FROM erlang:alpine

RUN mkdir /app
WORKDIR /app
ENV hostname consumer@consumer.com 
COPY src/ /app
RUN erlc consumer.erl
ENTRYPOINT erl -name ${hostname} -setcookie cookie -noshell -run consumer start