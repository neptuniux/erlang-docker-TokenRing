FROM erlang:alpine

RUN mkdir /app
WORKDIR /app
ENV hostname producer@producer.com 
COPY src/ /app
RUN erlc producer.erl
ENTRYPOINT erl -name ${hostname} -setcookie cookie -noshell -run producer start