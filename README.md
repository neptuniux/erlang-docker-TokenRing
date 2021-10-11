# Erlang dockerized Producer-consumer
## Presentation
This is a minimalistic example of an Erlang distributed [Producer-consumer](https://en.wikipedia.org/wiki/Producer%E2%80%93consumer_problem) program. The purpose of this project is mainly the communication of Erlang processes through Docker container.

## File structure
Each of the three actors has a dedicated folder, Erlang file and Dockerfile. The Docker images can be found on [Docker hub](https://hub.docker.com/u/wichtf).

## Instructions
- First of all make sure you have a proper [docker installation](https://docs.docker.com/engine/install/) with [docker-compose](https://docs.docker.com/compose/install/).
- Then bring up the project
```
docker-compose up -d
```
- When done, audit the logs of any of the three containers: `<container_name>` can be anything between `producer.com`, `consumer.com` or `buffer.com`
```
docker logs -f <container_name>
```
- You should be able to see the state of the container.
