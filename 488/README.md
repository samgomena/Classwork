# CS 488 - Cloud & Cluster Data Management

## Spring 2021

### David Maier

### Portland State University

Run the mongo container

```sh
docker run --detach --name mongodb-488 --port 27017:27017 mongo:latest
```

Exec onto the container to run queries via the mongo CLI

```sh
docker exec -it project-part-3 bash

# In the container (i.e. the prompt should look like dis: root@<hash>:/# )
 mongo
```
