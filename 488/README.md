# CS 488 - Cloud & Cluster Data Management

## Spring 2021

### David Maier

### Portland State University

#### Downloading data

The data downloaded is from the latest date available at the time the script was written.
It's possible newer data is available, or the data attempting to be downloading no longer exists.

You can find the data [here](http://insideairbnb.com/get-the-data.html).

```bash
./download_data.sh
```

Note: The script expects to be run in a `*nix` environment with `bash` and `wget` present.

#### Running the mongo container

Start the container

```sh
docker run --detach --name mongo -p 27017:27017 mongo:latest
```

Exec onto the container to run queries via the mongo CLI

```sh
docker exec -it project-part-3 bash

# In the container (i.e. the prompt should look like dis: root@<hash>:/# )
mongo
```

#### Load data into the container

```sh
node loadData.js
```

**Important Notes**:

- Not surprisingly, this should be done _after_ the data has been downloaded.
- The program doesn't automatically exit due to incompatibilities between promise and callback based async code being used. You'll have to `CTRL-C` when you want to exit.
  - Consequently, it only works in POSIX environments because of the way it captures `SIGINT` calls to close the connection to the database.
