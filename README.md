# test_harness

## Getting Started

Begin by forking this repo and cloning your fork. GitHub has apps for
[Mac](http://mac.github.com/) and
[Windows](http://windows.github.com/) that make this easier.

### 1. Set up an Erlang environment

This project is configured using Docker. Building the project and running tests
and deploys should be done through Docker. In case you need to configure and
setup your editor, the current Erlang version is `20.0.2`.

Get started by installing [Docker (Community
Edition)](https://docs.docker.com/install/).

### 2. Fetch & build dependencies

To fetch and build dependencies run

```
$ make deps
```

### 3. Configure the app settings

For simplicity just copy the sample configuration:

```
$ cp config/sys.config{.sample,}
```

### 4. Run the app

To start a local server run

```
$ make run
```

Quit running the app by typing `q().` on the Erlang shell you just started.

## Testing

In order to run the tests make sure you create a `config/test.config` file with
the right configuration. For simplicity, just copy the sample file.

```
$ cp config/test.config{.sample,}
```

To run the tests locally (with the local db started and cities
imported):

```
$ make tests
```
