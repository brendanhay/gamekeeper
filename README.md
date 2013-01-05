# gamekeeper

## Table of Contents

* [Introduction](#introduction)
* [Functionality](#functionality)
    - [Measure](#measure)
    - [Check](#check)
    - [Prune](#prune)
* [Install](#install)
* [Configuration](#configuration)
* [Running](#running)
* [Contribute](#contribute)
* [Licence](#licence)


## Introduction

> TODO


## Functionality

gamekeeper has three modes of operation, each corresponding to a different
subset of functionality and accessible via the following subcommands:

### Measure

The `measure` subcommand will emit a series of metrics from the specified
`--uri` for a number of RabbitMQ/AMQP primitives.

All metrics are prefixed into sinks (Ganglia, Graphite, etc) with the
identifier: `<node_name>.rabbit.`

> The `<node_name>` constant is currently determined by escaping the local hostname, and will be configurable in a future release.

**Overview**

* `message.total`
* `message.ready`
* `message.unacked`
* `rate.publish`
* `rate.deliver`
* `rate.redeliver`
* `rate.confirm`
* `rate.ack`

**Connections**

* `connection.total`
* `connection.idle` - Calculated relative to the specified `--days` setting

**Channels**

* `channel.total`
* `channel.publisher` - Number of publisher/ingress channels
* `channel.consumer` - Number of consumer/egress channels
* `channel.duplex` - Number of channels marked as both publishing and consuming
* `channel.inactive`

**Exchanges**

* `exchange.rate.<name>` - Message rate per exchange

**Queues**

* `queue.total`
* `queue.idle` - Determined by message residence and flow
* `queue.messages.<name>` - Ready messages per queue
* `queue.consumers.<name>` - Consumers per queue
* `queue.memory.<name>` - Memory usage per queue

**Bindings**

* `binding.total` - Overall number of AMQP bindings

The output sink can be configured to emit to `Stdout,,`,
`Ganglia,<host>,<port>`, or `Graphite,<host>,<port>` using the `--sink`
argument. The underlying [network-metrics](http://github.com/brendanhay/network-metrics) also
supports writing to `Statsd,<host>,<port>` but this is pointless, and not
recommended due to the fact the RabbitMQ management plugin performs pre-aggregation.

By default metrics will be printed to stdout.

> At time of writing [SoundCloud](http://www.soundcloud.com) emits all
> RabbitMQ metrics to Ganglia specifically

### Check

The `check` subcommand is used to perform a high-level inspection of
either the general node health, or a specific queue's health.

All output is to `stdout` in the
[Nagios NPRE Plugin](http://nagiosplug.sourceforge.net/developer-guidelines.html)
format.

**Node**

The Distributed Erlang [sname](http://www.erlang.org/doc/reference_manual/distributed.html) of the
RabbitMQ node needs to be specified via the `--name` argument, so gamekeeper can calculate the correct HTTP API uri to
request. For example, the node `rabbit@localhost` would result in HTTP requests to `http://localhost:15672/#/nodes/rabbit%40localhost`

Warning and critical levels can be specified for both message residence
and memory usage. A single check is performed and the output is combined.

A warning or critical for either memory residence or memory usage will
result in the most severe being used as the NPRE exit code and one line
summary.

> The memory usage warning and critical levels are specified in Gigabyte units

**Queue**

Queue checks are the same as the node level check, but local to a specifically
named queue.

> The memory usage warning and critical levels are specified in Megabyte units

### Prune

The `prune` subcommand is used via a manual invocation of gamekeeper and is
used to remove (via HTTP DELETE) idle connections and unused queues.

This is primarily useful if you do not use AMQP heartbeats and have problems
with dangling load-balancer connections through something like LVS or HAProxy.


## Install

At present, it is assumed the user knows some of the Haskell eco system and
in particular wrangling cabal-dev to obtain dependencies. I plan to offer pre-built binaries for x86_64 OSX and Linux in future.

You will need reasonably new versions of GHC and the Haskell Platform which
you can obtain [here](http://www.haskell.org/platform/), then run `make install` in the root directory to compile gamekeeper.

There is also a Chef Cookbook which can be used to manage gamekeeper, if that's how you swing: https://github.com/brendanhay/gamekeeper-cookbook


## Configuration

Command line flags are used to configure gamekeeper, you can access help for
the top-level program and various subcommands via the `--help` switch.


### Available Flags

<table width="100%">

  <tr>
    <th>Command</th>
    <th>Flag</th>
    <th>Default</th>
    <th>Format</th>
    <th>About</th>
  </tr>

</table>

<table width="100%">

  <tr>
    <td><code>measure</code></td>
    <td><code>--uri</code></td>
    <td><code>http://guest:guest@localhost:15672</code></td>
    <td><code>URI</code></td>
    <td>Address of the RabbitMQ API to poll</td>
  </tr>

  <tr>
    <td><code>measure</code></td>
    <td><code>--days</code></td>
    <td><code>30</code></td>
    <td><code>INT</code></td>
    <td>Number of days before a conncetion is considered stale</td>
  </tr>

  <tr>
    <td><code>measure</code></td>
    <td><code>--sink</code></td>
    <td><code>Stdout,,</code></td>
    <td><code>SINK,HOST,PORT</code></td>
    <td>Sink options describing the type and host/port combination</td>
  </tr>

</table>

## Running

After a successful compile, the `./gamekeeper` symlink will be pointing to
the built binary under `./dist`


## Contribute

For any problems, comments or feedback please create an issue [here on GitHub](github.com/brendanhay/gamekeeper/issues).


## Licence

gamekeeper is released under the [Mozilla Public License Version 2.0](http://www.mozilla.org/MPL/)
