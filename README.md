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
subset of functionality.

### Measure

The `measure` subcommand will emit a series of metrics from the specified
`--uri` for a number of RabbitMQ/AMQP primitives.

All metrics are prefixed into sinks (Ganglia, Graphite, etc) with the
identifier: `<node_name>.rabbit.`

The `<node_name>` constant is currently determined by escaping the local
hostname, and will be configurable in a future release.

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

### Check

> TODO

### Prune

> TODO


## Install

> TODO


## Configuration

> TODO

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
    <td><code>http://guest:guest@localhost:5672</code></td>
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

> TODO


## Contribute

For any problems, comments or feedback please create an issue [here on GitHub](github.com/brendanhay/gamekeeper/issues).


## Licence

gamekeeper is released under the [Mozilla Public License Version 2.0](http://www.mozilla.org/MPL/)
