# gamekeeper

## Table of Contents

* [Configuration](#configuration)
* [Running](#running)
* [Contribute](#contribute)
* [Licence](#licence)


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
