# NumbersD


Table of Contents
-----------------

* [Introduction](#introduction)
* [Compatibility](#compatibility)
* [Functionality](#functionality)
    - [Listeners](#listeners)
    - [Overview and Time Series](#overview-and-time-series)
    - [Graphites](#graphites)
    - [Broadcasters](#broadcasters)
    - [Downstreams](#downstreams)
* [Scenarios](#scenarios)
* [Install](#install)
* [Configuration](#configuration)
    - [Available Flags](#available-flags)
    - [Flag Types](#flag-types)
* [Running](#running)
* [Contribute](#contribute)
* [Licence](#licence)


## Introduction

> TODO


## Compatibility

> TODO


## Functionality

NumbersD has identical aggregation characteristics to StatsD. It differs significantly in terms of
philosophy and intended usage. Below are some of the behaviours available.

### Listeners

A listener is a scheme, host, and port specification for a listen socket which will accept and parse
metrics from incoming connections. They are specified with either a `tcp://` or `udp://` scheme to
control the type of listening socket.

Multiple listeners can be passed as a comma seperated list to the `--listeners` flag
to listen upon multiple ports and protocols simultaneously.


### Overview and Time Series

If an HTTP port is specified, NumbersD will start an embedded HTTP server. GET requests to
the following request paths will be responsed with an appropriate content type:

* `/overview.json` Internal counters and runtime information.
* `/numbersd.whisper` Low resolution time series in Graphite compatible format. (Identical to `&rawData=true`)
* `/numbersd.json` JSON representation of the `.whisper` format above

The `.whisper` response type is intended to be used from Nagios or other monitoring tools
to connect directly to a `NumbersD` instance running alongside an applicaiton.

There are a number of `check_graphite` Nagios NPRE plugins available which should work identically
to pointing directly at an instance of Graphite.


### Graphites

(Yes, plural)

As with all list styled command flags a list of tcp schemed URIs can be specified to
simultaneously connect to multiple backend Graphite instnaces.


### Broadcasters

Broadcasters perform identically to StatsD's `repeater` backend. They simply forward on received metrics
to a list of tcp and udp schemed URIs.

The intent being, you can listen on TCP and then broadcast over a UDP connection, or vice versa.


### Downstreams

Downstreams again take a list of tcp and udp schemed URIs, with the closest simalarity being StatsD's
`statsd-backend` plugin.

The metrics that can be safely aggregated without losing precision or causing 'slopes' (such as counters)
are forwarded upon `flush`, all the others are forwarded unmodified.


## Scenarios

The intent of many of the behaviours above, was to provide more granular mechanisms for scaling and organising
a herirachy of metric aggregators. Here are some scenarios that prompted the development of NumbersD.

### Monitoring

Using UDP for stats delivery is great, it makes it very easy to write a client and emit metrics but due to the lack
of reliable transmission it makes unsuitable for more critical tasks, even on a network under your control.

An example monitoring workflow I've observed in production, looks something like:

<a name="figure-1" />
<p align="center">
  <img src="https://raw.github.com/brendanhay/numbersd/master/img/statsd-monitoring.png" />
</p>
**Figure 1**

1. Application emits unreliable UDP packets that are (hopefully) delivered to a monolithic aggregator instance.
3. The aggregator sends packets over a TCP connection to Graphite.
4. Nagios invokes an NPRE check on the application host.
5. The NPRE check reaches out across the network to the Graphite API to quantify application health.

There are 4 actors involved in [Figure 1](#figure-1): the Application, Network, Aggregator, Graphite, and Nagios.

For monitoring to be (remotely) reliable we have to make some assumptions .. so in this case lets' remove
the Network (assume realible UDP transmission) and Nagios (13 year old software always works) from the equation.

If either the aggregator, or Graphite is temporarily unavailable the NPRE check local to the application will
fail and potentially raise a warning/critical alert.

By removing both the aggregator and Graphite from the monitoring workflow, it becomes a romantic dinner date for
two between the application and Nagios:

<a name="figure-2" />
<p align="center">
  <img src="https://raw.github.com/brendanhay/numbersd/master/img/numbersd-monitoring.png" />
</p>
**Figure 2**

1. The application emits UDP packets via the loopback interface to a local NumbersD daemon.
2. NumbersD pushes metrics over a TCP connection to Graphite.
3. Nagios invokes an NPRE check on the application host.
4. The NPRE check calls the local NumbersD daemon's `/numbersd.whisper` time series API.

This has two primary advantages. Firstly, reliability - by ensuring UDP packets are only transmitted
on the localhost. And secondly, by seperating the concerns of metric durability/storage/visualisation
and monitoring, two separate single point of failures have been removed from the monitoring workflow.


## Install

At present, it is assumed the user knows some of the Haskell eco system and
in particular wrangling cabal-dev to obtain dependencies. I plan to offer pre-built binaries for x86_64 OSX and Linux in future.

You will need reasonably new versions of GHC and the Haskell Platform which
you can obtain [here](http://www.haskell.org/platform/), then run `make install` in the root directory to compile numbersd.


## Configuration

Command line flags are used to configure numbersd, a full table of all the flags is available [here](#available-flags).


### Available Flags

<table width="100%">

  <tr>
    <th>Flag</th>
    <th>Default</th>
    <th>Format</th>
    <th>About</th>
    <th>Statsd Equivalent</th>
  </tr>

  <tr>
    <td><code>--listeners</code></td>
    <td><code>udp://0.0.0.0:8125</code></td>
    <td><code>URI,....</code></td>
    <td>Incoming stats UDP address and port</td>
    <td><code>address</code>, <code>port</code></td>
  </tr>

  <tr>
    <td><code>--http</code></td>
    <td></td>
    <td><code>PORT</code></td>
    <td>HTTP port to serve the overview and time series on</code></td>
    <td><code>mgmt_address</code>, <code>mgmt_port</code></td>
  </tr>

  <tr>
    <td><code>--resolution</code></td>
    <td><code>60</code></td>
    <td><code>INT</code></td>
    <td>Resolution in seconds for time series data</code></td>
    <td></td>
  </tr>

  <tr>
    <td><code>--interval</code></td>
    <td><code>10</code></td>
    <td><code>INT</code></td>
    <td>Interval in seconds between key flushes to subscribed sinks</td>
    <td><code>flushInterval</code></td>
  </tr>

  <tr>
    <td><code>--percentiles</code></td>
    <td><code>90</code></td>
    <td><code>INT,...</code></td>
    <td>Calculate the Nth percentile(s) for timers</td>
    <td><code>percentThreshold</code></td>
  </tr>

  <tr>
    <td><code>--events</code></td>
    <td></td>
    <td><code>EVENT,...</code></td>
    <td>Combination of receive, invalid, parse, or flush events to log</td>
    <td><code>debug</code>, <code>dumpMessages</code></td>
  </tr>

  <tr>
    <td><code>--prefix</code></td>
    <td></td>
    <td><code>STR</code></td>
    <td>Prepended to keys in the http interfaces and graphite</td>
    <td><code>log</code></td>
  </tr>

  <tr>
    <td><code>--graphites</code></td>
    <td></td>
    <td><code>URI,...</code></td>
    <td>Graphite hosts to deliver metrics to</td>
    <td><code>graphiteHost</code>, <code>graphitePort</code></td>
  </tr>

  <tr>
    <td><code>--broadcasts</code></td>
    <td></td>
    <td><code>URI,...</code></td>
    <td>Hosts to broadcast raw, unaggregated packets to</td>
    <td><code>repeater</code></td>
  </tr>

  <tr>
    <td><code>--downstreams</code></td>
    <td></td>
    <td><code>URI,...</code></td>
    <td>Hosts to forward aggregated, statsd formatted counters to</td>
    <td><code>statsd-backend</code></td>
  </tr>

</table>


### Flag Types

* `URI` Combination of scheme, host, and port. The scheme must be one of `(tcp|udp)`.
* `PORT` Port number. Must be within the valid bindable range for non-root users.
* `INT` A valid Haskell [Int](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Prelude.html#t:Int) type.
* `STR` An ASCII encoded string.
* `EVENT` Internal event types must be one of `(receive|invalid|parse|flush)`.
* `[...]` All list types are specified a comma seperated string containing no spaces. For example: `--listeners udp://0.0.0.0:8125,tcp://0.0.0.0:8126` is a valid `[URI]` list.


## Running

After a successful compile, the `./numbersd` symlink should be pointing to the built binary.


## Contribute

For any problems, comments or feedback please create an issue [here on GitHub](github.com/brendanhay/numbersd/issues).


## Licence

NumbersD is released under the [Mozilla Public License Version 2.0](http://www.mozilla.org/MPL/)
