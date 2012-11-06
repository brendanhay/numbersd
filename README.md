
NumbersD
=====

Table of Contents
-----------------

* [Compatibility](#compatibility)
* [Install](#install)
* [Running](#running)
* [Contribute](#contribute)
* [Licence](#licence)


<a name="compatibility" />

Compatibility
-------------

> TODO


<a name="install" />

Install
-------

At present, it is assumed the user knows some of the Haskell eco system and
in particular wrangling cabal-dev to obtain dependencies. I plan to offer pre-built binaries for x86_64 OSX and Linux in future.

You will need reasonably new versions of GHC and the Haskell Platform which
you can obtain [here](http://www.haskell.org/platform/), then run `make install` in the root directory to compile numbersd.


<a name="running">

Running
-------

After a successful compile, the `./numbersd` symlink should be pointing to the built binary.

Command line flags are used to configure numbersd, below is a table containing
the available settings and which statsd configuration keys they pertain to:

<table width="100%">

  <tr>
    <th>Flag</th>
    <th>Default</th>
    <th>Format</th>
    <th>About</th>
    <th>Statsd Equivalent</th>
  </tr>

  <tr>
    <td><code>--server</code></td>
    <td><code>0.0.0.0:8125</code></td>
    <td><code>HOST:PORT</code></td>
    <td>Incoming stats UDP address and port</td>
    <td><code>address</code>, <code>port</code></td>
  </tr>

   <tr>
    <td><code>--management</code></td>
    <td><code>0.0.0.0:8126</code></td>
    <td><code>HOST:PORT</code></td>
    <td>Management interface TCP address and port</td>
    <td><code>mgmt_address</code>, <code>mgmt_port</code></td>
  </tr>

   <tr>
    <td><code>--interval</code></td>
    <td><code>10</code></td>
    <td><code>INT</code></td>
    <td>Interval between key flushes to subscribed sinks</td>
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
    <td><code>--log</code></td>
    <td></td>
    <td><code>receive,...</code></td>
    <td>Combination of receive, invalid, parse, or flush events to log</td>
    <td><code>debug</code>, <code>dumpMessages</code></td>
  </tr>

  <tr>
    <td><code>--log-path</code></td>
    <td><code>stdout</code></td>
    <td><code>PATH</code></td>
    <td>Log file path to write events to</td>
    <td><code>log</code></td>
  </tr>

  <tr>
    <td><code>--graphite</code></td>
    <td></td>
    <td><code>HOST:PORT,...</code></td>
    <td>Graphite hosts to deliver metrics to</td>
    <td><code>graphiteHost</code>, <code>graphitePort</code></td>
  </tr>

  <tr>
    <td><code>--broadcast</code></td>
    <td></td>
    <td><code>HOST:PORT,...</code></td>
    <td>Hosts to broadcast raw, unaggregated packets to</td>
    <td><code>repeater</code></td>
  </tr>

  <tr>
    <td><code>--downstream</code></td>
    <td></td>
    <td><code>HOST:PORT,...</code></td>
    <td>Hosts to forward aggregated, statsd formatted counters to</td>
    <td><code>statsd-backend</code></td>
  </tr>

</table>

<a name="contribute" />

Contribute
----------

For any problems, comments or feedback please create an issue [here on GitHub](github.com/brendanhay/numbersd/issues).


<a name="licence" />

Licence
-------

NumbersD is released under the [Mozilla Public License Version 2.0](http://www.mozilla.org/MPL/)
