Vodki
=====

Table of Contents
-----------------

* [Install](#install)
* [Running](#running)
* [Contribute](#contribute)
* [Licence](#licence)


<a name="install" />

Install
-------

At present, it is assumed the user knows some of the Haskell eco system and
in particular wrangling cabal-dev to obtain dependencies.

I plan to offer pre-built binaries for x86_64 OSX and Linux in future.

You will need reasonably new versions of GHC and the Haskell Platform which
you can obtain here: http://www.haskell.org/platform/

Then run `make install` in the root directory to compile vodki.


<a name="running">

Running
-------

After a successful compile, the `./vodki` symlink should be pointing to the built binary.

Command line flags are used to configure vodki, below is a table containing
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
    <td><code>INT,INT,...</code></td>
    <td>Calculate the Nth percentile(s) for timers</td>
    <td><code>percentThreshold</code></td>
  </tr>

   <tr>
    <td><code>--console</code></td>
    <td></td>
    <td><code>receive,flush,...</code></td>
    <td>Which [receive,invalid,parse,flush] events to log</td>
    <td><code>debug</code>, <code>dumpMessages</code></td>
  </tr>

   <tr>
    <td><code>--graphite</code></td>
    <td></td>
    <td><code>HOST:PORT,HOST:PORT,...</code></td>
    <td>Graphite hosts to deliver metrics to</td>
    <td><code>graphiteHost</code>, <code>graphitePort</code></td>
  </tr>

  <tr>
    <td><code>--repeater</code></td>
    <td></td>
    <td><code>HOST:PORT,HOST:PORT,...</code></td>
    <td>Statsd hosts to forward raw (unaggregated) packets to</td>
    <td><code>repeater</code></td>
  </tr>

  <tr>
    <td><code>--statsd</code></td>
    <td></td>
    <td><code>HOST:PORT,HOST:PORT,...</code></td>
    <td>Statsd hosts to forward aggregated counters to</td>
    <td><code>backends</code></td>
  </tr>

</table>

<a name="contribute" />

Contribute
----------

For any problems, comments or feedback please create an issue [here on GitHub](github.com/brendanhay/vodki/issues).


<a name="licence" />

Licence
-------

Vodki is released under the [Mozilla Public License Version 2.0](http://www.mozilla.org/MPL/)
