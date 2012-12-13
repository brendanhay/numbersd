
var graphs = [];        // rickshaw objects
var datum = [];         // metric data
var aliases = [];       // alias strings
var descriptions = [];  // description strings
var realMetrics = [];   // non-false targets

// minutes of data in the live feed
var period = (typeof period == 'undefined') ? 5 : period;
var originalPeriod = period;

// gather our non-false targets
function gatherRealMetrics() {
  var falseTargets = 0;
  for (var i=0; i<metrics.length; i++) {
    if (metrics[i].target === false) {
      falseTargets++;
    } else if (metrics[i].target.match(/^<object/)) {
      falseTargets++;
    } else {
      realMetrics[i - falseTargets] = metrics[i];
    }
  }
}

// build our graph objects
function constructGraphs() {
  for (var i=0; i<realMetrics.length; i++) {
    aliases[i] = realMetrics[i].alias || realMetrics[i].target;
    descriptions[i] = realMetrics[i].description || null;
    datum[i] = [{ x:0, y:0 }];
    graphs[i] = new Rickshaw.Graph({
      element: document.querySelector('.graph' + i),
      width: 297,
      height: 105,
      interpolation: 'basis',
      series: [{
        name: aliases[i],
        color: '#b5d9ff',
        data: datum[i]
      }]
    });
    graphs[i].render();
  }
}

// construct url
var myUrl;
var padnulls = (typeof padnulls == 'undefined') ? true : padnulls;
function constructUrl(period) {
  var targets = '';
  for (var i=0; i<realMetrics.length; i++) {
    if (i != 0) {
      targets += '&';
    }
    if (padnulls === true) {
      targets += ('target=keepLastValue(' + encodeURI(realMetrics[i].target) + ')');
    } else {
      targets += ('target=' + encodeURI(realMetrics[i].target));
    }
  }
  myUrl = url + '/render?' + targets + '&from=-' + period + 'minutes&format=json';
}

// refresh the graph
function refreshData(immediately) {

  getData(function(values) {
    for (var i=0; i<graphs.length; i++) {
      for (var j=0; j<values[i].length; j++) {
        if (typeof values[i][j] !== 'undefined') {
          datum[i][j] = values[i][j];
        }
      }

      // check our thresholds and update color
      var lastValue = datum[i][datum[i].length - 1].y;
      var warning = realMetrics[i].warning;
      var critical = realMetrics[i].critical;
      if (critical > warning) {
        if (lastValue >= critical) {
          graphs[i].series[0].color = '#d59295';
        } else if (lastValue >= warning) {
          graphs[i].series[0].color = '#f5cb56';
        } else {
          graphs[i].series[0].color = '#afdab1';
        }
      } else {
        if (lastValue <= critical) {
          graphs[i].series[0].color = '#d59295';
        } else if (lastValue <= warning) {
          graphs[i].series[0].color = '#f5cb56';
        } else {
          graphs[i].series[0].color = '#b5d9ff';
        }
      }
      // we want to render immediately, i.e.
      // as soon as ajax completes
      // used for time period / pause view
      if (immediately) {
        updateGraphs(i);
      }
    }
    values = null;
  });

  // we can wait until all data is gathered, i.e.
  // the live refresh should happen synchronously
  if (!immediately) {
    for (var i=0; i<graphs.length; i++) {
      updateGraphs(i);
    }
  }
}

// retrieve dashboard list
function getDashboards(cb) {
  $.ajax({
    dataType: 'json',
    error: function(xhr, textStatus, errorThrown) { console.log(errorThrown); },
    url: '/'
  }).done(function(d) {
    cb(d.dashboards);
  });
}

// retrieve the data from Graphite
function getData(cb) {
  var myDatum = [];
  $.ajax({
    dataType: 'json',
    error: function(xhr, textStatus, errorThrown) { console.log(errorThrown); },
    url: myUrl
  }).done(function(d) {
    if (d.length > 0) {
      for (var i=0; i<d.length; i++) {
        myDatum[i] = [];
        for (var j=0; j<d[i].datapoints.length; j++) {
          myDatum[i][j] = { x: d[i].datapoints[j][1], y: d[i].datapoints[j][0] };
        }
      } 
    }
    cb(myDatum);
  });
}

// perform the actual graph object and
// overlay name and number updates
function updateGraphs(i) {
  // update our graph
  graphs[i].update();
  if (datum[i][datum[i].length - 1] !== undefined) {
    var lastValue = 0;
    for (var j = 0; j < datum[i].length; j++) {
      lastValue = lastValue + datum[i][j].y;
    }
    lastValue = lastValue / datum[i].length;

    var lastValueDisplay;
    if ((typeof lastValue == 'number') && lastValue < 10.0) {
      lastValueDisplay = Math.round(lastValue*100)/100;
    } else {
      lastValueDisplay = Math.round(lastValue);
    }
    $('.overlay-name' + i).text(aliases[i]);
    $('.overlay-number' + i).text(lastValueDisplay);
  } else {
    $('.overlay-name' + i).text(aliases[i]);
    $('.overlay-number' + i).html('<span class="error">NF</span>');
  }
}

// add our containers
function buildContainers() {
  var falseTargets = 0;
  for (var i=0; i<metrics.length; i++) {
    if (metrics[i].target === false) {
      $('.main').append('<div class="false"></div>');
      falseTargets++;
    } else if (metrics[i].target.match(/^<object/)) {
      $('.main').append('<div class="false">' + metrics[i].target + '</div>')
      falseTargets++;
    } else {
      var j = i - falseTargets;
      $('.main').append(
        '<div id="' + j + '" class="graph graph' + j + '">' +
        '<div class="overlay-name overlay-name' + j + '"></div>' +
        '<div class="overlay-number overlay-number' + j + '"></div>' +
        '</div>'
      );
    }
  }
}

// filter out false targets
gatherRealMetrics();

// build our div containers
buildContainers();

// build our graph objects
constructGraphs();

// build our url
constructUrl(period);

// hide our toolbar if necessary
var toolbar = (typeof toolbar == 'undefined') ? true : toolbar;
if (!toolbar) { $('div.toolbar').css('display', 'none'); }

// initial load screen
for (var i=0; i<graphs.length; i++) {
  if (realMetrics[i].target === false) {
    //continue;
    $('.overlay-number' + i).html('<img src="images/spin.gif" />');
  }
}

refreshData('now');

// define our refresh and start interval
var refreshInterval = (typeof refresh == 'undefined') ? 2000 : refresh;
var refreshId = setInterval(refreshData, refreshInterval);
