// ==UserScript==
// @name          2ch menu plus
// @include       http://*.2ch.net/test/read.cgi/*
// @include       http://*.bbspink.com/test/read.cgi/*
// ==/UserScript==

(function() {
  var m = location.href.match(/^http:\/\/([^\/]+)\/test\/read\.cgi\/([^\/]+)\/(\d+)/);
  if (!m) return;

  var SITEINFO = [
    {
      name: 'rep2',
      url: 'http://p2.2ch.net/p2/read.php?host=' + m[1] + '&bbs=' + m[2]
            + '&key=' + m[3] + '&ls=all'
    },
    {
      name: 'ranran2',
      url: 'http://ranran2.net/app/2ch/' + m[2] + '/' + m[3]
    },
    {
      name: 'unkar',
      url: 'http://www.unkar.org/read/' + m[1] + '/' + m[2] + '/' + m[3]
    },
    {
      name: 'mimizun',
      url: 'http://mimizun.com/log/2ch/' + m[2] + '/' + m[1] + '/' + m[2] + '/kako/'
           + m[3].substring(0, 4) + '/' + m[3].substring(0, 5) + '/' + m[3] + '.html'
    },
    {
      name: 'mirror',
      url: 'http://www.geocities.jp/mirrorhenkan/url?u=' + m[0]
    }
  ];

  var header = getFirstElementByXPath('//div[@style="margin-top:1em;"]', document);
  var right = getFirstElementByXPath('//span[@style="float:right;"]', document);
  var left = getFirstElementByXPath('//span[@style="float:left;"]', header);
  header.removeChild(right);
  SITEINFO.forEach(function(site) {
    var link = document.createElement('a');
    link.href = site.url;
    link.innerHTML = '&raquo;' + site.name;
    link.style.backgroundColor = '#ffffcc';
    left.appendChild(document.createTextNode(' '));
    left.appendChild(link);
  });

  function getFirstElementByXPath(xpath, node) {
    var result = document.evaluate(xpath, node, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null);
    return result.singleNodeValue;
  }
})();
