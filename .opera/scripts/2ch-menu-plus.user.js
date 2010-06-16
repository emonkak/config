// ==UserScript==
// @name     2ch-menu-plus
// @author   emonkak
// @include  http://*.2ch.net/test/read.cgi/*
// @include  http://*.bbspink.com/test/read.cgi/*
// ==/UserScript==

(function() {
  var match = location.href.match(/^http:\/\/([^\/]+)\/test\/read\.cgi\/([^\/]+)\/(\d+)/);
  if (!match)
    return;

  var siteinfo = function(_){
    with ({url: _[0], host: _[1], bbs: _[2], key: _[3]}) {
      return {
        'p2':      'http://p2.2ch.net/p2/read.php?host=' + host + '&bbs=' + bbs +
                   '&key=' + key + '&ls=all',
        'chbox':   'http://p2.chbox.jp/read.php?host=' + host + '&bbs=' + bbs +
                   '&key=' + key + '&ls=all',
        '2bangai': 'http://2bangai.net/read/' + host.split('.')[0] + '/' + bbs +
                   '/' + key + '/',
        'unkar':   'http://www.unkar.org/read/' + host + '/' + bbs + '/' + key,
        'ranran2': 'http://ranran2.net/app/2ch/' + bbs + '/' + key,
        'mimizun': 'http://mimizun.com/log/2ch/' + bbs + '/' + host + '/' + bbs +
                   '/kako/' + key.substring(0, 4) + '/' + key.substring(0, 5) +
                   '/' + key + '.html',
        'mirror':  'http://www.geocities.jp/mirrorhenkan/url?u=' + url,
      }
    }
  }(match);

  var nodes = document.querySelectorAll('div[style="margin-top:1em;"] > span');
  if (nodes.length !== 2)
    return;

  nodes[1].parentNode.removeChild(nodes[1]);
  for (var key in siteinfo) {
    var link = document.createElement('a');
    link.href = siteinfo[key];
    link.innerHTML = '&raquo;' + key;
    link.style.backgroundColor = '#ffc';
    nodes[0].appendChild(document.createTextNode(' '));
    nodes[0].appendChild(link);
  };
})();


// __END__
// vim: expandtab softtabstop=2 shiftwidth=2
