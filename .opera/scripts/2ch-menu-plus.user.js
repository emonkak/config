// ==UserScript==
// @name          2ch menu plus
// @include       http://*.2ch.net/test/read.cgi/*
// @include       http://*.bbspink.com/test/read.cgi/*
// ==/UserScript==

(function() {
	var match = location.href.match(/^http:\/\/([^\/]+)\/test\/read\.cgi\/([^\/]+)\/(\d+)/);
	if (!match) return;

	var url = match[0];
	var host = match[1];
	var bbs = match[2];
	var dat = match[3];

	var SITE_INFO = [
		{
			name: 'rep2',
			url: 'http://p2.2ch.net/p2/read.php?host=' + host + '&bbs=' + bbs
			      + '&key=' + dat + '&ls=all'
		},
		{
			name: 'ranran2',
			url: "http://ranran2.net/app/2ch/" + bbs + '/' + dat
		},
		{
			name: 'unkar',
			url: "http://www.unkar.org/read/" + host + '/' + bbs + '/' + dat
		},
		{
			name: 'mimizun',
			url: "http://mimizun.com/log/2ch/" + bbs + '/' + host + '/' + bbs + '/kako/'
			     + dat.substring(0, 4) + '/' + dat.substring(0, 5) + '/' + dat + '.html'
		},
		{
			name: 'mirror',
			url: 'http://www.geocities.jp/mirrorhenkan/url?u=' + url
		}
	];

	var header = getFirstElementByXPath('//div[@style="margin-top:1em;"]', document);
	var pr = getFirstElementByXPath('//span[@style="float:right;"]', header);
	var menu = getFirstElementByXPath('//span[@style="float:left;"]', header);
	header.removeChild(pr);
	SITE_INFO.forEach(function(site) {
		var link = document.createElement('a');
		link.href = site.url;
		link.innerHTML = '&raquo;' + site.name;
		link.style.backgroundColor = '#ffffcc';
		menu.appendChild(document.createTextNode(' '));
		menu.appendChild(link);
	});

	function getFirstElementByXPath(xpath, node) {
		var result = document.evaluate(xpath, node, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null);
		return result.singleNodeValue;
	}
})();
