// ==UserScript==
// @name         popup-anything
// @author       emonkak
// @namespace    http://github.com/emonkak/
// @description  Anything display arbitrary information in popup.
// @include      http://*
// @include      https://*
// ==/UserScript==

(function() {
  const IFRAME_WINDOW_NAME = 'POPUP_ANYTHING';
  const LOADING_IMAGE = 'data:image/gif;base64,' +
    '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' +
    'FikQADs=';
  const POPUP_STYLE = {
    '#popup-anything': {
      'background-color': '#000',
      'border': 'none',
      'bottom' : '0px',
      'color': '#ccc',
      'font-family': 'sans-serif',
      'font-size': '10pt',
      'left': '0px',
      'line-height': '150%',
      'max-height': '33%',
      'opacity':  0.9,
      'overflow': 'auto',
      'padding': '8px',
      'position': 'fixed',
      'text-align': 'left',
      'width': '100%',
      'z-index': 1000,
    },
    '#popup-anything *': {
      'border': 'none',
      'color': 'inherit',
      'font-family': 'inherit',
      'font-size': 'inherit',
      'line-height': 'inherit',
      'text-align': 'inherit',
    },
    '#popup-anything a': {
      'border-color': '#333',
      'border-style': 'solid',
      'border-width': '0 0 1px 0',
      'color': 'inherit',
      'text-decoration': 'none',
    },
    '#popup-anything a:hover': {
      'background-color': '#333',
      'border-color': '#666',
      'border-style': 'solid',
      'border-width': '0 0 1px 0',
      'color': '#fff',
    },
    '#popup-anything li': {
      'display': 'list-item',
      'list-style': 'inherit',
      'margin': '0 0 0 1.5em',
      'padding': 0,
    },
    '#popup-anything ul': {
      'display': 'block',
      'list-style': 'disc none outside',
      'margin': 0,
      'padding': 0,
    },
    '#popup-anything ol': {
      'display': 'block',
      'list-style': 'decimal none outside',
      'margin': 0,
      'padding': 0,
    },
  };
  const SITEINFO_ALT = [
    /*
    {
      method: '',
      url: 'http://',
      data: {},
      charset: '',
      pattern: //,
      xpath: '',
    },
    */
    {
      method: 'GET',
      url: 'http://eow.alc.co.jp/%s/UTF-8/',
      pattern: /^ *[a-zA-Z-]+ *$/,
      xpath: '//div[@id="resultsList"]/ul',
    },
    {
      method: 'GET',
      url: 'http://api.iknow.co.jp/items/matching/%s.html',
      data: {language: 'en', translation_language: 'ja'},
      pattern: /^ *[a-zA-Z-]+ *$/,
      xpath: '//div[@class="cue"]/a[@class=" item_link"]|//div[@class="response"]',
    },
    {
      method: 'GET',
      url: 'http://kotobank.jp/word/%s',
      pattern: /^ *[\u2e00-\uffff]+ *$/,
      xpath: '//div[@class="wordTitle"][1]/h1|descendant::div[@class="full"]/p[@class="doc"]',
    },
    {
      method: 'POST',
      url: 'http://honyaku.yahoo.co.jp/transtext',
      data: {both: 'TH', text: '%s', clearFlg: 1, eid: 'CR-EJ'},
      pattern: /^[\u0000-\u2dff]+$/,
      xpath: '//textarea[@id="trn_textText"]/text()',
    },
  ];
  const SITEINFO_CTRL = [
    /*
    {
      method: 'GET',
      url: 'http://d.hatena.ne.jp/keyword/%s',
      pattern: /^ *\S+ *$/,
      xpath: '//div[@class="keyword-container area-keyword"]/h2|//div[@class="keyword-body"]/div[@class="section"]',
    },
    */
    {
      method: 'POST',
      url: 'http://wikipedia.simpleapi.net/api',
      data: {keyword: '%s', output: 'html'},
      pattern: /^ *\S+ *$/,
      xpath: '//body/p',
    },
    {
      method: 'GET',
      url: 'http://www.google.com/search',
      data: {hl: 'ja', lr: '', q: '%s'},
      pattern: /^ *\S+ *$/,
      xpath: '//div[@id="res"]/div/ol',
    },
  ];

  if (window.parent === window) {
    addStyle(POPUP_STYLE);

    document.addEventListener('DOMContentLoaded', function(e){
      var popup = initPopup();

      document.addEventListener('mouseup', function(e){
        var siteinfo = e.altKey  ? SITEINFO_ALT :
                       e.ctrlKey ? SITEINFO_CTRL : '';
        if (!siteinfo)
          return;

        var query = document.getSelection();
        if (!query || /^\s*$/.test(query))
          return;

        var params = [];
        for (var i = 0, l = siteinfo.length; i < l; i++) {
          if (siteinfo[i].pattern.test(query))
            params.push(siteinfo[i]);
        };
        if (params.length === 0)
          return;

        popup.load();
        iFrameHttpReqest(params.shift(), query, function(e){
          if (!popup.disp())
            return;

          if (e.data) {
            popup.apply(e.data);
          } else if (params.length > 0) {
            iFrameHttpReqest(params.shift(), query, arguments.callee);
          } else {
            popup.hide();
          }
        });
      }, false);
    }, false);
  } else if (window.name.indexOf(IFRAME_WINDOW_NAME) === 0) {
    addStyle({'body': {'display': 'none'}});

    window.opera.addEventListener('BeforeScript', function(e){
      e.preventDefault();
    }, false);
    window.opera.addEventListener('BeforeExternalScript', function(e){
      e.preventDefault();
    }, false);
    window.opera.addEventListener('BeforeEventListener', function(e){
      if (e.event.type !== 'DOMContentLoaded')
        e.preventDefault();
    }, false);

    document.addEventListener('DOMContentLoaded', function(e){
      if (typeof(JSON) !== 'object') {
        var JSON = {};
        JSON.parse = function(value){
          return eval('(' + value + ')');
        };
      }
      var json = JSON.parse(window.name.slice(IFRAME_WINDOW_NAME.length));
      var entry = document.evaluate(json.xpath, document, null, XPathResult.ORDERED_NODE_SNAPSHOT_TYPE, null);

      var contents = '';
      for (var i = 0, l = entry.snapshotLength; i < l; i++) {
        var node = entry.snapshotItem(i);
        contents += parseNode(node);
      }

      window.parent.postMessage(contents, json.referrer);
    }, false);
  }

  function parseNode(node) {
    if (node.nodeType != Node.ELEMENT_NODE && node.nodeType != Node.TEXT_NODE)
      return;

    var children = node.childNodes;
    var contents = '';
    for (var i = 0, l = children.length; i < l; i++) {
      var childContents = arguments.callee(children[i]);
      if (childContents)
        contents += childContents;
    };

    switch (node.nodeType) {
    case Node.ELEMENT_NODE:
      var tag = node.tagName.toLowerCase();
      var attr = [''];

      switch (tag) {
      case 'a':
        attr.push('target="_blank"');
        if (node.href.indexOf('http:') === 0)
          attr.push('href="' + node.href + '"');
        break;
      case 'font':
      case 'h1':
      case 'h2':
      case 'h3':
      case 'h4':
      case 'h5':
      case 'h6':
        tag = 'strong';
        break;
      case 'span':
        // Delete ruby for alc dictionary.
        if (node.className === 'kana')
          return;
        break;
      case 'button':
      case 'img':
      case 'script':
        return;
      }
      return children.length === 0
           ? '<' + tag + attr.join(' ') + '/>' + contents
           : '<' + tag + attr.join(' ') + '>'  + contents + '</' + tag + '>';
    case Node.TEXT_NODE:
      return node.nodeValue;
    }
  }

  function initPopup()
  {
    var Popup = function(){
      this.node = document.createElement('div');
      this.node.id = 'popup-anything';
      this.node.style.display = 'none';

      var self = this;
      this.node.addEventListener('dblclick', function(e){
        self.hide.call(self);
      }, false);

      document.body.appendChild(this.node);
    };

    Popup.prototype.load = function(){
      var img = document.createElement('img');
      var text = document.createTextNode('Loading...');
      img.style.margin = '0 8px 0 0';
      img.style.padding = '0';
      img.style.verticalAlign = 'middle';
      img.src = LOADING_IMAGE;

      this.apply('');
      this.node.appendChild(img);
      this.node.appendChild(text);

      this.node.style.display = 'inline';
    };

    Popup.prototype.disp = function(){
      return this.node.style.display === 'none' ? false : true;
    };

    Popup.prototype.apply = function(contents){
      this.node.innerHTML = contents;
    };

    Popup.prototype.hide = function(){
      this.node.style.display = 'none';
      this.node.innerHTML = '';
    };

    return new Popup();
  }

  function addStyle(css)
  {
    var rules = '';
    for (var selector in css) {
      rules += selector + '{';
      for (var prop in css[selector])
        rules += prop + ':' + css[selector][prop] + ';';
      rules += '}';
    }

    var style = document.createElement("style");
    style.type = 'text/css';
    style.innerHTML = rules;

    if (document.getElementsByTagName('head').length > 0) {
      document.getElementsByTagName('head')[0].appendChild(style);
    } else {
      document.documentElement.appendChild(style);
    }
  }

  function iFrameHttpReqest(param, query, callback)
  {
    if (typeof(JSON) !== 'object') {
      var JSON = {};
      JSON.stringify = function(value){
        switch (typeof(value)) {
        case 'number':
        case 'boolean':
          return value;
        case 'string':
          return '"' + value.replace(/(['"])/g, '\\$1') + '"';
        case 'object':
          var data = [];
          if (value instanceof Array) {
            for (var i = 0, l = value.length; i < l; i++)
              data.push(arguments.callee(value[i]));
            return '[' + data.join(',') + ']';
          } else {
            for (var key in value)
              data.push('"' + key + '":' + arguments.callee(value[key]));
            return '{' + data.join(',') + '}';
          }
        }
      };
    }
    var json = JSON.stringify({referrer: location.href, xpath: param.xpath});

    var iframe = document.createElement('iframe');
    iframe.name = IFRAME_WINDOW_NAME + json;
    iframe.style.height = 0;
    iframe.style.width = 0;
    iframe.style.visibility = 'hidden';

    var form = document.createElement('form');
    form.acceptCharset = param.charset || 'utf-8';
    form.action = param.url.replace(/%s/, query);
    form.method = param.method || 'GET';
    form.target = iframe.name;

    for (var key in param.data) {
      var input = document.createElement('input');
      input.type = 'hidden';
      input.name = key;
      input.value = typeof(param.data[key]) === 'string'
                  ? param.data[key].replace(/%s/, query)
                  : param.data[key];
      form.appendChild(input);
    }

    document.body.appendChild(iframe);
    document.body.appendChild(form);

    window.addEventListener('message', function(){
      window.removeEventListener('message', callback, false);
      window.removeEventListener('message', arguments.callee, false);
      document.body.removeChild(iframe);
      document.body.removeChild(form);
    }, false);
    window.addEventListener('message', callback, false);

    form.submit();
  }
})();


// __END__
// vim: expandtab softtabstop=2 shiftwidth=2
