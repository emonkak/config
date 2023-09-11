# Makefile for skk dictionary

all: SKK-JISYO.LL.utf8

SKK-JISYO.LL.utf8: SKK-JISYO.L.utf8 SKK-JISYO.naist-jdic.utf8 SKK-JISYO.jinmei.utf8 SKK-JISYO.seiyu.utf8 SKK-JISYO.emoji-en.utf8 SKK-JISYO.emoji-ja.utf8
	skkdic-expr2 -o $@ $^

SKK-JISYO.L.utf8:
	curl -s https://raw.githubusercontent.com/skk-dev/dict/master/SKK-JISYO.L | nkf -w -Lu  > $@

SKK-JISYO.jinmei.utf8:
	curl -s https://raw.githubusercontent.com/skk-dev/dict/master/SKK-JISYO.jinmei | nkf -w -Lu > $@

SKK-JISYO.seiyu.utf8:
	curl -s 'https://web.archive.org/web/20170910102932/http://tomoch.s28.xrea.com/ime.cgi' | sed -n '/<pre>/,/<\/pre>/ { /<pre>/n; /<\/pre>/d; p; }' | awk -f ime2skk.awk > $@

SKK-JISYO.emoji-en.utf8:
	curl -s -L 'https://raw.githubusercontent.com/uasi/skk-emoji-jisyo/master/SKK-JISYO.emoji.utf8' > $@

SKK-JISYO.naist-jdic.utf8:
	curl -s 'https://nathancorvussolis.github.io/skkdic/skk-naist-jdic.utf16.txt' | nkf -w -Lu > $@

SKK-JISYO.emoji-ja.utf8: emoji_data.tsv emoji-data-to-skkdic.py
	cat $< | python emoji-data-to-skkdic.py | skkdic-expr2 > $@

emoji_data.tsv:
	curl -s 'https://raw.githubusercontent.com/google/mozc/master/src/data/emoji/emoji_data.tsv' -o $@

clean:
	rm -f SKK-JISYO.* emoji_data.tsv

.PHONY: all clean
