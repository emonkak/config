# Makefile for skk dictionary

all: SKK-JISYO.all.utf8

SKK-JISYO.all.utf8: SKK-JISYO.L.utf8 SKK-JISYO.naist-jdic.utf8 SKK-JISYO.jinmei.utf8 SKK-JISYO.seiyu.utf8 SKK-JISYO.emoji-en.utf8 SKK-JISYO.emoji-ja.utf8
	skkdic-expr2 -o $@ $^

SKK-JISYO.L.utf8:
	curl -s https://raw.githubusercontent.com/skk-dev/dict/master/SKK-JISYO.L | nkf -w -Lu > $@

SKK-JISYO.jinmei.utf8:
	curl -s https://raw.githubusercontent.com/skk-dev/dict/master/SKK-JISYO.jinmei | nkf -w -Lu > $@

SKK-JISYO.seiyu.utf8:
	# https://debuyoko.com/1388
	curl -s 'https://debuyoko.com/wp-content/uploads/2024/02/10033606521ba65db72aa3721c187e77.txt' | nkf -w -Lu | grep -v -e '^\s' | awk -f ime2skk.awk | skkdic-sort > $@

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
