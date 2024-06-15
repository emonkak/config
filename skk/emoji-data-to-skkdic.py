#!/usr/bin/env python3

import fileinput
import itertools
import re

ALPHANUM_TRANS = {
    **str.maketrans({ord('０') + i: chr(ord('0') + i) for i in range(ord('9') - ord('0') + 1)}),
    **str.maketrans({ord('Ａ') + i: chr(ord('A') + i) for i in range(ord('Z') - ord('A') + 1)}),
    **str.maketrans({ord('ａ') + i: chr(ord('a') + i) for i in range(ord('z') - ord('a') + 1)}),
    ord('！'): '!',
    ord('＃'): '#',
    ord('＋'): '+',
    ord('／'): '/',
    ord('？'): '?',
}

INVALID_YOMI_PATTERN = re.compile(r'[ぁ-ゖ][a-z]')

print(';; okuri-ari entries.')
print(';; okuri-nasi entries.')

for line in fileinput.input():
    if line[0] == '#':
        continue
    columns = line.split('\t')
    character = columns[1]
    yomis = columns[2].split()
    japanese_name = columns[4] or columns[3]
    for yomi in yomis:
        translated_yomi = yomi.translate(ALPHANUM_TRANS).lower()
        if not INVALID_YOMI_PATTERN.search(translated_yomi):
            if japanese_name != '':
                candidate = character + ';' + japanese_name
            else:
                candidate = character
            print(f'{translated_yomi} /{candidate}/')
