#!/usr/bin/env python3

import fileinput
import itertools

def alphanum_trans():
    return {
        **str.maketrans({ord('０') + i: chr(ord('0') + i) for i in range(ord('9') - ord('0') + 1)}),
        **str.maketrans({ord('Ａ') + i: chr(ord('A') + i) for i in range(ord('Z') - ord('A') + 1)}),
        **str.maketrans({ord('ａ') + i: chr(ord('a') + i) for i in range(ord('z') - ord('a') + 1)}),
        ord('！'): '!',
        ord('＃'): '#',
        ord('＋'): '+',
        ord('／'): '/',
        ord('？'): '?',
    }

def kana_trans(*dicts):
    VOWELS = ['a', 'i', 'u', 'e', 'o']
    dicts = [
        {c: VOWELS[i] for i, c in enumerate('あいうえお')},
        {c: 'x' + VOWELS[i] for i, c in enumerate('ぁぃぅぇぉ')},
        {c: 'k' + VOWELS[i] for i, c in enumerate('かきくけこ')},
        {c: 's' + VOWELS[i] for i, c in enumerate('さしすせそ')},
        {c: 't' + VOWELS[i] for i, c in enumerate('たちつてと')},
        {c: 'n' + VOWELS[i] for i, c in enumerate('なにぬねの')},
        {c: 'h' + VOWELS[i] for i, c in enumerate('はひふへほ')},
        {c: 'm' + VOWELS[i] for i, c in enumerate('まみむめも')},
        {c: 'r' + VOWELS[i] for i, c in enumerate('らりるれろ')},
        {c: 'g' + VOWELS[i] for i, c in enumerate('がぎぐげご')},
        {c: 'z' + VOWELS[i] for i, c in enumerate('ざじずぜぞ')},
        {c: 'd' + VOWELS[i] for i, c in enumerate('だぢづでど')},
        {c: 'b' + VOWELS[i] for i, c in enumerate('ばびぶべぼ')},
        {c: 'p' + VOWELS[i] for i, c in enumerate('ぱぴぷぺぽ')},
        {c: 'ky' + VOWELS[i] for i, c in enumerate(['きゃ', 'きぃ', 'きゅ', 'きぇ', 'きょ'])},
        {c: 'sh' + VOWELS[i] for i, c in enumerate(['しゃ', 'し', 'しゅ', 'しぇ', 'しょ'])},
        {c: 'th' + VOWELS[i] for i, c in enumerate(['てゃ', 'てぃ', 'てゅ', 'てぇ', 'てょ'])},
        {c: 'ty' + VOWELS[i] for i, c in enumerate(['ちゃ', 'ちぃ', 'ちゅ', 'ちぇ', 'ちょ'])},
        {c: 'ch' + VOWELS[i] for i, c in enumerate(['ちゃ', 'ち', 'ちゅ', 'ちぇ', 'ちょ'])},
        {c: 'ny' + VOWELS[i] for i, c in enumerate(['にゃ', 'にぃ', 'にゅ', 'にぇ', 'にょ'])},
        {c: 'hy' + VOWELS[i] for i, c in enumerate(['ひゃ', 'ひぃ', 'ひゅ', 'ひぇ', 'ひょ'])},
        {c: 'my' + VOWELS[i] for i, c in enumerate(['みゃ', 'みぃ', 'みゅ', 'みぇ', 'みょ'])},
        {c: 'ry' + VOWELS[i] for i, c in enumerate(['りゃ', 'りぃ', 'りゅ', 'りぇ', 'りょ'])},
        {c: 'gy' + VOWELS[i] for i, c in enumerate(['ぎゃ', 'ぎぃ', 'ぎゅ', 'ぎぇ', 'ぎょ'])},
        {c: 'j' + VOWELS[i] for i, c in enumerate(['じゃ', 'じ', 'じゅ', 'じぇ', 'じょ'])},
        {c: 'dh' + VOWELS[i] for i, c in enumerate(['でゃ', 'でぃ', 'でぇ', 'でゅ', 'でょ'])},
        {c: 'by' + VOWELS[i] for i, c in enumerate(['びゃ', 'びぃ', 'びゅ', 'びぇ', 'びょ'])},
        {c: 'py' + VOWELS[i] for i, c in enumerate(['ぴゃ', 'ぴぃ', 'ぴゅ', 'ぴぇ', 'ぴょ'])},
        {c: 'f' + VOWELS[i] for i, c in enumerate(['ふぁ', 'ふぃ', 'ふ', 'ふぇ', 'ふぉ'])},
        {c: 'v' + VOWELS[i] for i, c in enumerate(['ゔぁ', 'ゔぃ', 'ゔ', 'ゔぇ', 'ゔぉ'])},
    ]
    result = {
        'うぃ': ['wi'],
        'うぇ': ['we'],
        'っ': ['xtu'],
        'ゃ': ['xya'],
        'や': ['ya'],
        'ゅ': ['lyu'],
        'ゅ': ['xyu'],
        'ゆ': ['yu'],
        'ょ': ['xyo'],
        'よ': ['yo'],
        'わ': ['wa'],
        'を': ['wo'],
        'ん': ['n'],
    }
    for dict in dicts:
        for key, value in dict.items():
            if key in result:
                result[key].append(value)
            else:
                result[key] = [value]
    return result

ALPHANUM_TRANS = alphanum_trans()
KANA_TRANS = kana_trans()

def translate_alphanum(text):
    return text.translate(ALPHANUM_TRANS)

def translate_kana(text):
    results = ['']
    i = 0
    l = len(text)
    is_sokuon = False
    is_kana = False
    while i < l:
        character = text[i]
        if character == 'っ':
            is_sokuon = True
            i += 1
            continue
        double_character = text[i:i + 2]
        if double_character in KANA_TRANS:
            candidates = KANA_TRANS[double_character]
            found_trans = True
            i += 2
        else:
            character = text[i]
            if character in KANA_TRANS:
                candidates = KANA_TRANS[character]
                found_trans = True
            else:
                candidates = [character]
                found_trans = False
            i += 1
        if is_sokuon:
            candidates = list(map(lambda candidate: candidate[0] + candidate, candidates))
        for j in range(len(results)):
            entries = list(map(lambda candidate: results[j] + candidate, candidates))
            if found_trans and is_kana:
                for k in range(len(entries)):
                    entry = entries[k]
                    tail = entry[-2:]
                    if tail == 'uu' or tail == 'ee' or tail == 'ou' or tail == 'oo':
                        entries[k] = entry[:-1]
            results[j] = entries
        results = list(itertools.chain.from_iterable(results))
        is_sokuon = False
        is_kana = found_trans
    return results

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
        translated_yomi = translate_alphanum(yomi).lower()
        if japanese_name != '':
            candidate = character + ';' + japanese_name
        else:
            candidate = character
        print(f'{translated_yomi} /{candidate}/')
