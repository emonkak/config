#!/usr/bin/env python3

# Reference:
# https://github.com/r45635/HVAC-IR-Control/blob/master/Protocol/Panasonic%20HVAC%20IR%20Protocol%20specification.pdf

import argparse
import enum
import pathlib
import subprocess
import sys

MODE = {
    'auto': 0b0000,
    'dry': 0b0010,
    'cool': 0b0011,
    'heat': 0b0100,
    'fan': 0b0110,
}

POWER = {
    'off': 0b0000,
    'on': 0b0001,
}

SWING = {
    '1': 0b0001,
    '2': 0b0010,
    '3': 0b0011,
    '4': 0b0100,
    '5': 0b0101,
    'auto': 0b1111,
}

FAN = {
    '1': 0b0011,
    '2': 0b0100,
    '3': 0b0101,
    '4': 0b0110,
    '5': 0b0111,
    'auto': 0b1010,
}

PROFILE = {
    'normal': 0b0000_0000,
    'quite': 0b0010_0000,
    'boost': 0b0000_0001,
}

FEATURES1 = {
    'dry': 0b0100_0000,
}

FEATURES2 = {
    'deodorize': 0b0001_0000,
}

def lsb_bits(byte):
    for n in range(0, 8):
        yield (byte & (1 << n)) != 0

def msb_bits(byte):
    for n in range(7, -1, -1):
        yield (byte & (1 << n)) != 0

def to_aeha_frame(bytes, to_bits):
    yield [8, 4]

    for byte in bytes:
        for bit in to_bits(byte):
            if bit:
                yield [1, 3]
            else:
                yield [1, 1]

def create_panasonic_aircon_frames(temperature, mode, power, swing, fan, profile, features1, features2):
    first_frame = b'\x40\x04\x07\x20\x00\x00\x00\x60'

    yield from to_aeha_frame(first_frame, msb_bits)

    second_frame = [
        0x02,  # 00:
        0x20,  # 01:
        0xe0,  # 02:
        0x04,  # 03:
        0x00,  # 04:
        0x00,  # 05: MODE[3] MODE[2] MODE[1] MODE[0] 1 0 0 POWER
        0x00,  # 06: 0 0 1 TEMP[3] TEMP[2] TEMP[1] TEMP[0] 0
        0x80,  # 07:
        0x00,  # 08: FAN[3] FAN[2] FAN[1] FAN[0] SWING[3] SWING[2] SWING[1] SWING[0]
        0x00,  # 09:
        0x00,  # 10:
        0x06,  # 11:
        0x60,  # 12:
        0x00,  # 13: FEATURES1[1] FEATURES1[0] PROFILE[5] PROFILE[4] PROFILE[3] PROFILE[2] PROFILE[1] PROFILE[0]
        0x00,  # 14:
        0x80,  # 15:
        0x00,  # 16:
        0x06,  # 17: FEATURES2[3] FEATURES2[2] FEATURES2[1] FEATURES2[0] 1 0 1 0
        0x00,  # 18: CRC
    ]
    second_frame[5] = (mode << 4) | power
    second_frame[6] = ((temperature & 0x0f) | 0x10) << 1
    second_frame[8] = (fan << 4) | swing
    second_frame[13] = profile | features1
    second_frame[17] |= features2
    second_frame[18] = sum(second_frame) % 256

    yield [1, 23]  # Gap signal

    yield from to_aeha_frame(second_frame, lsb_bits)

def emit_frames(handle, frames, signal_period):
    # Avoid "leading space ignored" warning
    handle.write(f'pulse {signal_period}\n')

    for [pulse, space] in frames:
        handle.write(f'pulse {pulse * signal_period}\n')
        handle.write(f'space {space * signal_period}\n')

    handle.write(f'pulse {signal_period}\n')
    handle.flush()

if __name__ == '__main__':
    DEFAULT_SIGNAL_PERIOD = 435

    parser = argparse.ArgumentParser()
    parser.add_argument('-d', '--device', nargs=1, type=pathlib.Path, default="/dev/lirc0")
    parser.add_argument('-s', '--swing', choices=[k for k in SWING.keys()], default='auto')
    parser.add_argument('-f', '--fan', choices=[k for k in FAN.keys()], default='auto')
    parser.add_argument('-p', '--profile', choices=[k for k in PROFILE.keys()], default='normal')
    parser.add_argument('--dry', action='store_true')
    parser.add_argument('--deodorize', action='store_true')
    parser.add_argument('--signal-period', type=int, default=DEFAULT_SIGNAL_PERIOD)
    parser.add_argument('--print-frames', action='store_true')

    subparsers = parser.add_subparsers()
    subparsers.required = True
    subparsers.dest = 'command'

    for k in MODE:
        sub_parser = subparsers.add_parser(k)
        sub_parser.add_argument('temperature', help='Temperature (16-30C)', type=int, choices=range(16, 31))
        sub_parser.set_defaults(mode=k, power='on')

    sub_parser = subparsers.add_parser('off')
    sub_parser.set_defaults(temperature=16, mode='auto', power='off')

    args = parser.parse_args()
    temperature = args.temperature
    mode = MODE[args.mode]
    power = POWER[args.power]
    swing = SWING[args.swing]
    fan = FAN[args.fan]
    profile = PROFILE[args.profile]
    features1 = 0
    features2 = 0
    signal_period = args.signal_period

    if args.dry:
        features1 |= FEATURES1['dry']

    if args.deodorize:
        features2 |= FEATURES2['deodorize']

    frames = create_panasonic_aircon_frames(temperature, mode, power, swing, fan, profile, features1, features2)

    if args.print_frames:
        emit_frames(sys.stdout, frames, signal_period)
    else:
        proc = subprocess.Popen(['ir-ctl', '--device', args.device, '--send', '/dev/stdin'], stdin=subprocess.PIPE, universal_newlines=True, bufsize=1)
        emit_frames(proc.stdin, frames, signal_period)
        proc.stdin.close()
        proc.wait()
