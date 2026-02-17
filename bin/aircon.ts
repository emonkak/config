#!/usr/bin/env node

// Reference:
// https://github.com/r45635/HVAC-IR-Control/blob/master/Protocol/Panasonic%20HVAC%20IR%20Protocol%20specification.pdf

import { spawn } from 'node:child_process';
import type { Writable } from 'node:stream';

const DEFAULT_PULSE_DURATION = 435;

const USAGE = `USAGE:
  aircon.ts [OPTIONS] MODE TEMPERATURE
  aircon.ts [OPTIONS] off

ARGUMENTS:
  MODE                       Operation mode: auto, dry, cool, heat, fan
  TEMPERATURE                Celsius temperature: 16-30

OPTIONS:
  -d, --device PATH          IR device path (default: /dev/lirc0)
  -s, --swing POSITION       Swing position: 1-5, auto (default: auto)
  -f, --fan SPEED            Fan speed: 1-5, auto (default: auto)
  -p, --profile NAME         Operation profile: normal, quite, boost (default: normal)
  --dry                      Enable dry feature
  --deodorize                Enable deodorize feature
  --print-frames             Print frames to stdout instead of sending them to the IR device
  --pulse-duration DURATION  Pulse duration (default: 435)
  -h, --help                 Show this help message
`;

const Mode = {
  auto: 0b0000,
  dry: 0b0010,
  cool: 0b0011,
  heat: 0b0100,
  fan: 0b0110,
} as const;

const Power = {
  off: 0b0000,
  on: 0b0001,
} as const;

const Swing = {
  1: 0b0001,
  2: 0b0010,
  3: 0b0011,
  4: 0b0100,
  5: 0b0101,
  auto: 0b1111,
} as const;

const Fan = {
  1: 0b0011,
  2: 0b0100,
  3: 0b0101,
  4: 0b0110,
  5: 0b0111,
  auto: 0b1010,
} as const;

const Profile = {
  normal: 0b0000_0000,
  quite: 0b0010_0000,
  boost: 0b0000_0001,
} as const;

const Features1 = {
  dry: 0b0100_0000,
} as const;

const Features2 = {
  deodorize: 0b0001_0000,
} as const;

interface LaunchParams {
  deodorize: boolean;
  device: string;
  dry: boolean;
  fan: keyof typeof Fan;
  mode: keyof typeof Mode;
  power: keyof typeof Power;
  printFrames: boolean;
  profile: keyof typeof Profile;
  pulseDuration: number;
  swing: keyof typeof Swing;
  temperature: number;
}

type Frame = [number, number];

class ParseArgsError extends Error {}

function* createPanasonicIRFrames(
  temperature: number,
  mode: number,
  power: number,
  swing: number,
  fan: number,
  profile: number,
  features1: number,
  features2: number,
): Generator<Frame> {
  const firstFrame = [0x40, 0x04, 0x07, 0x20, 0x00, 0x00, 0x00, 0x60];

  yield* toAehaFrame(firstFrame, iterateBitsMsb);

  const secondFrame = [
    0x02, // 00:
    0x20, // 01:
    0xe0, // 02:
    0x04, // 03:
    0x00, // 04:
    0x00, // 05: MODE[3] MODE[2] MODE[1] MODE[0] 1 0 0 POWER
    0x00, // 06: 0 0 1 TEMP[3] TEMP[2] TEMP[1] TEMP[0] 0
    0x80, // 07:
    0x00, // 08: FAN[3] FAN[2] FAN[1] FAN[0] SWING[3] SWING[2] SWING[1] SWING[0]
    0x00, // 09:
    0x00, // 10:
    0x06, // 11:
    0x60, // 12:
    0x00, // 13: FEATURES1[1] FEATURES1[0] PROFILE[5] PROFILE[4] PROFILE[3] PROFILE[2] PROFILE[1] PROFILE[0]
    0x00, // 14:
    0x80, // 15:
    0x00, // 16:
    0x06, // 17: FEATURES2[3] FEATURES2[2] FEATURES2[1] FEATURES2[0] 1 0 1 0
    0x00, // 18: CRC
  ];

  secondFrame[5] = (mode << 4) | power;
  secondFrame[6] = ((temperature & 0x0f) | 0x10) << 1;
  secondFrame[8] = (fan << 4) | swing;
  secondFrame[13] = profile | features1;
  secondFrame[17]! |= features2;
  secondFrame[18] =
    secondFrame.reduce((totalBytes, byte) => totalBytes + byte, 0) % 256;

  yield [1, 23]; // Gap signal

  yield* toAehaFrame(secondFrame, iterateBitsLsb);
}

function* iterateBitsLsb(byte: number): Generator<boolean> {
  for (let n = 0; n < 8; n++) {
    yield (byte & (1 << n)) !== 0;
  }
}

function* iterateBitsMsb(byte: number): Generator<boolean> {
  for (let n = 7; n >= 0; n--) {
    yield (byte & (1 << n)) !== 0;
  }
}

function parseArgs(args: string[]): LaunchParams {
  let deodorize = false;
  let device = '/dev/lirc0';
  let dry = false;
  let fan: LaunchParams['fan'] = 'auto';
  let mode: LaunchParams['mode'] | undefined;
  let power: LaunchParams['power'] = 'on';
  let printFrames = false;
  let profile: LaunchParams['profile'] = 'normal';
  let pulseDuration = DEFAULT_PULSE_DURATION;
  let swing: LaunchParams['swing'] = 'auto';
  let temperature: number | undefined;

  for (let i = 0, l = args.length; i < l; i++) {
    switch (args[i]!) {
      case '-d':
      case '--device':
        device = parseString(
          args[++i],
          (value) => `Invalid device path ${JSON.stringify(value)}`,
        );
        break;

      case '-s':
      case '--swing':
        swing = parseEnum(
          args[++i],
          Swing,
          (value) => `Invalid swing position ${JSON.stringify(value)}`,
        );
        break;

      case '-f':
      case '--fan':
        fan = parseEnum(
          args[++i],
          Fan,
          (value) => `Invalid fan speed ${JSON.stringify(value)}`,
        );
        break;

      case '-p':
      case '--profile':
        profile = parseEnum(
          args[++i],
          Profile,
          (value) => `Invalid profile name ${JSON.stringify(value)}`,
        );
        break;

      case '--dry':
        dry = true;
        break;

      case '--deodorize':
        deodorize = true;
        break;

      case '--pulse-duration':
        pulseDuration = parseInteger(
          args[++i],
          (value) => `Invalid pulse duration ${JSON.stringify(value)}`,
        );
        break;

      case '--print-frames':
        printFrames = true;
        break;

      case '-h':
      case '--help':
        console.log(USAGE);
        process.exit(0);

      default:
        if (mode === undefined) {
          if (args[i] === 'off') {
            mode = 'auto';
            temperature = 16;
            power = 'off';
          } else {
            mode = parseEnum(
              args[i],
              Mode,
              (value) => `Invalid mode name ${JSON.stringify(value)}`,
            );
          }
        } else if (temperature === undefined) {
          temperature = parseInteger(
            args[i]!,
            (value) => `Invalid temperature ${JSON.stringify(value)}`,
          );
        } else {
          throw new ParseArgsError(`Unexpected argument "${args[i]}"`);
        }
    }
  }

  if (mode === undefined) {
    throw new ParseArgsError('No mode specified');
  }

  if (temperature === undefined) {
    throw new ParseArgsError('No temperature specified');
  }

  return {
    deodorize,
    device,
    dry,
    fan,
    mode,
    power,
    printFrames,
    profile,
    pulseDuration,
    swing,
    temperature,
  };
}

function parseEnum<T extends {}>(
  value: unknown,
  expectedEnum: T,
  getError: (value: unknown) => string,
): keyof T {
  if (typeof value !== 'string' || !Object.hasOwn(expectedEnum, value)) {
    throw new ParseArgsError(getError(value));
  }
  return value as keyof T;
}

function parseInteger(
  value: unknown,
  getError: (value: unknown) => string,
): number {
  const n = typeof value === 'string' ? parseInt(value, 10) : NaN;
  if (Number.isNaN(n)) {
    throw new ParseArgsError(getError(value));
  }
  return n;
}

function parseString(
  value: unknown,
  getError: (value: unknown) => string,
): string {
  if (typeof value !== 'string') {
    throw new ParseArgsError(getError(value));
  }
  return value;
}

function* toAehaFrame(
  bytes: number[] | Uint8Array,
  iterateBits: (byte: number) => Generator<boolean>,
): Generator<Frame> {
  yield [8, 4];

  for (const byte of bytes) {
    for (const bit of iterateBits(byte)) {
      if (bit) {
        yield [1, 3];
      } else {
        yield [1, 1];
      }
    }
  }
}

function writeChunk(
  stream: NodeJS.WritableStream,
  chunk: string,
  encoding?: BufferEncoding,
): Promise<void> {
  return new Promise<void>((resolve, reject) => {
    stream.write(chunk, encoding, (error) => {
      if (error != null) {
        reject(error);
      } else {
        resolve();
      }
    });
  });
}

async function writeFrames(
  stream: Writable,
  frames: Generator<Frame>,
  pulseDuration: number,
): Promise<void> {
  // This is required to supress "leading space ignored" warnings.
  await writeChunk(stream, `pulse ${pulseDuration}\n`);

  for (const [pulse, space] of frames) {
    await writeChunk(stream, `pulse ${pulse * pulseDuration}\n`);
    await writeChunk(stream, `space ${space * pulseDuration}\n`);
  }

  await writeChunk(stream, `pulse ${pulseDuration}\n`);
}

async function main(): Promise<void> {
  try {
    const params = parseArgs(process.argv.slice(2));
    let features1 = 0;
    let features2 = 0;

    if (params.dry) {
      features1 |= Features1.dry;
    }

    if (params.deodorize) {
      features2 |= Features2.deodorize;
    }

    const frames = createPanasonicIRFrames(
      params.temperature,
      Mode[params.mode],
      Power[params.power],
      Swing[params.swing],
      Fan[params.fan],
      Profile[params.profile],
      features1,
      features2,
    );

    if (params.printFrames) {
      await writeFrames(process.stdout, frames, params.pulseDuration);
    } else {
      // Node.js spawn() creates internal pipes that aren't real Unix pipes, so
      // child processes cannot access them via /dev/stdin. Using bash -c creates
      // a real Unix pipe that ir-ctl can read from /dev/stdin.
      const subprocess = spawn(
        'bash',
        ['-c', 'ir-ctl --device "$0" --send <(cat)', params.device],
        {
          stdio: ['pipe', 'inherit', 'inherit'],
        },
      );

      subprocess.on('exit', (code) => {
        process.exit(code);
      });

      await writeFrames(subprocess.stdin, frames, params.pulseDuration);

      subprocess.stdin.end();
    }
  } catch (error) {
    if (error instanceof ParseArgsError) {
      console.error('Error: ' + error.message);
      console.log(USAGE);
      process.exit(1);
    }
  }
}

main();
