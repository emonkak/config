#!/usr/bin/env node

import fs from 'node:fs/promises';
import path from 'node:path';
import {
  Transform,
  type TransformCallback,
  type TransformOptions,
} from 'node:stream';
import { pipeline } from 'node:stream/promises';

const DEFAULT_SOURCE_LANGUAGE = 'auto';
const DEFAULT_TARGET_LANGUAGE = 'ja';
const DEFAULT_BATCH_SIZE = 1000;

const SRT_INDEX_PATTERN = /^\d+$/;
const SRT_TIMESTAMP_PATTERN =
  /^\d{2}:\d{2}:\d{2},\d{3}\s+-->\s+\d{2}:\d{2}:\d{2},\d{3}$/;

const GOOGLE_TRANSLATION_API_KEY = 'AIzaSyATBXajvzQLTDHEQbcpq0Ihe0vWDHmO520';
const GOOGLE_TRANSLATION_API_URL =
  'https://translate-pa.googleapis.com/v1/translateHtml';

const TRANSLATION_WAIT_PER_REQUEST = 500;

interface LaunchParams {
  batchSize: number;
  inputPath: string;
  outputPath: string;
  sourceLanguage: string;
  targetLanguage: string;
}

interface Subtitle {
  index: number;
  timestamp: string;
  text: string;
}

class SRTParser extends Transform {
  private _buffer: string = '';

  private _pendingSubtitle: Subtitle | null = null;

  private _state: 'INDEX' | 'TIMESTAMP' | 'TEXT' = 'INDEX';

  constructor(options: TransformOptions = {}) {
    super({
      ...options,
      objectMode: true,
    });
  }

  override _transform(
    chunk: any,
    _encoding: BufferEncoding,
    callback: TransformCallback,
  ): void {
    this._buffer += chunk.toString();

    const lines = this._buffer.split('\n');

    this._buffer = lines.pop() ?? '';

    for (const line of lines) {
      this._processLine(line.trim());
    }

    callback();
  }

  override _flush(callback: TransformCallback): void {
    if (this._buffer.length > 0) {
      this._processLine(this._buffer.trim());
    }

    const pendingSubtitle = this._pendingSubtitle;

    if (pendingSubtitle !== null) {
      this.push(pendingSubtitle);
    }

    callback();
  }

  private _processLine(line: string): void {
    if (line === '') {
      const pendingSubtitle = this._pendingSubtitle;

      if (pendingSubtitle !== null) {
        this.push(pendingSubtitle);
        this._pendingSubtitle = null;
        this._state = 'INDEX';
      }

      return;
    }

    this._pendingSubtitle ??= { index: -1, timestamp: '', text: '' };

    switch (this._state) {
      case 'INDEX':
        if (!SRT_INDEX_PATTERN.test(line)) {
          throw new Error(
            `Invalid subtitle index at "${line}". Expected a positive integer.`,
          );
        }
        this._pendingSubtitle.index = parseInt(line, 10);
        this._state = 'TIMESTAMP';
        break;

      case 'TIMESTAMP':
        if (!SRT_TIMESTAMP_PATTERN.test(line)) {
          throw new Error(
            `Invalid timestamp format at "${line}". Expected format: "HH:MM:SS,mmm --> HH:MM:SS,mmm"`,
          );
        }
        this._pendingSubtitle.timestamp = line;
        this._state = 'TEXT';
        break;

      case 'TEXT':
        if (this._pendingSubtitle.text !== '') {
          this._pendingSubtitle.text += '\n';
        }
        this._pendingSubtitle.text += line;
        break;
    }
  }
}

class SRTWriter extends Transform {
  private _processedSubtitles = 0;

  private _startTime = Date.now();

  constructor(options = {}) {
    super({
      ...options,
      objectMode: true,
      readableObjectMode: false,
    });
  }

  override _transform(
    subtitle: Subtitle,
    _encoding: BufferEncoding,
    callback: TransformCallback,
  ): void {
    if (this._processedSubtitles > 0) {
      this.push('\n');
    }
    this.push(`${subtitle.index}\n${subtitle.timestamp}\n${subtitle.text}\n`);
    this._processedSubtitles++;
    callback();
  }

  override _flush(callback: TransformCallback): void {
    const elapsedTime = ((Date.now() - this._startTime) / 1000).toFixed(1);
    console.log(
      `Processed ${this._processedSubtitles} subtitle(s) in ${elapsedTime}s.`,
    );
    callback();
  }
}

class SRTTranslator extends Transform {
  private _sourceLanguage;

  private _targetLanguage;

  private _batchSize;

  private _pendingSubtitles: Subtitle[] = [];

  private _flushCount: number = 0;

  constructor(
    sourceLanguage: string,
    targetLanguage: string,
    batchSize: number,
    options: TransformOptions = {},
  ) {
    super({ ...options, objectMode: true });

    this._sourceLanguage = sourceLanguage;
    this._targetLanguage = targetLanguage;
    this._batchSize = batchSize;
  }

  override async _transform(
    subtitle: Subtitle,
    _encoding: BufferEncoding,
    callback: TransformCallback,
  ): Promise<void> {
    try {
      const length = this._pendingSubtitles.push(subtitle);
      if (length >= this._batchSize) {
        await this._flushSubtitles();
      }
      callback();
    } catch (error) {
      callback(error as Error);
    }
  }

  override async _flush(callback: TransformCallback): Promise<void> {
    try {
      if (this._pendingSubtitles.length > 0) {
        await this._flushSubtitles();
      }
      callback();
    } catch (error) {
      callback(error as Error);
    }
  }

  private async _flushSubtitles(): Promise<void> {
    if (this._flushCount > 0) {
      await new Promise((resolve) => {
        setTimeout(resolve, TRANSLATION_WAIT_PER_REQUEST);
      });
    }

    console.log(`Translating ${this._pendingSubtitles.length} subtitle(s)...`);

    const sourceTexts = this._pendingSubtitles.map(({ text }) => text);
    const targetTexts = await translateTexts(
      sourceTexts,
      this._sourceLanguage,
      this._targetLanguage,
    );

    for (let i = 0, l = this._pendingSubtitles.length; i < l; i++) {
      this.push({
        ...this._pendingSubtitles[i],
        text: targetTexts[i] ?? sourceTexts[i],
      });
    }

    this._pendingSubtitles = [];
    this._flushCount++;
  }
}

function parseArgs(args: string[]): LaunchParams {
  const params: Partial<LaunchParams> = {
    batchSize: DEFAULT_BATCH_SIZE,
    inputPath: undefined,
    outputPath: undefined,
    sourceLanguage: DEFAULT_SOURCE_LANGUAGE,
    targetLanguage: DEFAULT_TARGET_LANGUAGE,
  };

  for (let i = 0, l = args.length; i < l; i++) {
    switch (args[i]) {
      case '-b':
      case '--batch-size':
        params.batchSize = parseInt(args[++i]!, 10);
        break;

      case '-o':
      case '--output':
        params.outputPath = args[++i];
        break;

      case '-s':
      case '--source-language':
        params.sourceLanguage = args[++i];
        break;

      case '-t':
      case '--target-language':
        params.targetLanguage = args[++i];
        break;

      case '-h':
      case '--help':
        usage();
        process.exit(0);

      default:
        if (params.inputPath === undefined) {
          params.inputPath = args[i];
        } else {
          console.error(`Error: Unexpected argument '${args[i]}'`);
          usage();
          process.exit(1);
        }
        break;
    }
  }

  if (params.inputPath === undefined) {
    usage();
    process.exit(1);
  }

  if (params.outputPath === undefined) {
    const { name, dir, ext } = path.parse(params.inputPath);
    params.outputPath ??= path.join(
      dir,
      `${name}.${params.targetLanguage}${ext}`,
    );
  }

  return params as LaunchParams;
}

async function translateTexts(
  texts: string[],
  sourceLanguage: string,
  targetLanguage: string,
): Promise<string[]> {
  const body = JSON.stringify([[texts, sourceLanguage, targetLanguage], 'te']);
  const response = await fetch(GOOGLE_TRANSLATION_API_URL, {
    method: 'POST',
    headers: {
      'content-type': 'application/json+protobuf',
      'x-goog-api-key': GOOGLE_TRANSLATION_API_KEY,
    },
    body,
  });

  if (!response.ok) {
    throw new Error(`HTTP error! status: ${response.status}`);
  }

  const data = (await response.json()) as any;

  return data?.[0] ?? [];
}

function usage(): void {
  console.log(
    `
Usage: translate-srt [OPTIONS] INPUT

Translate SRT files using Google Translation API.

Arguments:
  INPUT                           Input SRT file to translate

Options:
  -o, --output PATH               Output file path (default: input file with language suffix)
  -s, --source-language LANGUAGE  Source language code (default: auto)
  -t, --target-language LANGUAGE  Target language code (default: ja)
  -b, --batch-size N              Number of subtitles to translate per batch (default: 1000)
  -h, --help                      Show this help message
`.trim(),
  );
}

async function main(): Promise<void> {
  const params = parseArgs(process.argv.slice(2));

  await using inputHandle = await fs.open(params.inputPath, 'r');
  await using outputHandle = await fs.open(params.outputPath, 'wx');

  await pipeline(
    inputHandle.createReadStream(),
    new SRTParser(),
    new SRTTranslator(
      params.sourceLanguage,
      params.targetLanguage,
      params.batchSize,
    ),
    new SRTWriter(),
    outputHandle.createWriteStream(),
  );

  console.log(`Saved to ${params.outputPath}`);
}

main();
