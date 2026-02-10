#!/usr/bin/env node

import {
  Transform,
  type TransformCallback,
  type TransformOptions,
} from 'node:stream';
import { pipeline } from 'node:stream/promises';

const DEFAULT_SOURCE_LANGUAGE = 'auto';
const DEFAULT_TARGET_LANGUAGE = 'ja';
const DEFAULT_BATCH_SIZE = 100;

const GOOGLE_TRANSLATION_API_KEY = 'AIzaSyATBXajvzQLTDHEQbcpq0Ihe0vWDHmO520';
const GOOGLE_TRANSLATION_API_URL =
  'https://translate-pa.googleapis.com/v1/translateHtml';

const TRANSLATION_WAIT_PER_REQUEST = 500;

const HTML_COMMENT_PATTERN = /<!--.*?-->/g;

interface LaunchParams {
  batchSize: number;
  sourceLanguage: string;
  targetLanguage: string;
}

class LineBuffer extends Transform {
  private _buffer: string = '';

  constructor(options: TransformOptions = {}) {
    super({ ...options, encoding: 'utf-8', objectMode: true });
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
      this.push(line);
    }

    callback();
  }

  override _flush(callback: TransformCallback): void {
    const lines = this._buffer.split('\n');

    for (const line of lines) {
      this.push(line);
    }

    callback();
  }
}

class LineTranslator extends Transform {
  private _sourceLanguage;

  private _targetLanguage;

  private _batchSize;

  private _pendingLines: string[] = [];

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
    line: string,
    _encoding: BufferEncoding,
    callback: TransformCallback,
  ): Promise<void> {
    try {
      const length = this._pendingLines.push(line);
      if (length >= this._batchSize) {
        await this._flushLines();
      }
      callback();
    } catch (error) {
      callback(error as Error);
    }
  }

  override async _flush(callback: TransformCallback): Promise<void> {
    try {
      if (this._pendingLines.length > 0) {
        await this._flushLines();
      }
      callback();
    } catch (error) {
      callback(error as Error);
    }
  }

  private async _flushLines(): Promise<void> {
    if (this._flushCount > 0) {
      await new Promise((resolve) => {
        setTimeout(resolve, TRANSLATION_WAIT_PER_REQUEST);
      });
    }

    const translatedLines = await translateTexts(
      this._pendingLines,
      this._sourceLanguage,
      this._targetLanguage,
    );
    // const translatedLines = this._pendingLines;

    for (const translatedLine of translatedLines) {
      this.push(translatedLine);
    }

    this._pendingLines = [];
    this._flushCount++;
  }
}

class LineWriter extends Transform {
  private _processedLines: number = 0;

  constructor(options = {}) {
    super({
      ...options,
      objectMode: true,
      readableObjectMode: false,
    });
  }

  override _transform(
    line: string,
    _encoding: BufferEncoding,
    callback: TransformCallback,
  ): void {
    if (this._processedLines > 0) {
      this.push('\n');
    }
    this.push(line);
    this._processedLines++;
    callback();
  }

  override _flush(callback: TransformCallback): void {
    callback();
  }
}

function parseArgs(args: string[]): LaunchParams {
  const params: LaunchParams = {
    batchSize: DEFAULT_BATCH_SIZE,
    sourceLanguage: DEFAULT_SOURCE_LANGUAGE,
    targetLanguage: DEFAULT_TARGET_LANGUAGE,
  };

  for (let i = 0, l = args.length; i < l; i++) {
    switch (args[i]) {
      case '-b':
      case '--batch-size':
        params.batchSize = parseInt(args[++i]!, 10);
        break;

      case '-s':
      case '--source-language':
        params.sourceLanguage = args[++i]!;
        break;

      case '-t':
      case '--target-language':
        params.targetLanguage = args[++i]!;
        break;

      case '-h':
      case '--help':
        usage();
        process.exit(0);

      default:
        console.error(`Error: Unexpected argument '${args[i]}'`);
        usage();
        process.exit(1);
    }
  }

  return params;
}

function stripHTMLComments(input: string): string {
  return input.replace(HTML_COMMENT_PATTERN, '');
}

async function translateTexts(
  texts: string[],
  sourceLanguage: string,
  targetLanguage: string,
): Promise<string[]> {
  const normalizedTexts = texts.map((text) =>
    text.trim() === '' ? '<!---->' : text,
  );
  const body = JSON.stringify([
    [normalizedTexts, sourceLanguage, targetLanguage],
    'te',
  ]);
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

  const data = await response.json();
  const translatedTexts: string[] = ((data as any)?.[0] ?? []).map(
    stripHTMLComments,
  );

  return translatedTexts;
}

function usage(): void {
  console.log(
    `
Usage: translate-lines [Options]

Translate lines using Google Translation API.

Options:
  -s, --source-language LANGUAGE  Source language code (default: auto)
  -t, --target-language LANGUAGE  Target language code (default: ja)
  -b, --batch-size N              Number of subtitles to translate per batch (default: 1000)
  -h, --help                      Show this help message
`.trim(),
  );
}

async function main(): Promise<void> {
  const params = parseArgs(process.argv.slice(2));

  await pipeline(
    process.stdin,
    new LineBuffer(),
    new LineTranslator(
      params.sourceLanguage,
      params.targetLanguage,
      params.batchSize,
    ),
    new LineWriter(),
    process.stdout,
  );
}

main();
