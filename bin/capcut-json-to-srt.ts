#!/usr/bin/env node

import fs from 'node:fs/promises';
import path from 'node:path';

interface LaunchParams {
  inputPath: string | null;
  outputPath: string | null;
}

interface Subtitle {
  text: string;
  startTime: number;
  endTime: number;
}

function extractSubtitles(payload: any): Subtitle[] {
  const textMaterials = new Map();
  const subtitles = [];

  for (const material of payload.materials.texts) {
    textMaterials.set(material.id, material);
  }

  for (const track of payload.tracks) {
    if (track.type === 'text' && Array.isArray(track.segments)) {
      for (const segment of track.segments) {
        const material = textMaterials.get(segment.material_id);

        if (material.recognize_text.length > 0) {
          const startTime = segment.target_timerange.start;
          const endTime = startTime + segment.target_timerange.duration;

          subtitles.push({
            text: material.recognize_text,
            startTime,
            endTime,
          });
        }
      }
    }
  }

  return subtitles.sort((x, y) => x.startTime - y.startTime);
}

async function flushSubtitles(
  subtitles: Subtitle[],
  stream: NodeJS.WritableStream,
): Promise<void> {
  for (let i = 0, l = subtitles.length; i < l; i++) {
    const { startTime, endTime, text } = subtitles[i]!;
    if (i > 0) {
      await writeChunk(stream, '\n');
    }
    await writeChunk(stream, `${i + 1}\n`);
    await writeChunk(
      stream,
      `${formatTimestamp(startTime)} --> ${formatTimestamp(endTime)}\n`,
    );
    await writeChunk(stream, `${text}\n`);
  }
}

function formatTimestamp(micros: number): string {
  const totalMillis = Math.floor(micros / 1000);
  const hours = Math.floor(totalMillis / 3600000);
  const minutes = Math.floor((totalMillis % 3600000) / 60000);
  const seconds = Math.floor((totalMillis % 60000) / 1000);
  const millis = totalMillis % 1000;
  return `${String(hours).padStart(2, '0')}:${String(minutes).padStart(2, '0')}:${String(seconds).padStart(2, '0')},${String(millis).padStart(3, '0')}`;
}

function guessOutputPath(payload: any): string | null {
  for (const materials of [
    payload.materials.videos,
    payload.materials.audios,
  ]) {
    for (const material of materials) {
      if (material.path.length > 0) {
        return path.parse(material.path).name + '.srt';
      }
    }
  }
  return null;
}

function parseArgs(args: string[]): LaunchParams {
  const params: LaunchParams = {
    inputPath: null,
    outputPath: null,
  };

  for (let i = 0, l = args.length; i < l; i++) {
    switch (args[i]) {
      case '-o':
      case '--output':
        if (i + 1 >= args.length) {
          console.error('Error: -o/--output option requires a value');
          process.exit(1);
        }
        params.outputPath = args[++i]!;
        break;

      case '-h':
      case '--help':
        usage();
        process.exit(0);

      default:
        if (params.inputPath === null) {
          params.inputPath = args[i]!;
        } else {
          console.error(`Error: Unexpected argument '${args[i]}'`);
          usage();
          process.exit(1);
        }
        break;
    }
  }

  return params;
}

function readStdin(): Promise<string> {
  return new Promise<string>((resolve, reject) => {
    const chunks: string[] = [];

    process.stdin.setEncoding('utf8');

    process.stdin.on('data', (chunk) => {
      chunks.push(chunk.toString());
    });

    process.stdin.on('end', () => {
      resolve(chunks.join(''));
    });

    process.stdin.on('error', (err) => {
      reject(err);
    });
  });
}

function usage(): void {
  console.log(
    `
Usage: capcut-json-to-srt [Options] INPUT

Extract subtitles from CapCut JSON and generate SRT file

Arguments:
  INPUT                Input JSON path (default: STDIN)

Options:
  -o, --output OUTPUT  Output SRT path (default: STDOUT)
  -h, --help           Show this help message
`.trim(),
  );
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

async function main(): Promise<void> {
  let { inputPath, outputPath } = parseArgs(process.argv.slice(2));

  const content =
    inputPath !== null
      ? await fs.readFile(inputPath, 'utf8')
      : await readStdin();
  const payload = JSON.parse(content);
  const subtitles = extractSubtitles(payload);

  outputPath ??= guessOutputPath(payload);

  if (outputPath !== null) {
    await using handle = await fs.open(outputPath, 'wx');
    const stream = handle.createWriteStream();
    await flushSubtitles(subtitles, stream);
    console.log(`Saved ${subtitles.length} subtitle(s) to ${outputPath}`);
  } else {
    await flushSubtitles(subtitles, process.stdout);
  }
}

main();
