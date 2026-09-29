"""Renders one synthetic clip at image build time.

The first render in a fresh container is slow: numba compiles the librosa
functions it meets, and matplotlib scans the system fonts to build its font
list. Running one render here leaves those caches on disk, and the Dockerfile
moves them into the image for app.py to restore on cold start.

The clip is AAC in MPEG-TS, the format the hydrophones stream, so the render
goes through the same decoder as a real segment. make_spectrogram skips the S3
download when the clip already exists at /tmp/<id>, and skips the upload when
image_key is None, so this touches no AWS.
"""

import subprocess

import app

CLIP = "warm.ts"

subprocess.run(
    [
        "ffmpeg",
        "-hide_banner",
        "-loglevel",
        "error",
        "-y",
        "-f",
        "lavfi",
        "-i",
        "anoisesrc=d=10:c=pink:r=48000",
        "-ac",
        "1",
        "-c:a",
        "aac",
        "-f",
        "mpegts",
        f"/tmp/{CLIP}",
    ],
    check=True,
)

app.make_spectrogram(
    {
        "id": CLIP,
        "audio_bucket": "none",
        "audio_key": "none",
        "sample_rate": None,
        "image_key": None,
        "image_bucket": None,
    }
)
