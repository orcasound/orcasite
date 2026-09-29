import os
import shutil

# Every new Lambda container starts with an empty /tmp, where numba, librosa
# and matplotlib keep their caches. Without them the first render recompiles
# librosa's numba functions and rebuilds the font list, which at 512 MB took
# longer than the function's timeout. The Dockerfile builds the caches into
# the image; restore them before those libraries load and look for them.
#
# Copy each cache directory separately: copying onto /tmp itself makes
# copytree set /tmp's permissions, which Lambda refuses. A failure here only
# costs a slow first render, so it must never fail the invocation.
_cache_seed = os.environ.get("CACHE_SEED")
if _cache_seed and os.path.isdir(_cache_seed):
    for _name in os.listdir(_cache_seed):
        try:
            shutil.copytree(
                os.path.join(_cache_seed, _name),
                os.path.join("/tmp", _name),
                dirs_exist_ok=True,
            )
        except OSError as e:
            print(f"Could not restore cache {_name}: {e}")

from matplotlib.pyplot import imshow
from spectrogram_generator import SpectrogramGenerator
from subprocess import check_output
from typing import TypedDict, Optional
import boto3
import io
import json
import librosa
import matplotlib
import sys

SpectrogramJob = TypedDict(
    "SpectrogramJob",
    {
        "id": str,
        "audio_bucket": str,
        "audio_key": str,
        "sample_rate": int,
        "image_key": Optional[str],
        "image_bucket": Optional[str],
    },
)


def lambda_handler(event, context):
    """Sample pure Lambda function

    Parameters
    ----------
    event: dict, required
        API Gateway Lambda Proxy Input Format

        Event doc: https://docs.aws.amazon.com/apigateway/latest/developerguide/set-up-lambda-proxy-integrations.html#api-gateway-simple-proxy-for-lambda-input-format

    context: object, required
        Lambda Context runtime methods and attributes

        Context doc: https://docs.aws.amazon.com/lambda/latest/dg/python-context-object.html

    Returns
    ------
    API Gateway Lambda Proxy Output Format: dict

        Return doc: https://docs.aws.amazon.com/apigateway/latest/developerguide/set-up-lambda-proxy-integrations.html
    """
    result = make_spectrogram(event)

    return {"status": 200, **result}


def make_spectrogram(
    job: SpectrogramJob, store_audio=False, show_image=False, store_image=False
):
    print(f"Received job: {json.dumps(job)}")
    local_path = f"/tmp/{job["id"]}"
    s3 = boto3.client("s3")

    if not os.path.exists(local_path):
        response = s3.get_object(Bucket=job["audio_bucket"], Key=job["audio_key"])
        audio_bytes = response["Body"].read()
        with open(local_path, "wb") as file:
            file.write(audio_bytes)

    sample_rate = job.get("sample_rate")
    if sample_rate is None:
        metadata = get_audio_metadata(local_path)
        sample_rate = int(metadata["streams"][0]["sample_rate"])

    audio, sr = load_audio(local_path, sample_rate)

    params = {"linear": True, "fmin": 1, "fmax": 15000, "cmap": "viridis"}

    generator = SpectrogramGenerator(
        sr, n_fft=1024, hop_length=256, db_range=(10, 80), **params
    )
    spectrogram = generator(audio)
    if show_image:
        matplotlib.pyplot.axis("off")
        imshow(spectrogram)

    # Store spectrogram image either as a file or in-memory
    image_size = None
    image_store = f"/tmp/{job["id"]}.png" if store_image else io.BytesIO()
    matplotlib.image.imsave(image_store, spectrogram)
    if store_image:
        image_size = os.path.getsize(image_store)
    else:
        image_store.seek(0)
        image_size = sys.getsizeof(image_store)
        image_store.seek(0)

    if job["image_key"] is not None and job["image_bucket"] is not None:
        image_content = open(image_store, "rb") if store_image else image_store
        s3.put_object(
            Bucket=job["image_bucket"], Key=job["image_key"], Body=image_content
        )

    return {
        "sample_rate": sample_rate,
        "image_size": image_size,
        "frequency_spacing": "linear" if params["linear"] else "log",
        "freq_min": params["fmin"],
        "freq_max": params["fmax"],
        "color_map": params["cmap"],
    }


def load_audio(local_path, sample_rate):
    """Decodes the segment with ffmpeg and hands librosa a WAV.

    The hydrophones stream AAC in MPEG-TS, which libsndfile does not read, and
    librosa 1.0 dropped the ffmpeg fallback that used to cover it.
    """
    wav = check_output(
        ["ffmpeg", "-hide_banner", "-loglevel", "error", "-i", local_path, "-f", "wav", "-"]
    )
    return librosa.load(io.BytesIO(wav), sr=sample_rate)


def get_audio_metadata(local_path):
    result = check_output(
        [
            "ffprobe",
            "-hide_banner",
            "-loglevel",
            "panic",
            "-show_format",
            "-show_streams",
            "-of",
            "json",
            local_path,
        ]
    )

    return json.loads(result)
