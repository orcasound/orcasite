# audio_viz

A Lambda function that renders one spectrogram PNG from one audio segment. Orcasite invokes it directly by name from [`Orcasite.Radio.AwsClient`](../lib/orcasite/radio/aws_client.ex), once per `AudioImage`, passing the S3 location of the segment and where to put the image. It has no other entry point.

- [`core/app.py`](core/app.py): the handler. Downloads the segment, decodes it with ffmpeg, renders with [`core/spectrogram_generator.py`](core/spectrogram_generator.py), uploads the PNG.
- [`core/Dockerfile`](core/Dockerfile): the image, built on the AWS Lambda Python base with a static ffmpeg.
- [`template.yaml`](template.yaml): the function's memory, timeout and IAM policy, as a SAM template. Stack name and region are in [`samconfig.toml`](samconfig.toml).

## Cold starts

A new Lambda container starts with an empty `/tmp`, where numba, librosa and matplotlib keep their caches, so the first render in a container recompiles librosa's numba functions and rebuilds the font list. The Dockerfile runs [`warm.py`](core/warm.py) once at build time and keeps the caches it leaves in the image; [`app.py`](core/app.py) copies them into `/tmp` before importing those libraries. If you add a library that caches on first use, give it a directory under `/tmp` in the Dockerfile and add it to the same `mv`.

To check a deployed function, find `platform.report` records in its CloudWatch log (the template sets `LogFormat: JSON`). Cold invocations carry `initDurationMs`; their `durationMs + initDurationMs` should be within a couple of seconds of a warm invocation's `durationMs`. Lambda's `Duration` metric excludes init time, so it alone understates a cold start.

## Build, test and deploy

Requires the [SAM CLI](https://docs.aws.amazon.com/serverless-application-model/latest/developerguide/serverless-sam-cli-install.html), Docker, and an AWS profile with access to the Orcasound account. Merging to `main` does not deploy; someone runs this:

```bash
cd server/audio_viz
sam build
AWS_PROFILE=orcasound sam deploy
```

`sam build` builds the image and `sam deploy` pushes it to ECR and updates the CloudFormation stack, showing the change set first.

To run the built function locally against a real event:

```bash
AWS_PROFILE=orcasound sam local invoke AudioVizFunction --event events/spectrogram_job.json
```

To tail its logs in AWS:

```bash
sam logs -n AudioVizFunction --stack-name audio-viz --tail
```

`tests/` holds SAM's generated unit tests; they do not yet exercise the handler.
