FROM public.ecr.aws/lambda/python:3.12

ARG FFMPEG_VERSION=ffmpeg-7.0.1-amd64-static

# Make writeable directory for Matplotlib, as /tmp is the only writeable directory
ENV MPLCONFIGDIR /tmp/matplotlib
RUN mkdir -m 777 /tmp/matplotlib

ENV LIBROSA_CACHE_DIR=/tmp/librosa_cache
RUN mkdir -m 777 /tmp/librosa_cache

ENV NUMBA_CACHE_DIR=/tmp/NUMBA_CACHE_DIR/
RUN mkdir -m 777 /tmp/NUMBA_CACHE_DIR

RUN dnf install -y tar gzip xz mesa-libGL

RUN curl -O https://johnvansickle.com/ffmpeg/releases/${FFMPEG_VERSION}.tar.xz
RUN tar -xvf "${FFMPEG_VERSION}.tar.xz"
RUN mv ${FFMPEG_VERSION}/ffmpeg /usr/local/bin
RUN mv ${FFMPEG_VERSION}/ffprobe /usr/local/bin

COPY app.py requirements.txt spectrogram_generator.py ./
RUN mkdir -p data

RUN python3.12 -m pip install -r requirements.txt -t .

# Command can be overwritten by providing a different command in the template directly.
CMD ["app.lambda_handler"]
