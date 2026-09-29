FROM public.ecr.aws/lambda/python:3.14

ARG FFMPEG_VERSION=ffmpeg-7.0.2-amd64-static

# /tmp is the only writeable directory at runtime, but Lambda mounts a fresh,
# empty one for every new container, so anything put there at build time is
# gone by the first invocation. The caches these libraries build on first use
# are therefore built into the image (see warm.py below) and copied into /tmp
# by app.py when a container starts.
ENV CACHE_SEED=/opt/cache-seed
ENV MPLCONFIGDIR=/tmp/matplotlib
ENV LIBROSA_CACHE_DIR=/tmp/librosa_cache
ENV NUMBA_CACHE_DIR=/tmp/numba_cache

RUN dnf install -y tar gzip xz

RUN curl -O https://johnvansickle.com/ffmpeg/releases/${FFMPEG_VERSION}.tar.xz
RUN tar -xvf "${FFMPEG_VERSION}.tar.xz"
RUN mv ${FFMPEG_VERSION}/ffmpeg /usr/local/bin
RUN mv ${FFMPEG_VERSION}/ffprobe /usr/local/bin

COPY app.py requirements.txt spectrogram_generator.py warm.py ./
RUN mkdir -p data

RUN python3 -m pip install -r requirements.txt -t .

# Render one clip so that every cache a render populates exists before the
# first real invocation, then keep the result where app.py can find it.
RUN mkdir -p ${MPLCONFIGDIR} ${LIBROSA_CACHE_DIR} ${NUMBA_CACHE_DIR} \
  && python3 warm.py \
  && mkdir -p ${CACHE_SEED} \
  && mv ${MPLCONFIGDIR} ${LIBROSA_CACHE_DIR} ${NUMBA_CACHE_DIR} ${CACHE_SEED}/

# Command can be overwritten by providing a different command in the template directly.
CMD ["app.lambda_handler"]
