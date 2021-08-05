
FROM ubuntu:focal

ENV LANG=C.UTF-8 LC_ALL=C.UTF-8 DEBIAN_FRONTEND=noninteractive

RUN apt-get update -q && \
    apt-get install -q -y --no-install-recommends \
    bzip2 \
    ca-certificates \
    git \
    libglib2.0-0 \
    libsm6 \
    libxext6 \
    libxrender1 \
    mercurial \
    subversion \
    wget \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

ENV PATH /opt/conda/bin:$PATH

CMD [ "/bin/bash" ]

# Leave these args here to better use the Docker build cache
ARG CONDA_VERSION=py39_4.9.2
ARG CONDA_MD5=b4e46fcc8029e2cfa731b788f25b1d36

RUN wget --quiet https://repo.anaconda.com/miniconda/Miniconda3-${CONDA_VERSION}-Linux-x86_64.sh -O miniconda.sh && \
    echo "${CONDA_MD5}  miniconda.sh" > miniconda.md5 && \
    if ! md5sum --status -c miniconda.md5; then exit 1; fi && \
    mkdir -p /opt && \
    sh miniconda.sh -b -p /opt/conda && \
    rm miniconda.sh miniconda.md5 && \
    ln -s /opt/conda/etc/profile.d/conda.sh /etc/profile.d/conda.sh && \
    echo ". /opt/conda/etc/profile.d/conda.sh" >> ~/.bashrc && \
    echo "conda activate base" >> ~/.bashrc && \
    find /opt/conda/ -follow -type f -name '*.a' -delete && \
    find /opt/conda/ -follow -type f -name '*.js.map' -delete && \
    /opt/conda/bin/conda clean -afy

RUN conda update --all -y && \
    conda install -y -c conda-forge \
    r-base \
    r-tidyverse  \ 
    r-janitor \
    r-furrr \
    r-benchmarkme \
    r-tictoc \
    r-arrow \
    r-lobstr \
    r-meta \
    r-here \
    r-tidytable \
    r-conflicted \
    r-data.table \
    r-rrapply && \ 
    conda install -y -c nclibz r-esc && \
    conda clean -afy 