
FROM condaforge/mambaforge:latest

RUN mamba install -y \
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
    mamba install -y -c nclibz r-esc && \
    mamba clean -afy 