FROM continuumio/miniconda3:latest

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
    r-conflicted && \ 
    conda install -y -c nclibz r-esc && \
    conda clean -ya 