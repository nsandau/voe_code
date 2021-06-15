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
    r-conflicted && \ 
    conda clean -ya 

# Install some via cran as they cause r-base to downgrade 
RUN R -e "install.packages(c('pacman', 'esc'), repos = 'https://cloud.r-project.org/')"