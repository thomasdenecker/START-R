FROM ubuntu:latest

# Installation of essential linux libraries
RUN apt-get update && apt-get install -y \
autoconf \
build-essential \
gfortran \
cron \
pkg-config \
gdebi-core \
libcurl4-openssl-dev \
libfreetype6-dev \
libpq-dev \
libssl-dev \
libxml2-dev \
libx11-dev \
libnlopt-dev \
libglu1-mesa-dev \
libnlopt-dev \
libbz2-dev \
liblzma-dev \
openjdk-8-jdk \
xorg \
r-base \
libglu1-mesa-dev \
libpcre3-dev \
wget

# Add Java
RUN ln -s /usr/lib/jvm/java-8-openjdk-amd64 /usr/lib/jvm/default-java && R CMD javareconf

# Add Rstudio and shiny server
RUN wget --no-verbose https://download3.rstudio.org/ubuntu-12.04/x86_64/shiny-server-1.5.1.834-amd64.deb && \
gdebi -n shiny-server-1.5.1.834-amd64.deb && \
sed -i '1 a sanitize_errors off;' /etc/shiny-server/shiny-server.conf

# Install R package from CRAN
RUN R -e "install.packages(c(\
'config',\
'devtools',\
'nloptr',\
'plotly',\
'pvclust',\
'pracma',\
'data.table',\
'car',\
'shiny',\
'shinyFiles',\
'shinyjs',\
'shinythemes',\
'colourpicker',\
'rgl',\
'clusterSim',\
'stringr'\
), repos='https://cran.rstudio.com/', dependencies = TRUE)"

RUN R -e "install.packages('nloptr', repos='https://cran.rstudio.com/', dependencies = TRUE)"
RUN R -e "install.packages('car', repos='https://cran.rstudio.com/', dependencies = TRUE)"

# Install R package from Bioconductor
RUN R -e "source('http://bioconductor.org/biocLite.R');biocLite('limma', ask = F)"
RUN R -e "source('http://bioconductor.org/biocLite.R');biocLite('DNAcopy', ask = F)"
RUN R -e "source('http://bioconductor.org/biocLite.R');biocLite('SNPchip', ask = F)"

# Remove Shiny example inherited from the base image
RUN rm -rf /srv/shiny-server/*

# Change workdirectory
WORKDIR /srv/shiny-server/

# Expose a port
EXPOSE 3838

CMD ["shiny-server"]
