#name of container: shiny-bio
#version of container: 0.1.1
FROM quantumobject/docker-baseimage:18.04
ARG DEBIAN_FRONTEND=noninteractive

MAINTAINER TaoYan "tyan@zju.edu.cn"

ENV TZ=Asia/Shanghai
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone
# system libraries of general use
## install ubuntu  packages

ADD sources.list /etc/apt/
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9


RUN apt-get update -qq && apt-get upgrade -y && apt-get -y --no-install-recommends install \
    r-base \
    r-base-dev \
    lsb-core \
    libxml2-dev \
    libcairo2-dev \
    libsqlite3-dev \
    libpq-dev \
    libssh2-1-dev \
    unixodbc-dev \
    libv8-dev \
    net-tools \
    libprotobuf-dev \
    protobuf-compiler \
    libjq-dev \
    libudunits2-0 \
    libudunits2-dev \
    libgdal-dev \
    libssl-dev \
    && apt-get remove && \
    apt-get clean \
    && rm -rf /tmp/* /var/tmp/*  \
    && rm -rf /var/lib/apt/lists/*


## COPY install_packages.R /install_packages.R
## install R-packages
## RUN Rscript /install_packages.R

COPY ./renv.lock ./renv.lock

# install renv & restore packages
RUN  R -e "install.packages('renv', repos='https://mirrors.tuna.tsinghua.edu.cn/CRAN/')"
RUN Rscript -e 'renv::restore()'



# 安装 shiny-server
RUN wget https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-1.5.15.953-amd64.deb \
          && dpkg -i --force-depends shiny-server-1.5.15.953-amd64.deb \
          && rm shiny-server-1.5.15.953-amd64.deb

# copy the app to the image
#RUN mkdir -p /srv/shiny-server
COPY ../bnasnpdb /srv/shiny-server/bnasnpdb

# expose port
EXPOSE 3838

# allow permission
RUN chown -R shiny:shiny /srv/shiny-server

# Copy further configuration files into the Docker image
COPY shiny-server.sh /usr/bin/shiny-server.sh

RUN ["chmod", "+x", "/usr/bin/shiny-server.sh"]

# run app
CMD ["/usr/bin/shiny-server.sh"]

