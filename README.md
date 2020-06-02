## BnaSNPDB: SNP Database of *Brassica napus* L. 

<img src="www/img/BnaSNPDB.png" align="center" width="900" />

## Overview

The SNP database of *Brassica napus* L. (**BnaSNPDB**) is an interactive web portal that provides multiple analysis modules to visualize and explore SNPs across 1007 rapeseed gerplasm accessions based on the data reported by previous research ([Wu et al., 2019](http://rapeseed.zju.edu.cn/pdf/mp.pdf)). The app is deployed at http://rapeseed.zju.edu.cn:3838/bnasnpdb for online use. **BnaSNPDB** is idle until you activate by accessing the [URL](http://rapeseed.zju.edu.cn:3838/bnasnpdb). So it may take some time to load for the first time. Once it was activated, **BnaSNPDB** could be used smoothly and easily.

The portal is built entirely in **R** and **Shiny** using the Rstudio development environment.

## Install

### Requirements

* R: https://www.r-project.org/ v4.0.0+
* RStudio: https://rstudio.com/products/rstudio/download
* Shiny Server: https://rstudio.com/products/shiny/download-server (only required for deploying **BnaSNPDB** on web linux server)

### Initialize app

#### To run the app locally:

1. Clone this repository

```
git clone https://github.com/YTLogos/BnaSNPDB.git
```

> The repository is large so it may need some time to finish it.

2. Open `BnaSNPDB.Rproj`

3. Install packages. In the RStudio console, run:

```
# try an http CRAN (Bioc) mirror if https CRAN (Bioc) mirror doesn't work
# First install Bioconductor
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install(version = "3.11")

BiocManager::install("shiny")
BiocManager::install("ggplot2")
BiocManager::install("stringr")
BiocManager::install("dplyr")
BiocManager::install("tidyr")
BiocManager::install("forcats")
BiocManager::install("patchwork")
BiocManager::install("glue")
BiocManager::install("ggpubr")
BiocManager::install("writexl")
BiocManager::install("snpStats")
BiocManager::install("IRanges")
BiocManager::install("LDheatmap")
BiocManager::install("ape")
BiocManager::install("pegas")
BiocManager::install("gridExtra")
BiocManager::install("grid")
BiocManager::install("ggtree")
BiocManager::install("shinycssloaders")
BiocManager::install("shinydashboard")
BiocManager::install("shinyWidgets")
BiocManager::install("gggenes")
BiocManager::install("DT")
BiocManager::install("shinythemes")
BiocManager::install("NAM")
BiocManager::install("adegenet")

if (require(devtools)) install.packages("devtools")#if not already installed
devtools::install_github("AnalytixWare/ShinySky")
```

> This may take some time to complete - walk away from your computer, rest your eyes, and catch up on those stretching exercises you are meant to be doing :)

4. Start tha app by running

```
shiny::runApp(launch.browser = TRUE)
```

#### Deploy BnaSNPDB on web Linux server

1. Clone/Upload this repository into /srv/shiny-server

```
$ cd /srv/shiny-server
git clone https://github.com/YTLogos/BnaSNPDB.git
# Or clone it locally and upload the directory to /srv/shiny-server using scp or other tools 
```

2. Configure Shiny Server (/etc/shiny-server/shiny-server.conf)

```
# Instruct Shiny Server to run applications as the user "shiny"
preserve_logs true;
sanitize_errors false;
run_as shiny;

# Define a server that listens on port 3838
server {
  listen 3838;

  # Define a location at the base URL
  location / {

    # Host the directory of Shiny Apps stored in this directory
    site_dir /srv/shiny-server;

    # Log all Shiny output to files in this directory
    log_dir /var/log/shiny-server;

    # When a user visits the base URL rather than a particular application,
    # an index of the applications available in this directory will be shown.
    directory_index on;
  }
}
```

3. Change the owner of the **BnaSNPDB** directory

```
$ chown -R shiny /srv/shiny-server/BnaSNPDB  
```

4. Start Shiny-Server

```
$ start shiny-server
```

Now you can access the **BnaSNPDB** app at http://IPAddressOfYourServer:3838/BnaSNPDB.

