
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![minimal R
version](https://img.shields.io/badge/R%3E%3D-4.1.0-brightgreen.svg)](https://cran.r-project.org/)
[![Licence](https://img.shields.io/github/license/mashape/apistatus.svg)](http://choosealicense.com/licenses/mit/)
[![Travis-CI Build
Status](https://travis-ci.org/mrc-ide/covid-mortality-ascertainment.png?branch=master)](https://travis-ci.org/mrc-ide/covid-mortality-ascertainment)

## Research compendium for analysis of COVID-19 mortality ascertainment

This is a working R compendium (think R package but for reproducible
analysis). The analysis directory contains R scripts used to generate
the results.

### Installation

    git clone https://github.com/mrc-ide/covid-mortality-ascertainment.git
    cd covid-mortality-ascertainment
    open covid-mortality-ascertainment.Rproj
    devtools::install_deps()

### Overview

The structure within analysis is as follows:

    analysis/
        |
        ├── 01_xxxxx /           # analysis scripts used for generating figures
        |
        ├── figures/              # location of figures produced by the analysis scripts
        |
        ├── data/
        │   ├── DO-NOT-EDIT-ANY-FILES-IN-HERE-BY-HAND
        │   ├── raw_data/       # data obtained from elsewhere
        │   └── derived_data/   # data generated during the analysis

### Compendium DOI:

<http://dx.doi.org/xxxxxxx>

The files at the URL above will generate the results as found in the
publication. The files hosted at
github.com/mrc-ide/covid-mortality-ascertainment are the development
versions and may have changed since the report was published

### Overview of contents

This repository is our research compendium for our analysis of the
ascertainment of COVID-19 mortality. The compendium contains all data,
code, and text associated with the publication (inquire for being added
to google doc). For analysis that was conducted on cluster/remote
resources due to size of analysis, the code used to conduct analysis is
included and the outputs saved in the `raw` directory in the `data/`
directory in the `analysis/` directory. Data generated through analysis
of these model runs or of other raw data is in the `derived` data
directory.

### The supplementary files

The `analysis/` directory contains:

-   all the data files (in csv or rds format, in the `data/`
    directory)  
-   all analysis R scripts (in the `01_xxxx` directories)
-   all the figures that are included in the paper (in the `figures/`
    directory)

### The R package

This repository is organized as an R package. There are a few R
functions exported in this package - the majority of the R code is in
the analysis directory. The R package structure is here to help manage
dependencies, to take advantage of continuous integration, and so we can
keep file and data management simple.

To download the package source as you see it on GitHub, for offline
browsing, use this line at the shell prompt (assuming you have Git
installed on your computer):

``` r
git clone https://github.com/mrc-ide/covid-mortality-ascertainment.git
```

Once the download is complete, open the
`covid-mortality-ascertainment.Rproj` in RStudio to begin working with
the package and compendium files. We will endeavour to keep all package
dependencies required listed in the DESCRIPTION. This has the advantage
of allowing `devtools::install_dev_deps()` to install the required R
packages needed to run the code in this repository

### Licenses

Code: [MIT](http://opensource.org/licenses/MIT) year: 2021, copyright
holder: OJ Watson

Data: [CC-0](http://creativecommons.org/publicdomain/zero/1.0/)
attribution requested in reuse
