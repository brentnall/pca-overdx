# pca-overdx

Analysis of prostate cancer overdiagnosis using data from the CAP trial.

The analysis code is used in a (currently unpublished) article.

## Overview 

The R analysis code is:

- **pcap-overdx.r**: R code for analysis in the report.

It uses the following data files:

- Data extracted from Martin RM, Turner EL, Young GJ, Metcalfe C, Walsh EI, Lane JA, et al. Prostate-Specific Antigen Screening and 15-Year Prostate Cancer Mortality: A Secondary Analysis of the CAP Randomized Clinical Trial. JAMA. 2024;331:1460-70. Figure 2c, extracted using https://plotdigitizer.com/app     
1. plot-data-control-arm.csv
1. plot-data-screening-arm.csv
- English  mortality rates 2021-23 release, downloaded from https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/datasets/mortalityratesqxbysingleyearofage/current
1. timeseries3yrqx19802021.xlsx: 

## Explore analysis in RStudio

[![Binder](http://mybinder.org/badge_logo.svg)](http://mybinder.org/v2/gh/brentnall/pca-overdx/main?urlpath=rstudio)

# License

GNU GPL v3

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

Copyright 2025, Adam Brentnall

