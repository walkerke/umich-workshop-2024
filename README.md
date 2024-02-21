# umich-workshop-2024

Slides and code for Census data workshops given at the University of Michigan in 2024

This repository contains materials for a series of workshops on using Census data in R and Python given for the University of Michigan's Social Science Data Analysis Network in February and March of 2024.  

Workshop slides are available from the links below:

* February 8, 2024: [Working with the 2022 American Community Survey with R and tidycensus](https://walker-data.com/umich-workshop-2024/acs-2022/)

* February 22, 2024: [Analyzing 2020 Decennial US Census Data in R](https://walker-data.com/umich-workshop-2024/census-2020)

* March 7, 2024: Doing "GIS"" and making maps with US Census Data in R

---

## How to get the workshop materials: 

- Users new to R and RStudio should use the pre-built Posit Cloud environment available at https://posit.cloud/content/7549022.  

- Advanced users familiar with R and RStudio should clone the repository to their computers with the command `git clone https://github.com/walkerke/umich-workshop-2024.git`.  They should then install the following R packages, if not already installed:

```r
pkgs <- c("tidycensus", "tidyverse", "mapview", "survey", "srvyr")

install.packages(pkgs)
```

Experienced users should re-install __tidycensus__ to get the latest updates and ensure that all code used in the workshop will run.  

Other packages used will be picked up as dependencies of these packages on installation. 

A Census API key is recommended to access the Census API.  Participants can sign up for a key at https://api.census.gov/data/key_signup.html (it generally takes just a few minutes). 


