## tdsa: An R package to perform time-dependent sensitivity analysis

Wee Hao Ng

April 17, 2023

### Introduction

This package automates the steps required to perform time-dependent sensitivity analysis (TDSA), for both continuous- and discrete-time models. Refer to the bibliography for the theory behind TDSA, and the vignette for a simple demonstration of how to use the package.

### Installation

#### CRAN
The package has been submitted to CRAN and is currently under review.

#### Github
To install from GitHub, the easiest way is to use **devtools**, using the R command
```R
devtools::install_github("weehaong/tdsa")
```

<br>

To install from GitHub without using **devtools**:
1. Install the dependencies **deSolve**, **mathjaxr** and **numDeriv**, e.g. from CRAN.
2. Download and unpack the repository from GitHub.
3. Build the package using the OS terminal command
```Shell
R CMD build path_to_folder
```
The argument `path_to_folder` in the command should point to the folder containing the unpacked repository. This will create a tarball (.tar.gz file), e.g. tdsa_1.0-0.tar.gz. Note that for Windows users, [RTools](https://cran.r-project.org/bin/windows/Rtools/) will need to be installed, and both R and RTools added to the system or user path.
4. Finally, install the tarball. This can be done using the R command
```R
install.packages(path_to_tarball, repos = NULL, type="source")
```
or using the OS terminal command
```Shell
R CMD install path_to_tarball
```
where `path_to_tarball` should point to the tarball file from the previous step.


### Bibliography

Ng, W. H., Myers, C. R., McArt, S., & Ellner, S. P. (in review). A time for every purpose: using time-dependent sensitivity analysis to help understand and manage dynamic ecological systems. *American Naturalist*. doi: [10.1101/2023.04.13.536769](https://doi.org/10.1101/2023.04.13.536769).

Ng, W. H., Myers, C. R., McArt, S., & Ellner, S. P. (in prep). **tdsa**: An R package to perform time-dependent sensitivity analysis. *Methods in Ecology and Evolution*.
