=======
# HUCAgg
Functions to aggregate WBD HUC12s
=================================
[![Travis](https://travis-ci.org/USGS-R/HUCAgg.svg?branch=master)](https://travis-ci.org/USGS-R/HUCAgg) 
[![Coverage Status](https://coveralls.io/repos/github/USGS-R/HUCAgg/badge.svg?branch=master)](https://coveralls.io/github/USGS-R/HUCAgg?branch=master)

This package works with Watershed Boundary Dataset data available from the USGS at the [National Hydrography Dataset Downloads page.](https://nhd.usgs.gov/data.html)

See the Vignettes for how to work with the WBD data.

See function examples for usage.

## Package Status

[![status](https://img.shields.io/badge/USGS-Support-yellow.svg)](https://owi.usgs.gov/R/packages.html#support)

This package is considered a 'support' package. For more information, see:
[https://owi.usgs.gov/R/packages.html#support](https://owi.usgs.gov/R/packages.html#support)

## Build Notes

Note that the vignettes in this package require data for WBD_06 and the national WBD. These data are ignored by general R Build tools, but must be available in the vignettes folder. Vignettes can be build with `devtools::build_vignettes()`.

## Disclaimers:

### Provisional:
This information is preliminary or provisional and is subject to revision. It is being provided to meet the need for timely best science. The information has not received final approval by the U.S. Geological Survey (USGS) and is provided on the condition that neither the USGS nor the U.S. Government shall be held liable for any damages resulting from the authorized or unauthorized use of the information. 

### USGS Software:
Although this software program has been used by the USGS, no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith.

## License: 
[CC0 1.0 Universal](http://creativecommons.org/publicdomain/zero/1.0/)
Note: This software is in the public domain because it contains materials that originally came from the United States Geological Survey, an agency of the United States Department of Interior. For more information, see the official USGS copyright policy at http://www.usgs.gov/visual-id/credit_usgs.html#copyright