# farspkg [![Build Status](https://travis-ci.com/mandyhee/farspkg.svg?branch=master)](https://travis-ci.com/mandyhee/farspkg)

#### R package: `farspkg`, this is the coursework for Coursera Mastering Software Development in R: Building R Package. 
The package will utilize data from the US National Highway Traffic Safety Administration's Fatality Analysis Reporting System, which is a nationwide census providing the American public yearly data regarding fatal injuries suffered in motor vehicle traffic crashes, to perform data and graphic analysis.

## Repo directories
### R/
Contains `fars_functions.R` that built up five functions in the `farspkg` package (`fars_read`, `make_filename`, `fars_read_years`, `fars_summarize_years`, `fars_map_state`)

### mans/
Contain documentation (`.Rd`) files generated by Roxygen, help files were converted from `fars_functions.R` using `devtools:document()`.

### tests/
Contain two test scripts: `test-csv_data_exist.R` and `test-check_column.R`, the first will check if the desired csv files exist in `inst/exdata`, the second test will check if the number of columns (attributes) in the data equal to 50. Scripts were created by `usethis::use_testthat()`.

### inst/extdata/
Contain csv raw FARS data. (`accident_2013.csv.bz2`, `accident_2014.csv.bz2`, `accident_2015.csv.bz2`)

### vignettes/
Contain Rmarkdown that generates vignette, created by `usethis::use_vignette("introduction")`.

### doc/
Contain vignettes files (`.R`, `.html`) knitted from Rmarkdown using `devtools::build_vignettes()`.

## Package installation
Install from github:   
`devtools::install_github("mandyhee/farspkg", build_vignettes = T)`

## Usage 
```R
# load library 
library(farspkg) 

# create filename  
filename_2013 = make_filename(year = 2013)  
print(filename_2013)  
## [1] "accident_2013.csv.bz2"  

# read file 
fars_data = fars_read(filename = filename_2013) 

# read file: multiple years 
fars_read_years(years = c(2013, 2014)) 

# summarize number of fatal injuries 
fars_summarize_years(years = c(2013, 2014)) 

# mapping fatal injuries (for state code, please refer to help documentation) 
fars_map_state(state.num = 06, year = 2013) 
```

