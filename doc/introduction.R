## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(farspkg)
library(knitr)

## -----------------------------------------------------------------------------
filename_2013 = make_filename(year = 2013)
print(filename_2013)

## -----------------------------------------------------------------------------
fars_data = fars_read(filename = filename_2013)
head(fars_data)
names(fars_data)

## -----------------------------------------------------------------------------
fars_read_years(years = c(2013, 2014))

## -----------------------------------------------------------------------------
knitr::kable(fars_summarize_years(years = c(2013:2015))) 

## -----------------------------------------------------------------------------
fars_map_state(state.num = 06, year = 2013) # California
fars_map_state(state.num = 26, year = 2013) # Michigan, Go blue!!

