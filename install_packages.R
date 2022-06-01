# PDU Projekt nr.3
# 16 maj 2022
# Automatyczne pobieranie pakietow

# List of necessary packages
package_list <- c(
  "data.table",
  "stringi",
  "pander",
  "ggplot2",
  "ggmap",
  "shiny",
  "shinydashboard",
  "tidyverse",
  "plotly",
  "leaflet",
  "rgdal"
)

# Create a list of only missing packages
missing <- package_list[!(package_list %in% installed.packages()[, "Package"])]

# Download missing packages if any
if (length(missing)) install.packages(missing,
                                      repos = "https://cran.uni-muenster.de")
