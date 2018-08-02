################################
# Adapted From Kyle Walker's Blog Post: 
## https://walkerke.github.io/tidycensus/articles/basic-usage.html
################################

## Census API Documentation: https://www.census.gov/data/developers/updates/new-discovery-tool.html

install.packages("tidycensus")
install.packages("fredr")

library(tidycensus)
library(tidyverse)
library(fredr)

##should get an api_key (add it into the name)
census_api_key("fe499e0f6785392817845b6671b35f04e753abf5") 
#To install your API key for use in future sessions, run this function with "install = TRUE"

##ger_decennial: Obtain data and feature geometry for the decennial Census
m90 <- get_decennial(geography = "state", variables = "H043A001", year = 1990)
m90

vars10 <- c("P0050003", "P0050004", "P0050006", "P0040003")
il <- get_decennial(geography = "county", variables = vars10, year = 2010,
                    summary_var = "P0010001", state = "IL", geometry = TRUE) %>%
  mutate(pct = 100 * (value / summary_value))

## What are these codes?
## https://www.socialexplorer.com/data/C2010/metadata/?ds=SF1&var=P0050003


ggplot(il, aes(fill = pct, color = pct)) +
  geom_sf() +
  facet_wrap(~variable)





vt <- get_acs(geography = "county", 
              variables = c(medincome = "B19013_001"), 
              state = "VT")


vt %>%
  mutate(NAME = gsub(" County, Vermont", "", NAME)) %>%
  ggplot(aes(x = estimate, y = reorder(NAME, estimate))) +
  geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe)) +
  geom_point(color = "#1215f3", size = 3) +
  labs(title = "Household income by county in Vermont",
       subtitle = "2012-2016 American Community Survey",
       y = "",
       x = "ACS estimate (bars represent margin of error)")



################################
# Adapted From Sam Boysel's Blog Post: 
## http://sboysel.github.io/fredr/articles/fredr.html
################################


fredr_set_key("")

unemployment <-fredr(series_id = "UNRATE",observation_start = as.Date("1990-01-01"))
ggplot(unemployment, aes(x=date, y=value)) + geom_line(color="#238912")


unemployment_quarterly_chg <- fredr_series_observations(
  series_id = "UNRATE",
  observation_start = as.Date("1990-01-01"),
  frequency = "q", ##quarterly data
  units = "pch" ##percentage
)

unemployment_quarterly_chg
ggplot(unemployment_quarterly_chg, aes(x=date, y=value)) + geom_line(color="#238912")


## What options can I change? See API documentation(that is all the variables which can be studied)
fredr_docs(endpoint = "series/observations")



## How can we change the units returned?
## How can we change the frequency of the data?




