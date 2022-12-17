library(tidyverse)
library(here)
here()


structure <- read_csv("./Data/Demand/struktura.csv", show_col_types = FALSE)
co2 <- read_csv("./Data/Demand/co2.csv", show_col_types = FALSE)

view(structure)
view(co2)


# Deleting rows without value
co2 <- co2 %>%
    rename("Emission" = "Annual CO2 emissions")

co2 %>%
    summary()

hist(co2$Year)
