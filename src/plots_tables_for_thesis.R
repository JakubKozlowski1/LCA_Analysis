library(tidyverse)
library(here)
library(readxl)
library(officer)
library(gridExtra)
library(flextable)
here()
coal <- read_excel("./Data/MonteCarlo/Cleaned/wegiel3_monte_carlo_edit.xlsx")


lignite <- read_excel("./Data/MonteCarlo/Cleaned/lignite2_monte_carlo_edit.xlsx")
biomass <- read_excel("./Data/MonteCarlo/Cleaned/biomass2_monte_carlo_edit.xlsx")
wind <- read_excel("./Data/MonteCarlo/Cleaned/wiatr2_monte_carlo_edit.xlsx")
gas <- read_excel("./Data/MonteCarlo/Cleaned/gaz4_monte_carlo_edit.xlsx")
nuclear <- read_excel("./Data/MonteCarlo/Cleaned/nuclear2_monte_carlo_edit.xlsx")
pv <- read_excel("./Data/MonteCarlo/Cleaned/pv3_monte_carlo_edit.xlsx")
hydro <- read_excel("./Data/MonteCarlo/Cleaned/hydro3_monte_carlo_edit.xlsx")



#### climate change

climate <- tibble(
  coal = coal$`Climate change`,
  lignite = lignite$`Climate change`,
  biomass = biomass$`Climate change`,
  wind = wind$`Climate change`,
  gas = gas$`Climate change`,
  nuclear = nuclear$`Climate change`,
  pv = pv$`Climate change`,
  hydro = hydro$`Climate change`)

climate <- climate %>%
  filter(
         coal > 0,
         lignite < 2000 & lignite > 0,
         biomass > 0,
         wind > 0,
         gas < 1250 & gas > 0,
         nuclear > 0 & nuclear < 50,
         pv < 200 & pv > 0,
         hydro > 5
         )


climate_pivot <- climate %>%
  rename(
    "W. kamienny" = "coal",
    "Biomasa" = "biomass",
    "W. brunatny" = "lignite",
    "Jądrowa" = "nuclear",
    "Gaz" = "gas",
    "Słońce" = "pv",
    "Wiatr" = "wind",
    "Woda" = "hydro"
  ) %>%
  pivot_longer(cols=everything())

climate_pivot_high <- climate %>%
  rename(
    "W. kamienny" = "coal",
    "Biomasa" = "biomass",
    "W. brunatny" = "lignite",
    "Jądrowa" = "nuclear",
    "Gaz" = "gas",
    "Słońce" = "pv",
    "Wiatr" = "wind",
    "Woda" = "hydro"
  ) %>%
  pivot_longer(cols=everything()) %>%
  filter(value > 90)

climate_pivot_low <- climate %>%
  rename(
    "W. kamienny" = "coal",
    "Biomasa" = "biomass",
    "W. brunatny" = "lignite",
    "Jądrowa" = "nuclear",
    "Gaz" = "gas",
    "Słońce" = "pv",
    "Wiatr" = "wind",
    "Woda" = "hydro"
  ) %>%
  pivot_longer(cols=everything()) %>%
  filter(value < 110)


cp <- climate_pivot %>% ggplot() +
  geom_histogram(aes(x=value, 
                     fill=as.factor(name)),
                     color=I("black"),
                 size=0.1,
                     alpha=0.6, 
                     position='identity',
                     binwidth = 10) +
  labs(title = "Rozkład wpływu na zmianę klimatu",
       y = "Liczba zdarzeń",
       x = "Wartość [kg CO2 eq]") +
  scale_fill_discrete(name = "Źródło") +
  scale_color_discrete(guide = "none") +
  theme_minimal()

cph <- climate_pivot_high %>% ggplot() +
  geom_histogram(aes(x=value,
                     fill=as.factor(name)),
                 color=I("black"),
                     size=0.1,
                     alpha=0.6,
                     position='identity',
                     binwidth = 8) +
  labs(title = "Rozkład wpływu na zmianę klimatu (wysokie wartości)",
      y = "Liczba zdarzeń",
      x = "Wartość [kg CO2 eq]") +
  scale_fill_discrete(name = "Źródło") +
  scale_color_discrete(guide = "none") +
  theme_minimal()

cpl <- climate_pivot_low %>% ggplot() +
  geom_histogram(aes(x=value,
                     fill=as.factor(name)),
                 color=I("black"),
                 size=0.1,
                     alpha=0.6,
                     position='identity',
                     binwidth = 0.5) +
  labs(title = "Rozkład wpływu na zmianę klimatu (niskie wartości)",
       y = "Liczba zdarzeń",
       x = "Wartość [kg CO2 eq]") +
  scale_fill_discrete(name = "Źródło") +
  scale_color_discrete(guide = "none") +
  theme_minimal()

#Fossil depletion
fossil <- tibble(
  coal = coal$`Fossil depletion`,
  lignite = lignite$`Fossil depletion`, 
  biomass = biomass$`Fossil depletion`, 
  wind = wind$`Fossil depletion`,
  gas = gas$`Fossil depletion`, 
  nuclear = nuclear$`Fossil depletion`, 
  pv = pv$`Fossil depletion`,
  hydro = hydro$`Fossil depletion`)

fossil <- fossil %>%
  filter(coal > 0 & coal < quantile(coal, 0.99),
         lignite > 0 & lignite < quantile(lignite, 0.99),
         biomass > 0 & biomass < quantile(biomass, 0.99),
         wind > 0 & wind < quantile(wind, 0.99),
         gas > 0 & gas < quantile(gas, 0.99),
         nuclear > 0 & nuclear < quantile(nuclear, 0.99),
         pv > 0 & pv < quantile(pv, 0.99),
         hydro > 0 & hydro < quantile(hydro, 0.99))

fossil_pivot <- fossil %>%
  select(!hydro) %>%
  rename(
    "W. kamienny" = "coal",
    "Biomasa" = "biomass",
    "W. brunatny" = "lignite",
    "Jądrowa" = "nuclear",
    "Gaz" = "gas",
    "Słońce" = "pv",
    "Wiatr" = "wind"
  ) %>%
  pivot_longer(cols=everything())

fossil_pivot_high <- fossil %>%
  select(!hydro) %>%
  rename(
    "W. kamienny" = "coal",
    "Biomasa" = "biomass",
    "W. brunatny" = "lignite",
    "Jądrowa" = "nuclear",
    "Gaz" = "gas",
    "Słońce" = "pv",
    "Wiatr" = "wind"
  ) %>%
  pivot_longer(cols=everything()) %>%
  filter(value > 40)

fossil_pivot_low <- fossil %>%
  select(!hydro) %>%
  rename(
    "W. kamienny" = "coal",
    "Biomasa" = "biomass",
    "W. brunatny" = "lignite",
    "Jądrowa" = "nuclear",
    "Gaz" = "gas",
    "Słońce" = "pv",
    "Wiatr" = "wind"
  ) %>%
  pivot_longer(cols=everything()) %>%
  filter(value < 60)

fp <- fossil_pivot %>% ggplot() +
  geom_histogram(aes(x=value,
                     fill=as.factor(name),
                     color=as.factor(name)),
                     alpha=0.6,
                     position='identity',
                     binwidth = 10,
                 color=I("black"),
                 size=0.1) +
  labs(title = "Rozkład wpływu na wyczerpywanie zasobów kopalnych",
         x = "Wartość [kg ropy eq]",
         y = "Liczba zdarzeń") +
  scale_fill_discrete(name = "Źródło") +
  scale_color_discrete(guide = "none") +
  theme_minimal()

fph <- fossil_pivot_high %>% ggplot() +
  geom_histogram(aes(x=value,
                     fill=as.factor(name)),
                     color=I("black"),
                 size=0.1,
                     alpha=0.6,
                     position='identity',
                     binwidth = 10)  + 
  labs(title = "Rozkład wpływu na wyczerpywanie zasobów kopalnych \n (wysokie wartości)",
       x = "Wartość [kg ropy eq]", 
       y = "Liczba zdarzeń") + 
  scale_fill_discrete(name = "Źródło") +
  scale_color_discrete(guide = "none") +
  theme_minimal()

fpl <- fossil_pivot_low %>% ggplot() +
  geom_histogram(aes(x=value,
                      fill=as.factor(name)),
                      color=I("black"),
                 size=0.1,
                      alpha=0.6,
                      position='identity',
                      binwidth = 0.3) +
  labs(title = "Rozkład wpływu na wyczerpywanie zasobów kopalnych \n (niskie wartości)",
       x = "Wartość [kg ropy eq]", 
       y = "Liczba zdarzeń") + 
  scale_fill_discrete(name = "Źródło") +
  scale_color_discrete(guide = "none") +
  theme_minimal()

#Freshwater ecotoxicity
freshwater <- tibble(
  coal = coal$`Freshwater ecotoxicity`,
  lignite = lignite$`Freshwater ecotoxicity`, 
  biomass = biomass$`Freshwater ecotoxicity`, 
  wind = wind$`Freshwater ecotoxicity`,
  gas = gas$`Freshwater ecotoxicity`, 
  nuclear = nuclear$`Freshwater ecotoxicity`, 
  pv = pv$`Freshwater ecotoxicity`,
  hydro = hydro$`Freshwater ecotoxicity`)

freshwater <- freshwater %>%
  filter(coal > 0 & coal < quantile(coal, 0.99),
         lignite > 0 & lignite < quantile(lignite, 0.99),
         biomass > 0 & biomass < quantile(biomass, 0.99),
         wind > 0 & wind < quantile(wind, 0.99),
         gas > 0 & gas < quantile(gas, 0.99),
         nuclear > 0 & nuclear < quantile(nuclear, 0.99),
         pv > 0 & pv < quantile(pv, 0.95),
         hydro > 0 & hydro < quantile(hydro, 0.99))

freshwater_pivot <- freshwater %>%
  rename(
    "W. kamienny" = "coal",
    "Biomasa" = "biomass",
    "W. brunatny" = "lignite",
    "Jądrowa" = "nuclear",
    "Gaz" = "gas",
    "Słońce" = "pv",
    "Wiatr" = "wind"
  ) %>%
  select(!hydro) %>%
  pivot_longer(cols=everything()) 

freshwater_pivot_high <- freshwater %>%
  rename(
    "W. kamienny" = "coal",
    "Biomasa" = "biomass",
    "W. brunatny" = "lignite",
    "Jądrowa" = "nuclear",
    "Gaz" = "gas",
    "Słońce" = "pv",
    "Wiatr" = "wind"
  ) %>%
  select(!hydro) %>%
  pivot_longer(cols=everything()) %>%
  filter(value > mean(value))

freshwater_pivot_low <- freshwater %>%
  rename(
    "W. kamienny" = "coal",
    "Biomasa" = "biomass",
    "W. brunatny" = "lignite",
    "Jądrowa" = "nuclear",
    "Gaz" = "gas",
    "Słońce" = "pv",
    "Wiatr" = "wind"
  ) %>%
  select(!hydro) %>%
  pivot_longer(cols=everything()) %>%
  filter(value < mean(value))

frp <- freshwater_pivot %>% ggplot() +
  geom_histogram(aes(x=value,
                     fill=as.factor(name)),
                     color=I("black"),
                 size=0.1,
                 alpha=0.6,
                 position='identity',
                 binwidth = 0.015) +
  labs(title = "Rozkład toksyczności dla wód słodkich",
       x = "Wartość [kg 1,4-DB eq]", 
       y = "Liczba zdarzeń") + 
  scale_fill_discrete(name = "Źródło") +
  scale_color_discrete(guide = "none") +
  theme_minimal()
# tutaj nie potrzebne to podzielenie na mniejsze bo wszystko widac na tym duzym
        frph<- freshwater_pivot_high %>% ggplot() +
          geom_histogram(aes(x=value,
                             fill=as.factor(name)),
                             color=I("black"),
                         size=0.1,
                         alpha=0.6,
                         position='identity',
                         binwidth = 0.015) +
          labs(title = "Rozkład toksyczności dla wód słodkich (wysokie wartości)",
               x = "Wartość [kg 1,4-DB eq]", 
               y = "Liczba zdarzeń") + 
          scale_fill_discrete(name = "Źródło") +
          scale_color_discrete(guide = "none") +
          theme_minimal()

        frpl <- freshwater_pivot_low %>% ggplot() +
          geom_histogram(aes(x=value,
                             fill=as.factor(name)),
                             color=I("black"),
                         size=0.1,
                         alpha=0.6,
                         position='identity',
                         binwidth = 0.0015) +
          labs(title = "Rozkład toksyczności dla wód słodkich (niskie wartości)",
               x = "Wartość [kg 1,4-DB eq]", 
               y = "Liczba zdarzeń") + 
          scale_fill_discrete(name = "Źródło") +
          scale_color_discrete(guide = "none") +
          theme_minimal()


#Marine ecotoxicity
marine <- tibble(
  coal = coal$`Marine ecotoxicity`,
  lignite = lignite$`Marine ecotoxicity`, 
  biomass = biomass$`Marine ecotoxicity`, 
  wind = wind$`Marine ecotoxicity`,
  gas = gas$`Marine ecotoxicity`, 
  nuclear = nuclear$`Marine ecotoxicity`, 
  pv = pv$`Marine ecotoxicity`,
  hydro = hydro$`Marine ecotoxicity`)

marine <- marine %>%
  filter(coal > 0 & coal < quantile(coal, 0.99),
         lignite > 0 & lignite < quantile(lignite, 0.99),
         biomass > 0 & biomass < quantile(biomass, 0.99),
         wind > 0 & wind < quantile(wind, 0.99),
         gas > 0 & gas < quantile(gas, 0.99),
         nuclear > 0 & nuclear < quantile(nuclear, 0.99),
         pv > 0 & pv < quantile(pv, 0.9),
         hydro > 0 & hydro < quantile(hydro, 0.99))

marine_pivot <- marine %>%
  rename(
    "W. kamienny" = "coal",
    "Biomasa" = "biomass",
    "W. brunatny" = "lignite",
    "Jądrowa" = "nuclear",
    "Gaz" = "gas",
    "Słońce" = "pv",
    "Wiatr" = "wind"
  ) %>%
  select(!hydro) %>%
  pivot_longer(cols=everything())

marine_pivot_high <- marine %>%
  rename(
    "W. kamienny" = "coal",
    "Biomasa" = "biomass",
    "W. brunatny" = "lignite",
    "Jądrowa" = "nuclear",
    "Gaz" = "gas",
    "Słońce" = "pv",
    "Wiatr" = "wind"
  ) %>%
  select(!hydro) %>%
  pivot_longer(cols=everything()) %>%
  filter(value > mean(value))

marine_pivot_low <- marine %>%
  select(!hydro) %>%
  rename(
    "W. kamienny" = "coal",
    "Biomasa" = "biomass",
    "W. brunatny" = "lignite",
    "Jądrowa" = "nuclear",
    "Gaz" = "gas",
    "Słońce" = "pv",
    "Wiatr" = "wind"
  ) %>%
  pivot_longer(cols=everything()) %>%
  filter(value < mean(value))

mp <- marine_pivot %>% ggplot() +
  geom_histogram(aes(x=value,
                     fill=as.factor(name)),
                     color=I("black"),
                 size=0.1,
                 alpha=0.6,
                 position='identity',
                 binwidth = 0.015) +
  labs(title = "Rozkład toksyczności dla wód słonych",
       x = "Wartość [kg 1,4-DB eq]", 
       y = "Liczba zdarzeń") + 
  scale_fill_discrete(name = "Źródło") +
  scale_color_discrete(guide = "none") +
  theme_minimal()

mph <- marine_pivot_high %>% ggplot() +
  geom_histogram(aes(x=value,
                     fill=as.factor(name)),
                     color=I("black"),
                 size=0.1,
                 alpha=0.6,
                 position='identity',
                 binwidth = 0.015) +
  labs(title = "Rozkład toksyczności dla wód słonych (wysokie wartości)",
       x = "Wartość [kg 1,4-DB eq]", 
       y = "Liczba zdarzeń") + 
  scale_fill_discrete(name = "Źródło") +
  scale_color_discrete(guide = "none") +
  theme_minimal()

mpl <- marine_pivot_low %>% ggplot() +
  geom_histogram(aes(x=value,
                     fill=as.factor(name)),
                     color=I("black"),
                 size=0.1,
                 alpha=0.6,
                 position='identity',
                 binwidth = 0.002) +
  labs(title = "Rozkład toksyczności dla wód słonych (niskie wartości)",
       x = "Wartość [kg 1,4-DB eq]", 
       y = "Liczba zdarzeń") + 
  scale_fill_discrete(name = "Źródło") +
  scale_color_discrete(guide = "none") +
  theme_minimal()


#Metal depletion
metal <- tibble(
  coal = coal$`Metal depletion`,
  lignite = lignite$`Metal depletion`, 
  biomass = biomass$`Metal depletion`, 
  wind = wind$`Metal depletion`,
  gas = gas$`Metal depletion`, 
  nuclear = nuclear$`Metal depletion`, 
  pv = pv$`Metal depletion`,
  hydro = hydro$`Metal depletion`)

metal <- metal %>%
  filter(coal > 0 & coal < quantile(coal, 0.99),
         lignite > 0 & lignite < quantile(lignite, 0.99),
         biomass > 0 & biomass < quantile(biomass, 0.99),
         wind > 0 & wind < quantile(wind, 0.99),
         gas > 0 & gas < quantile(gas, 0.99),
         nuclear > 0 & nuclear < quantile(nuclear, 0.99),
         pv > 0 & pv < quantile(pv, 0.99),
         hydro > 0 & hydro < quantile(hydro, 0.99))

metal_pivot <- metal %>%
  rename(
    "W. kamienny" = "coal",
    "Biomasa" = "biomass",
    "W. brunatny" = "lignite",
    "Jądrowa" = "nuclear",
    "Gaz" = "gas",
    "Słońce" = "pv",
    "Wiatr" = "wind"
  ) %>%
  select(!hydro) %>%
  pivot_longer(cols=everything())

metal_pivot_high <- metal %>%
  rename(
    "W. kamienny" = "coal",
    "Biomasa" = "biomass",
    "W. brunatny" = "lignite",
    "Jądrowa" = "nuclear",
    "Gaz" = "gas",
    "Słońce" = "pv",
    "Wiatr" = "wind"
  ) %>%
  select(!hydro) %>%
  pivot_longer(cols=everything()) %>%
  filter(value > mean(value))

metal_pivot_low <- metal %>%
  rename(
    "W. kamienny" = "coal",
    "Biomasa" = "biomass",
    "W. brunatny" = "lignite",
    "Jądrowa" = "nuclear",
    "Gaz" = "gas",
    "Słońce" = "pv",
    "Wiatr" = "wind"
  ) %>%
  select(!hydro) %>%
  pivot_longer(cols=everything()) %>%
  filter(value < mean(value))

mep <- metal_pivot %>% ggplot() +
  geom_histogram(aes(x=value,
                     fill=as.factor(name),
                     color=as.factor(name)),
                 alpha=0.6,
                 position='identity',
                 binwidth = 0.5,
                 color=I("black"),
                 size=0.1) +
  labs(title = "Rozkład wpływu na wyczerpywanie zasobów metali",
       x = "Wartość [kg Fe eq]", 
       y = "Liczba zdarzeń") + 
  scale_fill_discrete(name = "Źródło") +
  scale_color_discrete(guide = "none") +
  theme_minimal()

meph <- metal_pivot_high %>% ggplot() +
  geom_histogram(aes(x=value,
                     fill=as.factor(name),
                     color=as.factor(name)),
                 alpha=0.6,
                 position='identity',
                 binwidth = 0.45,
                 color=I("black"),
                 size=0.1) +
  labs(title = "Rozkład wpływu na wyczerpywanie zasobów metali (wysokie wartości)",
       x = "Wartość [kg Fe eq]", 
       y = "Liczba zdarzeń") + 
  scale_fill_discrete(name = "Źródło") +
  scale_color_discrete(guide = "none") +
  theme_minimal()

mepl <- metal_pivot_low %>% ggplot() +
  geom_histogram(aes(x=value,
                     fill=as.factor(name),
                     color=as.factor(name)),
                 alpha=0.6,
                 position='identity',
                 binwidth = 0.07,
                 color=I("black"),
                 size=0.1) +
  labs(title = "Rozkład wpływu na wyczerpywanie zasobów metali (niskie wartości)",
       x = "Wartość [kg Fe eq]", 
       y = "Liczba zdarzeń") + 
  scale_fill_discrete(name = "Źródło") +
  scale_color_discrete(guide = "none") +
  theme_minimal()


#Particulate matter formation
matter <- tibble(
  coal = coal$`Particulate matter formation`,
  lignite = lignite$`Particulate matter formation`, 
  biomass = biomass$`Particulate matter formation`, 
  wind = wind$`Particulate matter formation`,
  gas = gas$`Particulate matter formation`, 
  nuclear = nuclear$`Particulate matter formation`, 
  pv = pv$`Particulate matter formation`,
  hydro = hydro$`Particulate matter formation`)

matter <- matter %>%
  filter(coal > 0 & coal < quantile(coal, 0.99),
         lignite > 0 & lignite < quantile(lignite, 0.99),
         biomass > 0 & biomass < quantile(biomass, 0.99),
         wind > 0 & wind < quantile(wind, 0.99),
         gas > 0 & gas < quantile(gas, 0.99),
         nuclear > 0 & nuclear < quantile(nuclear, 0.99),
         pv > 0 & pv < quantile(pv, 0.99),
         hydro > 0 & hydro < quantile(hydro, 0.99))

matter_pivot <- matter %>%
  rename(
    "W. kamienny" = "coal",
    "Biomasa" = "biomass",
    "W. brunatny" = "lignite",
    "Jądrowa" = "nuclear",
    "Gaz" = "gas",
    "Słońce" = "pv",
    "Wiatr" = "wind"
  ) %>%
  select(!hydro) %>%
  pivot_longer(cols=everything())

matter_pivot_high <- matter %>%
  rename(
    "W. kamienny" = "coal",
    "Biomasa" = "biomass",
    "W. brunatny" = "lignite",
    "Jądrowa" = "nuclear",
    "Gaz" = "gas",
    "Słońce" = "pv",
    "Wiatr" = "wind"
  ) %>%
  select(!hydro) %>%
  pivot_longer(cols=everything()) %>%
  filter(value > mean(value))

matter_pivot_low <- matter %>%
  rename(
    "W. kamienny" = "coal",
    "Biomasa" = "biomass",
    "W. brunatny" = "lignite",
    "Jądrowa" = "nuclear",
    "Gaz" = "gas",
    "Słońce" = "pv",
    "Wiatr" = "wind"
  ) %>%
  select(!hydro) %>%
  pivot_longer(cols=everything()) %>%
  filter(value < mean(value))

map <- matter_pivot %>% ggplot() +
  geom_histogram(aes(x=value,
                     fill=as.factor(name),
                     color=as.factor(name)),
                 alpha=0.6,
                 position='identity',
                 binwidth = 0.022,
                 color=I("black"),
                 size=0.1) +
  labs(title = "Rozkład emisji cząstek stałych",
       x = "Wartość [kg PM10 eq]", 
       y = "Liczba zdarzeń") + 
  scale_fill_discrete(name = "Źródło") +
  scale_color_discrete(guide = "none") +
  theme_minimal()

maph <- matter_pivot_high %>% ggplot() +
  geom_histogram(aes(x=value,
                     fill=as.factor(name),
                     color=as.factor(name)),
                 alpha=0.6,
                 position='identity',
                 binwidth = 0.02,
                 color=I("black"),
                 size=0.1) +
  labs(title = "Rozkład emisji cząstek stałych (wysokie wartości)",
       x = "Wartość [kg PM10 eq]", 
       y = "Liczba zdarzeń") + 
  scale_fill_discrete(name = "Źródło") +
  scale_color_discrete(guide = "none") +
  theme_minimal()

mapl <- matter_pivot_low %>% ggplot() +
  geom_histogram(aes(x=value,
                     fill=as.factor(name),
                     color=as.factor(name)),
                 alpha=0.6,
                 position='identity',
                 binwidth = 0.0026,
                 color=I("black"),
                 size=0.1) +
  labs(title = "Rozkład emisji cząstek stałych (niskie wartości)",
       x = "Wartość [kg PM10 eq]", 
       y = "Liczba zdarzeń") + 
  scale_fill_discrete(name = "Źródło") +
  scale_color_discrete(guide = "none") +
  theme_minimal()


#Human toxicity
human <- tibble(
  coal = coal$`Human toxicity`,
  lignite = lignite$`Human toxicity`, 
  biomass = biomass$`Human toxicity`, 
  wind = wind$`Human toxicity`,
  gas = gas$`Human toxicity`, 
  nuclear = nuclear$`Human toxicity`, 
  pv = pv$`Human toxicity`,
  hydro = hydro$`Human toxicity`)

human <- human %>%
  filter(coal > 0 & coal < quantile(coal, 0.99),
         lignite > 0 & lignite < quantile(lignite, 0.99),
         biomass > 0 & biomass < quantile(biomass, 0.99),
         wind > 0 & wind < quantile(wind, 0.9),
         gas > 0 & gas < quantile(gas, 0.99),
         nuclear > 0 & nuclear < quantile(nuclear, 0.99),
         pv > 0 & pv < quantile(pv, 0.9),
         hydro > 0 & hydro < quantile(hydro, 0.99))

human_pivot <- human %>%
  rename(
    "W. kamienny" = "coal",
    "Biomasa" = "biomass",
    "W. brunatny" = "lignite",
    "Jądrowa" = "nuclear",
    "Gaz" = "gas",
    "Słońce" = "pv",
    "Wiatr" = "wind"
  ) %>%
  select(!hydro) %>%
  pivot_longer(cols=everything())

human_pivot_high <- human %>%
  rename(
    "W. kamienny" = "coal",
    "Biomasa" = "biomass",
    "W. brunatny" = "lignite",
    "Jądrowa" = "nuclear",
    "Gaz" = "gas",
    "Słońce" = "pv",
    "Wiatr" = "wind"
  ) %>%
  select(!hydro) %>%
  pivot_longer(cols=everything()) %>%
  filter(value > mean(value))

human_pivot_low <- human %>%
  rename(
    "W. kamienny" = "coal",
    "Biomasa" = "biomass",
    "W. brunatny" = "lignite",
    "Jądrowa" = "nuclear",
    "Gaz" = "gas",
    "Słońce" = "pv",
    "Wiatr" = "wind"
  ) %>%
  select(!hydro) %>%
  pivot_longer(cols=everything()) %>%
  filter(value < mean(value))

hp <- human_pivot %>% ggplot() +
  geom_histogram(aes(x=value,
                     fill=as.factor(name),
                     color=as.factor(name)),
                 alpha=0.6,
                 position='identity',
                 binwidth = 1.2,
                 color=I("black"),
                 size=0.1) +
  labs(title = "Rozkład toksyczności dla ludzi",
       x = "Wartość [kg 1,4-DB eq]", 
       y = "Liczba zdarzeń") + 
  scale_fill_discrete(name = "Źródło") +
  scale_color_discrete(guide = "none") +
  theme_minimal()

hph <- human_pivot_high %>% ggplot() +
  geom_histogram(aes(x=value,
                     fill=as.factor(name),
                     color=as.factor(name)),
                 alpha=0.6,
                 position='identity',
                 binwidth = 1.1,
                 color=I("black"),
                 size=0.1) +
  labs(title = "Rozkład toksyczności dla ludzi (wysokie wartości)",
       x = "Wartość [kg 1,4-DB eq]", 
       y = "Liczba zdarzeń") + 
  scale_fill_discrete(name = "Źródło") +
  scale_color_discrete(guide = "none") +
  theme_minimal()

hpl <- human_pivot_low %>% ggplot() +
  geom_histogram(aes(x=value,
                     fill=as.factor(name),
                     color=as.factor(name)),
                 alpha=0.6,
                 position='identity',
                 binwidth = 0.08,
                 color=I("black"),
                 size=0.1) +
  labs(title = "Rozkład toksyczności dla ludzi (niskie wartości)",
       x = "Wartość [kg 1,4-DB eq]", 
       y = "Liczba zdarzeń") + 
  scale_fill_discrete(name = "Źródło") +
  scale_color_discrete(guide = "none") +
  theme_minimal()


### LCA results barplots
result <- tibble(
  Kategoria = c("Wpływ na zmianę klimatu", "Wpływ na wyczerpywanie zasobów kopalnych",
                "Toksyczność dla wód słodkich",
                "Toksyczność dla wód słonych", "Wpływ na wyczerpywanie zasobów metali", 
                "Emisja cząstek stałych", "Toksyczność dla ludzi"),
  Jednostka = c("kg CO2 eq", "kg ropy eq", "kg 1,4-DB eq", "kg 1,4-DB eq", 
                "kg Fe eq", "kg PM10 eq", "kg 1,4-DB eq"),
  coal = c(780.973, 177.484, 0.222245, 0.289452, 4.01237, 0.336544, 7.91317),
  biomass = c(251.178, 29.8824, 0.268445, 0.304731, 10.5924, 1.84479, 8.12448),
  lignite = c(1141.71, 2.65451e+2, 9.99131e-1, 9.74843e-1, 4.0276, 3.22159e-1, 6.74096e+1),
  nuclear = c(7.54686, 2.41098, 1.32590e-1, 1.35534e-1, 7.81185, 2.28634e-2, 3.89817),
  gas = c(4.80401e+2, 7.58918e+2, 6.28718e-1, 8.79844e-1, 1.63664e+1, 3.83625e-1, 9.41514),
  pv = c(3.04107e+1, 9.47959, 2.72643e-1, 5.93286e-1, 2.78338e+1, 6.78965e-2, 2.48639e+1),
  wind = c(9.84271, 3.55934, 1.36576e-1, 1.72217e-1, 9.11564, 3.33153e-2, 1.01077e+1),
  hydro = c(1.23316e+1, 1.59561e-4, 3.0324e-7, 6.85906e-7, 1.44427e-3, 4.07149e-6, 4.03639e-4)
)

result_pivot <- result %>%
  rename(
    "W. kamienny" = "coal",
    "Biomasa" = "biomass",
    "W. brunatny" = "lignite",
    "Jądrowa" = "nuclear",
    "Gaz" = "gas",
    "Słońce" = "pv",
    "Wiatr" = "wind",
    "Woda" = "hydro"
  ) %>%
  pivot_longer(cols=!c(Kategoria, Jednostka))


resulttable <- function(category) {
  set_flextable_defaults(font.size = 12)
  i <- result$Kategoria[category]
  x <- result_pivot %>%
    filter(i == result_pivot$Kategoria) %>%
    select(name, value) %>%
    mutate(value = round(value, 2)) %>%
    arrange(name) %>%
    rename("Źródło" = "name",
           "Wartość" = "value") %>%
    flextable() %>%
    colformat_num(
      big.mark = "",
      decimal.mark = ","
    )
print(x, preview = "docx")
}

resulttable(1)



barplotresult <- function(dane) {
  i <- which(dane == result$Kategoria)
  z <- result$Jednostka[i]
  barplot <- result_pivot %>%
    filter(Kategoria == dane)%>%
    filter(name != "Woda")%>%
    ggplot(aes(x = name, y = value, fill=name, ymin = 0)) +
    geom_bar(stat = "identity", color=I("black")) +
    labs(title=glue("{dane}"),
         y = glue("Wartość [{z}]"),
         x=element_blank()) +
    scale_fill_discrete(name="Źródło") +
    scale_y_continuous(expand = c(0,0)) +
    theme_minimal() +
    theme(axis.text.x = element_blank())

    
  return(barplot)
}


brpl1 <- barplotresult(result$Kategoria[1])
brpl2 <- barplotresult(result$Kategoria[2])
brpl3 <- barplotresult(result$Kategoria[3])
brpl4 <- barplotresult(result$Kategoria[4])
brpl5 <- barplotresult(result$Kategoria[5])
brpl6 <- barplotresult(result$Kategoria[6])
brpl7 <- barplotresult(result$Kategoria[7])

ggsave(
  "brpl1.png", 
  brpl1, 
  path = "./Images/MonteCarlo_Plots/Barplots",
  width = 16, 
  height = 6, 
  units = c("cm")
)


grid_climate <- grid.arrange(cp, cpl, cph)
grid_fossil <- grid.arrange(fp, fpl, fph)
grid_freshwater <- grid.arrange(frp, frpl, frph)
grid_marine <- grid.arrange(mp, mpl, mph)
grid_metal <- grid.arrange(mep, mepl, meph)
grid_matter <- grid.arrange(map, mapl, maph)
grid_human <- grid.arrange(hp, hpl, hph)


ggsave(
  "grid_climate.png", 
  grid_climate, 
  path = "./Images/MonteCarlo_Plots/Grids",
  width = 16, 
  height = 18, 
  units = c("cm")
)


#rid.arrange(
  #cp, cpl, cph, fp, fpl, fph, frp, frpl, frph, mp, mpl,
  #mph, mep, mepl, meph, map, mapl, maph, hp, hpl, hph,
  #ncol=3
#)
