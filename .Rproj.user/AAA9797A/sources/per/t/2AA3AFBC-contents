#Problem Set 6 Script
install.packages('scales')
# packages
library(tidyverse)
library(scales)
library(lfe)
library(modelsummary)

# create the dataset ------------------
## 2021-2022 deaths (BIG FILES)
covid = data.table::fread('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties-2022.csv') |>
  filter(!is.na(fips), state != 'Puerto Rico') |>
  select(fips, county, state, date, deaths) |>
  group_by(fips, county, state) |>
  summarise(deaths = max(deaths, na.rm = T) - min(deaths, na.rm = T))

## estimated mask usage from July 2020 survey
mask = read_csv('https://raw.githubusercontent.com/nytimes/covid-19-data/master/mask-use/mask-use-by-county.csv') |>
  mutate(
    fips = as.integer(COUNTYFP),
    always.mask = ALWAYS, #always masking
    .keep = 'none'
  ) # for merging

## prep CDC data from directory
vax = read_csv('cdc vax mar1.csv') |>
  filter(
    FIPS != 'UNK', 
    Recip_State != 'VI', 
    Completeness_pct > 0, 
    !is.na(Administered_Dose1_Recip)
  ) |> # drop unknown/incomplete/questionable reports
  mutate(
    fips = as.integer(FIPS), 
    population = Census2019,
    vax.complete = Series_Complete_Pop_Pct, # percent vaxd
    svi.index = SVI_CTGY, # social vulnerability index
    .keep = 'none'
  )

## merge  
covid =
  left_join(covid, mask) |>
  left_join(vax) |>
  mutate(deaths.scaled = deaths / population * 100000) |>
  ungroup() # scale by population

rm(mask, vax)

save(covid, file = 'covid.RData')
summary(covid)

# COVID deaths nationally ----------
## VIZ  
covid |>
  ggplot(aes(x = 1+deaths)) +
  geom_histogram(color = 'black', fill = 'darkseagreen3') +
  scale_x_log10() +
  scale_y_continuous(
    expand = c(0,0)
  ) +
  theme(
    panel.background = element_blank(),
    axis.line = element_line(colour = 'black'),
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(
    title = 'COVID Deaths Nationwide',
    x = 'Deaths',
    y = 'Frequency'
  ) 

## stat summary and more
summary(covid$deaths)
# find some examples?

covid |>
  filter(deaths == max(deaths, na.rm = TRUE) | deaths == min(deaths, na.rm = TRUE)) |>
  print(n=60)

# Mask usage -----------------------
## VIZ: "Always wears a mask"
covid |>
  ggplot(aes(x = always.mask*100)) +
  geom_histogram(color = 'black', fill = 'steelblue3')+
  scale_y_continuous(
    expand = c(0,0)
  ) +
  scale_x_continuous(
    breaks = c(25, 50, 75),
    labels = c('25%', '50%', '75%')
  ) +
  theme(
    panel.background = element_blank(),
    axis.line = element_line(colour = 'black'),
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(
    title = 'Always Wearing a Mask',
    x = 'Wears a Mask',
    y = 'Frequency'
  )

## helpers
summary(covid$always.mask) 
# find hi/lo counties?

covid |>
  filter(always.mask == max(always.mask, na.rm = TRUE) | always.mask == min(always.mask, na.rm = TRUE))

covid |>
  ggplot(aes(x = vax.complete)) +
  geom_histogram(color = 'black', fill = 'indianred4')+
  scale_y_continuous(
    expand = c(0,0)
  ) +
  scale_x_continuous(
    breaks = c(25, 50, 75),
    labels = c('25%', '50%', '75%')
  ) +
  theme(
    panel.background = element_blank(),
    axis.line = element_line(colour = 'black'),
    plot.title = element_text(hjust = 0.5)
  ) +labs(
    title = 'Percentage of People Vaccinated',
    x = 'Vaccinated',
    y = 'Frequency'
  )
covid |>
  na.omit(svi.index) |>
  ggplot(aes(y = vax.complete, x = svi.index)) +
  geom_boxplot(fill = 'honeydew4') +
  scale_y_continuous(
    breaks = c(25, 50, 75),
    labels = c('25%', '50%', '75%')
  )+
  theme(
    panel.background = element_blank(),
    axis.line = element_line(colour = 'black'),
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(
    title = 'Vaccination Rate for Social Vulnerability Index',
    y = 'Vaccination Rates',
    x = 'Social Vulnerability Index'
  )

mods = list(
  m1 = felm(deaths.scaled ~ always.mask + population + svi.index | state, data = covid),
  m2 = felm(deaths.scaled ~ vax.complete + population + svi.index | state, data = covid),
  m3 = felm(deaths.scaled ~ always.mask + vax.complete + svi.index | state, data = covid)
)

stargazer::stargazer(
  mods,
  keep.stat = 'n', type = 'html', # change to html in Rmd
  add.lines = list(c('State fixed effects','Yes','Yes','Yes'))
)

modelsummary(mods, 
             statistic = NULL, 
             title = 'Effect of Vaccination and Mask Usage on 2022 COVID Deaths')|>
  save_tt("table.html")
  