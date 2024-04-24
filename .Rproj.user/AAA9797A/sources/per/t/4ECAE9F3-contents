# Statistics for Applied Analysts
# Spring 24

# SETUP --------------------------
# packages
  library(tidyverse)
  library(knitr)
  library(stargazer) # regression tables

# load QOG data
  load('qog23ex.rdata')
  

# CATEGORICAL OUTCOME ------------
## Tabulation ----
  # relative frequency table
  ft = count(qog, nelda_mbbe) |>
    na.omit() |>
    mutate(percent = n/sum(n) * 100) 
  
  ft
  
  # column plot (from freq table)
  ggplot(ft, aes(x = nelda_mbbe, y = percent)) +
    geom_col()
  
## Cross tabs ----
  # 1. raw cross-tabulation
  xtab =
    count(qog, nelda_mbbe, br_pvote) |>  # (OutcomeVar, ExposureVar)
    na.omit() |> # drop NA cats
    pivot_wider( # pivot to 2-way
      names_from = br_pvote, # MUST be ExposureVar
      values_from = n, 
      values_fill = 0
    )
  
# 2. test of independence (chi squared)
  chisq.test(xtab[-1]) # drop the names columns
  
# 3. for presentation
  xtab %>%
    mutate(Total = rowSums(.[-1])) |> # add total col
    mutate_at(-1, ~ 100 * ./sum(.)) |> # convert to % in column
    kable(digits = 1L) |>
    add_header_above(header = c(" " = 1, "PR system" = 2, " " = 1))
  
  
# NUMERIC OUTCOME ----------------
## Summary stats ----
  # tidyverse approach
  qog |>
    filter(!is.na(wdi_wip)) |>
    summarise(
      n = sum(!is.na(wdi_wip)),
      Avg = mean(wdi_wip, na.rm = T),
      StdDev = sd(wdi_wip, na.rm = T),
      min = min(wdi_wip, na.rm = T),
      minEx = filter(., wdi_wip == min(wdi_wip)) |> slice(1) |> pull(cname), 
      median = median(wdi_wip, na.rm = T),
      max = max(wdi_wip, na.rm = T),
      maxEx = filter(., wdi_wip == max(wdi_wip)) |> slice(1) |> pull(cname), 
    )
  
  # basic approach
  summary(qog$wdi_wip)

  # start a plot
  p = ggplot(qog, aes(x = wdi_wip)) + theme_minimal()
  
  # choose geometry 
  p + geom_density(fill = 'cornflowerblue')
  p + geom_histogram(binwidth = 5, color = 'gray', boundary = 0)  
  
## Hyp testing ----
  # one sample t-test
  t.test(qog$wdi_wip, mu = 25, alternative = 'less')
  

## Group comparison ----
  # create table
  tab = 
    group_by(qog, ht_region) |>
    summarise(
      n = sum(!is.na(wdi_wip)),
      Avg = mean(wdi_wip, na.rm = T),
      StdDev = sd(wdi_wip, na.rm = T),
      `0` = min(wdi_wip, na.rm = T),
      `50` = median(wdi_wip, na.rm = T),
      `100` = max(wdi_wip, na.rm = T)
    ) |>
    na.omit()
  
  # visual: boxplot
  ggplot(qog, aes(x = fct_reorder(ht_region, wdi_wip, .fun = 'median'), 
                  y = wdi_wip)) +
    geom_boxplot() +
    coord_flip()
  
  # visual: mean plot
  ggplot(tab, aes(x = fct_reorder(ht_region, Avg), y = Avg)) +
    geom_col() +
    coord_flip()

  # testing hypotheses: diff of means (t)
  t.test(wdi_wip ~ br_pvote, data = qog, alternative = 'less')
  
  # testing hypotheses: ANOVA
  aov(wdi_wip ~ ht_region, qog) |>
    summary(.)
  
  
# LINEAR REGRESSION --------------
## Visualize relationship ----
  # bivariate plot
  ggplot(qog, aes(y = vdem_corr, x = wdi_wip)) +
    geom_point(shape = 21) +
    geom_smooth(method = 'lm', se = F) +
    theme_minimal()

## Estimation ----
  # estimate three models
  mod1 = lm(vdem_corr ~ wdi_wip, data = qog)
  mod2 = lm(vdem_corr ~ wdi_wip + vdem_libdem, data = qog)
  mod3 = lm(vdem_corr ~ wdi_wip + vdem_libdem + 
              log(wdi_gdpcapcur) + ht_region, data = qog)
  
  # table of estimates
  stargazer(mod1, mod2, mod3, type = 'text', keep.stat = 'n')

## Log transformations ----
  # plot gdp and log(gdp)
  qog %>%
    mutate(loggdp = log(wdi_gdpcapcur)) |>
    pivot_longer(cols = contains('gdp')) |>
    ggplot(aes(x = value)) +
    geom_histogram(color = 'white') +
    facet_wrap(fct_rev(name) ~ ., scales = 'free')
  
  # plot corr as function of x and log(x)
  qog %>%
    mutate(loggdp = log(wdi_gdpcapcur)) |>
    pivot_longer(cols = contains('gdp')) |>
    ggplot(aes(x = value, y = vdem_corr)) +
    geom_point(shape = 21) +
    geom_smooth(method = 'loess', color = 'red', se = F) +
    geom_smooth(method = 'lm', se = F) +
    facet_wrap(fct_rev(name) ~ ., scales = 'free')
  
## Categorical predictors ----
  # calculate group means
  group_by(qog, nelda_mbbe) |>
    summarize(mean = mean(vdem_corr, na.rm = T))
  
  # Estimate
  catmod = lm(vdem_corr ~ nelda_mbbe, qog)
  
  # Table
  stargazer(catmod, type = 'text', keep.stat = 'n')  
  