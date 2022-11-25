# Financial Econometrics Practical

This is my README for the Financial Econometrics Practical Exam 2022

``` r
#rm(list = ls()) 
#gc() 

library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.2 --
    ## v ggplot2 3.4.0      v purrr   0.3.4 
    ## v tibble  3.1.7      v dplyr   1.0.10
    ## v tidyr   1.2.0      v stringr 1.4.0 
    ## v readr   2.1.2      v forcats 0.5.1 
    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
pacman::p_load(cowplot, glue)

list.files('code/', full.names = T, recursive = T) %>% .[grepl('.R', .)] %>% as.list() %>% walk(~source(.))
```

# Question 1: Yield Spreads

## Import Data

``` r
SA_bonds <- read_rds("data/SA_Bonds.rds")

BE_Inflation <- read_rds("data/BE_Infl.rds")

bonds_2y <- read_rds("data/bonds_2y.rds")

bonds_10y <- read_rds("data/bonds_10y.rds")

usdzar <- read_rds("data/usdzar.rds")

ZA_Inflation <- read_rds("data/ZA_Infl.rds")

IV <- read_rds("data/IV.rds")
```

## Compare spreads

``` r
pacman::p_load(fmxdat)

# Sort data and calculate spreads.

SA_Spreads <- SA_bonds %>% 
    
    arrange(date) %>% 
    
    group_by(date) %>% 
    
    mutate("10Yr2Yr" = ZA_10Yr - ZA_2Yr) %>% 
    
    mutate("10Yr3M" = ZA_10Yr - SA_3M) %>% 
    
    mutate("2Yr3M" = ZA_2Yr - SA_3M) %>% 

    ungroup() %>% 
    
    pivot_longer(c("10Yr2Yr", "10Yr3M", "2Yr3M"), names_to = "Spreads", values_to = "Rates") %>% 
    
    filter(date >= as.Date("2000/01/01"))
```

# Plot the spreads

``` r
SA_Spread_plot <- SA_Spreads %>% 
    
    ggplot() +
    
    geom_line(aes(date, Rates, colour = Spreads), alpha = 0.8) +
    
    labs(title = "Yeild Spread in Local Bond Market", 
         
         subtitle = "3 Month, 2 Year and 10 year bond yields",
         
         y = "Yield Spread", x ="") +
    
    fmxdat::theme_fmx() + 
    
    fmxdat::fmx_cols()


fmxdat::finplot(SA_Spread_plot, x.date.type = "%Y%m", x.vert = TRUE)
```

![](README_files/figure-markdown_github/unnamed-chunk-4-1.png)

I can confirm that bond yields have since 2020 been the highest in two
decades.

``` r
library(dplyr)
library(tidyr)
pacman::p_load(lubridate)

# Lets combine ZA and BE inflation (Monthly)
# Note: BE starting date (2012-05-07)

BE_Inflation_clean <- BE_Inflation %>% 
    
    arrange(date) %>% 
    
    rename(BE_Inflation = Price) %>% 
    
    select(-Name) %>%  
                  
    filter(date >= as.Date("2012/05/07")) %>% 
    
    mutate(YM = format(date, "%Y%m")) %>%
    
    group_by(YM) %>% 
    
    filter(date == last(date)) %>% 
    
    select(-date) %>% 
    
    ungroup()


ZA_Inflation_clean <- ZA_Inflation %>% 
                  
        arrange(date) %>%
            
        rename(ZAR_Infl = Price) %>% 
            
        select(-Name) %>% 
            
        filter(date >= as.Date("2012/05/07")) %>% 
    
        mutate(YM = format(date, "%Y%m")) %>%
    
        group_by(YM) %>% 
    
        filter(date == last(date)) %>% 
    
        select(-date) %>% 
            
        ungroup() 
        
ZA_BE_Inflation <- BE_Inflation_clean %>% 
    
    inner_join(ZA_Inflation_clean) %>% 
    
    inner_join(.,SA_bonds %>% 
    
    arrange(date) %>% 
    
    group_by(date) %>% 
    
    mutate("10Yr2Yr" = ZA_10Yr - ZA_2Yr) %>% 
        
        filter(date >= as.Date("2012/05/07")) %>% 
    
        mutate(YM = format(date, "%Y%m")) %>%
    
        group_by(YM) %>% 
    
        filter(date == last(date)) %>% 
    
        select(-date, -SA_3M, -ZA_10Yr, -ZA_2Yr) %>% 
            
    ungroup()) 
```

    ## Joining, by = "YM"
    ## Joining, by = "YM"

``` r
ZA_BE_Inflation <- ZA_BE_Inflation %>% 
    
    pivot_longer(c("10Yr2Yr", "BE_Inflation", "ZAR_Infl"),
                 
                 names_to = "Spreads", values_to = "Rates") %>%
    
    mutate(date = ym(YM))

# Plot 

Spread_Infl_plot <- ZA_BE_Inflation %>%
    
    ggplot() +
    
    geom_line(aes(date, Rates, colour = Spreads), alpha = 0.8) +
    
    labs(title = "SA 10 year Spread and Inflation",
         
         y = "Yield Spreads", x ="", 
         
         subtitle = "Includes Inflation, Break-Even 10 Year Inflation and 10/2 year yield spread") +
    
    fmxdat::theme_fmx(title.size = ggpts(25), subtitle.size = ggpts(20)) + 
    
    fmxdat::fmx_cols()

fmxdat::finplot(Spread_Infl_plot, x.date.type = "%Y%m", x.vert = TRUE)
```

![](README_files/figure-markdown_github/unnamed-chunk-5-1.png)
