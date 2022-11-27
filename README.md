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
library(zoo)
```

    ## 
    ## Attaching package: 'zoo'
    ## 
    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

``` r
library(factoextra)
```

    ## Welcome! Want to learn more? See two factoextra-related books at https://goo.gl/ve3WBa

``` r
pacman::p_load("MTS", "robustbase")

pacman::p_load("tidyverse", "devtools", "rugarch", "rmgarch", 
     "forecast", "tbl2xts", "lubridate", "PerformanceAnalytics", 
     "ggthemes", "dplyr", "cowplot", "fmxdat", "glue","MTS", "robustbase","tidyr") 

list.files('C:/Users/Cabous/OneDrive/Desktop/22017542_Exam/code/', full.names = T, recursive = T) %>% .[grepl('.R', .)] %>% as.list() %>% walk(~source(.))
```

# Question 1: Systematic AI Fund

For this question I put my fund’s performance into perspective by
comparing it to the benchmark (Capped SWIX) as well as some industry
peers (ASISA active managers). I have also created a PowerPoint
presentation located in the Tex folder of Question 1.

I will use a rolling return approach to compare my AI fund’s returns to
the relevant funds.

## Import Data

``` r
ASISA <- read_rds("data/ASISA.rds")

BM <- read_rds("data/Capped_SWIX.rds")

AI_Fund <- read_rds("data/AI_Max_Fund.rds")
```

-   Find average returns for each active fund

``` r
# Wrangle returns for active funds (i.e., get only one return entry for each month end)

ASISA_clean <- ASISA %>% 
    
    arrange(date) %>%
    
    rename(Tickers = Name) %>% 
    
    filter(!is.na(Returns)) %>% 
    
    group_by(date) %>% 
    
    mutate(Returns = mean(Returns)) %>% 
    
    ungroup() %>% 
    
    mutate(Tickers = "Active_Fund") %>% 
    
    group_by(date) %>% 
    
    unique()


head(ASISA_clean, 10)
```

    ## # A tibble: 10 x 3
    ## # Groups:   date [10]
    ##    date       Tickers      Returns
    ##    <date>     <chr>          <dbl>
    ##  1 2002-11-30 Active_Fund  0.0385 
    ##  2 2002-12-31 Active_Fund -0.0205 
    ##  3 2003-01-31 Active_Fund -0.0269 
    ##  4 2003-02-28 Active_Fund -0.0371 
    ##  5 2003-03-31 Active_Fund -0.0675 
    ##  6 2003-04-30 Active_Fund  0.00199
    ##  7 2003-05-31 Active_Fund  0.107  
    ##  8 2003-06-30 Active_Fund  0.00930
    ##  9 2003-07-31 Active_Fund  0.0533 
    ## 10 2003-08-31 Active_Fund  0.0370

``` r
# Bind all funds

combine_funds <- ASISA_clean %>% 
    
    rbind(BM, AI_Fund)

# Calculating Rolling Returns

combine_funds_plot <- combine_funds %>% 
    
    arrange(date) %>% 
    
    group_by(Tickers) %>% 
    
# Set NA Rets to zero to make cumprod work:
    
    mutate(Returns = coalesce(Returns, 0)) %>% 
    
    mutate(CP = cumprod(1 + Returns)) %>% 
    
    ungroup() %>% 
    
    ggplot() + 
    
    geom_line(aes(date, CP, color = Tickers)) + 
    
labs(title = "Cumulative Returns of Funds",
     
    subtitle = "", caption = "Note:\nStarting dates differ") +
    
    theme_fmx(title.size = ggpts(40), subtitle.size = ggpts(5), 
              
        caption.size = ggpts(25), CustomCaption = T)

# Level plot

combine_funds_plot
```

![](README_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
# Save

ggsave(filename = glue::glue("C:/Users/Cabous/OneDrive/Desktop/22017542_Exam/Questions/Question1/Tex/plot_1.png"), 
       
       plot = combine_funds_plot, device = "png", width = 12, height = 6)
```

From the graph it is clear that active managers struggle to outperform
the benchmark, as well as my AI fund. Let’s go further….

-   Log cumulative return

``` r
# Log cumulative plot:

combine_funds_plot_log <- combine_funds_plot + 
    
    coord_trans(y = "log10") +
    
    labs(title = paste0(combine_funds_plot$labels$title, 
    "\nLog Scaled"), y = "Log Scaled Cumulative Returns")

combine_funds_plot_log
```

![](README_files/figure-markdown_github/unnamed-chunk-5-1.png)

``` r
# Save

ggsave(filename = glue::glue("C:/Users/Cabous/OneDrive/Desktop/22017542_Exam/Questions/Question1/Tex/plot_2.png"), 
       
       plot = combine_funds_plot_log, device = "png", width = 12, height = 6)
```

-   Let’s compare the returns on a rolling 3 year annualized basis

``` r
# Rolling SD annualized :

RollSD_combine_funds <- combine_funds %>% 
    
    group_by(Tickers) %>% 

    mutate(RollRets = RcppRoll::roll_prod(1 + Returns, 36, fill = NA, align = "right")^(12/36) - 1) %>% 

    group_by(date) %>% 
    
    filter(any(!is.na(RollRets))) %>% 
    
    ungroup()


RollSD_gg <- RollSD_combine_funds %>% 
    
    ggplot() + 
    
    geom_line(aes(date, RollRets, color = Tickers), alpha = 0.7, size = 1.25) + 
    
    labs(title = "Rolling 3 Year Annualized Returns for the different Funds",
         
         subtitle = "", x = "", y = "Rolling 3 year Returns (Ann.)", 
         
         caption = "Note:\nDistortions are not evident now") + 
    
    theme_fmx(title.size = ggpts(30), 
              subtitle.size = ggpts(5), caption.size = ggpts(25), CustomCaption = T) + 
    fmx_cols()
```

    ## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ## i Please use `linewidth` instead.

``` r
finplot(RollSD_gg, x.date.dist = "1 year", x.date.type = "%Y", x.vert = T, 
    y.pct = T, y.pct_acc = 1)
```

    ## Warning: Removed 70 rows containing missing values (`geom_line()`).

![](README_files/figure-markdown_github/unnamed-chunk-6-1.png)

``` r
ggsave(filename = glue::glue("C:/Users/Cabous/OneDrive/Desktop/22017542_Exam/Questions/Question1/Tex/plot_3.png"), 
       
       plot = RollSD_gg, device = "png", width = 12, height = 6)
```

    ## Warning: Removed 70 rows containing missing values (`geom_line()`).

Finally, I will calculate the rolling annualized SD of my series

``` r
# log returns

library(PerformanceAnalytics)
library(tbl2xts)

dlog_fund_ret <- combine_funds %>% 
    
    mutate(YM = format(date, "%Y%B")) %>% 
    
    arrange(date) %>% 
    
    group_by(Tickers, YM) %>% 
    
    filter(date == last(date)) %>% 
    
    group_by(Tickers) %>% 
    
    mutate(dlogret = log(1 + Returns)) %>% 
    
    select(date, Tickers, dlogret) %>% 
    
# Rolling SD annualized calc now:
    
    mutate(RollSD = RcppRoll::roll_sd(1 + dlogret, 36, fill = NA, align = "right") * sqrt(12)) %>%
    
    filter(!is.na(RollSD))
# plot

dlog_fund_ret_plot <- dlog_fund_ret %>% 
    
    ggplot() + 
    
    geom_line(aes(date, RollSD, color = Tickers), alpha = 0.7, size = 1.25) + 
    
    labs(title = "Rolling 3 Year Annualized SD for the different Funds", 
         
    subtitle = "", x = "", y = "Rolling 3 year Returns (Ann.)", 
    
    caption = "Note:\nDistortions are not evident now.") + 
    
    theme_fmx(title.size = ggpts(30), 
              
    subtitle.size = ggpts(5), caption.size = ggpts(25), CustomCaption = T) + 
    
    fmx_cols()

finplot(dlog_fund_ret_plot, x.date.dist = "1 year", x.date.type = "%Y", x.vert = T, 
    y.pct = T, y.pct_acc = 1)
```

![](README_files/figure-markdown_github/unnamed-chunk-8-1.png)

``` r
ggsave(filename = glue::glue("C:/Users/Cabous/OneDrive/Desktop/22017542_Exam/Questions/Question1/Tex/plot_4.png"), 
       
       plot = dlog_fund_ret_plot, device = "png", width = 12, height = 6)
```

# Question 2: Yield Spreads

Economists claim that the current yield spreads in the local mid to
longer dated bond yields have since 2020 been the highest in decades. As
such, I will conduct a brief analysis to investigate this claim.

A yield spread is simply the difference between the rate of return on
different debt instruments which have varying maturities. I will start
by importing the data and plotting the different yield spreds for SA.

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

## Calculate and compare spreads

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
    
    pivot_longer(c("10Yr2Yr", "10Yr3M", "2Yr3M"), 
                 names_to = "Spreads", values_to = "Rates") %>% 
    
    filter(date >= as.Date("2000/01/01"))

head(SA_Spreads, 10)
```

    ## # A tibble: 10 x 6
    ##    date       SA_3M ZA_10Yr ZA_2Yr Spreads Rates
    ##    <date>     <dbl>   <dbl>  <dbl> <chr>   <dbl>
    ##  1 2000-01-03  11.2    13.7   12.3 10Yr2Yr  1.37
    ##  2 2000-01-03  11.2    13.7   12.3 10Yr3M   2.43
    ##  3 2000-01-03  11.2    13.7   12.3 2Yr3M    1.06
    ##  4 2000-01-04  11.2    13.8   12.3 10Yr2Yr  1.44
    ##  5 2000-01-04  11.2    13.8   12.3 10Yr3M   2.61
    ##  6 2000-01-04  11.2    13.8   12.3 2Yr3M    1.17
    ##  7 2000-01-05  11.1    14.0   12.4 10Yr2Yr  1.59
    ##  8 2000-01-05  11.1    14.0   12.4 10Yr3M   2.89
    ##  9 2000-01-05  11.1    14.0   12.4 2Yr3M    1.30
    ## 10 2000-01-06  11.0    13.9   12.3 10Yr2Yr  1.64

## Plot the spreads

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

![](README_files/figure-markdown_github/unnamed-chunk-11-1.png)

I can confirm that the current yield spreads in mid to longer dated
bonds are the highest they have been in two decades. However, the yield
spread between the 2 year and 3 month (short-term) local bonds have
remained relatively stable.

Next I will try to analyse the relationship between the SA 10 Year
Break-even inflation estimate, SA inflation, and the local long yield
spread.

``` r
# Lets combine ZA and BE Monthly inflation

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

# Combine clean data 

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

![](README_files/figure-markdown_github/unnamed-chunk-12-1.png)

From the graph it appears that there is an inverse relationship between
the break even inflation estimate and local longer dated bond yields.
Finally, I will compare the the local spread to international spreads. I
will specifically look at Germany, Thailand and the US.

# Compare to International Spreads

``` r
#bonds_10y %>% pull(Name) %>% unique()

Names_2yr <- c("Thailand_2yr", "Germany_2yr", "US_2yr")

Names_10yr <- c("Thailand_10Yr", "Germany_10Yr", "US_10Yr")


bonds_2y10y_combine <-  bonds_2y %>% 
    
    arrange(date) %>% 
    
    group_by(date) %>% 
    
    filter(date >= as.Date("2000/01/01")) %>%
    
    filter(Name %in% Names_2yr) %>% 
    
    spread(Name, Bond_2Yr) %>% 
    
    ungroup() %>% 
    
    left_join(., bonds_10y %>% 
                  
    arrange(date) %>% 
    
    group_by(date) %>% 
    
    filter(date >= as.Date("2000/01/01")) %>%
    
    filter(Name %in% Names_10yr) %>% 
    
    spread(Name, Bond_10Yr) %>% 
    
    ungroup())
```

    ## Joining, by = "date"

``` r
bonds_2y10y_spread <- bonds_2y10y_combine %>% 
    
    group_by(date) %>% 
    
    mutate(US_10Yr2Yr = US_10Yr - US_2yr, 
           Thai_10Yr2Yr = Thailand_10Yr - Thailand_2yr,
           Germany_10Yr2Yr = Germany_10Yr - Germany_2yr) %>% 
    
    ungroup() %>% 
    
    inner_join(.,SA_bonds %>% 
    
    arrange(date) %>% 
    
    group_by(date) %>% 
    
    mutate("SA_10Yr2Yr" = ZA_10Yr - ZA_2Yr) %>% 
    
    filter(date >= as.Date("2000/01/01"))) %>% 
    
    select(date, SA_10Yr2Yr, US_10Yr2Yr,Thai_10Yr2Yr, Germany_10Yr2Yr) %>% 
    
    pivot_longer(c("US_10Yr2Yr", 
                   "Thai_10Yr2Yr", 
                   "Germany_10Yr2Yr",
                   "SA_10Yr2Yr"), 
                 names_to = "Spreads", values_to = "Rates") 
```

    ## Joining, by = "date"

``` r
# Plot the spreads

compare_spread_plot <- bonds_2y10y_spread %>% 
    
    arrange(date) %>% 
    
    ggplot() +
    
    geom_line(aes(date, Rates, colour = Spreads), alpha = 0.8) +
    
    labs(title = "Relative Long Term Spreads", 
         y = "Yield Spreads", x ="", 
         subtitle = "Includes US, Thailand, Germany and SA") +
    
    fmxdat::theme_fmx(title.size = ggpts(30), subtitle.size = ggpts(22), legend.size = ggpts(20)) + 
    
    fmxdat::fmx_cols()

fmxdat::finplot(compare_spread_plot, x.date.type = "%Y%m", x.vert = TRUE)
```

    ## Warning: Removed 14 rows containing missing values (`geom_line()`).

![](README_files/figure-markdown_github/unnamed-chunk-13-1.png)

The graph strongly supports the notion that the local yield spread is
very high. This in part can be due to strong foreign demand for new
South African bonds since 2019.

# Question 3: Portfolio Construction

Firstly I will compare the SWIX and ALSI by looking at the performance
of different size indexes (large and small caps). I did not include Mid
Caps since the graphs did not make sense.

## Import Data

``` r
T40 <- read_rds("data/T40.rds")

RebDays <- read_rds("data/Rebalance_days.rds")
```

## Large Caps

``` r
# First: calculate ordinary returns
library(lubridate)

#------------------ 
# Step one: gather to make tidy:
 
# lets try: large caps J200

# First the weights:

W_xts_L200 <- T40 %>% 
    
    arrange(date) %>% 
    
    select(date, Tickers, Return, Index_Name, J200) %>% 
    
    mutate(J200 = J200*Return) %>% 
    
    filter(date >= as.Date("2010/01/01")) %>% 
    
    filter(Index_Name == "Large_Caps") %>% 
    
    filter(date == first(date)) %>%
    
    mutate(weight = 1/n()) %>% 
    
    tbl_xts(., cols_to_xts = weight, spread_by = Tickers)

# Now Returns:
R_xts_L200 <- T40 %>% 
    
    arrange(date) %>% 
    
    select(date, Tickers, Return, Index_Name, J200) %>%
    
    #mutate(J200 = J200*Return) %>% 
    
    filter(date >= as.Date("2010/01/01")) %>% 
    
    filter(Index_Name == "Large_Caps") %>% 
    
    tbl_xts(., cols_to_xts = Return, spread_by = Tickers)

# Now... first ensure that column names between R_xts and
# W_xts match:

R_xts_L200 <- R_xts_L200[, names(W_xts_L200)]

# Set all NA returns to zero:

R_xts_L200[is.na(R_xts_L200)] <- 0

# Set all NA weights to zero:

W_xts_L200[is.na(W_xts_L200)] <- 0

# Also set NA's to zero:

Portfolio_L200 <- rmsfuns::Safe_Return.portfolio(R = R_xts_L200, weights = W_xts_L200, 
                                            geometric = TRUE)

Portf_Rets_L200 <- Portfolio_L200$portfolio.returns %>% xts_tbl() 

# Now large caps J400

# First the weights:

W_xts_L400 <- T40 %>% 
    
    arrange(date) %>% 
    
    select(date, Tickers, Return, Index_Name, J400) %>% 
    
    filter(date >= as.Date("2010/01/01")) %>% 
    
    #mutate(Return = coalesce(Return, 0)) %>% 
    
    mutate(J400 = J400*Return) %>%
    
    filter(Index_Name == "Large_Caps") %>% 
    
    filter(date == first(date)) %>%
    
    mutate(weight = 1/n()) %>% 
    
    tbl_xts(., cols_to_xts = weight, spread_by = Tickers)

# Now Returns:
R_xts_L400 <- T40 %>% 
    
    arrange(date) %>% 
    
    select(date, Tickers, Return, Index_Name, J400) %>% 
    
    filter(date >= as.Date("2010/01/01")) %>% 
    
    filter(Index_Name == "Large_Caps") %>% 
    
    tbl_xts(., cols_to_xts = Return, spread_by = Tickers)

# Now... first ensure that column names between R_xts and
# W_xts match:

R_xts_L400 <- R_xts_L400[, names(W_xts_L400)]

# Set all NA returns to zero:

R_xts_L400[is.na(R_xts_L400)] <- 0

# Set all NA weights to zero:

W_xts_L400[is.na(W_xts_L400)] <- 0

# Also set NA's to zero:

Portfolio_L400 <- rmsfuns::Safe_Return.portfolio(R = R_xts_L400, weights = W_xts_L400, 
                                            geometric = TRUE)

Portf_Rets_L400 <- Portfolio_L400$portfolio.returns %>% xts_tbl()

# Join L200 and L400 to plot

Portf_Rets_L200 <- Portf_Rets_L200 %>% mutate(Index = "J200") 
    
Portf_Rets_L400 <- Portf_Rets_L400 %>% mutate(Index = "J400") 

# combine

Portf_Rets_L <- rbind.data.frame(Portf_Rets_L200, Portf_Rets_L400)
```

``` r
# Plot
Portf_Rets_L %>% 
    
    arrange(date) %>% 
    
    #group_by(Index) %>% 
     
# Set NA Rets to zero to make cumprod work:
#mutate(Rets = coalesce(ret, 0)) %>% 
    
    mutate(CP = cumprod(1 + portfolio.returns)) %>% 
    
    mutate(CP = CP / first(CP)) %>% 
    
    #ungroup() %>% 
    
   # arrange(date) %>% 
    
    ggplot() +
    
    geom_line(aes(date, CP, color = Index), alpha = 0.8) +
    
    labs(title = "Cumulative Returns per Index for ALSI and SWIX",
         subtitle = "Large Caps",
         y = "Cumulative Returns", x ="") +
    
    fmxdat::theme_fmx(title.size = ggpts(30), subtitle.size = ggpts(25), legend.size = ggpts(20))
```

![](README_files/figure-markdown_github/unnamed-chunk-16-1.png)

## Small Caps

``` r
W_xts_M200 <- T40 %>% 
    
    arrange(date) %>% 
    
    select(date, Tickers, Return, Index_Name, J200) %>% 
    
    #mutate(J200 = J200*Return) %>% 
    
    filter(date >= as.Date("2008/01/01")) %>% 
    
    filter(Index_Name == "Small_Caps") %>% 
    
    filter(date == first(date)) %>%
    
    mutate(weight = 1/n()) %>% 
    
    tbl_xts(., cols_to_xts = weight, spread_by = Tickers)
```

    ## The spread_by column only has one category. 
    ## Hence only the column name was changed...

``` r
# Now Returns:
R_xts_M200 <- T40 %>% 
    
    arrange(date) %>% 
    
    select(date, Tickers, Return, Index_Name, J200) %>%
    
    filter(date >= as.Date("2008/01/01")) %>% 
    
    filter(Index_Name == "Small_Caps") %>%
    
    na.locf(.,na.rm=T, 10) %>%
    
    tbl_xts(., cols_to_xts = Return, spread_by = Tickers)
```

    ## The spread_by column only has one category. 
    ## Hence only the column name was changed...

``` r
# Now... first ensure that column names between R_xts and
# W_xts match:

R_xts_M200 <- R_xts_M200[, names(W_xts_M200)]

# Set all NA returns to zero:

R_xts_M200[is.na(R_xts_M200)] <- 0

# Set all NA weights to zero:

W_xts_M200[is.na(W_xts_M200)] <- 0

# Also set NA's to zero:

Portfolio_M200 <- rmsfuns::Safe_Return.portfolio(R = R_xts_M200, weights = W_xts_M200, 
                                            geometric = TRUE)

Portf_Rets_M200 <- Portfolio_M200$portfolio.returns %>% xts_tbl() 

# Now large caps J400

# First the weights:

W_xts_M400 <- T40 %>% 
    
    arrange(date) %>% 
    
    select(date, Tickers, Return, Index_Name, J400) %>% 
    
    filter(date >= as.Date("2008/01/01")) %>% 
    
    #mutate(Return = coalesce(Return, 0)) %>% 
    
   # mutate(J400 = J400*Return) %>%
    
    filter(Index_Name == "Small_Caps") %>% 
    
    filter(date == first(date)) %>%
    
    mutate(weight = 1/n()) %>% 
    
    tbl_xts(., cols_to_xts = weight, spread_by = Tickers)
```

    ## The spread_by column only has one category. 
    ## Hence only the column name was changed...

``` r
# Now Returns:
R_xts_M400 <- T40 %>% 
    
    arrange(date) %>% 
    
    select(date, Tickers, Return, Index_Name, J400) %>% 
    
    filter(date >= as.Date("2008/01/01")) %>% 
    
    filter(Index_Name == "Small_Caps") %>%
    
    na.locf(.,na.rm=T, 10) %>% 
    
    tbl_xts(., cols_to_xts = Return, spread_by = Tickers)
```

    ## The spread_by column only has one category. 
    ## Hence only the column name was changed...

``` r
# Now... first ensure that column names between R_xts and
# W_xts match:

R_xts_M400 <- R_xts_M400[, names(W_xts_M400)]

# Set all NA returns to zero:

R_xts_M400[is.na(R_xts_M400)] <- 0

# Set all NA weights to zero:

W_xts_M400[is.na(W_xts_M400)] <- 0

# Also set NA's to zero:

Portfolio_M400 <- rmsfuns::Safe_Return.portfolio(R = R_xts_M400, weights = W_xts_M400, 
                                            geometric = TRUE)

Portf_Rets_M400 <- Portfolio_M400$portfolio.returns %>% xts_tbl()

# Join L200 and L400 to plot

Portf_Rets_M200 <- Portf_Rets_M200 %>% mutate(Index = "J200") 
    
Portf_Rets_M400 <- Portf_Rets_M400 %>% mutate(Index = "J400") 

# combine

Portf_Rets_M <- rbind.data.frame(Portf_Rets_M200, Portf_Rets_M400)

# Plot
Portf_Rets_M %>% 
    
    arrange(date) %>% 
    
# Set NA Rets to zero to make cumprod work:
#mutate(Rets = coalesce(ret, 0)) %>% 
    
    mutate(CP = cumprod(1 + portfolio.returns)) %>% 
    
    ungroup() %>% 
    
   # arrange(date) %>% 
    
    ggplot() +
    
    geom_line(aes(date, CP, color = Index), alpha = 0.8)+
    labs(title = "Cumulative Returns per Index for ALSI and SWIX",
         subtitle = "Small Caps",
         y = "Cumulative Returns", x ="") +
    
    fmxdat::theme_fmx(title.size = ggpts(30), subtitle.size = ggpts(25), legend.size = ggpts(20))
```

![](README_files/figure-markdown_github/unnamed-chunk-17-1.png)

To answer the JSE’s question on applying capping to the indexes I will
be looking at the impact different capping levels would have had on both
the SWIX and ALSI (6% and 10%).

-   Construct Capped Portfolio’s

``` r
# Construct Capped Portfolio and Determine Performance for ALSI

reb_ALSI <- T40 %>% 
    
    filter(date %in% RebDays$date) %>% 
    
# Now we have to distinguish rebalances - to create something
# to group by:
    
    mutate(RebalanceTime = format(date, "%Y%B")) %>% 
    
    select(date, Tickers, Return, J200, RebalanceTime) %>% 
    
    rename(weight = J200) %>% 
    
    mutate(weight = coalesce(weight , 0))
  
# Apply Proportional_Cap_Foo to ALSI to get capped return for cap of 10%

Capped_df <- reb_ALSI %>% 
    
    group_split(RebalanceTime) %>% 
    
    map_df(~Proportional_Cap_Foo(., W_Cap = 0.1) ) %>% 
    
    select(-RebalanceTime)
 

ALSI_wts <- Capped_df %>% 
    
    tbl_xts(cols_to_xts = weight, spread_by = Tickers)


ALSI_rts <- T40 %>% 
    
    filter(Tickers %in% unique(Capped_df$Tickers)) %>% 
    
    tbl_xts(cols_to_xts = Return, spread_by = Tickers)


ALSI_wts[is.na(ALSI_wts)] <- 0

ALSI_rts[is.na(ALSI_rts)] <- 0


ALSI_capped <- rmsfuns::Safe_Return.portfolio(R = ALSI_rts, weights = ALSI_wts, 
    lag_weights = T) %>% 
    
    xts_tbl() %>% 
    
    rename(ALSI = portfolio.returns)
```

    ## Warning in Return.portfolio.geometric(R = R, weights = weights, wealth.index =
    ## wealth.index, : The weights for one or more periods do not sum up to 1: assuming
    ## a return of 0 for the residual weights

``` r
# Construct Capped Portfolio and Determine Performance for SWIX

reb_SWIX <- T40 %>% 
    
    filter(date %in% RebDays$date) %>%
    
    mutate(RebalanceTime = format(date, "%Y%B")) %>%
    
    select(date, Tickers, Return, J400, RebalanceTime) %>% 
    
    rename(weight = J400) %>% 
    
    mutate(weight = coalesce(weight , 0))
  
# Apply Proportional_Cap_Foo to SWIX to get capped return for cap of 6%

Capped_df <- reb_SWIX %>% 
    
    group_split(RebalanceTime) %>% 
    
    map_df(~Proportional_Cap_Foo(., W_Cap = 0.06) ) %>% 
    
    select(-RebalanceTime)
 

SWIX_wts <- Capped_df %>% 
    
    tbl_xts(cols_to_xts = weight, spread_by = Tickers)


SWIX_rts <- T40 %>% 
    
    filter(Tickers %in% unique(Capped_df$Tickers)) %>%
    
    tbl_xts(cols_to_xts = Return, spread_by = Tickers)


SWIX_wts[is.na(SWIX_wts)] <- 0

SWIX_rts[is.na(SWIX_rts)] <- 0


SWIX_capped <- rmsfuns::Safe_Return.portfolio(R = SWIX_rts, weights = SWIX_wts, 
    lag_weights = T) %>% 
    
    xts_tbl() %>% 
    
rename(SWIX = portfolio.returns)
```

    ## Warning in Return.portfolio.geometric(R = R, weights = weights, wealth.index =
    ## wealth.index, : The weights for one or more periods do not sum up to 1: assuming
    ## a return of 0 for the residual weights

``` r
# Combine and Plot Performance

capped_indices <- left_join(ALSI_capped, SWIX_capped, by = "date") %>% 
    
    pivot_longer(c("ALSI", "SWIX"), names_to = "Meth", values_to = "returns")
```

``` r
# Apply Proportional_Cap_Foo to ALSI to get capped return for cap of 6%

Capped_df_6p <- reb_ALSI %>% 
    
    group_split(RebalanceTime) %>% 
    
    map_df(~Proportional_Cap_Foo(., W_Cap = 0.06) ) %>% 
    
    select(-RebalanceTime)
 

ALSI_wts_6p <- Capped_df_6p %>% 
    
    tbl_xts(cols_to_xts = weight, spread_by = Tickers)


ALSI_rts_6p <- T40 %>% 
    
    filter(Tickers %in% unique(Capped_df_6p$Tickers)) %>% 
    
    tbl_xts(cols_to_xts = Return, spread_by = Tickers)


ALSI_wts_6p[is.na(ALSI_wts_6p)] <- 0

ALSI_rts_6p[is.na(ALSI_rts_6p)] <- 0


ALSI_capped_6p <- rmsfuns::Safe_Return.portfolio(R = ALSI_rts_6p, weights = ALSI_wts_6p, 
    lag_weights = T) %>% 
    
    xts_tbl() %>% 
    
    rename(ALSI = portfolio.returns)
```

    ## Warning in Return.portfolio.geometric(R = R, weights = weights, wealth.index =
    ## wealth.index, : The weights for one or more periods do not sum up to 1: assuming
    ## a return of 0 for the residual weights

``` r
# Apply Proportional_Cap_Foo to SwIX to get capped return for cap of 10%

Capped_df_10p <- reb_SWIX %>% 
    
    group_split(RebalanceTime) %>% 
    
    map_df(~Proportional_Cap_Foo(., W_Cap = 0.1) ) %>% 
    
    select(-RebalanceTime)
 

SWIX_wts_10p <- Capped_df_10p %>% 
    
    tbl_xts(cols_to_xts = weight, spread_by = Tickers)


SWIX_rts_10p <- T40 %>% 
    
    filter(Tickers %in% unique(Capped_df_10p$Tickers)) %>%
    
    tbl_xts(cols_to_xts = Return, spread_by = Tickers)


SWIX_wts_10p[is.na(SWIX_wts_10p)] <- 0

SWIX_rts_10p[is.na(SWIX_rts_10p)] <- 0


SWIX_capped_10p <- rmsfuns::Safe_Return.portfolio(R = SWIX_rts_10p, weights = SWIX_wts_10p, 
    lag_weights = T) %>% 
    
    xts_tbl() %>% 
    
rename(SWIX = portfolio.returns)
```

    ## Warning in Return.portfolio.geometric(R = R, weights = weights, wealth.index =
    ## wealth.index, : The weights for one or more periods do not sum up to 1: assuming
    ## a return of 0 for the residual weights

``` r
# Combine and Plot Performance

capped_indices_6p <- left_join(ALSI_capped_6p, SWIX_capped_10p, by = "date") %>% 
    
    pivot_longer(c("ALSI", "SWIX"), names_to = "Meth", values_to = "returns")



ALSI_SWIX_6p <- left_join(ALSI_capped_6p, SWIX_capped_10p, by = "date") %>% 
    
    pivot_longer(c("ALSI", "SWIX"), names_to = "Meth", values_to = "Returns")


q2_p3_6p <- capped_indices_6p %>% 
    
    group_by(Meth) %>%
    
    mutate(Idx = cumprod(1 + returns)) %>% 
    
ggplot() + 
    
geom_line(aes(date, Idx, colour = Meth), alpha = 0.8) + 
    
labs(subtitle = "ALSI capped at 6% and SWIX at 10%", 
    x = "", y = "Cumulative Return") +
    
    fmx_cols() + 
    
fmxdat::theme_fmx(subtitle.size = ggpts(20))


q2_p3 <- capped_indices %>% 
    
    group_by(Meth) %>%
    
    mutate(Idx = cumprod(1 + returns)) %>% 
    
ggplot() + 
    
geom_line(aes(date, Idx, colour = Meth), alpha = 0.8) + 
    
labs(subtitle = "ALSI capped at 10% and SWIX at 6%", 
    x = "", y = "Cumulative Return") +
    
    fmx_cols() + 
    
fmxdat::theme_fmx(subtitle.size = ggpts(20))

plot_grid(finplot(q2_p3), finplot(q2_p3_6p), labels = list(title = "Comparing Capped returns of ALSI and SWIX"), label_size = ggpts(30), align = "h")
```

![](README_files/figure-markdown_github/unnamed-chunk-19-1.png)

# Question 4: Volatility Comparison

Using the Top 40 Index data I will perform Principal Component Analysis
(PCA) as well as a rolling constituent correlation analysis to better
understand the concentration and commonality of returns for the J200
index.

## Import Data

``` r
T40 <- read_rds("data/T40.rds")
```

-   Calculate Returns

``` r
T40_Q4 <- T40 %>% 
    
    na.locf(.,na.rm=T, 5) %>%
    
    select(date, Tickers, Return, J200) %>%
    
    mutate(Return = Return*J200) %>%
    
    select(date, Tickers, Return) %>% 
    
    group_by(Tickers) %>%
    
    mutate(Tickers = gsub(" SJ Equity", "", Tickers))

# PCA using princomp

# prcomp requires wide, numeric data:

return_mat <- T40_Q4 %>% spread(Tickers,Return)

colSums(is.na(T40_Q4))
```

    ##    date Tickers  Return 
    ##       0       0       0

``` r
options(scipen = 999) # Stop the scientific notation of

return_mat <- impute_missing_returns(return_mat, impute_returns_method = "Drawn_Distribution_Collective", Seed = as.numeric(format( Sys.time(), "%Y%d%H%M")))

return_mat_Nodate <- data.matrix(return_mat[, -1])

# Simple Sample covariance and mean:

Sigma <- RiskPortfolios::covEstimation(return_mat_Nodate)

Mu <- RiskPortfolios::meanEstimation(return_mat_Nodate)

#PCA calculations

pca <- prcomp(return_mat_Nodate,center=TRUE, scale.=TRUE)

# Plot

fviz_screeplot(pca, ncp = 10)
```

![](README_files/figure-markdown_github/unnamed-chunk-22-1.png)

There are 92 components that explain a percentage of the total variation
in the T40 returns. PC1 explains 8 % of the total variance, which is the
most out of the 92 PCA’s.

``` r
fviz_pca_var(pca, col.var = "steelblue") + theme_minimal()
```

![](README_files/figure-markdown_github/unnamed-chunk-23-1.png)

``` r
fviz_contrib(pca, choice = "var", axes = 1, top = 10)
```

![](README_files/figure-markdown_github/unnamed-chunk-23-2.png)

``` r
fviz_contrib(pca, choice = "var", axes = 2, top = 10)
```

![](README_files/figure-markdown_github/unnamed-chunk-23-3.png)

Next I will calculate the rolling average pairwise correlations between
all the stocks. That will require calculating the rolling pairwise
correlation between all the stock combinations in the index and then
take the mean of all those. Thus, I will tackle the problem in
bite-sized pieces. The graph below shows the 90-day Mean Rolling
Constituent Correlation. I sourced the code I used for the function
*rolling_cor_func* , from the following link:

(<https://robotwealth.com/rolling-mean-correlations-in-the-tidyverse/>)

-   Calculate Rolling Correlation

``` r
# Calculate rolling constituent correlation

df_Q4 <- T40_Q4

#df_Q4 %>% head(5) %>% pretty_table()

# rolling correlation calculation

pairwise_corrs <- rolling_cor_func(df_Q4, 90) %>% 
    
    ungroup() %>%
    
    filter(date > as.Date("2012/03/28"))

# Determine Mean 

Mean_pair_cor <- pairwise_corrs %>% 
    
    group_by(Tickers) %>% 
    
    summarise(Mean_Cor = mean(rollingcor)) 


# Plot Mean over time
mean_pw_cors <- pairwise_corrs %>%
    
  group_by(date) %>%
    
  summarise(mean_pw_corr = mean(rollingcor, na.rm = TRUE))

mean_cor_plot <- mean_pw_cors %>% 
    
    ggplot() + 
    
    geom_line(aes(date, mean_pw_corr), alpha = 0.8, color ="steelblue") +
    
    fmx_cols() + 
    
    theme_fmx(title = ggpts(25)) + 
    
    labs(y = "Rolling Mean Constituent Correlation", 
         x = "", title = "90-day Mean Rolling Constituent Correlation")

finplot(mean_cor_plot)
```

![](README_files/figure-markdown_github/unnamed-chunk-24-1.png)

# Question 5

The study of volatility is particularly important in financial
modelling. Thus, in this question I will look at the South African ZAR
since it has been quite volatile over the past couple of years. More
specifically, I will conduct a simple univariate-GARCH estimation to
analyse the volatility of the Rand.

## Import Data

``` r
# Load Data

cncy <- read_rds("data/currencies.rds")

cncy_Carry <- read_rds("data/cncy_Carry.rds")

cncy_value <- read_rds("data/cncy_value.rds")

cncyIV <- read_rds("data/cncyIV.rds")

bbdxy <- read_rds("data/bbdxy.rds")
```

## Implied Volatility

I will start by looking at implied volatility. The market’s estimate of
how much a currency pair will fluctuate over a certain period in the
future is known as implied volatility. Option traders can use a currency
volatility index to price options on currency pairs. Implied volatility
is generally considered a measure of sentiment.

``` r
## To analyse whether the Rand is volatile lets look at Implied volatility
# What cuurencies are in the data?

cncyIV %>% group_by(Name) %>% pull(Name) %>% unique 
```

    ##  [1] "Australia_IV"   "Brazil_IV"      "Canada_IV"      "Chile_IV"      
    ##  [5] "China_IV"       "Columbia_IV"    "Czech_IV"       "Denmark_IV"    
    ##  [9] "EU_IV"          "HongKong_IV"    "Hungary_IV"     "India_IV"      
    ## [13] "Israel_IV"      "Japan_IV"       "Malaysia_IV"    "Mexico_IV"     
    ## [17] "Norway_IV"      "NZ_IV"          "Peru_IV"        "Philipines_IV" 
    ## [21] "Poland_IV"      "Romania_IV"     "Russia_IV"      "Saudi_IV"      
    ## [25] "Singapore_IV"   "SouthAfrica_IV" "SouthKorea_IV"  "Sweden_IV"     
    ## [29] "Taiwan_IV"      "Thailand_IV"    "Turkey_IV"      "UK_IV"

``` r
IV_plot <- cncyIV %>% 
    
    filter(Name==c("China_IV", "India_IV", "SouthAfrica_IV",
                   "UK_IV", "EU_IV", "Singapore_IV")) %>%
    
    filter(date > as.Date("1999-12-31")) %>% 
    
    group_by(date) %>%
    
    ggplot() + 
    
    geom_line(aes(x=date, y=Price, color=Name)) +
    
    theme_bw() +  
    
    labs(x = "Dates", y = "Price (relative to USD)",
         
         title = "Implied Volatility ", subtitle = "",
         
         caption = "Note:\nOwn Calculations") +
    
    guides(col=guide_legend("Currency"))
```

    ## Warning in Name == c("China_IV", "India_IV", "SouthAfrica_IV", "UK_IV", : longer
    ## object length is not a multiple of shorter object length

``` r
IV_plot
```

![](README_files/figure-markdown_github/unnamed-chunk-27-1.png)

This suggests that the market foresees the highest future volatility for
the Rand \[for this sub-sample\].

Now I’ll calculate the returns to analyse the Auto-Persistence in
Returns

-   Calculate Returns

``` r
# Calculating Returns and Cleaning

zar_rtn <- cncy %>% 
    
    arrange(date) %>% 
    
    filter(date > as.Date("1999-12-31")) %>% 
    
    filter(Name %in% "SouthAfrica_Cncy") %>% 
    
    mutate(Price = na.locf(Price)) %>% 
    
    mutate(dlogret = log(Price) - log(lag(Price))) %>% 
    
    mutate(scaledret = (dlogret - mean(dlogret, na.rm = T))) %>% 
    
    filter(date > dplyr::first(date)) 

xts_zar_rtn <- zar_rtn %>% 
    
    tbl_xts(., cols_to_xts = "dlogret", spread_by = "Name")
```

    ## The spread_by column only has one category. 
    ## Hence only the column name was changed...

``` r
MarchTest(xts_zar_rtn)
```

    ## Q(m) of squared series(LM test):  
    ## Test statistic:  1016.07  p-value:  0 
    ## Rank-based Test:  
    ## Test statistic:  421.1523  p-value:  0 
    ## Q_k(m) of squared series:  
    ## Test statistic:  1012.159  p-value:  0 
    ## Robust Test(5%) :  210.8329  p-value:  0

The MARCH test indicates that all the MV portmanteau tests reject the
null of no conditional heteroskedasticity, motivating the use of a GARCH
model

``` r
xts_zar_rtn[is.na(xts_zar_rtn)] <- 0

PlotRtn = cbind(xts_zar_rtn, xts_zar_rtn^2, abs(xts_zar_rtn))

colnames(PlotRtn) = c("Returns", "Returns_Sqd", "Returns_Abs")

PlotRtn <- PlotRtn %>% 
    
    xts_tbl() %>%
    
    gather(ReturnType, Returns, -date) 

# Plot

ggplot(PlotRtn) + 
    
    geom_line(aes(x = date, y = Returns, colour = ReturnType, alpha = 0.5)) + 
    
    ggtitle("Return Type Persistence: USD/ZAR") + 
    
    facet_wrap(~ReturnType, nrow = 3, ncol = 1, scales = "free") + 
    
    guides(alpha = "none", colour = "none") + 
    
    fmxdat::theme_fmx()
```

![](README_files/figure-markdown_github/unnamed-chunk-29-1.png)

From the graph above it is clear that there is persistence in certain
periods of USDZAR returns. Moreover, we have first and second order
persistence as well as clear evidence of long-term memory in the second
order process.

Let’s investigate further…

``` r
forecast::Acf(xts_zar_rtn, main = "ACF: Equally Weighted Return")
```

![](README_files/figure-markdown_github/unnamed-chunk-30-1.png)

``` r
forecast::Acf(xts_zar_rtn, main = "ACF: Equally Weighted Return")
```

![](README_files/figure-markdown_github/unnamed-chunk-31-1.png)

``` r
forecast::Acf(xts_zar_rtn^2, main = "ACF: Squared Equally Weighted Return")
```

![](README_files/figure-markdown_github/unnamed-chunk-32-1.png)

``` r
forecast::Acf(abs(xts_zar_rtn), main = "ACF: Absolute Equally Weighted Return")
```

![](README_files/figure-markdown_github/unnamed-chunk-33-1.png)

The above proves we have very strong conditional heteroskedasticity, as
well as long memory.

A formal test for ARCH effects: LBQ stats on squared returns:

``` r
Box.test(coredata(xts_zar_rtn^2), type = "Ljung-Box", lag = 12)
```

    ## 
    ##  Box-Ljung test
    ## 
    ## data:  coredata(xts_zar_rtn^2)
    ## X-squared = 1078.3, df = 12, p-value < 0.00000000000000022

The test rejects the nulls of no ARCH effects - hence I will need to
control for the remaining conditional heteroskedasticity in the returns
series

## Find best model

``` r
# *Vol_Model_Sel & vol_func* are function I created (see code folder)

best_model <- Vol_Model_Sel(zar_rtn)

best_model
```

    ##                 sGARCH  gjrGARCH    eGARCH    apARCH
    ## Akaike       -6.397628 -6.400773 -6.399399 -6.396697
    ## Bayes        -6.391791 -6.393769 -6.392394 -6.388525
    ## Shibata      -6.397630 -6.400775 -6.399401 -6.396700
    ## Hannan-Quinn -6.395596 -6.398334 -6.396960 -6.393852

It appears that a *gjrGARCH* model will perform best, so I will use it
to model the data.

## Fit Model

``` r
garch_fit_gjrGARCH <- vol_func(zar_rtn, "gjrGARCH")

garch_fit_gjrGARCH
```

    ## 
    ## *---------------------------------*
    ## *          GARCH Model Fit        *
    ## *---------------------------------*
    ## 
    ## Conditional Variance Dynamics    
    ## -----------------------------------
    ## GARCH Model  : gjrGARCH(1,1)
    ## Mean Model   : ARFIMA(1,0,0)
    ## Distribution : norm 
    ## 
    ## Optimal Parameters
    ## ------------------------------------
    ##         Estimate  Std. Error    t value Pr(>|t|)
    ## mu      0.000290    0.000121   2.390615 0.016820
    ## ar1    -0.000390    0.013834  -0.028195 0.977507
    ## omega   0.000001    0.000000   2.740432 0.006136
    ## alpha1  0.079246    0.005432  14.589373 0.000000
    ## beta1   0.928959    0.007480 124.192092 0.000000
    ## gamma1 -0.037962    0.008027  -4.729034 0.000002
    ## 
    ## Robust Standard Errors:
    ##         Estimate  Std. Error  t value Pr(>|t|)
    ## mu      0.000290    0.000148  1.96288 0.049660
    ## ar1    -0.000390    0.014494 -0.02691 0.978531
    ## omega   0.000001    0.000002  0.54809 0.583629
    ## alpha1  0.079246    0.042597  1.86037 0.062833
    ## beta1   0.928959    0.043752 21.23217 0.000000
    ## gamma1 -0.037962    0.013465 -2.81922 0.004814
    ## 
    ## LogLikelihood : 18229 
    ## 
    ## Information Criteria
    ## ------------------------------------
    ##                     
    ## Akaike       -6.4008
    ## Bayes        -6.3938
    ## Shibata      -6.4008
    ## Hannan-Quinn -6.3983
    ## 
    ## Weighted Ljung-Box Test on Standardized Residuals
    ## ------------------------------------
    ##                         statistic p-value
    ## Lag[1]                    0.08601  0.7693
    ## Lag[2*(p+q)+(p+q)-1][2]   0.08836  1.0000
    ## Lag[4*(p+q)+(p+q)-1][5]   0.66589  0.9841
    ## d.o.f=1
    ## H0 : No serial correlation
    ## 
    ## Weighted Ljung-Box Test on Standardized Squared Residuals
    ## ------------------------------------
    ##                         statistic p-value
    ## Lag[1]                     0.4047  0.5247
    ## Lag[2*(p+q)+(p+q)-1][5]    3.5819  0.3110
    ## Lag[4*(p+q)+(p+q)-1][9]    4.6588  0.4812
    ## d.o.f=2
    ## 
    ## Weighted ARCH LM Tests
    ## ------------------------------------
    ##             Statistic Shape Scale P-Value
    ## ARCH Lag[3] 0.0007808 0.500 2.000  0.9777
    ## ARCH Lag[5] 0.0834832 1.440 1.667  0.9898
    ## ARCH Lag[7] 0.9556826 2.315 1.543  0.9207
    ## 
    ## Nyblom stability test
    ## ------------------------------------
    ## Joint Statistic:  470.2135
    ## Individual Statistics:               
    ## mu      0.18242
    ## ar1     0.03082
    ## omega  67.12581
    ## alpha1  0.14913
    ## beta1   0.08644
    ## gamma1  0.25460
    ## 
    ## Asymptotic Critical Values (10% 5% 1%)
    ## Joint Statistic:          1.49 1.68 2.12
    ## Individual Statistic:     0.35 0.47 0.75
    ## 
    ## Sign Bias Test
    ## ------------------------------------
    ##                    t-value      prob sig
    ## Sign Bias           0.2317 0.8167798    
    ## Negative Sign Bias  2.9915 0.0027879 ***
    ## Positive Sign Bias  0.3365 0.7365153    
    ## Joint Effect       18.1832 0.0004032 ***
    ## 
    ## 
    ## Adjusted Pearson Goodness-of-Fit Test:
    ## ------------------------------------
    ##   group statistic p-value(g-1)
    ## 1    20     63.67  0.000001001
    ## 2    30     82.47  0.000000501
    ## 3    40     90.43  0.000005773
    ## 4    50    104.96  0.000005948
    ## 
    ## 
    ## Elapsed time : 0.544688

``` r
pacman::p_load(xtable)

Table <- xtable(garch_fit_gjrGARCH@fit$matcoef, digits=c(0, 4, 4, 4, 4))


print(Table, type="html")
```

<!-- html table generated in R 4.1.3 by xtable 1.8-4 package -->
<!-- Sun Nov 27 22:08:48 2022 -->
<table border="1">
<tr>
<th>
</th>
<th>
Estimate
</th>
<th>
Std. Error
</th>
<th>
t value
</th>
<th>
Pr(\>\|t\|)
</th>
</tr>
<tr>
<td align="right">
mu
</td>
<td align="right">
0.0003
</td>
<td align="right">
0.0001
</td>
<td align="right">
2.3906
</td>
<td align="right">
0.0168
</td>
</tr>
<tr>
<td align="right">
ar1
</td>
<td align="right">
-0.0004
</td>
<td align="right">
0.0138
</td>
<td align="right">
-0.0282
</td>
<td align="right">
0.9775
</td>
</tr>
<tr>
<td align="right">
omega
</td>
<td align="right">
0.0000
</td>
<td align="right">
0.0000
</td>
<td align="right">
2.7404
</td>
<td align="right">
0.0061
</td>
</tr>
<tr>
<td align="right">
alpha1
</td>
<td align="right">
0.0792
</td>
<td align="right">
0.0054
</td>
<td align="right">
14.5894
</td>
<td align="right">
0.0000
</td>
</tr>
<tr>
<td align="right">
beta1
</td>
<td align="right">
0.9290
</td>
<td align="right">
0.0075
</td>
<td align="right">
124.1921
</td>
<td align="right">
0.0000
</td>
</tr>
<tr>
<td align="right">
gamma1
</td>
<td align="right">
-0.0380
</td>
<td align="right">
0.0080
</td>
<td align="right">
-4.7290
</td>
<td align="right">
0.0000
</td>
</tr>
</table>

``` r
#print(Table, type = "latex", comment = FALSE)
```

``` r
#Conditional variance Plot

persistence(garch_fit_gjrGARCH)
```

    ## [1] 0.9892241

``` r
# Persistence is alpha + beta, and it is typically very high and close to 1

# To view the conditional variance plot, use:
sigma <- sigma(garch_fit_gjrGARCH) %>% 
    
    xts_tbl() 

colnames(sigma) <- c("date", "sigma") 

sigma <- sigma %>% 
    
    mutate(date = as.Date(date))

Con_var_plot <- 
  
ggplot() + 
    
  geom_line(data = PlotRtn %>% filter(ReturnType == "Returns_Sqd") %>% 
                
                select(date, Returns) %>% 
              
              unique %>% mutate(Returns = sqrt(Returns)), 
            
            aes(x = date, y = Returns)) + 
  
  geom_line(data = sigma, aes(x = date, y = sigma), 
            color = "red", size = 2, alpha = 0.8) + 
  
  
  labs(title = "Comparison: Returns Sigma vs Sigma from Garch", 
       
       subtitle = "Note the smoothing effect of garch, as noise is controlled for.", x = "", y = "Comparison of estimated volatility",
       
       caption = "Source: Fin metrics class | Calculations: Own") + 
  
    fmxdat::theme_fmx(CustomCaption = TRUE)


fmxdat::finplot(Con_var_plot, y.pct = T, y.pct_acc = 1)
```

![](README_files/figure-markdown_github/unnamed-chunk-39-1.png)

``` r
news_plot <- newsimpact(z = NULL, garch_fit_gjrGARCH)

plot(news_plot$zx, news_plot$zy, ylab = news_plot$yexpr, xlab = news_plot$xexpr, type = "l", 
    main = "News Impact Curve")
```

![](README_files/figure-markdown_github/unnamed-chunk-40-1.png)

``` r
plot(garch_fit_gjrGARCH, which = 1)
```

![](README_files/figure-markdown_github/unnamed-chunk-41-1.png)

Lets investigate further by compare smoothed ZAR Volatility to mean
Global volatility

``` r
# Lets investigate further
# lets compare Smoothed ZAR Vol to mean Global VOL

# Calculate Mean Global VOL

mean_Gvol <- cncyIV %>% 
    
    filter(date > as.Date("1999-12-31")) %>% 
    
    group_by(date) %>% 
    
    summarise(mean_vol = mean(Price)) %>% 
    
    mutate(Global_vol = mean_vol/max(mean_vol))

# Wrangle Sigma 

sigma <- sigma %>% 
    
    mutate(Date = as.Date(date)) %>% 
    
    mutate(ZAR_sigma = sigma/max(sigma))%>%
    
    left_join(., mean_Gvol, by = "date")

# Plot 

Vol_compare_plot <- sigma %>% 
    
    select(-date) %>% 
    
    pivot_longer(c("ZAR_sigma", "Global_vol"), 
                 names_to = "Vol_type", 
                 values_to = "VOL") %>%
    
    ggplot() +
    
    geom_line(aes(Date, VOL, colour = Vol_type)) +
    
    labs(title = "ZAR Volatility and Mean Currency Volatility",
         x = "", 
         y = "Proprotion of Max Vol") +
    
    fmx_cols() +
    
    theme_fmx(title.size = ggpts(25)) 

finplot(Vol_compare_plot)
```

![](README_files/figure-markdown_github/unnamed-chunk-42-1.png)

# Question 6

``` r
# Load Data
pacman::p_load("MTS", "robustbase")
pacman::p_load("tidyverse", "devtools", "rugarch", "rmgarch", 
    "forecast", "tbl2xts", "lubridate", "PerformanceAnalytics", 
    "ggthemes")

msci <- read_rds("data/msci.rds")
bonds <- read_rds("data/bonds_10y.rds")
comms <- read_rds("data/comms.rds")
```

``` r
# Lets test for autocorrelation using the March Test

MarchTest(asset_classes_ret)
```

    ## Q(m) of squared series(LM test):  
    ## Test statistic:  6266.108  p-value:  0 
    ## Rank-based Test:  
    ## Test statistic:  1487.596  p-value:  0 
    ## Q_k(m) of squared series:  
    ## Test statistic:  13012.78  p-value:  0 
    ## Robust Test(5%) :  1770.574  p-value:  0

The MARCH test indicates that all the MV portmanteau tests reject the
null of no conditional heteroskedasticity, motivating our use of MVGARCH
models. Let’s set up the model

``` r
# GARCH specifications

uspec <- ugarchspec(variance.model = list(model = "gjrGARCH", 
    garchOrder = c(1, 1)), mean.model = list(armaOrder = c(1, 
    0), include.mean = TRUE), distribution.model = "sstd")

multi_univ_garch_spec <- multispec(replicate(ncol(asset_classes_ret), uspec))

# DCC specifications

spec.dcc = dccspec(multi_univ_garch_spec, dccOrder = c(1, 1), 
    distribution = "mvnorm", lag.criterion = c("AIC", "HQ", "SC", 
        "FPE")[1], model = c("DCC", "aDCC")[1])  

# Enable clustering for speed:

cl = makePSOCKcluster(10)

# Fit GARCH

multf = multifit(multi_univ_garch_spec, asset_classes_ret, cluster = cl)

# Fit DCC

fit.dcc = dccfit(spec.dcc, data = asset_classes_ret, solver = "solnp", 
    cluster = cl, fit.control = list(eval.se = FALSE), fit = multf)

# Check Model

RcovList <- rcov(fit.dcc)

covmat = matrix(RcovList, nrow(asset_classes_ret), ncol(asset_classes_ret) * ncol(asset_classes_ret), 
    byrow = TRUE)

mc1 = MCHdiag(asset_classes_ret, covmat)
```

    ## Test results:  
    ## Q(m) of et: 
    ## Test and p-value:  67.86301 0.0000000001144478 
    ## Rank-based test: 
    ## Test and p-value:  45.01247 0.000002163493 
    ## Qk(m) of epsilon_t: 
    ## Test and p-value:  251.1729 0.000005494596 
    ## Robust Qk(m):  
    ## Test and p-value:  149.6698 0.7098375

``` r
# Wrangle output
dcc.time.var.cor <- rcor(fit.dcc)
print(dcc.time.var.cor[, , 1:3])
```

    ## , , 2010-01-01
    ## 
    ##           MSCI_ACWI   US_10Yr   MSCI_RE Oil_Brent
    ## MSCI_ACWI 1.0000000 0.3672945 0.6989608 0.3916873
    ## US_10Yr   0.3672945 1.0000000 0.1060070 0.2437526
    ## MSCI_RE   0.6989608 0.1060070 1.0000000 0.2554721
    ## Oil_Brent 0.3916873 0.2437526 0.2554721 1.0000000
    ## 
    ## , , 2010-01-04
    ## 
    ##           MSCI_ACWI   US_10Yr   MSCI_RE Oil_Brent
    ## MSCI_ACWI 1.0000000 0.3669617 0.6988083 0.3913564
    ## US_10Yr   0.3669617 1.0000000 0.1055386 0.2433507
    ## MSCI_RE   0.6988083 0.1055386 1.0000000 0.2550748
    ## Oil_Brent 0.3913564 0.2433507 0.2550748 1.0000000
    ## 
    ## , , 2010-01-05
    ## 
    ##           MSCI_ACWI   US_10Yr   MSCI_RE Oil_Brent
    ## MSCI_ACWI 1.0000000 0.3340547 0.6777565 0.4364434
    ## US_10Yr   0.3340547 1.0000000 0.1027695 0.2291449
    ## MSCI_RE   0.6777565 0.1027695 1.0000000 0.2623880
    ## Oil_Brent 0.4364434 0.2291449 0.2623880 1.0000000

``` r
dcc.time.var.cor <- aperm(dcc.time.var.cor, c(3, 2, 1))
dim(dcc.time.var.cor) <- c(nrow(dcc.time.var.cor), ncol(dcc.time.var.cor)^2)
```

``` r
# Rename Output

dcc.time.var.cor <- renamingdcc(ReturnSeries = asset_classes_ret, DCC.TV.Cor = dcc.time.var.cor)
```

    ## Warning: `tbl_df()` was deprecated in dplyr 1.0.0.
    ## i Please use `tibble::as_tibble()` instead.

``` r
# Now we can plot ;)

# Equities

DCC_eq_plot <- ggplot(dcc.time.var.cor %>% 
                          
                          filter(grepl("MSCI_ACWI_", Pairs), 
                                 !grepl("_MSCI_ACWI", Pairs))) + 
    
    geom_line(aes(x = date, y = Rho, colour = Pairs)) + 
    
    theme_hc() + labs(subtitle = "Dynamic Conditional Correlations: MSCI_ACWI", x = "", y = "") +
    
    fmx_cols() + 
    
    theme_fmx(subtitle.size = ggpts(25), legend.size = ggpts(15))


# US Bonds
DCC_bond_plot <- ggplot(dcc.time.var.cor %>% 
                            
                            filter(grepl("US_10Yr_", Pairs), 
                                   !grepl("_US_10Yr", Pairs))) + 
    
    geom_line(aes(x = date, y = Rho, colour = Pairs)) + theme_hc() + 
    
    labs(subtitle="Dynamic Conditional Correlations: US_10Yr", x = "", y = "") +
    
    fmx_cols() + 
    
    theme_fmx(subtitle.size = ggpts(25), legend.size = ggpts(15))


# Real Estate

DCC_RE_plot <- ggplot(dcc.time.var.cor %>% 
                          
                          filter(grepl("MSCI_RE_", Pairs), 
                                 !grepl("_MSCI_RE", Pairs))) + 
    
    geom_line(aes(x = date, y = Rho, colour = Pairs)) + theme_hc() + 
    
    labs(subtitle = "Dynamic Conditional Correlations: MSCI_RE", x = "", y = "") +
    
    fmx_cols() + 
    
    theme_fmx(subtitle.size = ggpts(25), legend.size = ggpts(15))



# Oil

DCC_oil_plot <- ggplot(dcc.time.var.cor %>% 
                           
                           filter(grepl("Oil_Brent_", Pairs), 
                                  !grepl("_Oil_Brent", Pairs))) +
    
    geom_line(aes(x = date, y = Rho, colour = Pairs)) + 
    
    theme_hc() + 
    
    labs(subtitle = "Dynamic Conditional Correlations: Oil_Brent", x = "", y = "") +
    
    fmx_cols() + 
    
    theme_fmx(subtitle.size = ggpts(25), legend.size = ggpts(15))
```

``` r
plot_grid(DCC_eq_plot, DCC_bond_plot, DCC_RE_plot , DCC_oil_plot, labels = c('', '', '',''))
```

![](README_files/figure-markdown_github/unnamed-chunk-49-1.png)

-   Go Garch

``` r
# Go-GARCH following the Tutorial

# GARCH Specifications
spec.go <- gogarchspec(multi_univ_garch_spec, 
                       distribution.model = 'mvnorm', 
                       ica = 'fastica') 

# Speed:

cl <- makePSOCKcluster(10)

# Fit GARCH

multf <- multifit(multi_univ_garch_spec, asset_classes_ret, cluster = cl)

#GO-GARCH Specifications

fit.gogarch <- gogarchfit(spec.go, 
                      data = asset_classes_ret, 
                      solver = 'hybrid', 
                      cluster = cl, 
                      gfun = 'tanh', 
                      maxiter1 = 40000, 
                      epsilon = 1e-08, 
                      rseed = 100)

# Go-Garch Fit

print(fit.gogarch)
```

    ## 
    ## *------------------------------*
    ## *        GO-GARCH Fit          *
    ## *------------------------------*
    ## 
    ## Mean Model       : CONSTANT
    ## GARCH Model      : sGARCH
    ## Distribution : mvnorm
    ## ICA Method       : fastica
    ## No. Factors      : 4
    ## No. Periods      : 3086
    ## Log-Likelihood   : 38671.96
    ## ------------------------------------
    ## 
    ## U (rotation matrix) : 
    ## 
    ##        [,1]   [,2]    [,3]    [,4]
    ## [1,]  0.859 -0.359 -0.2106 -0.2992
    ## [2,]  0.398  0.277 -0.0869  0.8704
    ## [3,] -0.187 -0.882  0.1952  0.3854
    ## [4,]  0.264  0.127  0.9539 -0.0656
    ## 
    ## A (mixing matrix) : 
    ## 
    ##          [,1]    [,2]      [,3]      [,4]
    ## [1,] -0.00140 0.00865  0.002277  0.000678
    ## [2,] -0.02864 0.00726  0.006998  0.002690
    ## [3,] -0.00139 0.00877 -0.003286 -0.000216
    ## [4,] -0.00201 0.00728  0.000874  0.020820

``` r
# Wrangle Output

gog.time.var.cor <- rcor(fit.gogarch)

gog.time.var.cor <- aperm(gog.time.var.cor,c(3,2,1))

dim(gog.time.var.cor) <- c(nrow(gog.time.var.cor), ncol(gog.time.var.cor)^2)

# Rename Output

gog.time.var.cor <- renamingdcc(ReturnSeries = asset_classes_ret, DCC.TV.Cor = gog.time.var.cor)


# Plots

# Equities

GO_eq_plot <- ggplot(gog.time.var.cor %>% 
                   
                   filter(grepl("MSCI_ACWI_", Pairs), 
                          !grepl("_MSCI_ACWI", Pairs))) + 
    
    geom_line(aes(x = date, y = Rho, colour = Pairs)) +
    
    theme_hc() + 
    
    labs(subtitle = "Go-Garch: MSCI_ACWI", x = "", y = "") +
    
    fmx_cols() + 
    
    theme_fmx(subtitle.size = ggpts(25), legend.size = ggpts(15))

# Bonds

GO_bond_plot <- ggplot(gog.time.var.cor %>% 
                   
                   filter(grepl("US_10Yr_", Pairs), !grepl("_US_10Yr", Pairs))) +
    
    geom_line(aes(x = date, y = Rho, colour = Pairs)) + 
    
    theme_hc() + 
    
    labs(subtitle="Go-Garch: US_10Yr", x = "", y = "") +
    
    fmx_cols() + 
    
    theme_fmx(subtitle.size = ggpts(25), legend.size = ggpts(15))

# Real Estate

GO_RE_plot <- ggplot(gog.time.var.cor %>%
                   
                   filter(grepl("MSCI_RE_", Pairs), !grepl("_MSCI_RE", Pairs))) +
    
    geom_line(aes(x = date, y = Rho, colour = Pairs)) + 
    
    theme_hc() +
    
    labs(subtitle = "Go-Garch: MSCI_RE", x = "", y = "") +
    
    fmx_cols() +
    
    theme_fmx(subtitle.size = ggpts(25), legend.size = ggpts(15))


# Commodities

GO_oil_plot <- ggplot(gog.time.var.cor %>%
                   
                   filter(grepl("Oil_Brent_", Pairs), !grepl("_Oil_Brent", Pairs))) + 
    
    geom_line(aes(x = date, y = Rho, colour = Pairs)) + 
    
    theme_hc() + 
    
    labs(subtitle = "Go-GARCH: Oil_Brent", x = "", y = "") +
    
    fmx_cols() + 
    
    theme_fmx(subtitle.size = ggpts(25), legend.size = ggpts(15))
```

``` r
plot_grid(GO_eq_plot, GO_bond_plot, GO_RE_plot , GO_oil_plot, labels = c('', '', '',''))
```

![](README_files/figure-markdown_github/unnamed-chunk-52-1.png)

``` r
library(factoextra)
library(FactoMineR)
pacman::p_load("psych")

eq_pca <- msci %>% 
    
    spread(Name, Price) %>% 
    
    select(date, MSCI_ACWI)

bond_pca <- bonds %>% 
    
    spread(Name, Bond_10Yr) %>% 
    
    select(date, US_10Yr)

Re_pca <- msci %>% 
    
    spread(Name, Price) %>% 
    
    select(date, MSCI_RE)
    
oil_pca <- comms %>%  
    
    spread(Name, Price) %>% 
    
    select(date, Oil_Brent)

# Combine

asset_class_pca <- left_join(eq_pca, bond_pca, by = c("date")) %>% 
    
    left_join(., Re_pca, by = c("date")) %>% 
    
    left_join(., oil_pca, by = c("date")) %>% 
    
    gather(Tickers, Price, -date) %>%
    
    group_by(Tickers) %>%
    
    filter(date > as.Date("2009-12-31")) %>% 
    
    arrange(date) %>%  
    
    group_by(Tickers) %>%  
    
    mutate(dlogret = log(Price) - log(lag(Price))) %>% 
    
    mutate(scaledret = (dlogret -  mean(dlogret, na.rm = T))) %>% 
    
    filter(date > dplyr::first(date)) %>% 
    
    ungroup()


asset_classes_pca <- asset_class_pca %>%  
    
    select(date, Tickers, dlogret) %>% 
    
    spread(Tickers, dlogret) %>% 
    
    select(-date)

AC_PCA_plot <- prcomp(asset_classes_pca, center = TRUE, scale. = TRUE)

AC_PCA_plot$rotation
```

    ##                  PC1        PC2         PC3         PC4
    ## MSCI_ACWI -0.6080549  0.2681471  0.01713371 -0.74704269
    ## MSCI_RE   -0.5540251  0.5226063  0.09680532  0.64075547
    ## Oil_Brent -0.3932987 -0.6563281  0.63617819  0.09913059
    ## US_10Yr   -0.4106600 -0.4735115 -0.76525321  0.14674050

``` r
pairs.panels(asset_classes_pca)
```

![](README_files/figure-markdown_github/unnamed-chunk-53-1.png)

``` r
gviolion <- asset_classes_pca %>% 
    
    gather(Type, val) %>% 
    
    ggplot() + 
    
    geom_violin(aes(Type, val, fill = Type), alpha = 0.7) +
    
    fmxdat::theme_fmx() + 
    
    fmxdat::fmx_fills()

fmxdat::finplot(gviolion, y.pct = T, y.pct_acc = 1, x.vert = T)
```

![](README_files/figure-markdown_github/unnamed-chunk-54-1.png)

# Question 7

## \# Load in Data

``` r
MAA <- read_rds("data/MAA.rds")

msci <- read_rds("data/msci.rds") %>%
    
    filter(Name %in% c("MSCI_ACWI", "MSCI_USA", "MSCI_RE", "MSCI_Jap"))
```

``` r
library(rmsfuns)
pacman::p_load("tidyr", "tbl2xts","devtools","lubridate", "readr", "PerformanceAnalytics", "ggplot2", "dplyr")

# quarter

quarter_dates <- dateconverter(as.Date("2018-01-01"), as.Date("2021-10-29"), 
    "weekdayEOQ") 

# Cal returns for MAA

MAA <- MAA %>% 
    
    arrange(date) %>% 
    
    rename(Tickers = Name) %>% 
    
    filter(date %in% quarter_dates) %>% 
    
    group_by(Tickers) %>% 
    
    mutate(Return = Price / lag(Price)-1) %>% 
    
    ungroup() %>% 
    
    select(date, Tickers, Return) %>% 
    
    filter(!is.na(Return)) %>% 
    
    mutate(YearMonth = format(date, "%Y%B"))

# Cal returns for MAA

msci_q7 <- msci %>% 
    
    arrange(date) %>% 
    
    rename(Tickers = Name) %>%
    
    filter(date %in% quarter_dates) %>%
    
    group_by(Tickers) %>% 
    
    mutate(Return = Price / lag(Price)-1) %>% 
    
    ungroup() %>% 
    
    select(date, Tickers, Return) %>% 
    
    filter(!is.na(Return)) %>% 
    
    mutate(YearMonth = format(date, "%Y%B"))

# Combine

combine_assets_tut <- rbind(MAA, msci_q7) %>% 
    
    arrange(date)

# Consider only indexes with data from before 20080101, and use this as a common start date too...:
# Can you argue why?

#combine_assets_3yrtut <- combine_assets_tut %>% 
    
#    group_by(Tickers) %>% 
    
#    filter(date == first(date)) %>% 
    
#    ungroup() %>% 
    
#    filter(date < ymd(20180101)) %>% 
    
#    pull(Tickers) %>% 
    
#    unique

#combine_assets_tut <- combine_assets_tut %>% 
  
#  filter(Tickers %in% combine_assets_3yrtut) %>% 
  
#  filter(date > ymd(20180101))
```

``` r
# Impute missing values for return series
return_mat_q7 <- combine_assets_tut %>%
    
    select(date, Tickers, Return) %>% 
    
    spread(Tickers, Return)

options(scipen = 999)

return_mat_q7 <- impute_missing_returns(return_mat_q7,
                           impute_returns_method = "Drawn_Distribution_Collective", Seed =
                               as.numeric(format( Sys.time(), "%Y%d%H%M")))

# Create returns matrix

return_mat__q7_Nodate <- data.matrix(return_mat_q7[, -1])
```

``` r
# Create constraints

NStox <- ncol(return_mat__q7_Nodate)
LB = 0.01
UB = 0.4
Eq = 0.6 # Equity exposure
Bonds = 0.25 # Credit and Bonds exposure
meq = 1

# A Mat 
Eq_mat <- rbind(matrix(0, nrow = 9, ncol = 4),
                -diag(4))

C_B_mat <- rbind(matrix(0, 3, 6), 
                 -diag(6),
                 matrix(0, 4, 6))

bvec <- c(1, rep(LB, NStox), -rep(UB, NStox), -rep(Eq, 4), -rep(Bonds, 6))

Amat <- cbind(1, diag(NStox), -diag(NStox), Eq_mat, C_B_mat)
```

``` r
# Calculate optimal rolling weights for each type of portfolio optimization

EOM_datevec <- combine_assets_tut %>% 
    
    #filter(Tickers %in% comb_assets_3_years) %>% 
    
    #filter(date >= Start_Date[[1]]) %>% 
    
    select(date) %>% 
    
    unique %>% 
    
    mutate(YM = format(date, "%Y%B")) %>% 
    
    group_by(YM) %>% 
    
    filter(date == dplyr::last(date)) %>% 
    
    ungroup() %>% 
    
    pull(date) %>% 
    
    unique

Opt_roll_wgt <- EOM_datevec %>% 
    
    map_df(~Roll_optimizer(return_mat_q7, EOM_datevec = ., Amat = Amat, bvec = bvec, LookBack = 12))

head(Opt_roll_wgt, 10)
```

    ## # A tibble: 10 x 7
    ##    stocks                          mv minvol maxdecor sharpe date       Look_B~1
    ##    <chr>                        <dbl>  <dbl>    <dbl>  <dbl> <date>        <dbl>
    ##  1 Asia_dollar_Idx             0.0100 0.375    0.375  0.0769 2018-06-29       12
    ##  2 Bbg_EUCorpCred_Unhedged_USD 0.0100 0.0100   0.0100 0.0769 2018-06-29       12
    ##  3 Bbg_EuroBonds_UnhedgedEUR   0.0100 0.0100   0.0100 0.0769 2018-06-29       12
    ##  4 Bbg_GlBonds_HedgedUSD       0.0100 0.0395   0.0395 0.0769 2018-06-29       12
    ##  5 Bbg_GlCorpCred_Hedged_USD   0.0100 0.0100   0.0100 0.0769 2018-06-29       12
    ##  6 Bbg_USBonds_UnhedgedUSD     0.0100 0.246    0.246  0.0769 2018-06-29       12
    ##  7 Bbg_USCorpCred_Unhedged_USD 0.0100 0.0100   0.0100 0.0769 2018-06-29       12
    ##  8 Commod_Idx                  0.0100 0.01     0.01   0.0769 2018-06-29       12
    ##  9 Dollar_Idx                  0.0100 0.25     0.25   0.0769 2018-06-29       12
    ## 10 MSCI_ACWI                   0.400  0.0100   0.0100 0.0769 2018-06-29       12
    ## # ... with abbreviated variable name 1: Look_Back_Period
