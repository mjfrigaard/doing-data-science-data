doing\_data\_science\_data
================

Cleaning Multiple Data Sets Efficiently
=======================================

Data sets from Doing Data Science by Cathy O'Neil and Rachel Schutt (O'Reilly Media)  

This little tutorial solves a problem I was having when working through the exploratory data analysis exercises in [Doing Data Science](https://www.amazon.com/Doing-Data-Science-Straight-Frontline/dp/1449358659) by [Cathy O'Neil](https://mathbabe.org/) and [Rachel Schutt](https://industry.datascience.columbia.edu/profile/rachel-schutt). I highly recommended picking up a copy for yourself.

On pages 37-38, we are instructed to do the following (paraphrasing here):

This [folder](https://github.com/oreillymedia/doing_data_science) contains 31 simulated days of ads shown and clicks recorded on the New York Times home page. Rows represent users, and the variables are: `Age`, `Gender` (0 = female, 1 = male), `Impressions` (number impressions), `Clicks` (number clicks), and a binary indicator for signed in or not `Signed_in`. We need to create two new variables: `age_group`, which contains six levels of `Age` ("&lt;18", "18-24", "25-34", "35-44", "45-54", "55-64", and "65+"), and `CTR` or clickthrough-rate, caculated as the number of clicks / the number of impressions.

The EDA exercises in the book are intended for ***a single day***, but what if I wanted to look at an entire months worth of data? Luckily I remembered learning a handy workflow for processing multiple data files in the third ggplot2 course from [Datacamp](https://www.datacamp.com/courses/data-visualization-with-ggplot2-part-3) by [Rick Scavetta](https://twitter.com/rick_scavetta). If you have a situation where all your data files need similar wrangling/preparation before visulizing or modeling, you will find this helpful (I hope!).

### Data files

I've moved the data from the Github [repository](https://github.com/oreillymedia/doing_data_science) to a local data folder (`./data/`).

### Read data files

I will start by reading the first data set into RStudio using `readr::read_csv()` and then use `dplyr::glimpse()` to see what these data look like.

``` r
nyt1 <- readr::read_csv(file = "./data/nyt1.csv",
                col_names = TRUE)
```

    ## Parsed with column specification:
    ## cols(
    ##   Age = col_integer(),
    ##   Gender = col_integer(),
    ##   Impressions = col_integer(),
    ##   Clicks = col_integer(),
    ##   Signed_In = col_integer()
    ## )

``` r
nyt1 %>% dplyr::glimpse()
```

    ## Observations: 458,441
    ## Variables: 5
    ## $ Age         <int> 36, 73, 30, 49, 47, 47, 0, 46, 16, 52, 0, 21, 0, 5...
    ## $ Gender      <int> 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,...
    ## $ Impressions <int> 3, 3, 3, 3, 11, 11, 7, 5, 3, 4, 8, 3, 4, 6, 5, 6, ...
    ## $ Clicks      <int> 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,...
    ## $ Signed_In   <int> 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1, 0, 1,...

I see all 5 variables in this data frame. I will create a pipeline that creates the two variables from the exercises and a third `Female` variable that makes the categories in `Gender` less ambiguous. When I am done, I look at the data with `dplyr::glimpse()` again.

``` r
nyt1 <- nyt1 %>% 
    dplyr::mutate(
        age_group = case_when( # create age_group variable
            Age < 18 ~ "<18",
            Age >= 18 & Age < 25 ~ "18-24",
            Age >= 25 & Age < 35 ~ "25-34",
            Age >= 35 & Age < 45 ~ "35-44",
            Age >= 45 & Age < 55 ~ "45-54",
            Age >= 55 & Age < 65 ~ "55-64",
            Age >= 65 ~ "65+"), 
        CTR = Clicks/Impressions, # create CTR variable
        Female = case_when( # create new Female variable
            Gender == 0 ~ "Male", 
            Gender == 1 ~ "Female",
            TRUE ~ as.character(Gender)))
nyt1 %>% dplyr::glimpse()
```

    ## Observations: 458,441
    ## Variables: 8
    ## $ Age         <int> 36, 73, 30, 49, 47, 47, 0, 46, 16, 52, 0, 21, 0, 5...
    ## $ Gender      <int> 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,...
    ## $ Impressions <int> 3, 3, 3, 3, 11, 11, 7, 5, 3, 4, 8, 3, 4, 6, 5, 6, ...
    ## $ Clicks      <int> 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,...
    ## $ Signed_In   <int> 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1, 0, 1,...
    ## $ age_group   <chr> "35-44", "65+", "25-34", "45-54", "45-54", "45-54"...
    ## $ CTR         <dbl> 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.090...
    ## $ Female      <chr> "Male", "Female", "Male", "Female", "Female", "Mal...

Now I want to bundle the data reading and preparing commands as a function, `clean_nyt`.

``` r
clean_nyt <- function(file) {
                    nyt <- read_csv(file)
                    nyt %>% 
                    dplyr::mutate(
                        age_group = case_when( # create age_group variable
                                        Age < 18 ~ "<18",
                            Age >= 18 & Age < 25 ~ "18-24",
                            Age >= 25 & Age < 35 ~ "25-34",
                            Age >= 35 & Age < 45 ~ "35-44",
                            Age >= 45 & Age < 55 ~ "45-54",
                            Age >= 55 & Age < 65 ~ "55-64",
                            Age >= 65 ~ "65+"), 
                        CTR = Clicks/Impressions, # create CTR variable
                        Female = case_when( # create new Female variable
                                Gender == 0 ~ "Male", 
                                Gender == 1 ~ "Female",
                    TRUE ~ as.character(Gender)))
}
```

I will test `clean_nyt()` on a `nyt2.csv`

``` r
nyt2 <- clean_nyt("./data/nyt2.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   Age = col_integer(),
    ##   Gender = col_integer(),
    ##   Impressions = col_integer(),
    ##   Clicks = col_integer(),
    ##   Signed_In = col_integer()
    ## )

``` r
nyt2 %>% glimpse()
```

    ## Observations: 449,935
    ## Variables: 8
    ## $ Age         <int> 48, 0, 15, 0, 0, 0, 63, 0, 24, 16, 31, 0, 56, 52, ...
    ## $ Gender      <int> 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 1, 1, 0, 1,...
    ## $ Impressions <int> 3, 9, 4, 5, 7, 11, 3, 4, 2, 7, 5, 3, 5, 6, 2, 5, 4...
    ## $ Clicks      <int> 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,...
    ## $ Signed_In   <int> 1, 0, 1, 0, 0, 0, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1,...
    ## $ age_group   <chr> "45-54", "<18", "<18", "<18", "<18", "<18", "55-64...
    ## $ CTR         <dbl> 0.0000, 0.1111, 0.0000, 0.0000, 0.1429, 0.0000, 0....
    ## $ Female      <chr> "Female", "Male", "Female", "Male", "Male", "Male"...

It looks like `clean_nyt()` is working!

Now I need to create a vector with the files in `dir("./data")`. I will call this `nyt_files`. Then I will `paste0()` the file path to the files and store this in the `my_nyt_files` vector.

``` r
nyt_files <- dir("./data")
my_nyt_files <- paste0("./data/",nyt_files)
my_nyt_files %>% head()
```

    ## [1] "./data/nyt1.csv"  "./data/nyt10.csv" "./data/nyt11.csv"
    ## [4] "./data/nyt12.csv" "./data/nyt13.csv" "./data/nyt14.csv"

Great. Now I can create a `for` loop to pass the files through and build a master data frame, `my_nyt_data`.

``` r
# Build my_nyt_data with a for loop
my_nyt_data <- NULL
for (file in my_nyt_files) { # for every file...
    temp <- clean_nyt(file)  # clean it with clean_nyt()
    temp$id <- sub(".csv", "", file) # add an id column (but remove .csv)
    my_nyt_data <- rbind(my_nyt_data, temp) # then stick together by rows
}
my_nyt_data %>% glimpse()
```

    ## Observations: 14,905,865
    ## Variables: 9
    ## $ Age         <int> 36, 73, 30, 49, 47, 47, 0, 46, 16, 52, 0, 21, 0, 5...
    ## $ Gender      <int> 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,...
    ## $ Impressions <int> 3, 3, 3, 3, 11, 11, 7, 5, 3, 4, 8, 3, 4, 6, 5, 6, ...
    ## $ Clicks      <int> 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,...
    ## $ Signed_In   <int> 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1, 0, 1,...
    ## $ age_group   <chr> "35-44", "65+", "25-34", "45-54", "45-54", "45-54"...
    ## $ CTR         <dbl> 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.090...
    ## $ Female      <chr> "Male", "Female", "Male", "Female", "Female", "Mal...
    ## $ id          <chr> "./data/nyt1", "./data/nyt1", "./data/nyt1", "./da...

That is a big data set--14,905,865 observations and 9 variables.

### Clean up `id`

Now we just need to clean up the `id` variable a little with the `stringr::str_replace()` function and verify all 31 data sets are accounted for using `dplyr::distinct()` and `base::nrow()`.

``` r
my_nyt_data <- my_nyt_data %>% 
    dplyr::mutate(id = 
                      stringr::str_replace(id, 
                                pattern = "./data/" , 
                                replacement = "")) 
my_nyt_data %>% 
    dplyr::distinct(id) %>% 
    base::nrow()
```

    ## [1] 31

### Check `age_group`

Ok we should check our new variables, `age_group` and `Female`. Let's start with `age_group` using a combination of `dplyr::count()` and `tidyr::spread()`.

``` r
my_nyt_data %>% dplyr::count(age_group, Age) %>% 
    tidyr::spread(age_group, n) %>% head()
```

    ## # A tibble: 6 x 8
    ##     Age   `<18` `18-24` `25-34` `35-44` `45-54` `55-64` `65+`
    ##   <int>   <int>   <int>   <int>   <int>   <int>   <int> <int>
    ## 1     0 5613610      NA      NA      NA      NA      NA    NA
    ## 2     3       2      NA      NA      NA      NA      NA    NA
    ## 3     4       2      NA      NA      NA      NA      NA    NA
    ## 4     5      10      NA      NA      NA      NA      NA    NA
    ## 5     6      41      NA      NA      NA      NA      NA    NA
    ## 6     7     167      NA      NA      NA      NA      NA    NA

Yikes! There are 5613610 respondents with `Age` of 0. Let's remove these using `dplyr::filter()` and re-check those zeros.

``` r
my_nyt_data <- my_nyt_data %>%
    filter(Age != 0)
my_nyt_data %>% count(age_group, Age) %>% 
    spread(age_group, n) %>% head()
```

    ## # A tibble: 6 x 8
    ##     Age `<18` `18-24` `25-34` `35-44` `45-54` `55-64` `65+`
    ##   <int> <int>   <int>   <int>   <int>   <int>   <int> <int>
    ## 1     3     2      NA      NA      NA      NA      NA    NA
    ## 2     4     2      NA      NA      NA      NA      NA    NA
    ## 3     5    10      NA      NA      NA      NA      NA    NA
    ## 4     6    41      NA      NA      NA      NA      NA    NA
    ## 5     7   167      NA      NA      NA      NA      NA    NA
    ## 6     8   519      NA      NA      NA      NA      NA    NA

I should also check the top of the `Age` distribution with `base::tail()`.

``` r
my_nyt_data %>% count(age_group, Age) %>% 
    spread(age_group, n) %>% tail()
```

    ## # A tibble: 6 x 8
    ##     Age `<18` `18-24` `25-34` `35-44` `45-54` `55-64` `65+`
    ##   <int> <int>   <int>   <int>   <int>   <int>   <int> <int>
    ## 1   108    NA      NA      NA      NA      NA      NA     7
    ## 2   109    NA      NA      NA      NA      NA      NA     1
    ## 3   111    NA      NA      NA      NA      NA      NA     3
    ## 4   112    NA      NA      NA      NA      NA      NA     1
    ## 5   113    NA      NA      NA      NA      NA      NA     1
    ## 6   115    NA      NA      NA      NA      NA      NA     1

115 is old...but *possible*. Ok I also want to add the `dplyr::filter(Age != 0)` to the `clean_nyt()` function.

``` r
# update function
clean_nyt <- function(file) {
                    nyt <- readr::read_csv(file)
                    nyt %>% 
                    dplyr::filter(Age != 0) %>% 
                    dplyr::mutate(
                        age_group = case_when( # create age_group variable
                                        Age < 18 ~ "<18",
                            Age >= 18 & Age < 25 ~ "18-24",
                            Age >= 25 & Age < 35 ~ "25-34",
                            Age >= 35 & Age < 45 ~ "35-44",
                            Age >= 45 & Age < 55 ~ "45-54",
                            Age >= 55 & Age < 65 ~ "55-64",
                            Age >= 65 ~ "65+"), 
                        CTR = Clicks/Impressions, # create CTR variable
                        Female = case_when( # create new Female variable
                                Gender == 0 ~ "Male", 
                                Gender == 1 ~ "Female",
                    TRUE ~ as.character(Gender)))
}
```

Let's re-run the `for` loop and make sure we have nice, clean data in `my_nyt_data`.

``` r
# Build my_nyt_data with a for loop
my_nyt_data <- NULL
for (file in my_nyt_files) { # for every file...
    temp <- clean_nyt(file)  # clean it with clean_nyt()
    temp$id <- sub(".csv", "", file) # add an id column (but remove .csv)
    my_nyt_data <- rbind(my_nyt_data, temp) # then stick together by rows
}
my_nyt_data %>% glimpse()
```

    ## Observations: 9,292,255
    ## Variables: 9
    ## $ Age         <int> 36, 73, 30, 49, 47, 47, 46, 16, 52, 21, 57, 31, 40...
    ## $ Gender      <int> 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0,...
    ## $ Impressions <int> 3, 3, 3, 3, 11, 11, 5, 3, 4, 3, 6, 5, 3, 5, 4, 4, ...
    ## $ Clicks      <int> 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,...
    ## $ Signed_In   <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,...
    ## $ age_group   <chr> "35-44", "65+", "25-34", "45-54", "45-54", "45-54"...
    ## $ CTR         <dbl> 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.090...
    ## $ Female      <chr> "Male", "Female", "Male", "Female", "Female", "Mal...
    ## $ id          <chr> "./data/nyt1", "./data/nyt1", "./data/nyt1", "./da...

I see a new sample size of 9,292,255--this is promising! I'll re-check the `age_group` variable.

``` r
my_nyt_data %>% count(age_group, Age) %>% 
    spread(age_group, n)
```

    ## # A tibble: 111 x 8
    ##      Age `<18` `18-24` `25-34` `35-44` `45-54` `55-64` `65+`
    ##  * <int> <int>   <int>   <int>   <int>   <int>   <int> <int>
    ##  1     3     2      NA      NA      NA      NA      NA    NA
    ##  2     4     2      NA      NA      NA      NA      NA    NA
    ##  3     5    10      NA      NA      NA      NA      NA    NA
    ##  4     6    41      NA      NA      NA      NA      NA    NA
    ##  5     7   167      NA      NA      NA      NA      NA    NA
    ##  6     8   519      NA      NA      NA      NA      NA    NA
    ##  7     9  1384      NA      NA      NA      NA      NA    NA
    ##  8    10  3592      NA      NA      NA      NA      NA    NA
    ##  9    11  8187      NA      NA      NA      NA      NA    NA
    ## 10    12 17054      NA      NA      NA      NA      NA    NA
    ## # ... with 101 more rows

That looks much better.

### Check `Female`

Now check the new `Female` variable.

``` r
my_nyt_data %>% count(Gender, Female) %>% 
    spread(Female, n)
```

    ## # A tibble: 2 x 3
    ##   Gender  Female    Male
    ## *  <int>   <int>   <int>
    ## 1      0      NA 4476582
    ## 2      1 4815673      NA

Ok--these categories are all present and accounted for. Let's get a smaller sample of this data set to work with. I think 10% is enough. I'll grab this with `dplyr::sample_frac()`, store it as `nyt_data`, and view the contents with `dplyr::glimpse()`

``` r
nyt_data <- dplyr::sample_frac(my_nyt_data, size = 0.1)
nyt_data %>% dplyr::glimpse()
```

    ## Observations: 929,226
    ## Variables: 9
    ## $ Age         <int> 36, 33, 37, 42, 54, 37, 77, 30, 73, 37, 58, 46, 43...
    ## $ Gender      <int> 0, 1, 0, 1, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 1, 1, 1,...
    ## $ Impressions <int> 4, 4, 3, 1, 4, 1, 9, 6, 2, 5, 9, 9, 5, 5, 3, 2, 5,...
    ## $ Clicks      <int> 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,...
    ## $ Signed_In   <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,...
    ## $ age_group   <chr> "35-44", "25-34", "35-44", "35-44", "45-54", "35-4...
    ## $ CTR         <dbl> 0.2500, 0.0000, 0.3333, 0.0000, 0.0000, 0.0000, 0....
    ## $ Female      <chr> "Male", "Female", "Male", "Female", "Male", "Male"...
    ## $ id          <chr> "./data/nyt23", "./data/nyt9", "./data/nyt1", "./d...

Now to get a quick look at the distribution of `CTR` by `age_group` and `Female` in this sample.

``` r
nyt_data %>% filter(CTR != 0.0000) %>% 
    ggplot(aes(x = CTR, color = Female)) + 
                    geom_freqpoly(bins = 30) + 
                    facet_wrap(~ age_group, ncol = 3)
```

![](doing_data_science_data_files/figure-markdown_github/nyt_data_freqpoly_CTR-1.png)

``` r
ggsave("nyt_data_freqpoly_CTR.png", width = 8, height = 5, unit = "in", dpi = 480)
```

There you have it! 31 clean data sets and a visualization in under 250 lines!!!!

------------------------------------------------------------------------

File creation
-------------

How was this file created?

-   File creation date: 2018-01-18
-   R version 3.4.3 (2017-11-30)
-   R version (short form): 3.4.3
-   `tidyverse` package version: 1.2.1
-   Additional session information

<!-- -->

    ## Session info -------------------------------------------------------------

    ##  setting  value                       
    ##  version  R version 3.4.3 (2017-11-30)
    ##  system   x86_64, darwin15.6.0        
    ##  ui       X11                         
    ##  language (EN)                        
    ##  collate  en_US.UTF-8                 
    ##  tz       America/Los_Angeles         
    ##  date     2018-01-18

    ## Packages -----------------------------------------------------------------

    ##  package    * version    date       source                            
    ##  assertthat   0.2.0      2017-04-11 CRAN (R 3.4.0)                    
    ##  backports    1.1.2      2017-12-13 CRAN (R 3.4.3)                    
    ##  base       * 3.4.3      2017-12-07 local                             
    ##  bindr        0.1        2016-11-13 CRAN (R 3.4.0)                    
    ##  bindrcpp   * 0.2        2017-06-17 CRAN (R 3.4.0)                    
    ##  broom        0.4.3      2017-11-20 CRAN (R 3.4.2)                    
    ##  cellranger   1.1.0      2016-07-27 CRAN (R 3.4.0)                    
    ##  cli          1.0.0      2017-11-05 CRAN (R 3.4.2)                    
    ##  colorspace   1.3-2      2016-12-14 CRAN (R 3.4.0)                    
    ##  compiler     3.4.3      2017-12-07 local                             
    ##  crayon       1.3.4      2017-09-16 CRAN (R 3.4.1)                    
    ##  datasets   * 3.4.3      2017-12-07 local                             
    ##  devtools     1.13.4     2017-11-09 CRAN (R 3.4.2)                    
    ##  digest       0.6.14     2018-01-14 CRAN (R 3.4.3)                    
    ##  dplyr      * 0.7.4      2017-09-28 CRAN (R 3.4.2)                    
    ##  evaluate     0.10.1     2017-06-24 CRAN (R 3.4.0)                    
    ##  forcats    * 0.2.0.9000 2018-01-05 Github (tidyverse/forcats@fdde458)
    ##  foreign      0.8-69     2017-06-22 CRAN (R 3.4.3)                    
    ##  ggdendro     0.1-20     2016-04-27 CRAN (R 3.4.0)                    
    ##  ggformula  * 0.6.1      2018-01-03 CRAN (R 3.4.3)                    
    ##  ggplot2    * 2.2.1      2016-12-30 CRAN (R 3.4.0)                    
    ##  glue         1.2.0      2017-10-29 CRAN (R 3.4.2)                    
    ##  graphics   * 3.4.3      2017-12-07 local                             
    ##  grDevices  * 3.4.3      2017-12-07 local                             
    ##  grid         3.4.3      2017-12-07 local                             
    ##  gridExtra    2.3        2017-09-09 CRAN (R 3.4.1)                    
    ##  gtable       0.2.0      2016-02-26 CRAN (R 3.4.0)                    
    ##  haven        1.1.0.9000 2018-01-05 Github (tidyverse/haven@7f2b479)  
    ##  hms          0.4.0      2017-11-23 CRAN (R 3.4.2)                    
    ##  htmltools    0.3.6      2017-04-28 CRAN (R 3.4.0)                    
    ##  httr         1.3.1      2017-08-20 CRAN (R 3.4.1)                    
    ##  jsonlite     1.5        2017-06-01 CRAN (R 3.4.0)                    
    ##  knitr        1.18       2017-12-27 CRAN (R 3.4.3)                    
    ##  labeling     0.3        2014-08-23 CRAN (R 3.4.0)                    
    ##  lattice    * 0.20-35    2017-03-25 CRAN (R 3.4.3)                    
    ##  lazyeval     0.2.1      2017-10-29 CRAN (R 3.4.2)                    
    ##  lubridate    1.7.1      2017-11-03 CRAN (R 3.4.2)                    
    ##  magrittr   * 1.5        2014-11-22 CRAN (R 3.4.0)                    
    ##  MASS         7.3-48     2017-12-25 CRAN (R 3.4.3)                    
    ##  Matrix     * 1.2-12     2017-11-20 CRAN (R 3.4.3)                    
    ##  memoise      1.1.0      2017-04-21 CRAN (R 3.4.0)                    
    ##  methods    * 3.4.3      2017-12-07 local                             
    ##  mnormt       1.5-5      2016-10-15 CRAN (R 3.4.0)                    
    ##  modelr       0.1.1      2017-07-24 CRAN (R 3.4.1)                    
    ##  mosaic     * 1.1.1      2017-11-28 CRAN (R 3.4.2)                    
    ##  mosaicCore   0.4.2      2017-11-28 CRAN (R 3.4.2)                    
    ##  mosaicData * 0.14.0     2016-06-17 CRAN (R 3.4.0)                    
    ##  munsell      0.4.3      2016-02-13 CRAN (R 3.4.0)                    
    ##  nlme         3.1-131    2017-02-06 CRAN (R 3.4.3)                    
    ##  parallel     3.4.3      2017-12-07 local                             
    ##  pillar       1.1.0      2018-01-14 CRAN (R 3.4.3)                    
    ##  pkgconfig    2.0.1      2017-03-21 CRAN (R 3.4.0)                    
    ##  plyr         1.8.4      2016-06-08 CRAN (R 3.4.0)                    
    ##  psych        1.7.8      2017-09-09 CRAN (R 3.4.1)                    
    ##  purrr      * 0.2.4      2017-10-18 CRAN (R 3.4.2)                    
    ##  R6           2.2.2      2017-06-17 CRAN (R 3.4.0)                    
    ##  Rcpp         0.12.14    2017-11-23 CRAN (R 3.4.3)                    
    ##  readr      * 1.1.1      2017-05-16 CRAN (R 3.4.0)                    
    ##  readxl       1.0.0.9000 2017-06-10 Github (tidyverse/readxl@a1c46a8) 
    ##  reshape2     1.4.3      2017-12-11 CRAN (R 3.4.2)                    
    ##  rlang        0.1.6      2017-12-21 CRAN (R 3.4.3)                    
    ##  rmarkdown    1.8        2017-11-17 CRAN (R 3.4.2)                    
    ##  rprojroot    1.3-2      2018-01-03 CRAN (R 3.4.3)                    
    ##  rstudioapi   0.7        2017-09-07 CRAN (R 3.4.1)                    
    ##  rvest        0.3.2      2016-06-17 CRAN (R 3.4.0)                    
    ##  scales       0.5.0      2017-08-24 CRAN (R 3.4.1)                    
    ##  splines      3.4.3      2017-12-07 local                             
    ##  stats      * 3.4.3      2017-12-07 local                             
    ##  stringi      1.1.6      2017-11-17 CRAN (R 3.4.2)                    
    ##  stringr    * 1.3.0      2017-12-24 Github (tidyverse/stringr@5376e12)
    ##  tibble     * 1.4.1      2017-12-25 CRAN (R 3.4.3)                    
    ##  tidyr      * 0.7.2      2017-10-16 CRAN (R 3.4.2)                    
    ##  tidyselect   0.2.3      2017-11-06 CRAN (R 3.4.2)                    
    ##  tidyverse  * 1.2.1      2017-11-14 CRAN (R 3.4.2)                    
    ##  tools        3.4.3      2017-12-07 local                             
    ##  utf8         1.1.3      2018-01-03 CRAN (R 3.4.3)                    
    ##  utils      * 3.4.3      2017-12-07 local                             
    ##  withr        2.1.1      2017-12-19 CRAN (R 3.4.2)                    
    ##  xml2         1.1.1      2017-01-24 CRAN (R 3.4.0)                    
    ##  yaml         2.1.16     2017-12-12 CRAN (R 3.4.2)
