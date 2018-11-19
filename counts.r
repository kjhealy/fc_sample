library(tidyverse)

## Hex colors for sex
sex_colors <- c("#E69F00", "#993300")

## Hex color codes for Dem Blue and Rep Red
party_colors <- c("#2E74C0", "#CB454A")

## Group labels
mf_labs <- tibble(M = "Men", F = "Women")

theme_set(theme_minimal())

## Character vectors only, by default
df <- read_csv("data/fc_sample.csv")

df

#> > df
#> # A tibble: 280 x 4
#>      pid start_year party      sex
#>    <int> <date>     <chr>      <chr>
#>  1  3160 2013-01-03 Republican M
#>  2  3161 2013-01-03 Democrat   F
#>  3  3162 2013-01-03 Democrat   M
#>  4  3163 2013-01-03 Republican M
#>  5  3164 2013-01-03 Democrat   M
#>  6  3165 2013-01-03 Republican M
#>  7  3166 2013-01-03 Republican M
#>  8  3167 2013-01-03 Democrat   F
#>  9  3168 2013-01-03 Republican M
#> 10  3169 2013-01-03 Democrat   M
#> # ... with 270 more rows


df %>%
    group_by(start_year, party, sex) %>%
    summarize(N = n()) %>%
    mutate(freq = N / sum(N))

#> # A tibble: 14 x 5
#> # Groups:   start_year, party [8]
#>    start_year party      sex       N   freq
#>    <date>     <chr>      <chr> <int>  <dbl>
#>  1 2013-01-03 Democrat   F        21 0.362
#>  2 2013-01-03 Democrat   M        37 0.638
#>  3 2013-01-03 Republican F         8 0.101
#>  4 2013-01-03 Republican M        71 0.899
#>  5 2015-01-03 Democrat   M         1 1
#>  6 2015-01-03 Republican M         5 1
#>  7 2017-01-03 Democrat   F         6 0.24
#>  8 2017-01-03 Democrat   M        19 0.76
#>  9 2017-01-03 Republican F         2 0.0667
#> 10 2017-01-03 Republican M        28 0.933
#> 11 2019-01-03 Democrat   F        33 0.647
#> 12 2019-01-03 Democrat   M        18 0.353
#> 13 2019-01-03 Republican F         1 0.0323
#> 14 2019-01-03 Republican M        30 0.968


df %>%
    group_by(start_year, party, sex) %>%
    summarize(N = n()) %>%
    mutate(freq = N / sum(N)) %>%
    ggplot(aes(x = start_year,
               y = freq,
               fill = sex)) +
    geom_col() +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values = sex_colors, labels = c("Women", "Men")) +
    labs(x = "Year", y = "Percent", color = "Group") +
    facet_wrap(~ party)

ggsave("figures/df_chr_col.png")


df %>%
    group_by(start_year, party, sex) %>%
    summarize(N = n()) %>%
    mutate(freq = N / sum(N)) %>%
    ggplot(aes(x = start_year,
               y = freq,
               color = sex)) +
    geom_line(size = 1.1) +
    scale_y_continuous(labels = scales::percent) +
    scale_color_manual(values = sex_colors, labels = c("Women", "Men")) +
    guides(color = guide_legend(reverse = TRUE)) +
    labs(x = "Year", y = "Percent", color = "Group") +
    facet_wrap(~ party)

ggsave("figures/df_chr_line.png")


df_f <- df %>% modify_if(is.character, as.factor)

df_f %>%
    group_by(start_year, party, sex) %>%
    tally()

#> # A tibble: 16 x 4
#> # Groups:   start_year, party [8]
#>    start_year party      sex       n
#>    <date>     <fct>      <fct> <int>
#>  1 2013-01-03 Democrat   F        21
#>  2 2013-01-03 Democrat   M        37
#>  3 2013-01-03 Republican F         8
#>  4 2013-01-03 Republican M        71
#>  5 2015-01-03 Democrat   F         0
#>  6 2015-01-03 Democrat   M         1
#>  7 2015-01-03 Republican F         0
#>  8 2015-01-03 Republican M         5
#>  9 2017-01-03 Democrat   F         6
#> 10 2017-01-03 Democrat   M        19
#> 11 2017-01-03 Republican F         2
#> 12 2017-01-03 Republican M        28
#> 13 2019-01-03 Democrat   F        33
#> 14 2019-01-03 Democrat   M        18
#> 15 2019-01-03 Republican F         1
#> 16 2019-01-03 Republican M        30


df_f %>%
    group_by(start_year, party, sex) %>%
    summarize(N = n()) %>%
    mutate(freq = N / sum(N)) %>%
    ggplot(aes(x = start_year,
               y = freq,
               color = sex)) +
    geom_line(size = 1.1) +
    scale_y_continuous(labels = scales::percent) +
    scale_color_manual(values = sex_colors, labels = c("Women", "Men")) +
    guides(color = guide_legend(reverse = TRUE)) +
    labs(x = "Year", y = "Percent", color = "Group") +
    facet_wrap(~ party)

ggsave("figures/df_fac_line.png")


df %>%
    group_by(start_year, party, sex) %>%
    tally() %>%
    ungroup() %>%
    complete(start_year, party, sex,
             fill = list(n = 0))

#> # A tibble: 16 x 4
#>    start_year party      sex       n
#>    <date>     <chr>      <chr> <dbl>
#>  1 2013-01-03 Democrat   F        21
#>  2 2013-01-03 Democrat   M        37
#>  3 2013-01-03 Republican F         8
#>  4 2013-01-03 Republican M        71
#>  5 2015-01-03 Democrat   F         0
#>  6 2015-01-03 Democrat   M         1
#>  7 2015-01-03 Republican F         0
#>  8 2015-01-03 Republican M         5
#>  9 2017-01-03 Democrat   F         6
#> 10 2017-01-03 Democrat   M        19
#> 11 2017-01-03 Republican F         2
#> 12 2017-01-03 Republican M        28
#> 13 2019-01-03 Democrat   F        33
#> 14 2019-01-03 Democrat   M        18
#> 15 2019-01-03 Republican F         1
#> 16 2019-01-03 Republican M        30

df %>%
    group_by(start_year, party, sex) %>%
    summarize(N = n()) %>%
    mutate(freq = N / sum(N)) %>%
    ungroup() %>%
    complete(start_year, party, sex,
             fill = list(N = 0, freq = 0)) %>%
    ggplot(aes(x = start_year,
               y = freq,
               color = sex)) +
    geom_line(size = 1.1) +
    scale_y_continuous(labels = scales::percent) +
    scale_color_manual(values = sex_colors, labels = c("Women", "Men")) +
    guides(color = guide_legend(reverse = TRUE)) +
    labs(x = "Year", y = "Percent", color = "Group") +
    facet_wrap(~ party)

ggsave("figures/df_chr_line_2.png")
