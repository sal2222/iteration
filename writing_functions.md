Writing functions
================

``` r
x = rnorm(25, mean = 5, sd = 3)

(x - mean(x)) / sd(x)
##  [1] -0.10635957  1.18701524 -0.47377957 -1.01653059 -0.57576124
##  [6] -1.15369133  1.26324556  0.22776615 -1.24258024  0.17377481
## [11] -0.60333177 -0.14488705  1.35465252  0.66305063 -2.14523109
## [16]  0.53887309  0.05408914  1.44619034 -0.60430695 -0.33935653
## [21]  1.04652476 -1.11541993 -0.41323985  1.94925560  0.03003788
```

``` r
z_scores = function(x) {
  
  z = (x - mean(x)) / sd(x)
  z
  
}

z_scores(x)
##  [1] -0.10635957  1.18701524 -0.47377957 -1.01653059 -0.57576124
##  [6] -1.15369133  1.26324556  0.22776615 -1.24258024  0.17377481
## [11] -0.60333177 -0.14488705  1.35465252  0.66305063 -2.14523109
## [16]  0.53887309  0.05408914  1.44619034 -0.60430695 -0.33935653
## [21]  1.04652476 -1.11541993 -0.41323985  1.94925560  0.03003788
```

``` r
unif_sample = runif(100)

z_scores(x = unif_sample)
##   [1]  0.98599422  1.40627348  0.94750356 -0.39410528 -0.70103015
##   [6] -1.39905301 -0.22463776  1.26829442  0.04233519  0.78941196
##  [11] -0.06266514 -0.51680611 -0.12447976  1.41902453 -0.15552449
##  [16]  0.36792427  0.56395483  1.58689324 -0.71246976  0.72344798
##  [21]  0.86351386 -0.62088860 -1.22512740 -1.14206603  0.25302538
##  [26]  0.91167975 -1.37025351  1.51633438 -0.57911625  0.64392462
##  [31]  0.47009095 -0.30905216  1.17907930 -0.39200577 -1.57095242
##  [36] -0.43903330 -1.21149598 -1.54823828 -0.72779807  1.57863126
##  [41]  1.28434891  0.41688343  0.40997077  1.08550272  1.05584091
##  [46] -0.87841945  1.19550792 -1.34161223  0.53907332 -1.59812125
##  [51] -0.47353676  0.22276264 -0.56754500  0.28020521 -0.05141144
##  [56]  1.22336259 -0.57729780 -0.31542031  0.99954064 -1.50690277
##  [61] -0.85196143  1.26995233 -1.02298441 -1.26529545  1.32456018
##  [66] -1.37432052 -0.58728267  1.47482065 -1.36665926  1.39949284
##  [71]  0.11226853 -1.71369221  0.18518068  1.42948097 -0.41147432
##  [76] -0.17584593  0.82121301  0.72345585 -0.71839820  0.18559917
##  [81] -0.79698483 -1.54532147 -1.39926033 -1.61086261 -0.97912623
##  [86] -0.27248610 -1.34391527  1.56170435  0.31027564  0.53896589
##  [91] -0.39585480 -1.58855987  0.47288890  1.25258640  1.62516640
##  [96] -0.81148851 -0.29152930  1.15752070  1.17286748  0.01203376
```

Add checks

``` r
z_scores = function(x) {
  
  if (!is.numeric(x)) {
    stop("Argument x should be numeric")
  } else if (length(x) == 1) {
    stop("Z scores cannot be computed for length 1 vectors")
  }
  
  z = mean(x) / sd(x)
  
  z
}
```

### mean

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("Argument x should be numeric")
  } else if (length(x) == 1) {
    stop("Cannot be computed for length 1 vectors")
  }
  
  tibble(
  mean_x = mean(x),
  sd_x = sd(x)
)
  
 
}

mean_and_sd(unif_sample)
## # A tibble: 1 x 2
##   mean_x  sd_x
##    <dbl> <dbl>
## 1  0.511 0.297
```

Multiple Inputs
---------------

Simple regression.

``` r
sim_data = tibble(
  x = rnorm(30, mean = 1, sd = 1),
  y = 2 + 3 * x + rnorm(30, 0, 1)
)

sim_data
## # A tibble: 30 x 2
##         x      y
##     <dbl>  <dbl>
##  1  0.579  2.91 
##  2  2.13   7.89 
##  3  2.56   8.08 
##  4  0.770  3.69 
##  5  0.297  2.78 
##  6  1.59   5.90 
##  7 -0.118 -0.409
##  8  1.81   8.33 
##  9  0.749  3.32 
## 10  1.93   7.36 
## # ... with 20 more rows
sim_data %>% 
  ggplot(aes(x = x, y = y)) + geom_point()
```

<img src="writing_functions_files/figure-markdown_github/unnamed-chunk-7-1.png" width="90%" />

``` r

ls_fit = lm(y ~ x, data = sim_data)
  
beta0_hat = coef(ls_fit)[1]
beta1_hat = coef(ls_fit)[2]
```

Write a function that simulates data, fits the model, annd returns the estimate. Inputs are sample size and true cofficients.

``` r
sim_regression = function(n, beta0, beta1) {
  
  sim_data = tibble(
    x = rnorm(n, mean = 1, sd = 1),
    y = beta0 + beta1 * x + rnorm(n, 0, 1)
  )
  
  ls_fit = lm(y ~ x, data = sim_data)
  
  tibble(
    beta0_hat = coef(ls_fit)[1],
    beta1_hat = coef(ls_fit)[2]
  )
}
```

``` r
sim_regression(n = 100, beta0 = 0.2, beta1 = 1.3)
## # A tibble: 1 x 2
##   beta0_hat beta1_hat
##       <dbl>     <dbl>
## 1    0.0691      1.48
```

Scraping Amazon example
-----------------------

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

dynamite_html = read_html(url)

review_titles = dynamite_html %>%
  html_nodes("#cm_cr-review_list .review-title") %>%
  html_text()

review_stars = dynamite_html %>%
  html_nodes("#cm_cr-review_list .review-rating") %>%
  html_text()

review_text = dynamite_html %>%
    html_nodes(".review-data:nth-child(4)") %>%
    html_text()

reviews = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)

reviews
## # A tibble: 10 x 3
##    title                   stars       text                               
##    <chr>                   <chr>       <chr>                              
##  1 "Great \"Odd Ball\" mo~ 5.0 out of~ The dance scene was worth the time~
##  2 Nostalgic Stupidity     4.0 out of~ This movie is dumb. I won't lie an~
##  3 Happy                   5.0 out of~ Don't know why I lov this movie bu~
##  4 Go watch THE ROCK or d~ 2.0 out of~ This movie is horrible. How do so ~
##  5 My mom loves it         5.0 out of~ Got this for my mom for mother's d~
##  6 Nothing Quite Like It.  5.0 out of~ So much fun watching these listles~
##  7 Has pretty sweet bow s~ 5.0 out of~ Well, things are getting pretty se~
##  8 Great                   5.0 out of~ Great                              
##  9 Fast delivery           5.0 out of~ Bought as gift                     
## 10 Lol                     5.0 out of~ Funny
```

Write a function to extract reviews

``` r
read_page_reviews <- function(url) {
  
  h = read_html(url)
  
  review_titles = h %>%
    html_nodes("#cm_cr-review_list .review-title") %>%
    html_text()
  
  review_stars = h %>%
    html_nodes("#cm_cr-review_list .review-rating") %>%
    html_text() %>%
    str_extract("\\d") %>%
    as.numeric()
  
  review_text = h %>%
    html_nodes(".review-data:nth-child(4)") %>%
    html_text()
  
  tibble(
    title = review_titles,
    stars = review_stars,
    text = review_text
  )
}

read_page_reviews(url)
## # A tibble: 10 x 3
##    title                     stars text                                   
##    <chr>                     <dbl> <chr>                                  
##  1 "Great \"Odd Ball\" movi~     5 The dance scene was worth the time spe~
##  2 Nostalgic Stupidity           4 This movie is dumb. I won't lie and sa~
##  3 Happy                         5 Don't know why I lov this movie but ido
##  4 Go watch THE ROCK or dum~     2 This movie is horrible. How do so many~
##  5 My mom loves it               5 Got this for my mom for mother's day, ~
##  6 Nothing Quite Like It.        5 So much fun watching these listless pe~
##  7 Has pretty sweet bow ski~     5 Well, things are getting pretty seriou~
##  8 Great                         5 Great                                  
##  9 Fast delivery                 5 Bought as gift                         
## 10 Lol                           5 Funny
```

Extract a lot of reviews.

``` r
url_base = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="
urls = str_c(url_base, 1:5)

dynamite_reviews = bind_rows(
  read_page_reviews(urls[1]),
  read_page_reviews(urls[2]),
  read_page_reviews(urls[3]),
  read_page_reviews(urls[4]),
  read_page_reviews(urls[5])
)

dynamite_reviews
## # A tibble: 50 x 3
##    title                     stars text                                   
##    <chr>                     <dbl> <chr>                                  
##  1 "Great \"Odd Ball\" movi~     5 The dance scene was worth the time spe~
##  2 Nostalgic Stupidity           4 This movie is dumb. I won't lie and sa~
##  3 Happy                         5 Don't know why I lov this movie but ido
##  4 Go watch THE ROCK or dum~     2 This movie is horrible. How do so many~
##  5 My mom loves it               5 Got this for my mom for mother's day, ~
##  6 Nothing Quite Like It.        5 So much fun watching these listless pe~
##  7 Has pretty sweet bow ski~     5 Well, things are getting pretty seriou~
##  8 Great                         5 Great                                  
##  9 Fast delivery                 5 Bought as gift                         
## 10 Lol                           5 Funny                                  
## # ... with 40 more rows
```

LoTR
----

loading LoTR data

``` r
lotr_load_and_tidy = function(path, range, movie_name) {
  
  df = readxl::read_excel(path, range = range) %>%
    janitor::clean_names() %>%
    gather(key = sex, value = words, female:male) %>%
    mutate(race = tolower(race),
           movie = movie_name)
  
  df
  
}

lotr_tidy = 
  bind_rows(
    lotr_load_and_tidy("./data/LotR_Words.xlsx", "B3:D6", "fellowship_ring"),
    lotr_load_and_tidy("./data/LotR_Words.xlsx", "F3:H6", "two_towers"),
    lotr_load_and_tidy("./data/LotR_Words.xlsx", "J3:L6", "return_king")) %>%
  select(movie, everything()) 
## Error: `path` does not exist: './data/LotR_Words.xlsx'

lotr_tidy
## Error in eval(expr, envir, enclos): object 'lotr_tidy' not found
```
