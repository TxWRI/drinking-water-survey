Perceptions of Drinking Water and Safety: A Public Survey of U.S.
Residents
================

This is the data and code repository associated with the draft
manuscript, “Perceptions of Drinking Water and Safety: A Public Survey
of U.S. Residents” currently in submission. Authors include: Karissa
Palmer, T. Allen Berthold, Audrey McCrary, Stephanie deVilleneuve,
Michael Schramm, and Holli R. Leggette

This project uses the [renv](https://rstudio.github.io/renv/) and
[targets](https://docs.ropensci.org/targets/) R packages to facilitate
reproducibility. Once the project is downloaded and opened in RStudio
install the renv package:

``` r
install.packages("renv")
```

The packages and package versions used in the project can be installed
and restored using:

``` r
renv::restore()
```

To reproduce the analysis you will need a Census API key which can be
obtained from <https://api.census.gov/data/key_signup.html>. Then use
targets to rerun the analysis. This will take a little while to run:

``` r
tidycensus::census_api_key("API KEY GOES HERE")
targets::tar_make()
```

Survey responses used in our analysis are stored in the `clean_survey`
target:

``` r
targets::tar_read(clean_survey)
```

    ## # A tibble: 1,100 × 91
    ##    SEX   AGEP  RACE5 HOUSING HOWNSHP INCOME HHSIZE SCHL  RACE2 Q5    Q6    Q7   
    ##    <fct> <fct> <fct> <fct>   <fct>   <fct>   <dbl> <fct> <fct> <fct> <fct> <lgl>
    ##  1 Male  25:34 Asian Townho… Rent    $50,0…      5 Bach… Non-… Priv… Filt… TRUE 
    ##  2 Fema… 65+   Whit… Apartm… Rent    < $25…      1 High… White Publ… Filt… FALSE
    ##  3 Fema… 18:24 Whit… Single… Rent    $25,0…      3 Mast… White Publ… Bott… FALSE
    ##  4 Fema… 45:54 Two … Apartm… Rent    $25,0…      2 Other Non-… Publ… Bott… TRUE 
    ##  5 Fema… 35:44 Whit… Single… Own     $25,0…      4 Mast… White I do… Filt… FALSE
    ##  6 Fema… 35:44 Whit… Single… Own     $75,0…      5 High… White Publ… Bott… FALSE
    ##  7 Fema… 55:64 Whit… Single… Rent    < $25…      3 High… White Publ… Unfi… FALSE
    ##  8 Fema… 35:44 Blac… Apartm… Rent    $50,0…      2 High… Non-… Publ… Bott… TRUE 
    ##  9 Male  45:54 Two … Townho… Rent    $25,0…      2 Doct… Non-… Priv… Unfi… TRUE 
    ## 10 Fema… 35:44 Blac… Apartm… Rent    $25,0…      2 High… Non-… Priv… Filt… TRUE 
    ## # ℹ 1,090 more rows
    ## # ℹ 79 more variables: Q7_1 <fct>, Q7_2 <fct>, Q7_3 <fct>, Q7_4 <fct>,
    ## #   Q7_5 <fct>, Q7_6 <fct>, Q7_7 <fct>, Q7_8 <fct>, Q7_9 <fct>, Q7_10 <fct>,
    ## #   Q7_11 <fct>, Q7_12 <fct>, Q8 <int>, Q9 <lgl>, Q9_1 <fct>, Q9_2 <fct>,
    ## #   Q9_3 <fct>, Q9_4 <fct>, Q9_5 <fct>, Q9_6 <fct>, Q9_7 <fct>, Q9_8 <fct>,
    ## #   Q9_9 <fct>, Q9_10 <fct>, Q9_11 <fct>, Q9_12 <fct>, Q10_1 <fct>,
    ## #   Q10_2 <fct>, Q10_3 <fct>, Q10_4 <fct>, Q10_5 <fct>, Q10_6 <fct>, …

## Models

The RQ1 model is stored in target `m1`:

``` r
summary(targets::tar_read(m1))
```

    ## Call:
    ## svyolr(Q8 ~ SEX + AGEP + RACE2 + SCHL + HOWNSHP + INCOME + Q5 + 
    ##     Q7, design = survey_design, method = "logistic")
    ## 
    ## Coefficients:
    ##                                                        Value Std. Error
    ## SEXFemale                                       -0.274701660  0.1266707
    ## SEXOther                                        -2.067315899  0.4589247
    ## AGEP25:34                                       -0.066747588  0.2381329
    ## AGEP35:44                                       -0.231593290  0.2518102
    ## AGEP45:54                                       -0.781993928  0.2581768
    ## AGEP55:64                                       -0.327905311  0.2645996
    ## AGEP65+                                          0.133118083  0.2680915
    ## RACE2Non-white                                  -0.244458941  0.1577425
    ## SCHLHigh school graduate or GED                  0.217450430  0.3885722
    ## SCHLAssociate degree                             0.024914494  0.4076448
    ## SCHLBachelor's degree                            0.414621378  0.4067680
    ## SCHLMaster's degree                              0.693733871  0.4179859
    ## SCHLDoctorate or terminal degree                 0.521461672  0.4583307
    ## SCHLOther                                       -0.050149331  0.4867792
    ## HOWNSHPRent                                     -0.391949606  0.1400295
    ## INCOME$25,000 - $49,999                          0.171330220  0.1913481
    ## INCOME$50,000 - $74,999                          0.072266902  0.2041589
    ## INCOME$75,000 - $99,999                         -0.103794407  0.2301979
    ## INCOME$100,000 - $200,000                        0.406661811  0.2268313
    ## INCOME> $200,000                                 0.600258175  0.3622566
    ## Q5Public supply - rural water district          -0.212583192  0.1677026
    ## Q5Private supply - well, river, pond, rainwater  0.434568035  0.1739697
    ## Q5I don't know                                  -0.008299212  0.2419604
    ## Q7Yes                                           -1.136204904  0.1306765
    ##                                                     t value
    ## SEXFemale                                       -2.16862809
    ## SEXOther                                        -4.50469523
    ## AGEP25:34                                       -0.28029551
    ## AGEP35:44                                       -0.91971379
    ## AGEP45:54                                       -3.02890887
    ## AGEP55:64                                       -1.23925115
    ## AGEP65+                                          0.49653982
    ## RACE2Non-white                                  -1.54973413
    ## SCHLHigh school graduate or GED                  0.55961398
    ## SCHLAssociate degree                             0.06111814
    ## SCHLBachelor's degree                            1.01930683
    ## SCHLMaster's degree                              1.65970638
    ## SCHLDoctorate or terminal degree                 1.13774112
    ## SCHLOther                                       -0.10302275
    ## HOWNSHPRent                                     -2.79905063
    ## INCOME$25,000 - $49,999                          0.89538492
    ## INCOME$50,000 - $74,999                          0.35397385
    ## INCOME$75,000 - $99,999                         -0.45089197
    ## INCOME$100,000 - $200,000                        1.79279371
    ## INCOME> $200,000                                 1.65699714
    ## Q5Public supply - rural water district          -1.26762012
    ## Q5Private supply - well, river, pond, rainwater  2.49795224
    ## Q5I don't know                                  -0.03429989
    ## Q7Yes                                           -8.69479441
    ## 
    ## Intercepts:
    ##      Value   Std. Error t value
    ## 0|1  -4.9282  0.5223    -9.4348
    ## 1|2  -4.0875  0.4952    -8.2544
    ## 2|3  -3.6972  0.4689    -7.8845
    ## 3|4  -3.1975  0.4498    -7.1087
    ## 4|5  -2.6688  0.4388    -6.0825
    ## 5|6  -1.7134  0.4341    -3.9474
    ## 6|7  -1.3037  0.4344    -3.0010
    ## 7|8  -0.6665  0.4330    -1.5390
    ## 8|9   0.1443  0.4345     0.3320
    ## 9|10  0.9221  0.4376     2.1073
    ## (90 observations deleted due to missingness)

The RQ2 model is stored in target `m3` and the RQ3 model is stored in
target `m2`.
