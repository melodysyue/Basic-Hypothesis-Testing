Basic Hypothesis Testing
================
Yue Shi, PhD candidate, University of Washington,
4/12/2018

Import packages and data
------------------------

We will look at scaled\_effects in ww\_data file.

``` r
library(ggplot2)
ww_data = read.table(file="http://faculty.washington.edu/dfowler/teaching/2017/GNOM560/560_ww_data.txt", header = T, sep = '\t')
pab_data = read.table(file="http://faculty.washington.edu/dfowler/teaching/2017/GNOM560/560_pab_data.txt", header = T, sep = '\t') # Load up another DMS (of the yeast protein Pab1) as a comparator
```

Compare Two means
-----------------

##### One-sample T test

Compare sample mean to a value. For example, does sample mean deviate from *α*.

``` r
t.test(ww_data$scaled_effect, mu=0) ## By default, R will do two sides t test.
```

    ## 
    ##  One Sample t-test
    ## 
    ## data:  ww_data$scaled_effect
    ## t = 57.11, df = 376, p-value < 2.2e-16
    ## alternative hypothesis: true mean is not equal to 0
    ## 95 percent confidence interval:
    ##  0.8019972 0.8591921
    ## sample estimates:
    ## mean of x 
    ## 0.8305947

``` r
t.test(ww_data$scaled_effect, mu = 0, alternative = "two.sided")
```

    ## 
    ##  One Sample t-test
    ## 
    ## data:  ww_data$scaled_effect
    ## t = 57.11, df = 376, p-value < 2.2e-16
    ## alternative hypothesis: true mean is not equal to 0
    ## 95 percent confidence interval:
    ##  0.8019972 0.8591921
    ## sample estimates:
    ## mean of x 
    ## 0.8305947

``` r
t.test(ww_data$scaled_effect, mu = 0, alternative = "less")
```

    ## 
    ##  One Sample t-test
    ## 
    ## data:  ww_data$scaled_effect
    ## t = 57.11, df = 376, p-value = 1
    ## alternative hypothesis: true mean is less than 0
    ## 95 percent confidence interval:
    ##       -Inf 0.8545763
    ## sample estimates:
    ## mean of x 
    ## 0.8305947

``` r
t.test(ww_data$scaled_effect, mu = 0, alternative = "greater")
```

    ## 
    ##  One Sample t-test
    ## 
    ## data:  ww_data$scaled_effect
    ## t = 57.11, df = 376, p-value < 2.2e-16
    ## alternative hypothesis: true mean is greater than 0
    ## 95 percent confidence interval:
    ##  0.8066131       Inf
    ## sample estimates:
    ## mean of x 
    ## 0.8305947

##### Two-sample T test

##### First, check the assumption of normality. But remember, t test is robust to non-normality.

Make a histogram to check the data distribution.

``` r
ggplot(ww_data, aes(scaled_effect))+
  geom_histogram(fill="blue",alpha=0.25)
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](hypothesis_testing_files/figure-markdown_github/unnamed-chunk-3-1.png)

``` r
ggplot(pab_data, aes(scaled_effect))+
  geom_histogram(fill="red",alpha=0.25)
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](hypothesis_testing_files/figure-markdown_github/unnamed-chunk-3-2.png)

Make a QQ plot. A Q-Q plot is a graphical method for comparing two probability distributions by plotting their quantiles against each other. If the distributions are identical, the points will lie on a line with a slope of 1If they have the same shape but are related by a linear transformation, the Q-Q plot will be linear but with a slope other than 1. Q-Q plots allow you to spot outliers. QQ plot has the following format qqplot(distribution1, distribution2)

``` r
qqplot(dnorm(seq(min(ww_data$scaled_effect), max(ww_data$scaled_effect), 0.01),
             mean(ww_data$scaled_effect), sd(ww_data$scaled_effect)), ww_data$scaled_effect)
```

![](hypothesis_testing_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
qqplot(dnorm(seq(min(pab_data$scaled_effect), max(pab_data$scaled_effect), 0.01),
            mean(pab_data$scaled_effect), sd(pab_data$scaled_effect)), pab_data$scaled_effect)
```

![](hypothesis_testing_files/figure-markdown_github/unnamed-chunk-4-2.png)

Compare probability distributions between ww\_data and pab\_data.

``` r
qqplot(ww_data$scaled_effect, pab_data$scaled_effect)
```

![](hypothesis_testing_files/figure-markdown_github/unnamed-chunk-5-1.png)

The Kolmogorov-Smirnov test is a nonparametric test that can be used to compare a sample distribution ot a reference distribution or to compare two sample distributions. Try to use the Kolmogorov-Smirnov test (ks.test()) to check for normality. As we'll learn, the KS test compares a set of data to a cumulative distribution function. Here, you will want to use "pnorm" (the normal cumulative distribution function).

``` r
ks.test(ww_data$scaled_effect, "pnorm", mean(ww_data$scaled_effect), sd(ww_data$scaled_effect))
```

    ## 
    ##  One-sample Kolmogorov-Smirnov test
    ## 
    ## data:  ww_data$scaled_effect
    ## D = 0.13099, p-value = 4.812e-06
    ## alternative hypothesis: two-sided

``` r
ks.test(pab_data$scaled_effect, "pnorm", mean(pab_data$scaled_effect), sd(pab_data$scaled_effect))
```

    ## Warning in ks.test(pab_data$scaled_effect, "pnorm", mean(pab_data
    ## $scaled_effect), : ties should not be present for the Kolmogorov-Smirnov
    ## test

    ## 
    ##  One-sample Kolmogorov-Smirnov test
    ## 
    ## data:  pab_data$scaled_effect
    ## D = 0.2313, p-value < 2.2e-16
    ## alternative hypothesis: two-sided

##### Second, check the assumption for homogeneity of variance.

Bartlett's test can be used to determine if samples are drawn from populations with the same variance. Try to use Bartlett's test (bartlett.test()) test to see if the variances can be pooled.

``` r
bartlett.test(list(ww_data$scaled_effect, pab_data$scaled_effect))
```

    ## 
    ##  Bartlett test of homogeneity of variances
    ## 
    ## data:  list(ww_data$scaled_effect, pab_data$scaled_effect)
    ## Bartlett's K-squared = 1.9762, df = 1, p-value = 0.1598

##### Third, do a two-sample 2-sides t test

``` r
t.test(ww_data$scaled_effect, pab_data$scaled_effect, paired=FALSE,var.equal=TRUE)
```

    ## 
    ##  Two Sample t-test
    ## 
    ## data:  ww_data$scaled_effect and pab_data$scaled_effect
    ## t = 1.898, df = 1651, p-value = 0.05788
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.001004963  0.061135480
    ## sample estimates:
    ## mean of x mean of y 
    ## 0.8305947 0.8005294

``` r
ggplot(ww_data, aes(scaled_effect)) + geom_density(fill = "blue", alpha = 0.25) + geom_density(data = pab_data, aes(scaled_effect), fill = "red", alpha = 0.25)
```

![](hypothesis_testing_files/figure-markdown_github/unnamed-chunk-8-1.png)
