Basic Hypothesis Testing
================
Yue Shi, PhD candidate, University of Washington,
4/24/2018

-   [Import packages and data sets](#import-packages-and-data-sets)
-   [Parametric testing: Compare two means](#parametric-testing-compare-two-means)
    -   [One-sample T test](#one-sample-t-test)
    -   [Two-sample T test](#two-sample-t-test)
        -   [First, check the assumption of normality. But remember, t test is robust to non-normality.](#first-check-the-assumption-of-normality.-but-remember-t-test-is-robust-to-non-normality.)
        -   [Second, check the assumption for homogeneity of variance.](#second-check-the-assumption-for-homogeneity-of-variance.)
        -   [Third, do a two-sample 2-sides t test.](#third-do-a-two-sample-2-sides-t-test.)
-   [Non-parametric hypothesis testing](#non-parametric-hypothesis-testing)
    -   [Sign Test](#sign-test)
    -   [Wilconxon Signed-Rank Test](#wilconxon-signed-rank-test)
    -   [Wilcoxon Rank Sum Test (a.k.a Mann-Whitney test or Wilcoxon-Mann-Whitney test)](#wilcoxon-rank-sum-test-a.k.a-mann-whitney-test-or-wilcoxon-mann-whitney-test)
    -   [Kruskal-Wallis Test](#kruskal-wallis-test)
-   [Resampling](#resampling)
    -   [Sample Randomization Test](#sample-randomization-test)
    -   [Bootstrapping](#bootstrapping)

Import packages and data sets
-----------------------------

We will import ww\_data file and pac\_data file as a comparator, and two libraries, ggplot2 (for plotting) and BSDA (basic statistics and data analysis).

``` r
library(ggplot2)
library(BSDA)
ww_data = read.table(file="http://faculty.washington.edu/dfowler/teaching/2017/GNOM560/560_ww_data.txt", header = T, sep = '\t')
pab_data = read.table(file="http://faculty.washington.edu/dfowler/teaching/2017/GNOM560/560_pab_data.txt", header = T, sep = '\t') 
```

Parametric testing: Compare two means
-------------------------------------

#### One-sample T test

Compare sample mean to a value. For example, does sample mean deviate from *α*.

``` r
t.test(ww_data$scaled_effect, mu=0) 
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
t.test(ww_data$scaled_effect, mu = 0, alternative = "two.sided") ## By default, R will do two-side t test.
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

#### Two-sample T test

##### First, check the assumption of normality. But remember, t test is robust to non-normality.

**Make a histogram** to check the data distribution.

``` r
ggplot(ww_data, aes(scaled_effect))+
  geom_histogram(fill="blue",alpha=0.25)
```

![](hypothesis_testing_files/figure-markdown_github/unnamed-chunk-3-1.png)

``` r
ggplot(pab_data, aes(scaled_effect))+
  geom_histogram(fill="red",alpha=0.25)
```

![](hypothesis_testing_files/figure-markdown_github/unnamed-chunk-3-2.png)

**Make a QQ plot**. A Q-Q plot is a graphical method for comparing two probability distributions by plotting their quantiles against each other. If the distributions are identical, the points will lie on a line with a slope of 1If they have the same shape but are related by a linear transformation, the Q-Q plot will be linear but with a slope other than 1. Q-Q plots allow you to spot outliers. QQ plot has the following format qqplot(distribution1, distribution2)

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

**The Kolmogorov-Smirnov test** is a nonparametric test that can be used to compare a sample distribution ot a reference distribution or to compare two sample distributions. Try to use the Kolmogorov-Smirnov test (ks.test()) to check for normality. As we'll learn, the KS test compares a set of data to a cumulative distribution function. Here, you will want to use "pnorm" (the normal cumulative distribution function).

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

    ## 
    ##  One-sample Kolmogorov-Smirnov test
    ## 
    ## data:  pab_data$scaled_effect
    ## D = 0.2313, p-value < 2.2e-16
    ## alternative hypothesis: two-sided

##### Second, check the assumption for homogeneity of variance.

**Bartlett's test** can be used to determine if samples are drawn from populations with the same variance. Try to use Bartlett's test (bartlett.test()) test to see if the variances can be pooled.

``` r
bartlett.test(list(ww_data$scaled_effect, pab_data$scaled_effect))
```

    ## 
    ##  Bartlett test of homogeneity of variances
    ## 
    ## data:  list(ww_data$scaled_effect, pab_data$scaled_effect)
    ## Bartlett's K-squared = 1.9762, df = 1, p-value = 0.1598

##### Third, do a two-sample 2-sides t test.

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

Non-parametric hypothesis testing
---------------------------------

``` r
x=rnorm(100,2) #Generate some data
```

#### Sign Test

Background: For example, when the data is not normally distributed, rather than apply a transformation, you can use sign test. It simply allocates a sign (+ or -) to each observation. It tests the equality of matched pairs of observations. The null hypotheis is that the median of the differences is zero. No further assumptions are made. **It is used in situations in which the one-sample or paired t-test might traditionally be applied**. As a rule, nonparametric methods, particularly when used in small samples, have rather less power than their parametric equivalents.

Let's test to see if this data really does come from a distribution with a median of 2

``` r
SIGN.test(x, md = 2) # it use median (md) instead of mean (mu). 
```

    ## 
    ##  One-sample Sign-Test
    ## 
    ## data:  x
    ## s = 53, p-value = 0.6173
    ## alternative hypothesis: true median is not equal to 2
    ## 95 percent confidence interval:
    ##  1.807113 2.259526
    ## sample estimates:
    ## median of x 
    ##    2.081437 
    ## 
    ## Achieved and Interpolated Confidence Intervals: 
    ## 
    ##                   Conf.Level L.E.pt U.E.pt
    ## Lower Achieved CI     0.9431 1.8093 2.2454
    ## Interpolated CI       0.9500 1.8071 2.2595
    ## Upper Achieved CI     0.9648 1.8024 2.2900

What about using one-sample t-test on this?

``` r
t.test(x,mu=2)
```

    ## 
    ##  One Sample t-test
    ## 
    ## data:  x
    ## t = 1.0453, df = 99, p-value = 0.2984
    ## alternative hypothesis: true mean is not equal to 2
    ## 95 percent confidence interval:
    ##  1.904462 2.308246
    ## sample estimates:
    ## mean of x 
    ##  2.106354

#### Wilconxon Signed-Rank Test

Background: Though the sign test is extremely simple to perform, one obvious disadvantage is that it does not take the maginitude of the observation into account and may reduce the statistical power of the test. The alternative that accounts for the magnitude of the observations is the Wilcoxon signed rank test. It tests the equality of matched pairs of observations. It assumes the distribution is symmetrical. The null hypothesis is that both distributions are the same.

``` r
wilcox.test(x, mu = 2) #it use mean (mu) instead of median (md)
```

    ## 
    ##  Wilcoxon signed rank test with continuity correction
    ## 
    ## data:  x
    ## V = 2796, p-value = 0.3523
    ## alternative hypothesis: true location is not equal to 2

Now, you try with paired data. First, generate some random data from the normal distribution, calculate the difference and do the appropriate test.

``` r
y=rnorm(100,2.5)
dif=x-y
t.test(dif,mu=0)
```

    ## 
    ##  One Sample t-test
    ## 
    ## data:  dif
    ## t = -4.0036, df = 99, p-value = 0.0001207
    ## alternative hypothesis: true mean is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.8838244 -0.2980645
    ## sample estimates:
    ##  mean of x 
    ## -0.5909445

``` r
SIGN.test(dif,md=0)
```

    ## 
    ##  One-sample Sign-Test
    ## 
    ## data:  dif
    ## s = 38, p-value = 0.02098
    ## alternative hypothesis: true median is not equal to 0
    ## 95 percent confidence interval:
    ##  -1.0084734 -0.1347159
    ## sample estimates:
    ## median of x 
    ##  -0.7669838 
    ## 
    ## Achieved and Interpolated Confidence Intervals: 
    ## 
    ##                   Conf.Level  L.E.pt  U.E.pt
    ## Lower Achieved CI     0.9431 -1.0015 -0.1394
    ## Interpolated CI       0.9500 -1.0085 -0.1347
    ## Upper Achieved CI     0.9648 -1.0234 -0.1246

``` r
wilcox.test(dif,mu=0)
```

    ## 
    ##  Wilcoxon signed rank test with continuity correction
    ## 
    ## data:  dif
    ## V = 1440, p-value = 0.0001923
    ## alternative hypothesis: true location is not equal to 0

**Wilcoxon signed-rank test is more powerful than the simply sign test.** Using the rank data in addition to the sign data gave us much better precision, since it has smaller p value.

#### Wilcoxon Rank Sum Test (a.k.a Mann-Whitney test or Wilcoxon-Mann-Whitney test)

Background: the sign test and Wilcoxon signed rank test are useful non-parametric alternatives to the one-sample and paired t-tests. **A nonparametric alternative to the unpaired t-test is given by the Wilcoxon rank sum test, which is also known as the Mann-Whitney test.** When used in one-sample or paired test, wilcox.test means Wilcoxon signed rank test, whereas when used in unpaired two-sample test, *wilcox.test* means Wilcoxon rank sum test.

``` r
z=rnorm(100,2)
wilcox.test(x,z) 
```

    ## 
    ##  Wilcoxon rank sum test with continuity correction
    ## 
    ## data:  x and z
    ## W = 5567, p-value = 0.1663
    ## alternative hypothesis: true location shift is not equal to 0

Now make random normal data set of 1000 elements with a mean of 2 and a random gamma data set whose shape parameter is 2 (will also have an expected value of 2). Make density plot of each, marking the means.

``` r
normd=rnorm(1000, mean=2)
mean_norm=mean(normd)
gammad=rgamma(1000, shape=2)
median_gamma=median(gammad)
normd=as.data.frame(normd)
gammad=as.data.frame(gammad)
library(ggplot2)
ggplot(normd, aes(normd)) + 
  geom_density(fill = "blue", alpha = 0.25) + 
  geom_vline(xintercept=mean_norm, col="blue")+
  geom_density(data = gammad, aes(gammad), fill = "red", alpha = 0.25) +
  geom_vline(xintercept=median_gamma, col="red")
```

![](hypothesis_testing_files/figure-markdown_github/unnamed-chunk-15-1.png)

What do you think will happen when you do a Wilcoxon rank sum test with these data? Give it a try and see. Remember, wilcoxon test assumes symmetric disetribution, whereas gamma distribution is not necessarily symmetrical.

``` r
wilcox.test(normd$normd,gammad$gammad)
```

    ## 
    ##  Wilcoxon rank sum test with continuity correction
    ## 
    ## data:  normd$normd and gammad$gammad
    ## W = 544190, p-value = 0.0006217
    ## alternative hypothesis: true location shift is not equal to 0

Next, do a WRST on the WW domain reported\_effect scores vs the Pab1 reported\_effect scores, and compare the results with t test.

``` r
wilcox.test(ww_data$reported_effect,pab_data$reported_effect)
```

    ## 
    ##  Wilcoxon rank sum test with continuity correction
    ## 
    ## data:  ww_data$reported_effect and pab_data$reported_effect
    ## W = 271280, p-value = 0.0001586
    ## alternative hypothesis: true location shift is not equal to 0

``` r
t.test(ww_data$reported_effect,pab_data$reported_effect)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  ww_data$reported_effect and pab_data$reported_effect
    ## t = 4.3899, df = 701.15, p-value = 1.309e-05
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  0.2076716 0.5437370
    ## sample estimates:
    ##  mean of x  mean of y 
    ## -0.8239443 -1.1996486

``` r
ggplot(ww_data, aes(reported_effect)) + 
  geom_density(fill = "blue", alpha = 0.25) + 
  geom_density(data = pab_data, aes(reported_effect), fill = "red", alpha = 0.25)
```

![](hypothesis_testing_files/figure-markdown_github/unnamed-chunk-17-1.png)

Since both datasets on reported effect are not normally distributed, use a non-parametrid test such as wilcoxon rank sum test is a good idea.

#### Kruskal-Wallis Test

Background: Wilcoxon rank sum test is for comparing two independent groups, **kruskal-wallis test is an non-parametric alternative of anova for comparing three independent groups.**

Create three random gamma-distributed data set with 100 elements and an identical shape parameter of your choice. Use kruskal.test() to verify that the medians are the same

``` r
d1=rgamma(100,shape=2)
d2=rgamma(100,shape=2)
d3=rgamma(100,shape=2)
kruskal.test(list(d1,d2,d3))
```

    ## 
    ##  Kruskal-Wallis rank sum test
    ## 
    ## data:  list(d1, d2, d3)
    ## Kruskal-Wallis chi-squared = 2.0815, df = 2, p-value = 0.3532

OK, now double the shape parameter for d3 and test again

``` r
d3=rgamma(100,shape=4)
kruskal.test(list(d1,d2,d3))
```

    ## 
    ##  Kruskal-Wallis rank sum test
    ## 
    ## data:  list(d1, d2, d3)
    ## Kruskal-Wallis chi-squared = 96.901, df = 2, p-value < 2.2e-16

Use **pairwise.wilcox.test()** to see which of our three data sets is different from the others (note the automatic correction for multiple hypothesis testing). It requires some formatting work.

``` r
combined.vector=c(d1,d2,d3)
grouping.vector=c(rep("d1",length(d1)),rep("d2",length(d2)),rep("d3",length(d3)))
pairwise.wilcox.test(combined.vector,grouping.vector, paired = TRUE)
```

    ## 
    ##  Pairwise comparisons using Wilcoxon signed rank test 
    ## 
    ## data:  combined.vector and grouping.vector 
    ## 
    ##    d1      d2     
    ## d2 0.18    -      
    ## d3 3.7e-13 3.4e-15
    ## 
    ## P value adjustment method: holm

To get a sense of the power of the KW test, try varying the shape parameter in increments of 1% up or down and find the threshold for detectiong the difference.

``` r
shape.param=seq(2,4,0.02)
random.gamma=sapply(shape.param,function(x){rgamma(100,x)}, simplify=FALSE)
pvalues=sapply(random.gamma, function(x){kruskal.test(list(d1,d2,x))$p.value})
par(mfrow=c(1,1))
plot(seq(from=2, to=4, by=0.02), pvalues)
abline(h=0.05,col="red")
```

![](hypothesis_testing_files/figure-markdown_github/unnamed-chunk-21-1.png)

Repeast this exercise 100 times.

``` r
random.gamma = function(x) {
shape.param = seq(2, 4, 0.02)
random.gammas = sapply(shape.param, function(x) {rgamma(100, x)}, simplify = FALSE)
pvalues = sapply(random.gammas, function(x) {kruskal.test(list(d1, d2, x))$p.value})
largest.shape = max(which(pvalues > 0.05))
return(2 + 0.02*largest.shape)
}

largest.shapes = sapply(1:100, random.gamma)

plot(density(largest.shapes), col = "red")
abline(v = mean(largest.shapes), col = "gray", lwd = 4)
```

![](hypothesis_testing_files/figure-markdown_github/unnamed-chunk-22-1.png)

Resampling
----------

There are two categories of resampling, randomization and bootstrapping. Randomization is sampling without replacement, it is for hypothesis testing. Bootstrapping is sampling with replacement, it is for parameter estimation. Resampling and non-parametric testing methods discussed above are both good for the situations when we know little about the distribution and the assumptions are violated.

##### Sample Randomization Test

Let's examine the difference in mean outcome in a dominant model for a single SNP. First, we make up some "true" data. Here, the carrier vector is binary and specifies whether each subject either is or is not a carrier of the SNP. The null.y and alt.y vectors give the outcome (let's say it's the expression of an associated transcript) for each subject.

``` r
carrier = rep(c(0,1), c(100,200))
null.y = rnorm(300) #by default, mean=0, sd=1.
alt.y = rnorm(300, mean=carrier/2)
## Note that rnorm() will take a vector of means - the random value returned for the ith element will be drawn from a normal distribution with the ith mean.  Here, the first 100 values will be drawn from a normal with a mean of zero whereas the next 200 will be drawn from a normal with a mean of 0.5, note carrier/2.
```

Just for the sake of comparison, deploy the appropriate parametric test for the difference in means.

``` r
t.test(null.y~carrier, var.equal=TRUE)
```

    ## 
    ##  Two Sample t-test
    ## 
    ## data:  null.y by carrier
    ## t = 0.78312, df = 298, p-value = 0.4342
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.1375898  0.3194718
    ## sample estimates:
    ## mean in group 0 mean in group 1 
    ##      0.10551389      0.01457289

``` r
t.test(alt.y~carrier, var.equal=TRUE)
```

    ## 
    ##  Two Sample t-test
    ## 
    ## data:  alt.y by carrier
    ## t = -4.1452, df = 298, p-value = 4.431e-05
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.8077091 -0.2876679
    ## sample estimates:
    ## mean in group 0 mean in group 1 
    ##       0.0315445       0.5792330

Now, define test statistics for randomization tests for the null and alternate distributions

``` r
null.diff = mean(null.y[carrier==1])-mean(null.y[carrier==0])
alt.diff = mean(alt.y[carrier==1])-mean(alt.y[carrier==0])
```

Generate 1000 random test statistics from the data.

``` r
random.test = function(x,y) {
sp = sample(x, replace=FALSE) # By default, it is without replacement. 
mean(y[sp==1])-mean(y[sp==0])
}

null.rand = replicate(1000, random.test(carrier, null.y))
alt.rand = replicate(1000, random.test(carrier, alt.y))
```

Plot your results and compare p-values for each distribution.

``` r
par(mfrow = c(1,2))
hist(null.rand, xlim=c(-0.6,0.6))
abline(v=null.diff, lwd=2, col="red")
hist(alt.rand, xlim=c(-0.6,0.6))
abline(v=alt.diff, lwd=2, col="red")
```

![](hypothesis_testing_files/figure-markdown_github/unnamed-chunk-27-1.png)

``` r
mean(abs(null.rand) > abs(null.diff))
```

    ## [1] 0.466

``` r
#take absolute values, since you can do .rand-.diff, or .diff-.rand. 
mean(abs(alt.rand) > abs(alt.diff))
```

    ## [1] 0

Results of t-tests and randomization tests are very comparable, which is expected. Since the data is sampled from a normal distribution with equal variance and the sample size is very big. Both assumptions of t.test are met. Rampling is nearly as powerful as parametric tests.

##### Bootstrapping

The bootstrap can be used to obtain the variance of a test statistic. Let's say we wish to set a 95% CI on the mean of our alternate hypothesis distribution (alt.y) from above. First, create a boostrapped distribution of the mean.

``` r
bp.mean = function (x) {
sp = sample(x, replace = TRUE)
mean(sp)
}
sp.means = replicate(1000, bp.mean(alt.y))
```

Next, find the variance of the boostrapped values without using the var() function. Compare your results to var().

``` r
sumsqr = sum((sp.means-mean(sp.means))^2)
variance = sumsqr* (1/(length(sp.means)-1))
variance
```

    ## [1] 0.004120182

``` r
var(sp.means)
```

    ## [1] 0.004120182

Now, calculate 95% CIs for the sample mean using this value and make a histogram showing where they fall. And compare this to the percentile method for 95% CIs we learned during lecture (97.5% and 2.5%).

``` r
CI_upper = mean(alt.y) + 1.96*sqrt(variance)
CI_lower = mean(alt.y) - 1.96*sqrt(variance)
CI_upper_percentile = sort(sp.means)[975]
CI_lower_percentile = sort(sp.means)[25]

par(mfrow = c(1,1))
hist(sp.means, col = "gray", breaks = 20)
abline(v = CI_upper, col = "red")
abline(v = CI_lower, col = "red")
abline(v = CI_upper_percentile, col = "blue")
abline(v = CI_lower_percentile, col = "blue")
```

![](hypothesis_testing_files/figure-markdown_github/unnamed-chunk-30-1.png)

Pretty close, as you can see.

Now let's repeat on a skewed distribution. First, we make up some skewed data.

``` r
skewed.data = rexp(15, 0.1)
```

Next, we create a boostrapped distribution of the mean.

``` r
sp.means.skewed = replicate(1000, bp.mean(skewed.data))
```

Next, find the variance of the boostrapped values without using the var() function. Compare your results to var().

``` r
sumsqr = sum((sp.means.skewed-mean(sp.means.skewed))^2)
variance = sumsqr* (1/(length(sp.means.skewed)-1))
variance
```

    ## [1] 9.314156

``` r
var(sp.means.skewed)
```

    ## [1] 9.314156

Now, calculate 95% CIs for the sample mean using this value and make a histogram showing where they fall. And then compare this to the percentile method for 95% CIs we learned during lecture.

``` r
CI_upper = mean(skewed.data) + 1.96*sqrt(variance)
CI_lower = mean(skewed.data) - 1.96*sqrt(variance)
CI_upper_percentile = sort(sp.means.skewed)[975]
CI_lower_percentile = sort(sp.means.skewed)[25]

par(mfrow=c(1,1))
hist(sp.means.skewed, col = "gray", breaks = 20)
abline(v = CI_upper, col = "red")
abline(v = CI_lower, col = "red")
abline(v = CI_upper_percentile, col = "blue")
abline(v = CI_lower_percentile, col = "blue")
```

![](hypothesis_testing_files/figure-markdown_github/unnamed-chunk-34-1.png)

As you can see, the two methods produce very different results on a skewed distribution. The percentile method is preferred in this case.
