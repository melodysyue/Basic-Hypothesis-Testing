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
-   [Power Analysis](#power-analysis)
    -   [Power/sample size curve.](#powersample-size-curve.)
    -   [Power/alpha curve](#poweralpha-curve)
    -   [Power/effect size curve.](#powereffect-size-curve.)
    -   [Compare the power between WRST and t test.](#compare-the-power-between-wrst-and-t-test.)
-   [Analysis of Variance (ANOVA)](#analysis-of-variance-anova)
    -   [One-Way Anova](#one-way-anova)
        -   [Example 1](#example-1)
        -   [Example 2](#example-2)
    -   [Two-way ANOVA](#two-way-anova)
        -   [Example 1](#example-1-1)
        -   [Example 2](#example-2-1)
-   [Multiple hypotheses testing](#multiple-hypotheses-testing)
    -   [Family-wise error rate (FWER)](#family-wise-error-rate-fwer)
    -   [B-H False discovery rate (FDR)](#b-h-false-discovery-rate-fdr)
    -   [Example: gene expression differences between European and African populations.](#example-gene-expression-differences-between-european-and-african-populations.)

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
    ## s = 55, p-value = 0.3682
    ## alternative hypothesis: true median is not equal to 2
    ## 95 percent confidence interval:
    ##  1.956654 2.297322
    ## sample estimates:
    ## median of x 
    ##    2.123881 
    ## 
    ## Achieved and Interpolated Confidence Intervals: 
    ## 
    ##                   Conf.Level L.E.pt U.E.pt
    ## Lower Achieved CI     0.9431 1.9597 2.2970
    ## Interpolated CI       0.9500 1.9567 2.2973
    ## Upper Achieved CI     0.9648 1.9500 2.2981

What about using one-sample t-test on this?

``` r
t.test(x,mu=2)
```

    ## 
    ##  One Sample t-test
    ## 
    ## data:  x
    ## t = 0.25453, df = 99, p-value = 0.7996
    ## alternative hypothesis: true mean is not equal to 2
    ## 95 percent confidence interval:
    ##  1.822595 2.229615
    ## sample estimates:
    ## mean of x 
    ##  2.026105

#### Wilconxon Signed-Rank Test

Background: Though the sign test is extremely simple to perform, one obvious disadvantage is that it does not take the maginitude of the observation into account and may reduce the statistical power of the test. The alternative that accounts for the magnitude of the observations is the Wilcoxon signed rank test. It tests the equality of matched pairs of observations. It assumes the distribution is symmetrical. The null hypothesis is that both distributions are the same.

``` r
wilcox.test(x, mu = 2) #it use mean (mu) instead of median (md)
```

    ## 
    ##  Wilcoxon signed rank test with continuity correction
    ## 
    ## data:  x
    ## V = 2698, p-value = 0.5531
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
    ## t = -4.5135, df = 99, p-value = 1.759e-05
    ## alternative hypothesis: true mean is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.9546931 -0.3716247
    ## sample estimates:
    ##  mean of x 
    ## -0.6631589

``` r
SIGN.test(dif,md=0)
```

    ## 
    ##  One-sample Sign-Test
    ## 
    ## data:  dif
    ## s = 36, p-value = 0.006637
    ## alternative hypothesis: true median is not equal to 0
    ## 95 percent confidence interval:
    ##  -1.30823004 -0.07847351
    ## sample estimates:
    ## median of x 
    ##  -0.6878422 
    ## 
    ## Achieved and Interpolated Confidence Intervals: 
    ## 
    ##                   Conf.Level  L.E.pt  U.E.pt
    ## Lower Achieved CI     0.9431 -1.3035 -0.0892
    ## Interpolated CI       0.9500 -1.3082 -0.0785
    ## Upper Achieved CI     0.9648 -1.3184 -0.0555

``` r
wilcox.test(dif,mu=0)
```

    ## 
    ##  Wilcoxon signed rank test with continuity correction
    ## 
    ## data:  dif
    ## V = 1351, p-value = 5.463e-05
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
    ## W = 5190, p-value = 0.6433
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
    ## W = 550160, p-value = 0.0001026
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
    ## Kruskal-Wallis chi-squared = 0.27693, df = 2, p-value = 0.8707

OK, now double the shape parameter for d3 and test again

``` r
d3=rgamma(100,shape=4)
kruskal.test(list(d1,d2,d3))
```

    ## 
    ##  Kruskal-Wallis rank sum test
    ## 
    ## data:  list(d1, d2, d3)
    ## Kruskal-Wallis chi-squared = 88.239, df = 2, p-value < 2.2e-16

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
    ## d2 0.46    -      
    ## d3 2.3e-12 2.9e-13
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
    ## t = -2.2358, df = 298, p-value = 0.02611
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.51065849 -0.03253247
    ## sample estimates:
    ## mean in group 0 mean in group 1 
    ##     -0.20381539      0.06778009

``` r
t.test(alt.y~carrier, var.equal=TRUE)
```

    ## 
    ##  Two Sample t-test
    ## 
    ## data:  alt.y by carrier
    ## t = -3.5099, df = 298, p-value = 0.0005176
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.6916571 -0.1946891
    ## sample estimates:
    ## mean in group 0 mean in group 1 
    ##      0.05300897      0.49618209

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

    ## [1] 0.02

``` r
#take absolute values, since you can do .rand-.diff, or .diff-.rand. 
mean(abs(alt.rand) > abs(alt.diff))
```

    ## [1] 0

Results of t-tests and randomization tests are very comparable, which is expected. Since the data is sampled from a normal distribution with equal variance and the sample size is very big. Both assumptions of t.test are met. Rampling is nearly as powerful as parametric tests.

##### Bootstrapping

The bootstrap can be used to obtain the variance of a test statistic. For example, obtain 1000 bootstrap samples from the data, for each bootstrap sample, calculate the mean, use sampling distribution to compute the sample mean and variance. According to the central limit theorem (CLT), the distribution of the sample means will be approximately normal distributed, and the mean of all samples from the sample population will be approximately equal to the mean of the population. Let's say we wish to set a 95% CI on the mean of our alternate hypothesis distribution (alt.y) from above. First, create a boostrapped distribution of the mean.

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

    ## [1] 0.003401459

``` r
var(sp.means)
```

    ## [1] 0.003401459

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

    ## [1] 7.891829

``` r
var(sp.means.skewed)
```

    ## [1] 7.891829

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

Power Analysis
--------------

Power of a statistical test is the probability of rejecting the null when the alternative is true, also called as true positive rate. It is common to aim for a power = 0.8 or greater, e. g. an 80% of chance you will correctly reject the null hypothesis. To do a power analysis, you need variability *σ*<sup>2</sup>, and effect size *Δ*. Simply put, power analysis will tell you what effect size you can actually detect given the test you intend to perform.

First, let's get the "pwr" package. Pwr contains power functions for most parametric tests.

``` r
library(pwr)
```

Use pwr.t.test to find the power of a two sided, two-sample t-test assuming an effect size of 0.5 (50% increase/decrease), an alpha of 0.05 and an n of 5.

``` r
pwr.t.test(n = 5, d = 0.5, sig.level = 0.05, type = "two.sample", alternative = "two.sided") #d is Effect Size. 
```

    ## 
    ##      Two-sample t test power calculation 
    ## 
    ##               n = 5
    ##               d = 0.5
    ##       sig.level = 0.05
    ##           power = 0.107686
    ##     alternative = two.sided
    ## 
    ## NOTE: n is number in *each* group

It tells us only 11% of chance you will correctly reject the null hypothesis given this study design.

##### Power/sample size curve.

``` r
sizes=pwr.t.test(n=seq(5,200,25),d=0.5,sig.level=0.05,type="two.sample",alternative="two.sided")
df.size=data.frame("size"=seq(5,200,25),"power"=sizes$power)
ggplot(df.size,aes(x=size,y=power))+
  geom_line(col="red")+
  theme_classic()
```

![](hypothesis_testing_files/figure-markdown_github/unnamed-chunk-37-1.png)

##### Power/alpha curve

``` r
alphas=pwr.t.test(n=5,d=0.5,sig.level=seq(0,1,0.001),type="two.sample",alternative="two.sided")
df.alpha=data.frame("alpha"=seq(0,1,0.001),"power"=alphas$power)
ggplot(df.alpha,aes(x=alpha,y=power))+
  geom_line(col="red")+
  theme_classic()
```

![](hypothesis_testing_files/figure-markdown_github/unnamed-chunk-38-1.png)

##### Power/effect size curve.

``` r
effects=pwr.t.test(n=5,d=seq(0,4,0.01),sig.level=0.05,type="two.sample",alternative="two.sided")
df.effect=data.frame("effect.size"=seq(0,4,0.01),"power"=effects$power)
ggplot(df.effect,aes(x=effect.size,y=power))+
  geom_line(col="red")+
  theme_classic()
```

![](hypothesis_testing_files/figure-markdown_github/unnamed-chunk-39-1.png)

##### Compare the power between WRST and t test.

Next, use an empirical approach to derive the power of the Wilcoxon Rank Sum for data from two random normal distributions, one with a mean of 0 and another with a mean of 1. Try sample sizes ranging from 5 to 50 and use a critical value of 0.05. Repeat 1,000 times for each sample size and measure power.

``` r
two.wilcox = function(x) {
wilcox.test(rnorm(x), rnorm(x, mean = 1))$p.value <= 0.05
}
powers.wilcox = sapply(seq(5, 50, 5), function(x) {sum(replicate(1000, two.wilcox(x)))/1000})

two.ttest = function(x) {
t.test(rnorm(x), rnorm(x, mean = 1))$p.value <= 0.05
}

powers.ttest = sapply(seq(5, 50, 5), function(x) {sum(replicate(1000, two.ttest(x)))/1000})

df=data.frame("size"=seq(5,50,5),"WRST"=powers.wilcox,"ttest"=powers.ttest)

ggplot(df,aes(x=size,y=ttest))+
  geom_line(col="red",lwd=1)+
  geom_line(aes(y=WRST),col="blue",lwd=1)+
  theme_classic()
```

![](hypothesis_testing_files/figure-markdown_github/unnamed-chunk-40-1.png)

Conclusion: WRST is nearly as powerful as the t-test, even when the assumptions of the t-test are met.

Analysis of Variance (ANOVA)
----------------------------

ANOVA is a collection of statistical models used to analyze the differences between group means. It is about partitioning total variation.

Total variation: SST (sum of squared deviations about the grand mean across all N observations)
$$
SST=\\sum\_{i=1}^{K}\\sum\_{j=1}^{J}(x\_{ij}-\\bar{x})^2
$$
 Between group variation: *S**S**T*<sub>*G*</sub>(sum of squared deviations for each group mean about the grand mean)

$$
SST\_G=\\sum\_{i=1}^{K}n\_i\\cdot(\\bar{x\_i}-\\bar{x})^2  
$$
$$
MST\_G=\\frac{\\sum\_{i=1}^{K}n\_i\\cdot(\\bar{x\_i}-\\bar{x})^2}{K-1}  
$$

Within group variation (also called ERROR): *S**S**T*<sub>*E*</sub> (sum of squared deviations for all observations within each group from that group mean, summed across all groups)

$$
SST\_E=\\sum\_{i=1}^{K}\\sum\_{j=1}^{J}(x\_{ij}-\\bar{x\_i})^2
$$
$$
MST\_E=\\frac{\\sum\_{i=1}^{K}\\sum\_{j=1}^{J}(x\_{ij}-\\bar{x\_i})^2}{N-K}
$$
 Total variation can be partitioned into between-group variation and within-group variation.
*S**S**T* = *S**S**T*<sub>*G*</sub> + *S**S**T*<sub>*E*</sub>

The F-test: F statistic has an F-distribution under the null hypothesis. You cannot use the ratio between *S**S**T*<sub>*G*</sub> and *S**S**T*<sub>*E*</sub>, since *S**S**T*<sub>*G*</sub> tends to be smaller than *S**S**T*<sub>*E*</sub>. So we need to standardize these two terms by the degrees of freedom.
$$
F=\\frac{MST\_G}{MST\_E}
$$

### One-Way Anova

The term one-way, also called one-factor, indicates that there is a single explanatory variable "treatment" with two or more levels, and only one lefvel of treatment is applied at any time for a given subject.

#### Example 1

``` r
ff <- read.table(file="http://www.cs.washington.edu/homes/suinlee/genome560/data/example_ANOVA.txt")  
head(ff)
```

    ##   grp        x
    ## 1   1 29.11601
    ## 2   1 29.00317
    ## 3   1 28.75443
    ## 4   1 29.00025
    ## 5   1 28.56627
    ## 6   1 28.84333

``` r
mm <- lm(x ~ factor(grp), data=ff) 
anova(mm) 
```

    ## Analysis of Variance Table
    ## 
    ## Response: x
    ##             Df Sum Sq Mean Sq F value    Pr(>F)    
    ## factor(grp)  3 43.774 14.5912  215.33 < 2.2e-16 ***
    ## Residuals   36  2.439  0.0678                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
mm ## it gives you the means of different groups. Use the first group as the reference, and means for other groups will be plus/minus the coefficients from the "intercept".
```

    ## 
    ## Call:
    ## lm(formula = x ~ factor(grp), data = ff)
    ## 
    ## Coefficients:
    ##  (Intercept)  factor(grp)2  factor(grp)3  factor(grp)4  
    ##      28.8418        2.0495        1.7954       -0.3022

#### Example 2

These are (real) measurements of adductor muscle scar length on shells of mussels collected at a number of localities. A one-way ANOVA tests whether those localities differ. The numerical value of variable V1 is to be used as a "factor", so that it simply indicates group membership and is not a numerical value of interest.

``` r
gg <- read.table(file="http://www.cs.washington.edu/homes/suinlee/genome560/data/mussels.txt") 
head(gg)
```

    ##          V1     V2
    ## 1 Tillamook 0.0571
    ## 2 Tillamook 0.0813
    ## 3 Tillamook 0.0831
    ## 4 Tillamook 0.0976
    ## 5 Tillamook 0.0817
    ## 6 Tillamook 0.0859

``` r
str(gg)
```

    ## 'data.frame':    39 obs. of  2 variables:
    ##  $ V1: Factor w/ 5 levels "Magadan","Newport",..: 4 4 4 4 4 4 4 4 4 4 ...
    ##  $ V2: num  0.0571 0.0813 0.0831 0.0976 0.0817 0.0859 0.0735 0.0659 0.0923 0.0836 ...

``` r
m2=lm(V2~V1, data=gg)
anova(m2)
```

    ## Analysis of Variance Table
    ## 
    ## Response: V2
    ##           Df    Sum Sq    Mean Sq F value    Pr(>F)    
    ## V1         4 0.0045197 0.00112992   7.121 0.0002812 ***
    ## Residuals 34 0.0053949 0.00015867                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
m2
```

    ## 
    ## Call:
    ## lm(formula = V2 ~ V1, data = gg)
    ## 
    ## Coefficients:
    ##  (Intercept)     V1Newport  V1Petersburg   V1Tillamook   V1Tvarminne  
    ##     0.078012     -0.003213      0.025430      0.002187      0.017687

### Two-way ANOVA

#### Example 1

Suppose we have two capsule types (C or V) and two digestive fluids (Gastric or Duodenal). We randomly assign 5 capsules of each type to each type of digesetive juice and observe dissolve time. We are going to do a two-way analysis of variance where there the “rows” might be the type of digestive juice and the “columns” capsule type (so a 2 x 2 table) with 5 data points in each cell.

``` r
gg <- read.table(file="http://www.cs.washington.edu/homes/suinlee/genome560/data/dissolve.txt") 
m1 <- lm(V4 ~ V2*V3, data=gg)
anova(m1) ## when there is an interaction, we don't trust the rest of the table. It means both main effects have impacts. 
```

    ## Analysis of Variance Table
    ## 
    ## Response: V4
    ##           Df Sum Sq Mean Sq F value   Pr(>F)   
    ## V2         1 151.25  151.25  5.0232 0.039542 * 
    ## V3         1   0.20    0.20  0.0066 0.936055   
    ## V2:V3      1 320.00  320.00 10.6277 0.004916 **
    ## Residuals 16 481.76   30.11                    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#### Example 2

Mannose phosphate isomerase enzyme had its enzyme activity measured in some fish of three genotypes and two sexes, in multiple fish of the same species. (The genotypes ff, fs, and ss refer to the two alleles at the locus, which in old-fashioned protein electrophoresis were the “fast” and “slow” alleles which refers to where they move on the gel and not to enzyme activity). We are going to do a two-way analysis of variance where there the “rows” might be the sexes and the “columns” genotypes (so a 2 x 3 table) with multiple data points in each cell.

``` r
hh <- read.table(file="http://www.cs.washington.edu/homes/suinlee/genome560/data/mpi.txt")      
m2 <- lm(V4 ~ V2*V3, data=hh)
anova(m2) ## When there is no interaction, then we  can trust the rest of the table. The values will be similar as the one-way ANOVA results. 
```

    ## Analysis of Variance Table
    ## 
    ## Response: V4
    ##           Df  Sum Sq Mean Sq F value Pr(>F)
    ## V2         1  0.0681 0.06808  0.0861 0.7712
    ## V3         2  0.2772 0.13862  0.1754 0.8400
    ## V2:V3      2  0.8146 0.40732  0.5153 0.6025
    ## Residuals 30 23.7138 0.79046

Conclusion: When check for two-way ANOVA table, you always check for the interaction term first, then look at the individual main effects.

Multiple hypotheses testing
---------------------------

P(Making an false positive error in 1 test)= *α*
P(Not making a false positive error in 1 test)=1 − *α*
P(Not making an false positive error in m tests)=(1 − *α*)<sup>*m*</sup> P(Making at least 1 false positive error in m tests)=1 − (1 − *α*)<sup>*m*</sup>

``` r
m=seq(1,200,1)
p=1-(1-0.05)^m
plot(p~m)
```

![](hypothesis_testing_files/figure-markdown_github/unnamed-chunk-45-1.png)

When m is large, you will for sure make at least one false positve error! So what can we do to avoid this? Ajust p value!

### Family-wise error rate (FWER)

Family referes to the collection of the hypotheses. FWER will try to minimize P(making at least one false positive among all tests).

*Single Step (equivalent adjustment to all tests): Bonferroni*

The probability of making even one false positive error in the family is controlled at the level of alpha. Bonferroni method rejects hypotheses when $p&lt;\\frac{\\alpha}{m}$. This is adjusted p value and it is very stringent. However, it often leaves very few hypothesies that are deemed significant.

*Sequential adjustment: Holm's method*

It orders the unadjusted p values first such that p1&lt;p2&lt;...pm. We do not mulitply every p by the same factor m.

### B-H False discovery rate (FDR)

When you are not worried about making ONE mistake, and you just want to lower the false discovery rate.

False rejection/total rejection

### Example: gene expression differences between European and African populations.

``` r
a <- read.table(header = T, file="http://www.cs.washington.edu/homes/suinlee/genome560/RMA_Filtered.txt")
dim(a)
```

    ## [1] 5194   33

``` r
b=a[,2:33]
dim(b)
```

    ## [1] 5194   32

``` r
fun=function(d){
  return(t.test(d[1:16], d[17:32])$p.value)
}
p=apply(b,1,fun) ##apply the function for each row
head(p)
```

    ## [1] 0.002156265 0.883625427 0.972905815 0.607205923 0.368235513 0.484618218

``` r
hist(p, breaks=40)
```

![](hypothesis_testing_files/figure-markdown_github/unnamed-chunk-46-1.png)

``` r
tabulate(as.numeric(p<0.05)) #count
```

    ## [1] 705

``` r
tabulate(as.numeric(p<0.01))
```

    ## [1] 258

``` r
length(p)*0.05
```

    ## [1] 259.7

``` r
length(p)*0.01
```

    ## [1] 51.94

``` r
p_bon=p.adjust(p,"bonferroni")
p_hol=p.adjust(p,"holm")
p_bh=p.adjust(p,"BH")
alpha=0.05
tabulate(as.numeric(p_bon<alpha))
```

    ## [1] 2

``` r
which(p_bon<alpha)
```

    ## [1] 2186 2670

``` r
tabulate(as.numeric(p_hol<alpha))
```

    ## [1] 2

``` r
which(p_hol<alpha)
```

    ## [1] 2186 2670

``` r
tabulate(as.numeric(p_bh<alpha))
```

    ## [1] 36

``` r
which(p_bh<alpha)
```

    ##  [1]   17  371  418  602  828  865  926 1216 1232 1454 1465 1700 1833 1998
    ## [15] 2035 2186 2325 2363 2644 2670 2798 2861 3340 3420 3465 3587 3599 3779
    ## [29] 4125 4134 4212 4237 4274 4661 5026 5190

You can see BH-FDR is more leninet than Bonferroni and Holm methods. Now let's compare the distribution of p value before and after adjustment.

``` r
par(mfrow=c(2,2))
hist(p,breaks=40)
hist(p_bon, breaks=40)
hist(p_hol, breaks=40)
hist(p_bh, breaks=40)
```

![](hypothesis_testing_files/figure-markdown_github/unnamed-chunk-47-1.png)
