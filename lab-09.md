Lab 09 - Grading the professor, Pt. 1. Lab: Modeling professor
attractiveness and course evaluations
================
Yuxin Xie
3/5/25

## Load Packages and Data

``` r
#library(tidyverse) 
#library(tidymodels)
#library(openintro)

library(tidyverse)
library(broom)
library(openintro)
```

\##background info the use of these student evaluations as an indicator
of course quality and teaching effectiveness is often criticized because
these measures may reflect the influence of non-teaching related
characteristics, such as the physical appearance of the instructor. The
article titled, “Beauty in the classroom: instructors’ pulchritude and
putative pedagogical productivity” (Hamermesh and Parker, 2005) found
that instructors who are viewed to be better looking receive higher
instructional ratings.

The data were gathered from end of semester student evaluations for a
large sample of professors from the University of Texas at Austin. In
addition, six students rated the professors’ physical appearance. (This
is a slightly modified version of the original data set that was
released as part of the replication data for Data Analysis Using
Regression and Multilevel/Hierarchical Models (Gelman and Hill, 2007).)

The result is a data frame where each row contains a different course
and columns represent variables about the courses and professors.

The dataset we’ll be using is called evals from the openintro package.
Take a peek at the codebook with ?evals. 463 courses 23 variables.

## Exercise Part 1: Exploratory Data Analysis

``` r
data(evals)

summary(evals$score)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   2.300   3.800   4.300   4.175   4.600   5.000

``` r
ggplot(evals, aes(x = score)) +
  geom_histogram(binwidth = 0.25, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Course Evaluation Scores",
       x = "Score",
       y = "Count") +
  theme_minimal()
```

![](lab-09_files/figure-gfm/1-1.png)<!-- -->

``` r
#yes, the distribution is left skewed. This means most ratings are high, with a small number of lower ratings. 
# min is 2.3, max is 5, mean is 4.175, median is 4.3, mean<median, left skewed
# this is what i expected to see. because students tend to rate higher. 
```

``` r
# the relationship between score and the variable bty_avg, a professor’s average beauty rating.
ggplot(evals, aes(x = bty_avg, y = score)) +
  geom_point(alpha = 0.5, color = "steelblue") +  # Scatter points
  geom_smooth(method = "lm", color = "red", se = TRUE) +  # Regression line
  labs(title = "Relationship Between Beauty Rating and Course Score",
       x = "Professor's Average Beauty Rating (bty_avg)",
       y = "Course Evaluation Score") +
  theme_minimal()
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](lab-09_files/figure-gfm/2-1.png)<!-- -->

``` r
#just by looking at it, there is a weak positive relationship between average beauty rating and score. 
```

``` r
#"The jitter geom is a convenient shortcut for geom_point(position = "jitter"). It adds a small amount of random variation to the location of each point."
#"is a useful way of handling overplotting"
ggplot(evals, aes(x = bty_avg, y = score)) +
  geom_jitter(alpha = 0.4, color = "steelblue") +  
  geom_smooth(method = "lm", color = "red", se = TRUE) + 
  labs(title = "Relationship Between Beauty Rating and Course Score",
       x = "Professor's Average Beauty Rating",
       y = "Course Evaluation Score") +
  theme_minimal()
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](lab-09_files/figure-gfm/3-1.png)<!-- -->

``` r
##we can see that there are a lot of spots overlapping each other. 
##in the original graph, it is hard to tell the real density. 
```

## Exercise Part 2: Linear regression with a numerical predictor

``` r
#linear regression model 
m_bty<-lm(score~bty_avg, data=evals)
summary (m_bty)
```

    ## 
    ## Call:
    ## lm(formula = score ~ bty_avg, data = evals)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.9246 -0.3690  0.1420  0.3977  0.9309 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  3.88034    0.07614   50.96  < 2e-16 ***
    ## bty_avg      0.06664    0.01629    4.09 5.08e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5348 on 461 degrees of freedom
    ## Multiple R-squared:  0.03502,    Adjusted R-squared:  0.03293 
    ## F-statistic: 16.73 on 1 and 461 DF,  p-value: 5.083e-05

``` r
#the estimated intercept is 3.88, the estimated slope is 0.07

# score  = 0.07 bty_avg + 3.88
```

``` r
ggplot(evals, aes(x = bty_avg, y = score)) +
  geom_jitter(alpha = 0.4, color = "steelblue") +  
  geom_abline(intercept =3.88,slope = 0.07, color = "orange", linewidth = 1.5) + 
  labs(title = "Relationship Between Beauty Rating and Course Score",
       x = "Professor's Average Beauty Rating",
       y = "Course Evaluation Score") +
  theme_minimal()
```

![](lab-09_files/figure-gfm/5-1.png)<!-- -->

``` r
# score  = 0.07 bty_avg + 3.88
# the slope =0.07 means, when the average beauty rating inceases 1 unit, the rating score expected to increase 0.07 units. 
```

``` r
# score  = 0.07 bty_avg + 3.88
# the intercept = 3.88 means that when the average beauty score =0, the course rating score expected to be 3.88. In this case, a beauty rating of 0 is not realistic. 
```

``` r
summary(m_bty)$r.squared
```

    ## [1] 0.03502226

``` r
# the r squared is 0.035. that means bty_avg explains 3.5% of the variability in course evaluation scores. 96.5% of the course rating scores are due to some other factors. 
```

## Exercise Part 3

``` r
## Hint
#For Exercise 12, the `relevel()` function can be helpful!
```
