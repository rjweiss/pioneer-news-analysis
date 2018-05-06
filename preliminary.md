---
title: "test_html"
output: 
  html_document:
    keep_md: true
---



We begin with a description of the population itself.

# Data cleaning and preparation

## Dwell, idle, and active time



## Distribution of activity during the study period

```r
visitdate_df = activity_tbl %>% 
  group_by(id, visit_start_date, stage) %>%
  count %>% collect

startdate_df = activity_tbl %>% 
  group_by(id, days_since_appearance, stage) %>%
  count %>% collect

ggplot(visitdate_df, aes(
  x=visit_start_date,
  y=n,
  color=stage
)) +
  geom_bar(stat='identity', width=.5) +
  guides(color=F) +
  theme_bw() + scale_color_solarized() + 
  labs(title = "Total domain dwell events observed during study by calendar date", subtitle = "Colored by enrollment period") +
  xlab('Date during study') + ylab('Total dwell events observed')
```

![](preliminary_files/figure-html/enrollment_plots-1.png)<!-- -->

```r
ggplot(startdate_df, aes(
  x=days_since_appearance,
  y=n,
  color=stage
)) +
  geom_bar(stat='identity', width=.5)+
  guides(color=F) +
  theme_bw() + scale_color_solarized() + 
  labs(title = "Total domain dwell events observed during study by enrollment date", subtitle = "Colored by enrollment period") +
  xlab('Days since enrollment') + ylab('Total dwell events observed')
```

![](preliminary_files/figure-html/enrollment_plots-2.png)<!-- -->

```r
ggplot(startdate_df, aes(
  x=days_since_appearance,
  y=log(n),
  group=days_since_appearance,
  color=stage)) +
  geom_boxplot() +
  guides(color=F) +
  theme_bw() + scale_color_solarized() +
  labs(title = "Logged total domain dwell events observed during study by enrollment date", subtitle = "Colored by enrollment period") +
  xlab('Days since enrollment') + ylab('Total dwell events observed')
```

![](preliminary_files/figure-html/enrollment_plots-3.png)<!-- -->

## General activity of enrollees over all study periods

```r
ecdf_activedays_df = activity_tbl %>%
  group_by(visit_start_date, branch, id) %>%
  count %>%
  filter(n>0)%>%
  group_by(id, branch)%>%
  count %>%
  collect

ggplot(ecdf_activedays_df, aes(nn)) + geom_histogram(binwidth=1,position='dodge') + 
  theme_bw()+
  labs(title = "Observed distribution of participant active days", subtitle = "Over all study periods")
```

![](preliminary_files/figure-html/daysactive_plots-1.png)<!-- -->

```r
ggplot(ecdf_activedays_df, aes(nn)) + stat_ecdf(geom='step') + 
  theme_bw() +
  labs(title = "Cumulative distribution of participant active days", subtitle = "Over all study periods")
```

![](preliminary_files/figure-html/daysactive_plots-2.png)<!-- -->

```r
ggplot(ecdf_activedays_df, aes(nn, fill=branch)) + geom_histogram(binwidth=1,position='dodge') + 
  scale_fill_canva(palette='Clean and crisp') +
  labs(title = "Observed distribution of participant active days by branch", subtitle = "By branch")
```

![](preliminary_files/figure-html/daysactive_plots-3.png)<!-- -->

```r
ggplot(ecdf_activedays_df, aes(nn, color=branch)) + stat_ecdf(geom='step') + theme_bw()+ 
  scale_color_canva(palette='Elegant and sophisticated') +
  labs(title = "Cumulative distribution of participant active days", subtitle = "By branch")
```

![](preliminary_files/figure-html/daysactive_plots-4.png)<!-- -->

### Without weekends?

```r
pdf_activewdays_df = activity_tbl %>%
  group_by(visit_start_date, branch, id) %>%
  count %>%
  collect %>%
  mutate(wday=wday(visit_start_date)) %>%
  filter(n>0)%>%
  ungroup %>% group_by(wday) %>%
  count %>% collect

ecdf_activewdays_df = activity_tbl %>%
  group_by(visit_start_date, branch, id) %>%
  count %>%
  collect %>%
  mutate(wday=wday(visit_start_date)) %>%
  filter(n>0 & wday !=1 & wday !=7)%>%
  group_by(id, branch)%>%
  count %>% collect

ggplot(ecdf_activewdays_df, aes(nn)) + 
  geom_histogram(binwidth=1,position='dodge') + 
  theme_bw() +
  labs(title = "New plot title", subtitle = "A subtitle")
```

![](preliminary_files/figure-html/wdaysactive_plots-1.png)<!-- -->

```r
ggplot(ecdf_activewdays_df, aes(nn)) + 
  stat_ecdf(geom='step') + 
  theme_bw() +
  labs(title = "New plot title", subtitle = "A subtitle")
```

![](preliminary_files/figure-html/wdaysactive_plots-2.png)<!-- -->

```r
ggplot(ecdf_activewdays_df, aes(nn, fill=branch)) + 
  geom_histogram(binwidth=1,position='dodge') + 
  theme_bw()+ 
  scale_fill_canva(palette='Sleek and modern') +
  labs(title = "New plot title", subtitle = "A subtitle")
```

![](preliminary_files/figure-html/wdaysactive_plots-3.png)<!-- -->

```r
ggplot(ecdf_activewdays_df, aes(nn, color=branch)) + 
  stat_ecdf(geom='step') + 
  theme_bw()+ 
  scale_color_canva(palette='Modern and minimal') +
  labs(title = "New plot title", subtitle = "A subtitle")
```

![](preliminary_files/figure-html/wdaysactive_plots-4.png)<!-- -->

```r
kable(quantile(ecdf_activewdays_df$nn, probs=c(0, .25, .5, .75, .99, .999, .9999, 1)))
```

           x
-------  ---
0%         1
25%        7
50%       13
75%       16
99%       17
99.9%     17
99.99%    17
100%      17

## Distribution of time of browsing


```r
# # time of day
activity_tbl %>%
  group_by(id, visit_start_time) %>%
  mutate(timestamp = unix_timestamp(as.character(visit_start_time)))
```

```
## # Source:   lazy query [?? x 15]
## # Database: spark_connection
## # Groups:   id, visit_start_time
##    pioneer_id branch document_ids visit_start_date domain visit_start_time
##    <chr>      <chr>  <list>       <date>           <chr>             <dbl>
##  1 15227a5e-… contr… <list [1]>   2018-04-16       kenne…       1523838305
##  2 15227a5e-… contr… <list [1]>   2018-04-16       usg.e…       1523840050
##  3 15227a5e-… contr… <list [1]>   2018-04-16       micro…       1523840051
##  4 15227a5e-… contr… <list [1]>   2018-04-16       credi…       1523840054
##  5 15227a5e-… contr… <list [1]>   2018-04-16       nav.c…       1523840061
##  6 15227a5e-… contr… <list [1]>   2018-04-16       credi…       1523840104
##  7 15227a5e-… contr… <list [1]>   2018-04-16       credi…       1523849104
##  8 15227a5e-… contr… <list [1]>   2018-04-16       kenne…       1523849107
##  9 15227a5e-… contr… <list [1]>   2018-04-16       nav.c…       1523849109
## 10 15227a5e-… contr… <list [1]>   2018-04-16       moder…       1523849110
## # ... with more rows, and 9 more variables: total_dwell_time <dbl>,
## #   total_idle_time <dbl>, nav_event_count <int>,
## #   days_since_appearance <int>, log_events <list>, id <dbl>, stage <chr>,
## #   total_active_time <dbl>, timestamp <dbl>
```


## Average number of pageviews per day

```r
daily_pagevisits = activity_tbl %>%
  group_by(id,visit_start_date) %>%
  count %>%
  ungroup %>% group_by(id) %>%
  summarise(
    mu=mean(n, na.rm=T)
  ) %>%
  filter(mu<800) %>%
  collect

ggplot(daily_pagevisits, aes(x=mu)) + 
  geom_histogram(binwidth=1) + 
  theme_bw() +
  labs(title = "New plot title", subtitle = "A subtitle")
```

![](preliminary_files/figure-html/avg_pv_plots-1.png)<!-- -->

```r
ggplot(daily_pagevisits, aes(x=mu)) + 
  stat_ecdf() + 
  theme_bw() +
  labs(title = "New plot title", subtitle = "A subtitle")
```

![](preliminary_files/figure-html/avg_pv_plots-2.png)<!-- -->

```r
ggplot(daily_pagevisits, aes(x=log(mu))) + 
  geom_histogram(binwidth=.01) + 
  theme_bw() +
  labs(title = "New plot title", subtitle = "A subtitle")
```

![](preliminary_files/figure-html/avg_pv_plots-3.png)<!-- -->

```r
ggplot(daily_pagevisits, aes(x=log(mu))) + 
  stat_ecdf() + 
  theme_bw() +
  labs(title = "New plot title", subtitle = "A subtitle")
```

![](preliminary_files/figure-html/avg_pv_plots-4.png)<!-- -->

```r
kable(quantile(daily_pagevisits$mu, probs=c(0, .25, .5, .75, .99, .999, .9999, 1)))
```

                  x
-------  ----------
0%          1.00000
25%        19.50000
50%        40.66667
75%        81.77124
99%       374.78551
99.9%     666.11930
99.99%    768.28998
100%      799.05263

### Average total active browsing per day

```r
daily_active_browsing = activity_tbl %>%
  group_by(id,days_since_appearance) %>%
  summarise(total_s=sum(total_active_time, na.rm=T)) %>%
  filter(total_s<17290.93) %>%
  mutate(total_m=total_s/60) %>%
  mutate(total_h=total_m/24) %>%
  filter(total_m>1)%>%
  collect

ggplot(daily_active_browsing, aes(x=total_h)) + 
  stat_ecdf() + 
  theme_bw() +
  labs(title = "New plot title", subtitle = "A subtitle")
```

![](preliminary_files/figure-html/avg_ab_plots-1.png)<!-- -->

```r
ggplot(daily_active_browsing, aes(x=log(total_h))) + 
  stat_ecdf() + 
  theme_bw() +
  labs(title = "New plot title", subtitle = "A subtitle")
```

![](preliminary_files/figure-html/avg_ab_plots-2.png)<!-- -->

```r
ggplot(daily_active_browsing, aes(x=total_m)) + 
  stat_ecdf() + 
  theme_bw() +
  labs(title = "New plot title", subtitle = "A subtitle")
```

![](preliminary_files/figure-html/avg_ab_plots-3.png)<!-- -->

```r
ggplot(daily_active_browsing, aes(x=log(total_m))) + 
  stat_ecdf() + 
  theme_bw() +
  labs(title = "New plot title", subtitle = "A subtitle")
```

![](preliminary_files/figure-html/avg_ab_plots-4.png)<!-- -->

```r
ggplot(daily_active_browsing, aes(x=total_h)) + 
  geom_histogram(bins=1000, labels=comma) + 
  theme_bw() +
  labs(title = "New plot title", subtitle = "A subtitle")
```

```
## Warning: Ignoring unknown parameters: labels
```

![](preliminary_files/figure-html/avg_ab_plots-5.png)<!-- -->

```r
ggplot(daily_active_browsing, aes(x=total_m)) + 
  geom_histogram(bins=1000, labels=comma) + 
  theme_bw() +
  labs(title = "New plot title", subtitle = "A subtitle")
```

```
## Warning: Ignoring unknown parameters: labels
```

![](preliminary_files/figure-html/avg_ab_plots-6.png)<!-- -->

```r
ggplot(daily_active_browsing, aes(x=log(total_m))) + 
  geom_histogram(bins=1000, labels=comma) + 
  theme_bw() +
  labs(title = "New plot title", subtitle = "A subtitle")
```

```
## Warning: Ignoring unknown parameters: labels
```

![](preliminary_files/figure-html/avg_ab_plots-7.png)<!-- -->

```r
kable(quantile(daily_active_browsing$total_m, probs=c(0, .25, .5, .75, .99, .999, .9999, 1)))
```

                   x
-------  -----------
0%          1.016667
25%         6.150000
50%        15.316667
75%        33.366667
99%       146.133333
99.9%     238.725067
99.99%    279.778267
100%      288.116667



### Length of average site visit

```r
daily_avg_activity=activity_tbl %>%
   group_by(id, visit_start_date) %>%
   summarise(
    mean=mean(total_active_time, na.rm=T),
    n = n()) %>%
  ungroup %>% group_by(visit_start_date) %>%
  filter(mean>1) %>%
  collect

ggplot(daily_avg_activity, aes(x=mean)) + 
  geom_histogram(binwidth=.1) + 
  theme_bw()
```

![](preliminary_files/figure-html/avg_sv_plots-1.png)<!-- -->

```r
ggplot(daily_avg_activity, aes(x=log(mean))) + 
  geom_histogram(binwidth=.01) + 
  theme_bw()
```

![](preliminary_files/figure-html/avg_sv_plots-2.png)<!-- -->

```r
ggplot(daily_avg_activity, aes(x=visit_start_date, y=mean, group=visit_start_date)) + 
  geom_boxplot() + 
  theme_bw()
```

![](preliminary_files/figure-html/avg_sv_plots-3.png)<!-- -->

```r
ggplot(daily_avg_activity, aes(x=visit_start_date, y=log(mean), group=visit_start_date)) + 
  geom_boxplot() + 
  theme_bw()
```

![](preliminary_files/figure-html/avg_sv_plots-4.png)<!-- -->

```r
kable(quantile(daily_avg_activity$mean, probs=c(0, .25, .5, .75, .99, .999, .9999, 1)))
```

                    x
-------  ------------
0%           1.004878
25%         10.600000
50%         16.852941
75%         28.214286
99%        178.492588
99.9%      647.046000
99.99%    2172.125680
100%      4161.000000

## Self report

### Demographics: Partisan ID, Age, Gender

### Browsing patterns against self report

# Analysis

## Scored domains

```r
scored_activity_tbl = activity_tbl %>% 
  inner_join(scored_domains_tbl, copy=T) %>% 
  select(-domain2, -pioneer_id) %>%
  mutate(scored_active_time = total_active_time * abs(score)) # every session's active time * absolute value of the score
```

```
## Joining, by = "domain"
```

```r
align_dist = scored_activity_tbl %>%
  group_by(id, score, branch, stage) %>%
  summarise(scored_active=mean(log(total_active_time), na.rm=T)*abs(score)) %>%
  filter(stage != 'treatment') %>%
  collect
  
ggplot(align_dist, aes(
  x=scored_active,
  fill=stage)) + 
  geom_histogram(binwidth=.01, position='dodge') + 
  facet_wrap(~branch, ncol=1) +
  labs(title = "New plot title", subtitle = "A subtitle")
```

```
## Warning: Removed 17613 rows containing non-finite values (stat_bin).
```

![](preliminary_files/figure-html/align_activity-1.png)<!-- -->


## Distribution of visits across scored domains

```r
scaled_browsing = scored_activity_tbl %>%
  group_by(id, stage, branch, days_since_appearance) %>%
  summarise(
    scored_active_time_s = sum(scored_active_time, na.rm=T),
    total_active_time_s = sum(total_active_time, na.rm=T)) %>%
  mutate(scaled_active_bias_s = scored_active_time_s/total_active_time_s)

treatment_spark = scaled_browsing %>%
  sdf_pivot(id + days_since_appearance + branch + scaled_active_bias_s ~ stage) %>%
  na.replace(0) %>%
  mutate(pretreatment = pretreatment * scaled_active_bias_s) %>%
  mutate(treatment = treatment * scaled_active_bias_s) %>%
  mutate(posttreatment = posttreatment * scaled_active_bias_s) %>%
  mutate(
    condition = case_when(
      branch=='control'~'Control',
      branch=='treatment-bias'~'Treatment',
      branch=='treatment-whois'~'APlacebo'
    ))

fit1 = glm(data=treatment_spark, formula=posttreatment~pretreatment + condition)
summary(fit1)
```

```
## 
## Call:
## glm(formula = posttreatment ~ pretreatment + condition, data = treatment_spark)
## 
## Deviance Residuals: 
##      Min        1Q    Median        3Q       Max  
## -0.09390  -0.09061  -0.06793   0.01101   0.91518  
## 
## Coefficients:
##                      Estimate Std. Error t value Pr(>|t|)    
## (Intercept)         0.0938976  0.0007036 133.460  < 2e-16 ***
## pretreatment       -0.2091995  0.0023345 -89.612  < 2e-16 ***
## conditionControl   -0.0013789  0.0009668  -1.426 0.153830    
## conditionTreatment -0.0032830  0.0009625  -3.411 0.000648 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.02604761)
## 
##     Null deviance: 4567.0  on 167297  degrees of freedom
## Residual deviance: 4357.6  on 167294  degrees of freedom
## AIC: -135497
## 
## Number of Fisher Scoring iterations: 2
```

```r
fit2 = glm(data=treatment_spark, formula=posttreatment~pretreatment*condition + condition)
summary(fit2)
```

```
## 
## Call:
## glm(formula = posttreatment ~ pretreatment * condition + condition, 
##     data = treatment_spark)
## 
## Deviance Residuals: 
##      Min        1Q    Median        3Q       Max  
## -0.09400  -0.09022  -0.06796   0.01080   0.91489  
## 
## Coefficients:
##                                   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                      0.0940023  0.0007489 125.523  < 2e-16 ***
## pretreatment                    -0.2104982  0.0039466 -53.336  < 2e-16 ***
## conditionControl                -0.0011904  0.0010671  -1.116 0.264597    
## conditionTreatment              -0.0037785  0.0010620  -3.558 0.000374 ***
## pretreatment:conditionControl   -0.0024866  0.0057183  -0.435 0.663672    
## pretreatment:conditionTreatment  0.0062984  0.0056568   1.113 0.265524    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.02604754)
## 
##     Null deviance: 4567.0  on 167297  degrees of freedom
## Residual deviance: 4357.5  on 167292  degrees of freedom
## AIC: -135496
## 
## Number of Fisher Scoring iterations: 2
```

```r
dat = ggpredict(fit2, terms = c('pretreatment','condition'))
```

```
## Following variables had many unique values and were prettified: pretreatment. Use `pretty = FALSE` to get smoother plots with all values, however, at the cost of increased memory usage.
```

```r
plot(dat, facet=T) + theme_bw()
```

![](preliminary_files/figure-html/maineffect-1.png)<!-- -->


```r
plot = scaled_browsing %>% group_by(id, days_since_appearance) %>% select(id, days_since_appearance, scaled_active_bias_s) %>% collect

ggplot(plot, aes(
  x=scaled_active_bias_s
)) + geom_density() +
  theme_bw()
```

```
## Warning: Removed 9824 rows containing non-finite values (stat_density).
```

![](preliminary_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

