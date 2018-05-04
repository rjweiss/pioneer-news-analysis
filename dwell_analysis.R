#install.packages('lubridate')
#install.packages('stringr')
library(stringr)
library(lubridate)
library(sparklyr)
library(dplyr)
library(ggplot2)
library(scales)
library(jsonlite)
library(stringr)
library(fuzzyjoin)

#spark config

Sys.setenv(SPARK_HOME='/usr/lib/spark')
config <- spark_config()
config$spark.driver.memory <- "12G"
config$spark.executor.memory <- "4G"
config$spark.executor.core <- 3
config$spark.executor.instances <- 5
config$spark.sql.shuffle.partitions <- 320
sc <- spark_connect(master = "yarn-client", version = "1.6.2", config = config)

#dwell_parquet_dir = 's3://net-mozaws-data-us-west-2-data-pioneer-analysis/online_news_v2/dwell_time_complete/'
dwell_parquet_dir = 's3://net-mozaws-data-us-west-2-data-pioneer-analysis/online_news_v2/dwell_time_new_handling/'
dwell = spark_read_parquet(sc, name='dwell_time', path=dwell_parquet_dir, memory=TRUE)
sdf_register(dwell, "dwell_tbl")

bias_json = 'https://raw.githubusercontent.com/mozilla/pioneer-study-online-news-2/master/extension/bias-domains.json'
whois_json = 'https://raw.githubusercontent.com/mozilla/pioneer-study-online-news-2/master/extension/whois-domains.json'

bias_scores = fromJSON(bias_json)
whois_scores = fromJSON(whois_json)
bias_scores$domain = str_replace(bias_scores$domain, 'www.','')

domains = dwell %>% group_by(domain) %>% count %>% collect
sum(domains$domain %in% bias_scores$domain)

scored_domains = stringdist_inner_join(bias_scores, domains, by='domain', max_dist=0.5)
names(scored_domains) = c('domain', 'score','domain2','n')
sdf_copy_to(sc, scored_domains)
tbl_cache(sc, 'scored_domains')

scored_dwell = dwell %>% inner_join(scored_domains, copy=T)
sdf_register(scored_dwell, 'scored_dwell_tbl')
tbl_cache(sc, 'scored_dwell_tbl')

### some data transformations

scored_dwell_stages = scored_dwell %>% 
  mutate(stage = case_when(
    days_since_appearance == 0 ~ 'enrollment',
    days_since_appearance > 0 & days_since_appearance < 8 ~ 'pretreatment',
    days_since_appearance >= 8 & days_since_appearance < 15 ~ 'treatment',
    days_since_appearance >=15 & days_since_appearance < 23 ~ 'posttreatment',
    days_since_appearance >=23 ~ 'after'
  ))

n_treatments = scored_dwell_stages %>%
  filter(domain != 'youtube.com' & domain != 'google.com') %>%
  group_by(pioneer_id, stage) %>%
  count %>%
  filter(stage=='treatment') %>%
  ungroup() %>%
  select(pioneer_id, nn) %>%
  collect

names(n_treatments) = c('pioneer_id','treatments')

scored_dwell_stages_treatments = scored_dwell_stages %>%
  inner_join(n_treatments, by='pioneer_id', copy=T)

# scored_dwell_stages_treatments %>%
#   mutate(total_active_time = total_dwell_time - total_idle_time) %>%
#   filter(domain != 'youtube.com' & domain != 'google.com') %>%
#   sdf_quantile("total_active_time", probabilities=c(.5, .9,.99,.999,1.0))

### simple analysis

treatment_df = scored_dwell_stages_treatments %>% 
  mutate(total_active_time = total_dwell_time - total_idle_time) %>%
  filter(domain != 'youtube.com' & domain != 'google.com') %>%
  group_by(pioneer_id, stage, score, branch, treatments) %>%
  summarise(mean_active_s = mean(total_active_time , na.rm=T)) %>%
  sdf_pivot(pioneer_id + branch + mean_active_s + score + treatments ~ stage) %>%
  na.replace(0) %>%
  mutate(pretreatment = pretreatment * mean_active_s) %>%
  mutate(treatment = treatment * mean_active_s) %>%
  mutate(posttreatment = posttreatment * mean_active_s) %>%
  mutate(
    branch_num = case_when(
      branch=='control'~'C',
      branch=='treatment-bias'~'T',
      branch=='treatment-whois'~'B'
    ))

# fit = ml_linear_regression(x=treatment_df, formula=posttreatment~pretreatment + branch)
# summary(fit)

#fit1 = glm(data=treatment_df, formula=posttreatment~pretreatment + branch)
#summary(fit1)

#fit2 = glm(data=treatment_df, formula=posttreatment~pretreatment + branch + treatments)
#summary(fit2)

#fit3 = glm(data=treatment_df, posttreatment~pretreatment*branch_num + branch_num)
#summary(fit3)
# 
# tmp = scored_dwell_stages_treatments %>%
#   mutate(total_active_time = total_dwell_time - total_idle_time) %>%
#   mutate(bias_active_time = total_active_time * abs(score)) %>%
#   filter(domain != 'youtube.com' & domain != 'google.com') %>%
#   group_by(pioneer_id, stage, score, branch, treatments) %>%
#   summarise(
#     mean_active_s = mean(total_active_time , na.rm=T),
#     total_active_s = sum(total_active_time, na.rm=T),
#     mean_bias_s = mean(bias_active_time , na.rm=T)) %>%
#   sdf_pivot(pioneer_id + branch + mean_bias_s + total_active_s + score + treatments ~ stage) %>%
#   na.replace(0) %>%
#   mutate(pretreatment = pretreatment * mean_bias_s/total_active_s) %>%
#   mutate(treatment = treatment * mean_bias_s/total_active_s) %>%
#   mutate(posttreatment = posttreatment * mean_bias_s) %>%
#   mutate(
#     branch_num = case_when(
#       branch=='control'~'C',
#       branch=='treatment-bias'~'T',
#       branch=='treatment-whois'~'B'
#     ))
# 
# #scored_dwell_stages %>% 
# 
# fit1 = glm(data=tmp, formula=posttreatment~pretreatment + branch_num)
# #summary(fit1)
# 
# fit2 = glm(data=tmp, formula=posttreatment~pretreatment + branch + treatments)
# #summary(fit2)
# 
# fit3 = glm(data=tmp, posttreatment~pretreatment + branch*treatments)
# 
# 
# scored_dwell_stages_treatments %>%
#   mutate(total_active_time = total_dwell_time - total_idle_time) %>%
#   mutate(scaled_active_time = mean(total_active_time, na.rm=TRUE) / sd(total_active_time, na.rm=TRUE)) %>%
#   mutate(bias_active_time = abs(scaled_active_time * score)) %>%
#   sdf_quantile("bias_active_time", probabilities=c(.5, .9,.99,.999,1.0))


## Scratch, lots of little summaries

# tmp = scored_dwell %>% group_by(days_since_appearance, branch, domain) %>%
#   filter(visit_start_date > "2018-01-01" & visit_start_date <"2018-05-01") %>%
#   filter(days_since_appearance < 23) %>%
#   summarise(total=sum(total_dwell_time, na.rm=T)) %>%
#   inner_join(scored_domains, copy=T) %>%
#   mutate(
#     stage = case_when(
#       days_since_appearance >= 1 & days_since_appearance < 8 ~ 'pretreatment',
#       days_since_appearance >= 8 & days_since_appearance < 15 ~ 'treatment',
#       days_since_appearance >=15 ~ 'posttreatment'
#     )) %>%
#   collect

# foo = scored_dwell_stages %>% group_by(branch, stage) %>% count %>% collect
# foo = scored_dwell %>% group_by(days_since_appearance) %>% count %>% collect

#foo$stage = factor(foo$stage, levels= c('pretreatment','treatment','posttreatment'))

# conservative = foo %>% 
#   filter(score > 0.0634)
# 
# ggplot(conservative,aes(
#   x=score,
#   y=total,
#   fill=branch 
# )) + geom_point(aes(color=branch)) +
#   facet_wrap(~stage)
# 
# 
# 
# # 
# ggplot(tmp, aes(
#   x=days_since_appearance,
#   y=total,
#   fill=branch
# )) + geom_bar(stat='identity', position='dodge', aes(color=branch))
# 
# 
# tmp = scored_dwell %>% 
#   mutate(
#     stage = case_when(
#       days_since_appearance < 8 ~ 'pretreatment',
#       days_since_appearance >= 8 & days_since_appearance < 15 ~ 'treatment',
#       days_since_appearance >=15 ~ 'posttreatment'
#   )) # %>%
#   #collect
# 
# 

#dwell_samp = sample_frac(dwell, size=0.01)
#sdf_register(dwell_samp, "dwell_samp_tbl")
#tbl_cache(sc, 'dwell_samp_tbl')

# visit_counts = dwell_samp %>% 
#   group_by(visit_start_date, branch) %>% tally %>% collect
# 
# ggplot(visit_counts, aes(
#   x=visit_start_date,
#   y=n,
#   fill=branch
# )) + geom_bar(stat='identity', aes(color=branch),position='dodge')
# 
# average_duration = dwell %>% 
#   filter(visit_start_date > "2018-01-01" & visit_start_date <"2018-05-01") %>%
#   group_by(branch, visit_start_date) %>% 
#   summarise(mean=mean(total_dwell_time, na.rm=T),
#             n=n()) %>%
#   collect
# 
# ggplot(na.omit(average_duration), aes(
#   x=visit_start_date,
#   y=mean,
#   fill=branch,
#   label=n
# )) +   geom_histogram(stat='identity', position="dodge")

# total_durations = dwell %>% 
# #  filter(visit_start_date > "2018-01-01" & visit_start_date <"2018-05-01") %>%
#   select(total_dwell_time, branch) %>% 
#   sample_frac(size=1) #%>%
#   #collect
# 
# foo = dwell %>% 
#   #mutate(total_dwell_time_log = log(total_dwell_time)) %>%
#   group_by(total_dwell_time, branch) %>%
#   filter(total_dwell_time>5 & total_dwell_time<1800) %>%
#   count %>%
#   collect 
# 
# ggplot(foo, aes(
#   x=total_dwell_time,
#   y=n,
#   fill=branch
# )) + geom_bar(stat='identity', aes(color=branch), position='dodge')
# 
# counts = dwell %>% 
#   group_by(domain, branch) %>% 
#   summarise(sum=sum(total_dwell_time, na.rm=T)/60/60) %>%
#   filter(sum>100) %>%
#   arrange(desc(sum)) %>%
#   collect
# 
# ggplot(counts[1:100,], aes(
#   x=reorder(domain,sum),
#   y=sum)) +
#     geom_bar(stat='identity') + coord_flip() + facet_wrap(~branch)
# 