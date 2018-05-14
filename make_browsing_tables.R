#install.packages('lubridate')
#install.packages('stringr')
# install.packages("ggthemes")
# install.packages("pander")
# install.packages('ggeffects')
library(lubridate)
library(sparklyr)
library(dplyr)
library(ggplot2)
library(scales)
library(jsonlite)
library(stringr)
library(fuzzyjoin)
library(tidyr)
library(magrittr)
library(ggthemes)
library(knitr)
library(sjPlot)
library(pander)
library(ggeffects)
library(xtable)
library(stargazer)

spark_disconnect_all()

#spark config
Sys.setenv(SPARK_HOME='/usr/lib/spark')
config <- spark_config()
#config$spark.driver.memory <- "5G"
#config$spark.executor.memory <- "10G"
#config$spark.yarn.executor.memoryOverhead <- "1G"
#config$spark.executor.cores <- 15 # default 1
#config$spark.executor.instances <- 10 # default 2
#config$spark.sql.shuffle.partitions <- 320
sc <- spark_connect(master = "yarn-client", version = "1.6.2", config = config)

#dwell_parquet_dir = 's3://net-mozaws-data-us-west-2-data-pioneer-analysis/online_news_v2/dwell_time_complete/'
dwell_parquet_dir = 's3://net-mozaws-data-us-west-2-data-pioneer-analysis/online_news_v2/dwell_time_new_handling/'
dwell_tbl = spark_read_parquet(sc, name='dwell_tbl', path=dwell_parquet_dir, repartition=600, memory=F) %>%
  select(-document_ids, -log_events)

dwell_clean_tbl = dwell_tbl %>% 
  filter(visit_start_date > "2018-01-01" & visit_start_date <"2018-05-01") %>% # remove odd clients, likely clock skew
  filter(days_since_appearance > 0 & days_since_appearance < 22) %>% # only look at study periods
  ft_string_indexer(input_col='pioneer_id', output_col='id') #%>% # recode pioneer ids

# annotate site session events per client with likely stage of study
dwell_stages_tbl = dwell_clean_tbl %>% 
  mutate(stage = case_when(
    days_since_appearance == 0 ~ 'enrollment',
    days_since_appearance > 0 & days_since_appearance < 8 ~ 'pretreatment',
    days_since_appearance >= 8 & days_since_appearance < 15 ~ 'treatment',
    days_since_appearance >=15 & days_since_appearance < 22 ~ 'posttreatment',
    days_since_appearance >=22 ~ 'after'
  ))

# compute active time (where dwell = active + idle time)
activity_tbl = dwell_stages_tbl %>% 
  #filter(domain != 'youtube.com' & domain != 'google.com' & domain!= 'facebook.com') %>% 
  mutate(total_active_time = total_dwell_time - total_idle_time) %>%
  filter(total_active_time < 5000) %>% # remove clients with active time greater than 99.999%
  compute('activity_tbl')

bias_json = 'https://raw.githubusercontent.com/mozilla/pioneer-study-online-news-2/master/extension/bias-domains.json'
whois_json = 'https://raw.githubusercontent.com/mozilla/pioneer-study-online-news-2/master/extension/whois-domains.json'
bias_scores = fromJSON(bias_json)
whois_scores = fromJSON(whois_json)
bias_scores$domain = str_replace(bias_scores$domain, 'www.','')

domains = dwell_tbl %>% group_by(domain) %>% count %>% collect
scored_domains = inner_join(bias_scores, domains, by='domain')
names(scored_domains) = c('domain', 'score','n')
scored_domains_tbl = sdf_copy_to(sc,scored_domains, overwrite = T)

scored_activity_tbl = activity_tbl %>%
  inner_join(scored_domains_tbl) %>%
  filter(domain != 'youtube.com' & domain != 'google.com' & domain!= 'facebook.com') %>% 
  mutate(total_active_time = total_dwell_time - total_idle_time,
         scored_active_time = score * total_active_time) %>%
  filter(total_active_time < 5000) %>% # remove clients with active time greater than 99.999%
  #compute('scored_activity_tbl') %>%
  sdf_repartition(partitions=500) %>%
  select(-nav_event_count, -total_dwell_time, -total_idle_time, -n)

