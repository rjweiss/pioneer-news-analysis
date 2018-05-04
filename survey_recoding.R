library(readr)
#source('~/analyses/rweiss/dwell_analysis.R')
wave1_url <- 'https://data.surveygizmo.com/reportsview/?key=28049-8643772-b09004d64e9d03e77a7cbafb71caa0a8' 
wave2_url <- 'https://data.surveygizmo.com/reportsview/?key=28049-8643771-cdd2d8e36da1d9e01059b28a2b9fbc2c' 
wave1 <- read_csv(wave1_url)
wave2 <- read_csv(wave2_url)
sum(wave1$`URL Variable: pioneer_id` %in% wave2$`URL Variable: pioneer_id`)
#1453 complete responses

wave1$wave <- '1'
wave2$wave <- '2'

wave1_subset = wave1[,names(wave1) %in% names(wave2)]

full_df = rbind(wave1_subset, wave2)

library(ggplot2)
library(dplyr)

completes = full_df %>% group_by(`URL Variable: pioneer_id`) %>% summarize(n = n()) %>% filter(n>1)
names(completes) = c('id', 'n')

pid = select(wave1, pioneer_id, `Generally speaking, do you usually think of yourself as a Republican, a Democrat, an Independent, or what?`)
names(pid) = c('pioneer_id','pid')
sdf_copy_to(sc, pid)
restricted_df = filter(full_df, `URL Variable: pioneer_id` %in% completes$id) %>% full_join(pid, by=c(`URL Variable: pioneer_id` ="pioneer_id"))
completes2 = dwell %>% group_by(pioneer_id) %>% summarise(n=n_distinct(pioneer_id)) %>% select(pioneer_id) %>% collect
sum(wave1$`URL Variable: pioneer_id` %in% completes2$pioneer_id)

######
# If we only want all responses to both waves

# valids = completes[completes$id %in% completes2$pioneer_id,]
# names(valids) = c('pioneer_id','n')
# sdf_copy_to(sc, valids)
# 
# joined_dwell = dwell %>% inner_join(valids, by='pioneer_id', copy=T) %>% inner_join(pid, by='pioneer_id', copy=T)
# #sdf_register(joined_dwell %>% inner_join(pid, by='pioneer_id', copy=T), 'joined_dwell_pid_spark')
# sdf_register(joined_dwell, 'joined_dwell_spark')
# tbl_cache(sc, 'joined_dwell_spark')
# 
# counts = joined_dwell %>%
#   group_by(domain, pid) %>%
#   summarise(sum=sum(total_dwell_time, na.rm=T)/60) %>%
#   filter(sum>100) %>%
#   arrange(desc(sum)) %>%
#   collect
# 
# ggplot(counts[1:100,], aes(
#   x=reorder(domain,sum),
#   y=sum)) +
#     geom_bar(stat='identity') + coord_flip() + facet_wrap(~pid, ncol=5)
# 
# pop = wave1 %>% 
#   filter(`What is your age?` != 'Under 18') %>%
#   filter(`What is your age?` != 'I prefer not to say') %>%
#   mutate(age=as.numeric(`What is your age?`))
# 
# ggplot(pop, aes(
#   x=`Generally speaking, do you usually think of yourself as a Republican, a Democrat, an Independent, or what?`, 
#   y=age)) + 
#   geom_boxplot() + coord_flip()


##########
# Let's say we want to look at browsing data of all wave1 responses

valids = data.frame(pioneer_id=wave1[wave1$`URL Variable: pioneer_id` %in% completes2$pioneer_id,]$`URL Variable: pioneer_id`)

sdf_copy_to(sc, valids, overwrite = T)

# joined_dwell = dwell %>% inner_join(valids, by='pioneer_id', copy=T) %>% inner_join(pid, by='pioneer_id', copy=T)
# #sdf_register(joined_dwell %>% inner_join(pid, by='pioneer_id', copy=T), 'joined_dwell_pid_spark')
# sdf_register(joined_dwell, 'joined_dwell_spark')
# tbl_cache(sc, 'joined_dwell_spark')
# 
# counts = joined_dwell %>%
#   group_by(domain, pid) %>%
#   summarise(sum=sum(total_dwell_time, na.rm=T)/60) %>%
#   filter(sum>100) %>%
#   arrange(desc(sum)) %>%
#   collect
# 
# ggplot(counts[1:100,], aes(
#   x=reorder(domain,sum),
#   y=sum)) +
#   geom_bar(stat='identity') + coord_flip() + facet_wrap(~pid, ncol=5)
# 
# pop = wave1 %>% 
#   filter(`URL Variable: pioneer_id` %in% completes$id) %>%
#   filter(`What is your age?` != 'Under 18') %>%
#   filter(`What is your age?` != 'I prefer not to say') %>%
#   mutate(age=as.numeric(`What is your age?`))

# counts = joined_dwell %>% 
#   filter(visit_start_date > "2018-01-01" & visit_start_date <"2018-05-01") %>%
#   group_by(branch, pid, visit_start_date) %>%
#   count %>%
#   collect
# 
# ggplot(counts, aes(
#   x=visit_start_date,
#   y=n,
#   fill=pid)) +
#     geom_bar(aes(color=pid), stat='identity',position='dodge') +
#     facet_wrap(~branch)
# 
# durations = joined_dwell %>% 
#   filter(visit_start_date > "2018-01-01" & visit_start_date <"2018-05-01") %>%
#   group_by(branch, pid, visit_start_date) %>%
#   summarise(total=sum(total_dwell_time, na.rm=T)) %>%
#   collect
# 
# ggplot(durations, aes(
#   x=visit_start_date,
#   y=total,
#   fill=pid)) +
#   geom_bar(aes(color=pid), stat='identity',position='dodge') +
#   facet_wrap(~branch)
# 
# days_active = joined_dwell %>%
#   group_by(pioneer_id,visit_start_date) %>%
#   count %>%
#   filter(n>0) %>%
#   ungroup %>%
#   group_by(pioneer_id) %>%
#   count %>%
#   collect
# 
# days_active_unres = dwell %>%
#   group_by(pioneer_id,visit_start_date) %>%
#   count %>%
#   filter(n>0) %>%
#   ungroup %>%
#   group_by(pioneer_id) %>%
#   count %>%
#   collect
# 
# days_active_cens = days_active_unres %>% filter(!pioneer_id %in% days_active$pioneer_id) %>% collect
# 
# days_active$type='survey'
# days_active_unres$type='all'
# days_active_cens$type='censored'
# active_days = rbind(days_active, days_active_unres,days_active_cens)
# 
# ggplot(active_days, aes(
#   x=nn, fill=type)) + stat_ecdf(aes(color=type))


