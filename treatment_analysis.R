# library(jsonlite)
# library(stringr)
# library(fuzzyjoin)
# 
# bias_json = 'https://raw.githubusercontent.com/mozilla/pioneer-study-online-news-2/master/extension/bias-domains.json'
# whois_json = 'https://raw.githubusercontent.com/mozilla/pioneer-study-online-news-2/master/extension/whois-domains.json'
# 
# bias_scores = fromJSON(bias_json)
# whois_scores = fromJSON(whois_json)
# bias_scores$domain = str_replace(bias_scores$domain, 'www.','')
# 
# domains = dwell %>% group_by(domain) %>% count %>% collect
# sum(domains$domain %in% bias_scores$domain)
# 
# scored_domains = stringdist_inner_join(bias_scores, domains, by='domain', max_dist=0.5)
# names(scored_domains) = c('domain', 'score','domain2','n')
# sdf_copy_to(sc, scored_domains)
# tbl_cache(sc, 'scored_domains')
# 
# scored_dwell = dwell %>% inner_join(scored_domains, copy=T)
# sdf_register(scored_dwell, 'scored_dwell_tbl')
# tbl_cache(sc, 'scored_dwell_tbl')


# haves = domains[domains$domain %in% bias_scores$domain,]
# have_notes = domains[!domains$domain %in% bias_scores$domain,]
# 
# a = str_split(counts$domain, '\\.', simplify=T)
# b = str_split(bias_scores$domain, '\\.', simplify=T)
# b = cbind(b, bias_scores$avgAlign)
# 
# sum(a[,1] %in% b[,1])
# matches1 = b[b[,1] %in% a[,1],]
# 
# a[(a[,1] %in% b[,2]),]
# matches2 = b[b[,2] %in% a[,1],]
# 
# #tmp = as.data.frame(matches2)
# 
# dplyr::filter(tmp, !V2 %in% c('com','org','ca','co'))


# test = scored_dwell %>% 
#   group_by(visit_start_date, score, branch) %>%
#   filter(total_dwell_time>5 & total_dwell_time<1800) %>%
#   summarise(total_s=sum(total_dwell_time, na.rm=T)) %>%
#   mutate(total_m=total_s/60) %>%
#   mutate(total_h=total_m/60) %>%
#   collect

# tmp = scored_dwell %>% 
#   filter(total_dwell_time>5 & total_dwell_time<1800) %>%
#   group_by(days_since_appearance, score, branch) %>%
#   summarise(total_s=sum(total_dwell_time, na.rm=T)) %>%
#   mutate(total_m=total_s/60) %>%
#   mutate(total_h=total_m/60) %>%
#   collect


# density = scored_dwell %>% 
#   select(visit_start_date,score,branch) %>%
#   collect

# ggplot(density, aes(
#   x=score,
#   y=nn
# )) + geom_point()


# ggplot(test, aes(
#   x=score,
#   y=total_h,
#   fill=branch
# )) + geom_point(aes(color=branch)) + facet_wrap(~visit_start_date)

# ggplot(tmp, aes(
#   x=score,
#   y=total_h,
#   fill=days_since_appearance
# )) + geom_point(aes(color=days_since_appearance)) + facet_wrap(~branch)
# 
