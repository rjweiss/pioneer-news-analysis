library(readr)
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

wave1_url <- 'https://data.surveygizmo.com/reportsview/?key=28049-8643772-b09004d64e9d03e77a7cbafb71caa0a8' 
wave2_url <- 'https://data.surveygizmo.com/reportsview/?key=28049-8643771-cdd2d8e36da1d9e01059b28a2b9fbc2c' 
wave1 <- read_csv(wave1_url)
wave2 <- read_csv(wave2_url)
sum(wave1$`URL Variable: pioneer_id` %in% wave2$`URL Variable: pioneer_id`)
#1453 complete responses

wave1$wave <- '1'
wave2$wave <- '2'

# renaming variables
names(wave1)[1] = NA# "Response ID"                                                                                                                                                                
names(wave1)[2] = NA#"Time Started"                                                                                                                                                               
names(wave1)[3] = NA#"Date Submitted"                                                                                                                                                             
names(wave1)[4] = NA#"Status"                                                                                                                                                                     
names(wave1)[5] = NA#"Contact ID"                                                                                                                                                                 
names(wave1)[6] = NA#"Legacy Comments"                                                                                                                                                            
names(wave1)[7] = NA#"Comments"                                                                                                                                                                   
names(wave1)[8] = NA#"Language"                                                                                                                                                                   
names(wave1)[9] = NA#"Referer"                                                                                                                                                                    
names(wave1)[10] = NA#"Extended Referer"                                                                                                                                                           
names(wave1)[11] = NA#"SessionID"                                                                                                                                                                  
names(wave1)[12] = NA#"User Agent"                                                                                                                                                                 
names(wave1)[13] = NA#"Extended User Agent"                                                                                                                                                        
names(wave1)[14] = NA#"Tags"                                                                                                                                                                       
names(wave1)[15] = NA#"IP Address"                                                                                                                                                                 
names(wave1)[16] = NA#"Longitude"                                                                                                                                                                  
names(wave1)[17] = NA#"Latitude"                                                                                                                                                                   
names(wave1)[18] = NA#"Country"                                                                                                                                                                    
names(wave1)[19] = NA#"City"                                                                                                                                                                       
names(wave1)[20] = NA#"State/Region"                                                                                                                                                               
names(wave1)[21] = NA#"Postal"                                                                                                                                                                     
names(wave1)[22] = 'url_pioneer_id'#"URL Variable: pioneer_id"                                                                                                                                                   
names(wave1)[23] = NA#"URL Variable: utm_campaign"                                                                                                                                                 
names(wave1)[24] = NA#"URL Variable: utm_source"                                                                                                                                                   
names(wave1)[25] = "pioneer_id"                                                                                                                                                                 
names(wave1)[26] = 'age' #"What is your age?"                                                                                                                                                          
names(wave1)[27] = 'gender' #"What gender do you identify as?"                                                                                                                                            
names(wave1)[28] = 'education' #"What is the highest level of school you have completed or the highest degree you have received?"                                                                            
names(wave1)[29] = 'pid3' # "Generally speaking, do you usually think of yourself as a Republican, a Democrat, an Independent, or what?"                                                                 
names(wave1)[30] = 'pidother'# "Other:Generally speaking, do you usually think of yourself as a Republican, a Democrat, an Independent, or what?"                                                           
names(wave1)[31] = 'pidrep' #Would you call yourself a strong Republican or a not very strong Republican?"                                                                                               
names(wave1)[32] = 'piddem' #"Would you call yourself a strong Democrat or a not very strong Democrat?"                                                                                                   
names(wave1)[33] = 'pidleaner' #"Do you think of yourself as closer to the Republican or Democratic party?"                                                                                                  
names(wave1)[34] = NA#"time-page-3"                                                                                                                                                                
names(wave1)[35] = 'media_bias1' #Thinking about the views you see in online news about government and politics, how often are they in line with your own views?"                                             
names(wave1)[36] = 'media_bias2' #"In presenting the news dealing with political and social issues, do you think that news organizations deal fairly with all sides, or do they tend to favor one side?"       
names(wave1)[37] = 'media_bias3' #"In general, how much trust and confidence do you have in the mass media such as newspapers, TV and radio when it comes to reporting the news fully, accurately, and fairly?"
names(wave1)[38] = 'affcare_supp' #"The Affordable Care Act (\"Obamacare\") :Do you support or oppose the following policies:"                                                                                  
names(wave1)[39] = 'gc_supp' #"Gun control :Do you support or oppose the following policies:"                                                                                                              
names(wave1)[40] = 'lowtax_supp' =#"Lower taxes :Do you support or oppose the following policies:"                                                                                                              
names(wave1)[41] = 'govspd_supp' =#"Lower levels of government spending :Do you support or oppose the following policies:"                                                                                      
names(wave1)[42] = 'ct_supp' =#"\"Cap and trade\"' (bill to limit carbon emissions):Do you support or oppose the following policies:"                                                                       
names(wave1)[43] = 'cpspd_supp' #"Stronger controls on campaign spending:Do you support or oppose the following policies:"                                                                                    
names(wave1)[44] = 'daca_supp' #"DREAM Act (path to citizenship for young undocumented immigrants):Do you support or oppose the following policies:"                                                         
names(wave1)[45] = 'bc_supp' #"Mandating insurance companies to provide contraception:Do you support or oppose the following policies:"                                                                    
names(wave1)[46] = 'abt_supp' #"Tighter legal restrictions on abortion:Do you support or oppose the following policies:"                                                                                    
names(wave1)[47] = 'ssm_supp' #"Federal legalization of same-sex marriage:Do you support or oppose the following policies:"                                                                                 
names(wave1)[48] = 'feeltherm_rv' #"Republican voters:On a scale from 0 (coldest) to 100 (warmest) how do you feel about the following people and groups?"                                                      
names(wave1)[49] = 'feeltherm_rc' #"Republican Congressmen:On a scale from 0 (coldest) to 100 (warmest) how do you feel about the following people and groups?"                                                 
names(wave1)[50] = 'feeltherm_trump' #"Donald Trump:On a scale from 0 (coldest) to 100 (warmest) how do you feel about the following people and groups?"                                                           
names(wave1)[51] = 'feeltherm_clinton' #"Hillary Clinton:On a scale from 0 (coldest) to 100 (warmest) how do you feel about the following people and groups?"                                                        
names(wave1)[52] = 'feeltherm_dv' #"Democratic voters:On a scale from 0 (coldest) to 100 (warmest) how do you feel about the following people and groups?"                                                      
names(wave1)[53] = 'feeltherm_dc' #"Democratic Congressmen:On a scale from 0 (coldest) to 100 (warmest) how do you feel about the following people and groups?"                                                 
names(wave1)[54] = 'feeltherm_usps' #"The US Postal Service:On a scale from 0 (coldest) to 100 (warmest) how do you feel about the following people and groups?"                                                  
names(wave1)[55] = 'feeltherm_irs' #"The IRS:On a scale from 0 (coldest) to 100 (warmest) how do you feel about the following people and groups?"                                                                
names(wave1)[56] = 'otherside1' #"Here is a list of the groups that have been in the news. Of the groups on this list, which do you dislike the most?"                                                        
names(wave1)[57] = 'otherside2' #"[question('value'), id='19'] should be allowed to a make a speech in our community"                                                                                         
names(wave1)[58] = 'otherside3' #"[question('value'), id='19'] should be banned from running for public office"                                                                                               
names(wave1)[59] = 'otherside4' #"[question('value'), id='19'] should be allowed to hold public rallies and demonstrations in our community"                                                                  
###
names(wave2)[1] = NA# "Response ID"                                                                                                                                                                
names(wave2)[2] = NA# "Time Started"                                                                                                                                                               
names(wave2)[3] = NA# "Date Submitted"                                                                                                                                                             
names(wave2)[4] = NA# "Status"                                                                                                                                                                     
names(wave2)[5] = NA# "Contact ID"                                                                                                                                                                 
names(wave2)[6] = NA# "Legacy Comments"                                                                                                                                                            
names(wave2)[7] = NA# "Comments"                                                                                                                                                                   
names(wave2)[8] = NA# "Language"                                                                                                                                                                   
names(wave2)[9] = NA# "Referer"                                                                                                                                                                    
names(wave2)[10] = NA# "Extended Referer"                                                                                                                                                           
names(wave2)[11] = NA# "SessionID"                                                                                                                                                                  
names(wave2)[12] = NA# "User Agent"                                                                                                                                                                 
names(wave2)[13] = NA# "Extended User Agent"                                                                                                                                                        
names(wave2)[14] = NA# "Tags"                                                                                                                                                                       
names(wave2)[15] = NA# "IP Address"                                                                                                                                                                 
names(wave2)[16] = NA# "Longitude"                                                                                                                                                                  
names(wave2)[17] = NA# "Latitude"                                                                                                                                                                   
names(wave2)[18] = NA# = NA# "Country"                                                                                                                                                                    
names(wave2)[19] = NA# "City"                                                                                                                                                                       
names(wave2)[20] = NA# "State/Region"                                                                                                                                                               
names(wave2)[21] = NA# "Postal"                                                                                                                                                                     
names(wave2)[22] = 'pioneer_id'# "URL Variable: pioneer_id"                                                                                                                                                   
names(wave2)[23] = NA# "URL Variable: utm_campaign"                                                                                                                                                 
names(wave2)[24] = NA# "URL Variable: utm_source"                                                                                                                                                   
names(wave2)[25] = 'media_bias1'# "Thinking about the views you see in online news about government and politics, how often are they in line with your own views?"                                             
names(wave2)[26] = 'media_bias2'# "In presenting the news dealing with political and social issues, do you think that news organizations deal fairly with all sides, or do they tend to favor one side?"       
names(wave2)[27] = 'media_bias3'# "In general, how much trust and confidence do you have in the mass media such as newspapers, TV and radio when it comes to reporting the news fully, accurately, and fairly?"
names(wave2)[28] = 'feeltherm_rv'# "Republican voters:On a scale from 0 (coldest) to 100 (warmest) how do you feel about the following people and groups?"                                                      
names(wave2)[29] = 'feeltherm_rc'# "Republican Congressmen:On a scale from 0 (coldest) to 100 (warmest) how do you feel about the following people and groups?"                                                 
names(wave2)[30] = 'feeltherm_trump'# "Donald Trump:On a scale from 0 (coldest) to 100 (warmest) how do you feel about the following people and groups?"                                                           
names(wave2)[31] = 'feeltherm_clinton'# "Hillary Clinton:On a scale from 0 (coldest) to 100 (warmest) how do you feel about the following people and groups?"                                                        
names(wave2)[32] = 'feeltherm_dv'# "Democratic voters:On a scale from 0 (coldest) to 100 (warmest) how do you feel about the following people and groups?"                                                      
names(wave2)[33] = 'feeltherm_irs'# "The IRS:On a scale from 0 (coldest) to 100 (warmest) how do you feel about the following people and groups?"                                                                
names(wave2)[34] = 'feeltherm_usps'# "The US Postal Service:On a scale from 0 (coldest) to 100 (warmest) how do you feel about the following people and groups?"                                                  
names(wave2)[35] = 'feeltherm_dc'# "Democratic Congressmen:On a scale from 0 (coldest) to 100 (warmest) how do you feel about the following people and groups?"

# drop all the columns that aren't useful
wave1 = wave1[!is.na(names(wave1))]
wave2 = wave2[!is.na(names(wave2))]


# recoding
wave1$education=recode(wave1$education, 
                'Four year college degree/bachelor’s degree '=3,
                'High school graduate or GED (includes technical/vocational training that doesn’t count towards college credit)'=1,
                'High school incomplete or less' = 0,
                'Postgraduate or professional degree, including master’s, doctorate, medical or law degree' =5,
                'Some college (some community college, associate’s degree)' =2,
                'Some postgraduate or professional schooling, no postgraduate degree'=4,
       .default = -1)

wave1 = unite_(data=wave1, col='pid4', c('pidleaner','pid3')) %>% mutate(pid4=str_remove(pid4, 'NA_'))
wave1 = unite_(data=wave1, col='pidfull', c('piddem','pid4')) %>% mutate(pidfull=str_remove(pidfull, 'NA_'))
wave1 = unite_(data=wave1, col='pidfull', c('pidrep','pidfull')) %>% mutate(pidfull=str_remove(pidfull, 'NA_'))

wave1$education=recode(wave1$education, 
                       'Four year college degree/bachelor’s degree '=3,
                       'High school graduate or GED (includes technical/vocational training that doesn’t count towards college credit)'=1,
                       'High school incomplete or less' = 0,
                       'Postgraduate or professional degree, including master’s, doctorate, medical or law degree' =5,
                       'Some college (some community college, associate’s degree)' =2,
                       'Some postgraduate or professional schooling, no postgraduate degree'=4,
                       .default = -1)
wave1$pid3=recode(wave1$pidfull,
                  'Democrat'='D',
                  'Democratic_Independent'='D',
                  'Democratic_Other'='D',
                  'I decline to state'='N',
                  'Independent'='I',
                  'Not very strong Democrat_Democrat'='D',
                  'Not very strong Republican_Republican'='R',
                  'Other'='O',
                  'Republican'='R',
                  'Republican_Independent'='R',
                  'Republican_Other'='R',
                  'Strong Democrat_Democrat'='D',
                  'Strong Republican_Republican'='R')
wave1$pid7=recode(wave1$pidfull,
                  'Democrat'='D',
                  'Democratic_Independent'='D',
                  'Democratic_Other'='D',
                  'I decline to state'='N',
                  'Independent'='I',
                  'Not very strong Democrat_Democrat'='D',
                  'Not very strong Republican_Republican'='R',
                  'Other'='O',
                  'Republican'='R',
                  'Republican_Independent'='R',
                  'Republican_Other'='R',
                  'Strong Democrat_Democrat'='SD',
                  'Strong Republican_Republican'='SR')


wave1_subset = wave1[,names(wave1) %in% names(wave2)]
full_df = rbind(wave1_subset, wave2)

# > table(full_df$media_bias1)
 
full_df$media_bias1=recode(full_df$media_bias1,
                           'Always or nearly all of the time'=3,
                           'Most of the time'=2,
                           'Some of the time'= 1,
                           'Not too often'=0,
                           'I don\'t read political news'=-1,
                           'Don\'t know'=-1)

full_df$media_bias2=recode(full_df$media_bias2,
       'Deal fairly with all sides'=1,
       'No opinion'=-1,
       'Tend to favor one side'=0)

full_df$media_bias3=recode(full_df$media_bias3,
       'A fair amount'=3,
       'A great deal'=2,
       'Not very much'=1,
       'None at all'=0)

full_df=full_df %>% 
  mutate(media_bias1=replace(media_bias1, which(media_bias1<0), NA)) %>%
  mutate(media_bias2=replace(media_bias2, which(media_bias2<0), NA)) %>%
  mutate(media_bias3=replace(media_bias3, which(media_bias3<0), NA))

# 
# ###

completes = full_df %>% group_by(pioneer_id) %>% summarize(n = n()) %>% filter(n>1) %>% select(pioneer_id)
branches = activity_tbl %>% select(pioneer_id, branch) %>% group_by(pioneer_id) %>% filter(row_number(branch)==1)

pid = select(wave1, pioneer_id, pid3)
pid = select(wave1, pioneer_id, pid7)
names(pid) = c('pioneer_id','pid')
sdf_copy_to(sc, pid, overwrite=T)
restricted_df = filter(full_df, pioneer_id %in% completes$pioneer_id) %>% full_join(pid, by='pioneer_id')
completes2 = dwell_tbl %>% group_by(pioneer_id) %>% summarise(n=n_distinct(pioneer_id)) %>% select(pioneer_id) %>% collect
valids = data.frame(pioneer_id=wave1[wave1$pioneer_id %in% completes2$pioneer_id,]$pioneer_id)

pid_tbl = sdf_copy_to(sc,pid, overwrite=T)
valids_tbl = sdf_copy_to(sc,valids,overwrite=T)