#crossvalidation = https://eddjberry.netlify.com/post/2017-11-02-sparklyr/

pid_tbl = sdf_copy_to(sc,pid, overwrite=T)
valids_tbl = sdf_copy_to(sc,valids,overwrite=T)

# joined_dwell_tbl = scored_dwell_stages_treatments %>% 
#   inner_join(valids_tbl, by='pioneer_id') %>% 
#   inner_join(pid_tbl, by='pioneer_id') %>%
#   filter(domain != 'youtube.com' & domain != 'google.com') %>%
#   group_by(pioneer_id, domain) %>%
#   count %>%
#   sdf_register('joined_dwell_tbl')
  
#top1000 = dwell_stages %>% count(domain) %>% top_n(10000)
#top1000_tbl = sdf_copy_to(sc, top1000, overwrite=T)
# 
# visit_count_tbl = scored_dwell_stages_treatments %>% 
#   inner_join(valids_tbl, by='pioneer_id') %>% 
# #  inner_join(top1000_tbl, by='domain') %>% 
#   filter(domain != 'youtube.com' & domain != 'google.com') %>%
#   mutate(domain = regexp_replace(domain, "[_\"\'():;,.!?\\-]", ""))  %>%
#   mutate(domain = regexp_replace(domain, "[0-9]", "X"))  %>%
#   group_by(pioneer_id, domain) %>%
#   count %>% 
#   sdf_pivot(pioneer_id ~ domain, list(nn="sum")) %>%
#   na.replace(0) %>%
#   inner_join(pid_tbl) %>% 
#   ft_string_indexer('pid','pid_num') %>% # XXX break this off into the ml transform step
#   sdf_register('visit_count_tbl')
# 
# tbl_cache(sc, 'visit_count_tbl')#, overwrite=T)  

#active_time_count_tbl = scored_dwell_stages_treatments %>%
active_time_count_tbl = scored_activity_tbl %>%
  filter(stage == 'pretreatment') %>%
#  mutate(total_active_time = total_dwell_time - total_idle_time) %>%
#  inner_join(valids_tbl, by='pioneer_id') %>% # joins with scored domains
#  filter(domain != 'youtube.com' & domain != 'google.com') %>%
  mutate(domain = regexp_replace(domain, "[_\"\'():;,.!?\\-]", ""))  %>%
  mutate(domain = regexp_replace(domain, "[0-9]", ""))  %>%
  #group_by(pioneer_id, domain) %>%
  group_by(pioneer_id, domain) %>%
  #count %>%
  summarise(
#    mean_active_s = mean(total_active_time , na.rm=T),
    total_scored_active_s = sum(scored_active_time , na.rm=T)) %>%
#  ungroup %>% group_by(pioneer_id, score_q) %>%
  #summarise(
  #  all_mean_active_s = mean(mean_active_s , na.rm=T),
  #  all_total_active_s = sum(total_active_s , na.rm=T)) %>%
  sdf_pivot(pioneer_id ~ domain, list(total_scored_active_s="sum")) %>%
  na.replace(0) %>%
  inner_join(pid_tbl) %>%
  ft_string_indexer('pid','pid_num') %>%
  select(-pid) %>%
  sdf_register('active_time_count_tbl')

tbl_cache(sc, 'active_time_count_tbl')#, overwrite=T)

  #ft_quantile_discretizer(input_col='score',output_col='score_qn',num_buckets=8) %>%
  #ft_index_to_string(input_col='score_qn','score_q',labels=paste('q',seq_along(1:8),sep='')) %>%
  #sdf_register('joined_dwell_tbl')
#  select(score_q) %>% group_by(score_q) %>% count
  #mutate(bin = ntile(desc(score), 10))# %>% 
   # mutate(score_q = case_when(
   #   score < -0.998400 ~ 'q1',
   #   score >= -0.998400 & score < -0.439625 ~ 'q25',
   #   score >= -0.439625 & score < -0.048700 ~ 'q50',
   #   score >= -0.048700 & score < 0.798275 ~ 'q75',
   #   score >= 0.798275 & score <= 1.029500 ~ 'q99'
   # ))
#pid_tbl = sdf_copy_to(sc,pid)

# data = joined_dwell_tbl %>% 
#   mutate(total_active_time = total_dwell_time - total_idle_time) %>%
#   group_by(pioneer_id, score_q, visit_start_date) %>%
#   summarise(
#     mean_active_s = mean(total_active_time , na.rm=T), 
#     total_active_s = sum(total_active_time , na.rm=T)) %>%
#   ungroup %>% group_by(pioneer_id, score_q) %>%
#   summarise(
#     all_mean_active_s = mean(mean_active_s , na.rm=T), 
#     all_total_active_s = sum(total_active_s , na.rm=T)) %>%
#   sdf_pivot(pioneer_id ~ score_q, list(all_total_active_s="sum")) %>%
#   na.replace(0) %>%
#   inner_join(pid_tbl) %>%
#   ft_string_indexer('pid','pid_num') %>%
#   #compute
#   sdf_register('data_tbl')
# 
# tbl_cache(sc, 'data_tbl')


## Drawn from https://beta.rstudioconnect.com/content/1518/notebook-classification.html
#part = data %>% 
#visit_count_tbl %<>% inner_join(pid_tbl, by='pioneer_id') 
#visit_count_tbl %<>% ft_string_indexer('pid','pid_num')

#part = visit_count_tbl %>% 
part=active_time_count_tbl%>%
  select(-pioneer_id) %>%
  sdf_partition(training = 0.75, test = 0.25)
#part = sdf_partition(data, training = 0.75, test = 0.25)
train_tbl <- part$train
test_tbl <- part$test

ml_formula <- formula(pid_num ~ . )  

# vector_assembler <- ft_vector_assembler(
#   sc,
#   input_cols = colnames(train_tbl %>% select(-pid_num, pid)),
#   output_col = "features"
# )
# vector_assembler %>% ml_transform(train_tbl)

## logistic regression model
ml_log <- ml_logistic_regression(train_tbl, ml_formula)

# ml_rf = ml_random_forest(x = train_tbl,
#                  response = "pid_num", 
#                  features = colnames(train_tbl %>% select(-pid_num, pid)),
#                  num.trees = 10L,
#                  type = "classification")

## Decision Tree
#ml_dt <- ml_decision_tree(train_tbl, ml_formula)

## Random Forest
ml_rf <- ml_random_forest(train_tbl, ml_formula)

## Gradient Boosted Tree
#ml_gbt <- ml_gradient_boosted_trees(train_tbl, ml_formula)

## Naive Bayes
#ml_nb <- ml_naive_bayes(train_tbl, ml_formula)

## Neural Network
#ml_nn <- ml_multilayer_perceptron(train_tbl, ml_formula, layers = c(11,15,2))  
#   
 ml_models <- list(
#   "Logistic" = ml_log,
#   "Decision Tree" = ml_dt,
   "Random Forest" = ml_rf#,
#   "Gradient Boosted Trees" = ml_gbt,
#   "Naive Bayes" = ml_nb#,
#   "Neural Net" = ml_nn
 )
# 

# predict <- ml_predict(ml_nb, test_tbl) %>%#sdf_predict(ml_nb, test_tbl) %>%
#     ft_string_indexer("pid_num", "pid_num_idx") %>%
#     collect
#   
# table(predict$pid_num_idx, predict$prediction)

# # Create a function for scoring
 score_test_data <- function(model, data=test_tbl){
   pred <- ml_predict(model, data)#sdf_predict(model, data)
   select(pred, pid_num, prediction)
 }
# 
# # Score all the models
 ml_score <- lapply(ml_models, score_test_data)  
# #   
# Lift function
calculate_lift <- function(scored_data) {
  scored_data %>%
    mutate(bin = ntile(desc(prediction), 10)) %>%
    group_by(bin) %>%
    summarize(count = sum(pid_num)) %>%
    mutate(prop = count / sum(count)) %>%
    arrange(bin) %>%
    mutate(prop = cumsum(prop)) %>%
    select(-count) %>%
    collect() %>%
    as.data.frame()
}

# Initialize results
ml_gains <- data.frame(bin = 1:10, prop = seq(0, 1, len = 10), model = "Base")

# Calculate lift
for(i in names(ml_score)){
  ml_gains <- ml_score[[i]] %>%
    calculate_lift %>%
    mutate(model = i) %>%
    rbind(ml_gains, .)
}

# Plot results
ggplot(ml_gains, aes(x = bin, y = prop, colour = model)) +
  geom_point() + geom_line() +
  ggtitle("Lift Chart for Predicting Party ID - Test Data Set") +
  xlab("") + ylab("")

# # Function for calculating accuracy
calc_accuracy <- function(data, cutpoint = 0.5){
  data %>%
    mutate(prediction = if_else(prediction > cutpoint, 1.0, 0.0)) %>%
    ml_classification_eval("prediction", "pid_num", "accuracy")
}

# Calculate AUC and accuracy
perf_metrics <- data.frame(
  model = names(ml_score),
  AUC = 100 * sapply(ml_score, ml_binary_classification_eval, "pid_num", "prediction"),
  #AUC = 100 * sapply(ml_score, ml_classification_eval, "pid_num", "prediction"),
  Accuracy = 100 * sapply(ml_score, calc_accuracy),
  row.names = NULL, stringsAsFactors = FALSE)

# Plot results
gather(perf_metrics, metric, value, AUC, Accuracy) %>%
  ggplot(aes(reorder(model, value), value, fill = metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  xlab("") +
  ylab("Percent") +
  ggtitle("Performance Metrics")

#   

ml_tree_feature_importance(sc = sc, model = ml_rf)[1:10,]  


##################
# Cross validated

# function from https://eddjberry.netlify.com/post/2017-11-02-sparklyr/
ml_classification_cross_validation <- function(
  data,            # a tbl_spark
  response,        # the response column as a character
  features = NULL, # character vector or else all the columns except response
  model_fun,       # the modelling function to use (unquoted)
  k,               # number of folds
  ...) {           # additional arguments
  
  # first off create weights for partitioning the data
  weights <- rep(1 / k, times = k)
  # name the elements of weights
  names(weights) <- paste0("fold", as.character(1:k))
  
  # partition the data using weights
  data_cv <- sdf_partition(data, weights = weights)
  
  # get the indicies for our different training sets
  # e.g. a dataFrame for partitions 1, 2, 3, and 4
  K <- 1:k
  indices <- purrr::map(K, ~ K[-.x])
  
  # create our training sets by binding together partitions of data_cv
  # We get back a list where each element is a dataFrame of the partions
  # indexed by each element of indices combined together
  data_splits <- purrr::map(indices, ~ sdf_bind_rows(data_cv[.x]))
  
  # If a vector of feature names hasn't been specified
  if (is.null(features)) {
    # Get the column names of the data
    columns <- colnames(data_splits[[1]])
    # Create the feature names by using all the columns except the response
    features <- columns[columns != response]
  }
  
  # Map the specified model_fun over each of our training sets (the elements of
  # data splits)
  fits <- purrr::map(.x = data_splits,
                     .f = purrr::as_mapper(model_fun),
                     response = response,
                     features = features,
                     type = "classification",
                     ...)
  
  # Get some predictions for each model for the partiton that wasn't used
  # to train that model.
  preds <- purrr::map2(fits, K, ~ sdf_predict(.x, data_cv[[.y]]))
  
  # Checking whether a model that outputs probabilities was used.
  # If so the relevant evaluation function is used.
  # (Should also test for whether the response is binary but doesn't atm)
  if (any(stringr::str_detect(colnames(preds[[1]]), "probability"))) {
    
    evals <- purrr::map(preds,
                        ml_binary_classification_eval,
                        label = response,
                        score = "probability")
  } else {
    
    evals <- purrr::map(preds,
                        ml_classification_eval,
                        predicted_lbl = "prediction",
                        label = response)
  }
  # This is what will be returned
  list(fits = fits,
       predictions = preds,
       evals = evals)
  
}

# ml_classification_cross_validation(df, 
#                                    response = "y",
#                                    model_fun = ml_gradient_boosted_trees,
#                                    k = 5,
#                                    max.depth = 3L,
#                                    num.trees = 10L,
#                                    seed = 2017)

#  select(pid, total_active_s, mean_active_s, pioneer_id, score_q, visit_start_date)# %>%
  #filter(pioneer_id == '0352a4bf-bcf0-4c7c-abbf-692cd16d2bcb')
#  sdf_pivot(pioneer_id + total ~ score_q)



#score_quantiles = quantile(bias_scores$avgAlign) 
# copy_to(sc, as.data.frame(score_quantiles))
# 
# joined_dwell %>% 
#   ft_quantile_discretizer('score', 'score_q', num_buckets=5) %>%
#   group_by(score_q) %>%
#   count

# joined_dwell %>% 
#   filter(domain != 'youtube.com' & domain != 'google.com') %>%
#   mutate(score_q = case_when(
#     score < -0.998400 ~ 'q1',
#     score >= -0.998400 & score < -0.439625 ~ 'q25',
#     score >= -0.439625 & score < -0.048700 ~ 'q50',
#     score >= -0.048700 & score < 0.798275 ~ 'q75',
#     score >= 0.798275 & score <= 1.029500 ~ 'q99'
#   )) %>%
#   group_by(score_q) %>%
#   count