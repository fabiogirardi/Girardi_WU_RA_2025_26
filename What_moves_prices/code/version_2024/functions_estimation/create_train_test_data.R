
create_train_test_data <- function(data, train.test.split, initial.train.end, time.variable, variables, as.time.series=T, data.freq,
                                  only.train=TRUE, only.test=FALSE){
  train.data = NULL
  test.data = NULL
  # create test data
  if (only.test){
    test.data = data %>% 
                  filter(all_of(!!as.symbol(time.variable)) > initial.train.end) %>%  
                  mutate(across(where(is.numeric), ~na_if(., Inf)), across(where(is.numeric), ~na_if(., -Inf))) %>% 
                  dplyr::select(c(time.variable, all_of(variables))) %>% 
                  drop_na()
                  
    test.data <- test.data
  } 
  if (only.train){
    # create train data
    train.data = data  %>% 
        filter(all_of(!!as.symbol(time.variable)) < train.test.split) %>% dplyr::select(c(all_of(time.variable), all_of(variables))) %>%
        mutate(across(where(is.numeric), ~na_if(., Inf)), across(where(is.numeric), ~na_if(., -Inf))) %>% 
        drop_na()
    
    if (as.time.series){
      time.series.var = as.numeric(c(format(train.data[1,time.variable], format ="%Y"), get_quarter(train.data[1,time.variable])))
      # set to time series data
      if (data.freq == "q") {
        train.data.ts = ts(data = train.data[,-1], start = time.series.var, frequency = 4)
      } else{ 
        train.data.ts = ts(data = train.data[,-1], start = time.series.var[1], frequency = 1)
      }
      # return times series test data
      train.data <- train.data.ts
    } else{
      # return normal test data
      train.data <- train.data
    }
  }
  return(list(train.data=train.data, test.data=test.data))   
}
