library(tidyverse)
library(Stat2Data)
library(dplyr)

## 2 (Q5)
### 2 (Q5.2)
itermap <- function(.x, .f) {
  result <- list()
  for (item in .x) {
    result <- c(result, list(.f(item)))
  }
  return(result)
}
itermap( c(1,2,3), function(x){ return(c(x,x^2)) } )

itermap_dbl <- function(.x, .f) {
  result <- numeric(length(.x))
  for (i in 1:length(.x)) {
    result[i] <- .f(.x[[i]])
  }
  return(result)
}
itermap_dbl( c(1,2,3), function(x){ return(x^3) } )

num_trials <- 1000
set.seed(0)
sampling_with_replacement_simulation<-data.frame(trial=1:num_trials) %>%
  mutate(sample_balls = itermap(.x=trial, function(x){sample(10,22, replace = TRUE)})) %>%
  mutate(num_reds = itermap_dbl(sample_balls, 
                                function(x){
                                  sum(x<=3)
                                }))
head(sampling_with_replacement_simulation, 3)

prob_red_spheres <- function(z){
  n <- 10
  r <- 3
  k <- 22
  prob <- choose(k,z) * (r/n)^z * ((n-r)/n)^(k-z)
  return(prob)
}
print(prob_red_spheres(10))

num_reds <- seq(1:22)
prob <- sapply(num_reds, prob_red_spheres)
prob_by_num_reds = data.frame(num_reds = num_reds, prob = prob)
prob_by_num_reds %>% head(3)

num_reds_in_simulation <- sampling_with_replacement_simulation %>% 
  pull(num_reds)
prob_by_num_reds <- prob_by_num_reds %>%
  mutate(predicted_prob = itermap_dbl(num_reds,
                                      function(.x){
                                        sum(num_reds_in_simulation == .x)/num_trials
                                      }))
head(prob_by_num_reds, 5)


missing_proportion <-function(num_trial){
  set.seed(123)
  sampling_without_replacement_simulation <- data.frame(trial = 1:num_trial) %>%
    mutate(sample_balls = itermap(trial,
                                  function(x){
                                    sample(100, 10,replace=FALSE )
                                  })) %>%
    mutate(num_reds = itermap_dbl(sample_balls,
                                  function(x){sum(x<=50)}),
           num_blue = itermap_dbl(sample_balls,
                                  function(x){sum(x>50 & x<=80)}),
           num_green = itermap_dbl(sample_balls,
                                   function(x){sum(x>80 & x<=100)})) %>%
    mutate(min_count = pmin(num_reds, num_blue, num_green))
  
  missing_color_proportion <- mean(sampling_without_replacement_simulation$min_count == 0)
  return(missing_color_proportion)
}

num_trials <- c(10, 100, 500, 1000)
missing_proportion_data <- data.frame(num_trail = num_trials) %>%
  mutate(missing_proportion = itermap(num_trail,
                                       function(x){
                                         missing_proportion(x)
                                       }))
print(missing_proportion_data)