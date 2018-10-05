library(tidyverse)
# Define a named list of parameter values
gs <- list(min_termfreq = seq(0, 1, by = .05),
           max_docfreq = seq(0, 1, by = .05),
           topics_numbers = c(1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50)) %>% 
  cross_df() # Convert to data frame grid

mod <- function(dod, min_termfreq, max_docfreq, topics_number) {
  news_dfm <- dfm(dod$abstract, remove_punct = TRUE, stem = FALSE,
                  remove = c(stopwords('en'))) %>% 
    dfm_remove(c("wiley", "inc", "j", "res", "sci")) %>% 
    dfm_trim(min_termfreq = min_termfre, termfreq_type = "quantile", 
             max_docfreq = max_docgreq, docfreq_type = "prop")
  
  LDA(dtm, k = topics_number)
}

gs <- gs %>% mutate(fit = pmap(gs, mod))