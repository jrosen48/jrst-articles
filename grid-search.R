library(tidyverse)
library(quanteda)
library(topicmodels)
library(tidytext)
library("ldatuning")

# l <- read_rds("all-jrst-data.rds")
l <- read_rds("all-scied-data.rds")

abstracts <- map(l, ~ .[["abstract"]])
dates <- map(l, ~ .[["date"]])

do <- tibble(date = unlist(dates),
             abstract = unlist(abstracts))

do$date <- lubridate::parse_date_time(do$date, "b! Y!")
do$year <- lubridate::year(do$date)

dod <- do %>% filter(!is.na(abstract) & !is.na(year))

c <- corpus(dod$abstract,
            docvars = data.frame(grp = dod$year))

# Define a named list of parameter values
gs <- list(min_termfreq = seq(0, 1, by = .05),
           max_docfreq = seq(0, 1, by = .05),
           topics_numbers = c(5, 10, 15, 20, 25, 30, 35, 40)) %>% 
  cross_df() # Convert to data frame grid

mod <- function(dod, min_termfreq, max_docfreq, topics_number) {
  news_dfm <- dfm(dod$abstract, remove_punct = TRUE, stem = FALSE,
                  remove = c(stopwords('en'))) %>% 
    dfm_remove(c("wiley", "inc", "j", "res", "sci")) %>% 
    dfm_trim(min_termfreq = min_termfreq, termfreq_type = "quantile", 
             max_docfreq = max_docfreq, docfreq_type = "prop")
  
  news_dfm <- news_dfm[ntoken(news_dfm) > 0,]
  dtm <- convert(news_dfm, to = "topicmodels") 
  
  result <- FindTopicsNumber(
    dtm,
    topics = topics_number,
    metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
    method = "Gibbs",
    control = list(seed = 77),
    mc.cores = 2L,
    verbose = TRUE
  )
  
  out <- result %>% 
    mutate(min_termfreq = min_termfreq, max_docfreq = max_docfreq)
  
  # out <- LDA(dtm, k = topics_number)
  print(str_c("Processed min_termfreq = ", min_termfreq, "; maxdocfreq = ", max_docfreq, " at ", Sys.time()))
  
  saveRDS(out, str_c("data/lda-out/lda-model-", min_termfreq, "-", max_docfreq, ".rds"))
  out
}

lda_out <- pmap(list(gs$min_termfreq, gs$max_docfreq, gs$topics_numbers), mod, dod = dod)


dir <- "data/lda-out/"
f <- list.files("data/lda-out/")

rf <- function(file_name, dir) {
  readRDS(stringr::str_c(dir, file_name))
}

ll <- purrr::map(f, rf, dir = dir)
