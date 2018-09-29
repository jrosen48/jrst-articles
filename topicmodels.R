library(tidyverse)
library(quanteda)
library(topicmodels)
library(tidytext)
library("ldatuning")

l <- read_rds("all-jrst-data.rds")

abstracts <- map(l, ~ .[["abstract"]])
dates <- map(l, ~ .[["date"]])

do <- tibble(date = unlist(dates),
             abstract = unlist(abstracts))

do$date <- lubridate::parse_date_time(do$date, "b! Y!")
do$year <- lubridate::year(do$date)

dod <- do %>% filter(!is.na(abstract) & !is.na(year))

c <- corpus(dod$abstract,
            docvars = data.frame(grp = dod$year))

news_dfm <- dfm(dod$abstract, remove_punct = TRUE, stem = FALSE,
                remove = c(stopwords('en'))) %>% 
  dfm_remove(c("wiley", "inc", "j", "res", "sci", "article", "study", "paper")) %>% 
  dfm_trim(min_termfreq = 0.35 ,termfreq_type = "quantile", 
           max_docfreq = 0.175, docfreq_type = "prop")

news_dfm <- news_dfm[ntoken(news_dfm) > 0,]

# news_dfm <- dfm_group(news_dfm, "grp")
# topfeatures(dfm_g)
dtm <- convert(news_dfm, to = "topicmodels") 

result <- FindTopicsNumber(
  dtm,
  topics = seq(from = 2, to = 30, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 4L,
  verbose = TRUE
)

FindTopicsNumber_plot(result)

lda <- LDA(dtm, k = 18)

# word topic

ap_topics <- tidy(lda, matrix = "beta")

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

p1 <- ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

p1

# by year

chapters_gamma <- tidy(lda, matrix = "gamma")

ddd <- c$documents %>% rownames_to_column("document")  %>% as_tibble()

as_data_frame()

docs_with_gamma <- chapters_gamma %>% 
  spread(topic, gamma) %>% 
  left_join(ddd)

p <- docs_with_gamma %>% 
  select(`1`:`18`, grp) %>% 
  group_by(grp) %>% 
  summarize_all(mean) %>% 
  gather(key, val, -grp)

p2 <- p %>% 
  mutate(key = as.factor(key)) %>% 
  ggplot(aes(x = grp, y = val, color = key)) +
  geom_point() +
  geom_smooth(se = F, alpha = .5) +
  theme_bw() 

p2

# Cowplot
po <- cowplot::plot_grid(p1, p2, ncol = 1)
po

p1
