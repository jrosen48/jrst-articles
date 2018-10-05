library(tidyverse)

l <- read_rds("all-jrst-data.rds")

abstracts <- map(l, ~ .[["abstract"]])
dates <- map(l, ~ .[["date"]])

do <- tibble(date = unlist(dates),
             abstract = unlist(abstracts))

do$date <- lubridate::parse_date_time(do$date, "b! Y!")

tp <- do %>%
  mutate(year = lubridate::year(date),
         abstract = tolower(abstract),
         inquiry= as.integer(str_detect(abstract, "teaching")),
         practices = as.integer(str_detect(abstract, "learning")))

tp %>% 
  group_by(year) %>% 
  summarize(s_inquiry = sum(inquiry, na.rm = T) / n(),
            s_practices = sum(practices, na.rm = T) / n()) %>% 
  gather(key, val, -year) %>% 
  ggplot(aes(x = year, y = val, group = key, color = key)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  ylab(NULL) +
  xlab(NULL) +
  scale_color_viridis_d("Abstracts Contains", labels = c("Teaching", "Learning"), option = "E", direction = -1) +
  geom_smooth(se = F) +
  labs(title = "Is 'teaching' or 'learning' included more in the abstracts of science education research articles?",
       subtitle = "Analysis of Journal of Research in Science Teaching (JRST) abstracts from 1963-2018",
       caption = "See TOS from Wiley here: https://onlinelibrary.wiley.com/doi/abs/10.1111/jbl.12120")

ggsave("jrst-teaching-learning.png", width = 16.18/2, height = 10/2)
