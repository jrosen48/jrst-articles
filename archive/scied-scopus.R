library(tidyverse)

d1 <- read_csv("scied-scopus/scopus.csv")
d1 <- select(d1, -`Funding Text 1`, -`Funding Text 2`, -`Funding Text 3`)
d2 <- read_csv("scied-scopus/scopus(1).csv")
d3 <- read_csv("scied-scopus/scopus(2).csv")

d <- rbind(d1, d2, d3)

# do$date <- lubridate::parse_date_time(d$date, "b! Y!")

f <- function(term1, term2, d) {
  tp <- d %>%
    mutate(Abstract = tolower(Abstract),
           t1var= as.integer(str_detect(Abstract, term1)),
           t2var = as.integer(str_detect(Abstract, term2)))
  
  tp %>% 
    group_by(Year) %>% 
    summarize(s_inquiry = sum(t1var, na.rm = T) / n(),
              s_practices = sum(t2var, na.rm = T) / n()) %>% 
    gather(key, val, -Year) %>% 
    ggplot(aes(x = Year, y = val, group = key, color = key)) +
    geom_point() +
    # geom_line() +
    theme_bw() +
    scale_y_continuous(labels = scales::percent) +
    ylab(NULL) +
    xlab(NULL) +
    scale_color_viridis_d("", labels = c(toupper(term1), toupper(term2)), option = "C", direction = -1) +
    geom_smooth(se = F) +
    labs(title = str_c(toupper(term1), " and ", term2, " in the abstracts of Science Education (1916-1918) research articles"),
         caption = "See TOS from Wiley here: https://onlinelibrary.wiley.com/doi/abs/10.1111/jbl.12120")
  
  # ggsave("scied-inquiry-practices.png", width = 16.18/2, height = 10/2)
}

f("teaching", "learning", d)
