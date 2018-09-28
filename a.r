library(tidyverse)
library(rvest)

b <- "https://onlinelibrary.wiley.com/"
btoc <- "https://onlinelibrary.wiley.com/loi/10982736/year/"
i <- "https://onlinelibrary.wiley.com/loi/10982736/"

t <- i %>% 
  read_html() %>% 
  html_nodes(".loi-tab-item") %>% 
  html_text()
Sys.sleep(1)

# creates year URL
yd <- t %>% 
  as_data_frame() %>% 
  filter(str_detect(value, "Volume")) %>% 
  mutate(year = str_sub(value, start = 1, end = 4),
         volume = str_sub(value, start = -2, end = -1),
         volume = str_trim(volume)) %>% 
  mutate(year_url = str_c(btoc, year, sep = ""))

# getting article URLs

for (year in seq(nrow(yd))) {
  # gets issue URLs for a single year/volume
  iu <- yd$year_url[year] %>% 
    read_html() %>% 
    html_nodes(".parent-item a") %>% 
    html_attr("href") %>% 
    str_sub(start = 2) %>%  
    str_c(b, ., sep = "")
  Sys.sleep(1)
  
  for (issue in seq(length(iu))) {
    # gets article URLs for a single issue
    au <- iu[issue] %>% 
      read_html() %>% 
      html_nodes(".issue-item__details li:nth-child(1) a") %>% 
      html_attr("href") %>% 
      str_sub(start = 2) %>%  
      str_c(b, ., sep = "")
    write_rds(au, str_c("data/year-", year, "-issue-", issue, ".rds", sep = ""))
    message(str_c("Processed data/year-", year, "-issue-", issue, ".rds", sep = ""))
    Sys.sleep(1)
  }
}

# loading data

r <- function(fname) {
  str_c("data/", fname, sep = "")
}

v <- list.files("data") %>%
  map(r) %>%
  map(read_rds) %>%
  unlist() %>% 
  as_data_frame()

write_csv(v, "all-jrst-articles.csv")
