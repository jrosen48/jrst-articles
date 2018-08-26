library(tidyverse)
library(rvest)

d <- read_csv("all-jrst-articles.csv")

# did not work: 1048

for (article in 1049:nrow(d)) {
  d$value[article] %>% 
    read_html() %>% 
    write_xml(str_c("article-html/", article, ".html"))
  message(str_c("Processed article-html/", article, ".html"))
  Sys.sleep(1)
}



f <- function(article) {
  message(str_c("Processing article-html/", article, ".html"))
  
  d$value[article] %>% 
    read_html() %>% 
    possibly(write_xml(str_c("article-html/", article, ".html")), message("error!"))
  
  Sys.sleep(1)
}

d$value[1630:1631] %>% 
  map(f)

d$value[1630:1631]
