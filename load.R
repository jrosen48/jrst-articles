library(tidyverse)
library(rvest)

d <- read_csv("all-scied-articles.csv")

proc_article <- function(article) {
  
  h <- read_html(article)
  
  keywords <- h %>% 
    html_nodes(xpath = '//meta[@name="citation_keywords"]/@content') %>%
    html_text()
  
  title <- h %>% 
    html_nodes(xpath = '//h2[@class="citation__title"]') %>% 
    html_text()
  
  abstract <- h %>% 
    html_node("#section-1-en p") %>% 
    html_text() %>% 
    str_replace_all("[\r\n]" , "") %>% 
    str_replace_all("(?<=[\\s])\\s*|^\\s+|\\s+$", "")
  
  authors <- h %>% 
    html_node(".accordion-tabbed") %>% 
    html_text() %>% 
    str_replace_all("[\r\n]" , "") %>% 
    str_replace_all("(?<=[\\s])\\s*|^\\s+|\\s+$", "")
  
  funding <- h %>% 
    html_node(".no-truncate p") %>% 
    html_text() %>% 
    str_replace_all("[\r\n]" , "") %>% 
    str_replace_all("(?<=[\\s])\\s*|^\\s+|\\s+$", "")
  
  volume_issue <- h %>% 
    html_nodes(".val") %>% 
    html_text()
  
  date <- h %>% 
    html_node(".volume-issue+ p") %>% 
    html_text()
  
  l <- list(title=title, 
            keywords=keywords,
            abstract=abstract,
            authors=abstract,
            funding=funding,
            volume_issue=volume_issue,
            date=date)

  l

}

safe_proc_article <- function(i, d) {
  out <- tryCatch(
    {
      x <- proc_article(d$value[i])
      write_rds(x, str_c("data/scied/", i, ".rds"))
      x
    },
    error=function(cond) {
      message(str_c("Article caused an error:", i), " (", i, "/", nrow(d), ")")
      message(cond)
      # Choose a return value in case of error
      return(NULL)
    },
    warning=function(cond) {
      message(str_c("Article caused a warning: ", i), " (", i, "/", nrow(d), ")")
      message(cond)
      return(NULL)
    },
    finally={
      message(str_c("Processed article: ", i), " (", i, "/", nrow(d), ")")
    }
  )    
  return(out)
}

l <- seq(nrow(d)) %>% 
  map(safe_proc_article, d = d)
