library(tidyverse)
library(rvest)

RECORDS <- Sys.getenv("RECORDS")

## VINYL COLLECTION
count <-
  read_html(RECORDS) %>%
  html_nodes(., '#full-width-header-bar > div > div > ul > li.active_header_section > a > small') %>%
  html_text(.) %>%
  parse_number(.)

rated <- data.frame()
RECORDS <- paste0(RECORDS,"1&limit=250&header=1&layout=big")
for (i in 1:ceiling(count/250)) {
  page <-
    read_html(RECORDS) %>%
    html_nodes('#cards_list_releases > div') %>%
    map_df(~{
      Album <- .x  %>% html_node(.,'h4 > a') %>% html_attr("title")
      Jacket= .x %>% html_nodes(.,'a > span.thumbnail_center > img') %>% html_attr("data-src")
      Artist <- .x %>% html_nodes(.,'h5 > span') %>% html_attr("title")
      tibble(Album, Artist, Jacket)
    })
  rated <- bind_rows(rated, page)
  RECORDS <- paste0(RECORDS,i,"&limit=250&header=1&layout=big")
}

write.csv(rated, "datasets/albums.csv", row.names = FALSE)
