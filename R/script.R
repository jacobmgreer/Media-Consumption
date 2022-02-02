library(tidyverse)
library(rvest)

RATINGS <- Sys.getenv("RATINGS")
RECORDS <- Sys.getenv("RECORDS")

## MOVIE RATINGS
count <-
  read_html(RATINGS) %>%
  html_nodes(., '#lister-header-current-size') %>%
  html_text(.) %>%
  parse_number(.)

rated <- data.frame()
for (i in 1:ceiling(count/100)) {
  link <- read_html(RATINGS)
  page <-
    data.frame(
      ItemTitle= link %>% html_nodes(.,'.lister-item-header a:first-of-type') %>% html_text(.) %>% gsub("^\\s+|\\s+$", "", .),
      IMDBid= link %>% html_nodes(.,'.lister-item-image') %>% html_attr("data-tconst"),
      Rating= link %>% html_nodes(.,'div.lister-item-content > div.ipl-rating-widget > div.ipl-rating-star.ipl-rating-star--other-user.small > span.ipl-rating-star__rating') %>% html_text(.),
      Rated.Date= link %>% html_nodes(.,'div.ipl-rating-widget + p') %>% html_text(.)
    )
  rated <- rbind(rated, page)
  RATINGS <- paste0("https://www.imdb.com",link %>% html_nodes(.,'#ratings-container > div.footer.filmosearch > div > div > a.flat-button.lister-page-next.next-page') %>% html_attr("href"))
}

write.csv(rated, "datasets/ratings.csv", row.names = FALSE)

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
