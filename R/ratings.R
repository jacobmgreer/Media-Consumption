library(tidyverse)
library(rvest)
library(jsonlite)

RATINGS <- Sys.getenv("RATINGS")
OMDBkey <- Sys.getenv("OMDB")

ratingslist <- read_csv("raw-lists/ratings.csv")

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


test <- anti_join(rated, ratingslist, by="IMDBid") %>%
  rowwise %>%
  mutate(Response = list(fromJSON(content(GET(paste0('https://www.omdbapi.com/?i=',IMDBid,OMDBkey)), 'text'), simplifyVector = TRUE, flatten = TRUE))) %>%
  unnest_wider(Response) %>%
  filter(Response != "False") %>%
  unnest(cols = c(Ratings), names_sep = ".") %>%
  spread(Ratings.Source, Ratings.Value) %>%
  select(-c(DVD,BoxOffice,Production,Website,Response)) %>%
  mutate(
    Rated.Date = as.character(as.Date(str_remove(Rated.Date, "Rated on "), format = "%d %b %Y")),
    Rated.Year = as.double(paste0(year(Rated.Date),".",yday(Rated.Date))),
    Released = year(as.Date(Released, format = "%d %b %Y")),
    Rating = as.numeric(Rating)
  ) %>%
  arrange(desc(Rated.Date))

bind_rows(ratingslist, test) %>%
write.csv(., "datasets/ratings.csv", row.names = FALSE)
