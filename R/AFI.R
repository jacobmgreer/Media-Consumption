library(tidyverse)
library(rvest)
library(jsonlite)
library(httr)
library(lubridate)
library(magrittr)
library(numform)

options(readr.show_col_types = FALSE)
options(warn=-1)

recoded.lineup <-
  read_csv("datasets/AFI-Silver/recoded-lineup.csv") %>%
  mutate(FilmID = gsub("/.*","",str_remove(IMDB, "https://www.imdb.com/title/"))) %>%
  select(-c(Title,IMDB))

AFIlineup <-
  read_html("https://silver.afi.com/Browsing/Movies/NowShowing") %>%
  html_nodes('#movies-list > .movie') %>%
  map_df(~{
    Poster <- .x  %>% html_node(.,'div.image-outer div img') %>% html_attr("src")
    Trailer= .x %>% html_nodes(.,'div.image-outer a.play') %>% html_attr("href")
    Title <- .x %>% html_nodes(.,'div.item-details h3.item-title') %>% html_text(.)
    Screening <- .x %>% html_nodes(.,'div.main-action a') %>% html_attr("href")
    tibble(Poster,Trailer,Title,Screening)
  }) %>%
  mutate(
    TheatreID = str_remove(Screening, "//silver.afi.com/Browsing/Movies/Details/"),
    Title = trimws(Title)) %>%
  left_join(., recoded.lineup, by = "TheatreID")

uncoded.lineup <- AFIlineup %>%
  mutate(
    TheatreID = str_remove(Screening, "//silver.afi.com/Browsing/Movies/Details/"),
    Title = trimws(Title)) %>%
  select(TheatreID, Title) %>%
  anti_join(., recoded.lineup, by = c("TheatreID")) %>%
  mutate(IMDB = "") %T>%
  write.csv(., "datasets/AFI-Silver/uncoded-lineup.csv", row.names = FALSE)

showtimes <- data.frame()
for (i in 1:nrow(AFIlineup)) {
  link <- read_html(paste0("http:",AFIlineup$Screening[i]))
  page <-
    data.frame(
      FilmID = AFIlineup$FilmID[i],
      TheatreID = AFIlineup$TheatreID[i],
      ScreenTitle = AFIlineup$Title[i],
      ScreenTrailer = AFIlineup$Trailer[i],
      ScreenPoster = AFIlineup$Poster[i],
      ScreenDate= link %>% html_node(.,'.film-showtimes .session .session-date') %>% html_text(.),
      ScreenTimestamp= link %>% html_nodes(.,'.film-showtimes .session .session-times time') %>% html_attr("datetime"),
      ScreenTime= link %>% html_nodes(.,'.film-showtimes .session .session-times time') %>% html_text(.),
      #ScreenTimeDetails= link %>% html_nodes(.,'.film-showtimes .session .session-times a'),
      ScreenTicketLink= link %>% html_nodes(.,'.film-showtimes .session .session-times a') %>% html_attr("href"),
      ScreenGenre= link %>% html_nodes(.,'.film-info-item > div:nth-child(2) > span') %>% html_text(.),
      ScreenRuntime= link %>% html_nodes(.,'.film-info-item > div:nth-child(1) > span') %>% html_text(.)
    )
  showtimes <- rbind(showtimes, page)
}

theatres <- data.frame()
for (i in 1:nrow(showtimes)) {
  link <- read_html(paste0("http:",showtimes$ScreenTicketLink[i]))
  page <-
    data.frame(
      TheatreID = showtimes$TheatreID[i],
      ScreenTicketLink= showtimes$ScreenTicketLink[i],
      ScreenTheatre= link %>% html_node(.,'.cinema-screen-name') %>% html_text(.)
    )
  theatres <- rbind(theatres, page)
}

showtimes <- left_join(showtimes, theatres, by = c("TheatreID", "ScreenTicketLink"))

rm(page,link,i)
