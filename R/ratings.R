library(tidyverse)
library(rvest)
library(jsonlite)
library(httr)
library(lubridate)
library(magrittr)
library(numform)

RATINGS <- Sys.getenv("RATINGS")
OMDBkey <- Sys.getenv("OMDB")

ratingslist <- read_csv("datasets/ratings.csv") %>% mutate(totalSeasons = as.character(totalSeasons)) %>% select(-c('Response','<NA>')

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
  mutate(Response = list(fromJSON(content(GET(paste0('https://www.omdbapi.com/?i=',IMDBid,'&apikey=',OMDBkey)), 'text'), simplifyVector = TRUE, flatten = TRUE))) %>%
  unnest_wider(Response) %>%
  filter(Response != "False") %>%
  unnest(cols = c(Ratings), names_sep = ".") %>%
  spread(Ratings.Source, Ratings.Value) %>%
  select(-c(DVD,BoxOffice,Production,Website,Response)) %>% #,totalSeasons
  mutate(
    Rated.Date = as.Date(str_remove(Rated.Date, "Rated on "), format = "%d %b %Y"),
    Rated.Year = as.double(paste0(year(Rated.Date),".",yday(Rated.Date))),
    Released = year(as.Date(Released, format = "%d %b %Y")),
    Rating = as.numeric(Rating),
    imdbVotes = as.double(imdbVotes)
  )

myratings <- bind_rows(ratingslist, test) %>%
  arrange(desc(Rated.Date)) %>%
  select(-Title) %T>%
  write.csv(., "datasets/ratings.csv", row.names = FALSE)

## Prime Availability
Streaming.Available <-
  rbind(read_csv("raw-lists/Prime-Docs.csv") %>%
          mutate(Service = "Prime") %>%
          mutate(Type = "Documentary"),
        read_csv("raw-lists/Prime-Films.csv") %>%
          mutate(Service = "Prime") %>%
          mutate(Type = "Feature Film")) %>%
  rbind(.,
        read_csv("raw-lists/Prime-OscarShorts.csv") %>%
          mutate(Service = "Prime") %>%
          mutate(Type = "Short Films"))

## Oscar Ceremony Data for Summary and Graph
OscarCeremonies.corrected <- read_csv("raw-lists/OscarCeremonies.csv")
left_join(OscarCeremonies.corrected, myratings %>% select(IMDBid, Rating, Rated.Date), by=c("FilmID" = "IMDBid")) %>%
  left_join(., Streaming.Available %>% rename(Amazon.Type = Type, Prime = Service), by=c("FilmID" = "IMDBid")) %>%
  write.csv(.,"datasets/Oscars/OscarsTracking.csv", row.names = FALSE)
left_join(OscarCeremonies.corrected, myratings %>% select(IMDBid, Rating, Rated.Date), by=c("FilmID" = "IMDBid")) %>%
  filter(FilmID != "") %>%
  mutate(AwardWinner = ifelse(AwardWinner == "Winner",TRUE,FALSE)) %>%
  select(AwardCeremony, AwardWinner, FilmID, Rating) %>%
  distinct %>%
  dplyr::group_by(FilmID) %>%
  dplyr::mutate(
    filmwon=ifelse(any(AwardWinner),TRUE,FALSE),
    filmwon=ifelse(all(is.na(filmwon)),FALSE,filmwon)
  ) %>%
  dplyr::mutate(keep_row=ifelse(filmwon,AwardWinner,TRUE)) %>%
  dplyr::filter(!(filmwon == TRUE & is.na(keep_row))) %>%
  ungroup %>%
  mutate(
    Seen = ifelse(is.na(Rating), FALSE, TRUE),
    Year = sub('.*-', '', AwardCeremony),
    Ceremony = ifelse(f_ordinal(sub("\\-.*", "", str_remove(AwardCeremony, "^0+"))) == 13, "13th", f_ordinal(sub("\\-.*", "", str_remove(AwardCeremony, "^0+")))),
    Menu = paste0("<h5>",Ceremony," Academy Awards</h5><h1>",Year,"</h1>")) %>%
  dplyr::group_by(AwardCeremony, Year) %>%
  dplyr::summarise(
    Winner.Y =
      ifelse(
        any(Seen == TRUE & AwardWinner == TRUE),
        ifelse(
          any(AwardWinner == TRUE),
          n_distinct(FilmID[Seen == TRUE & AwardWinner == TRUE]),
          NA),
        NA),
    Winner.N =
      ifelse(
        any(Seen == FALSE & AwardWinner == TRUE),
        ifelse(
          any(AwardWinner == TRUE),
          n_distinct(FilmID[Seen == FALSE & AwardWinner == TRUE]),
          NA),
        NA),
    Nominee.Y =
      ifelse(
        any(Seen == TRUE & is.na(AwardWinner)),
        n_distinct(FilmID[Seen == TRUE & is.na(AwardWinner)]),
        NA),
    Nominee.N =
      ifelse(
        any(Seen == FALSE & is.na(AwardWinner)),
        n_distinct(FilmID[Seen == FALSE & is.na(AwardWinner)]),
        NA)) %>%
  arrange(Year) %>%
  select(-Year) %>%
  write.csv(.,"datasets/Oscars/OscarsSummary.csv", row.names = FALSE)

## NYT-1000 Data for Summary and Graph
combinedNYT1000 <-
  left_join(read_csv("raw-lists/nyt1000.csv"), myratings %>% select(IMDBid, Rating, Rated.Date), by="IMDBid") %>%
    mutate(Seen = ifelse(is.na(Rating), "No", "Yes")) %>%
    left_join(., Streaming.Available, by="IMDBid") %T>%
    write.csv(.,"datasets/NYT1000/NYT1000Data.csv", row.names = FALSE)
combinedNYT1000 %>%
  dplyr::group_by(ItemYear = as.numeric(ItemYear)) %>%
  dplyr::summarize(
    Y = n_distinct(IMDBid[Seen == "Yes"]),
    N = n_distinct(IMDBid[Seen == "No"])) %>%
  select(ItemYear, Y, N) %>%
  write.csv(.,"datasets/NYT1000/NYT1000Summary.csv", row.names = FALSE)

## Great Films Ebert
left_join(read_csv("raw-lists/ebert.csv"), myratings %>% select(IMDBid, Rating, Rated.Date), by="IMDBid") %>%
  mutate(Decade = paste0(10 * floor(as.numeric(ItemYear)/10),"s")) %>%
  mutate(Seen = ifelse(is.na(Rating), "No", "Yes")) %>%
  left_join(., Streaming.Available, by="IMDBid") %>%
  write.csv(.,"datasets/GreatFilmsEbert/Data.csv", row.names = FALSE)
## AFI Top 100 from 1998
left_join(read_csv("raw-lists/afi1998.csv"), myratings %>% select(IMDBid, Rating, Rated.Date), by="IMDBid") %>%
  mutate(Decade = paste0(10 * floor(as.numeric(ItemYear)/10),"s")) %>%
  mutate(Seen = ifelse(is.na(Rating), "No", "Yes")) %>%
  left_join(., Streaming.Available, by="IMDBid") %>%
  write.csv(.,"datasets/AFITop100/1998/Data.csv", row.names = FALSE)
## AFI Top 100 from 2007
left_join(read_csv("raw-lists/afi2007.csv"), myratings %>% select(IMDBid, Rating, Rated.Date), by="IMDBid") %>%
  mutate(Decade = paste0(10 * floor(as.numeric(ItemYear)/10),"s")) %>%
  mutate(Seen = ifelse(is.na(Rating), "No", "Yes")) %>%
  left_join(., Streaming.Available, by="IMDBid") %>%
  write.csv(.,"datasets/AFITop100/2007/Data.csv", row.names = FALSE)

#mutate(Decade = floor(as.numeric(ItemYear)/10)*10)
