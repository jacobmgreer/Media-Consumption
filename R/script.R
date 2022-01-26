## load Personal Ratings on IMDB
nextlink <- 'https://www.imdb.com/user/ur28723514/ratings/'
count <-
  read_html(nextlink) %>%
  html_nodes(., '#lister-header-current-size') %>%
  html_text(.) %>%
  parse_number(.)
IMDBratings <- data.frame()
for (i in 1:ceiling(count/100)) {
  link <- read_html(nextlink)
  page <-
    data.frame(
      ItemTitle= link %>% html_nodes(.,'.lister-item-header a:first-of-type') %>% html_text(.) %>% gsub("^\\s+|\\s+$", "", .),
      IMDBid= link %>% html_nodes(.,'.lister-item-image') %>% html_attr("data-tconst"),
      Rating= link %>% html_nodes(.,'div.lister-item-content > div.ipl-rating-widget > div.ipl-rating-star.ipl-rating-star--other-user.small > span.ipl-rating-star__rating') %>% html_text(.),
      Rated.Date= link %>% html_nodes(.,'div.ipl-rating-widget + p') %>% html_text(.)
    )
  IMDBratings <- rbind(IMDBratings, page)
  nextlink <- paste0("https://www.imdb.com",link %>% html_nodes(.,'#ratings-container > div.footer.filmosearch > div > div > a.flat-button.lister-page-next.next-page') %>% html_attr("href"))
}

write.csv(IMDBratings,paste0("vinyl/data_", make.names(Sys.time()), ".csv"), row.names = FALSE)
