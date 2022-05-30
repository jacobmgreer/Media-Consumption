ScreeningCal <- function(showtimes2) {
  ics_header <- readLines("ics_template/template_header.ics", warn = F)
  ics_body <- readLines("ics_template/template_body.ics", warn = F)
  ics_footer <- readLines("ics_template/template_footer.ics", warn = F)

  ics_events <- ""
  for(i in 1:nrow(showtimes2)) {
    ics_body <- str_replace(ics_body, "UID:.*", paste0("UID:", showtimes2$SessionID[i]))
    ics_body <- str_replace(ics_body, "DTSTAMP:.*", paste0("DTSTAMP:", format(now(tzone = "EST"), "%Y%m%dT%H%M%S")))
    ics_body <- str_replace(ics_body, "DTSTART:.*", paste0("DTSTART:", format(ymd_hms(showtimes2$ScreenTimestamp[i]), "%Y%m%dT%H%M%S")))
    ics_body <- str_replace(ics_body, "DTEND:.*", paste0("DTEND:", format(ymd_hms(showtimes2$ScreenTimestamp[i]) + minutes(str_remove(showtimes2$ScreenRuntime[i],' Minutes')), "%Y%m%dT%H%M%S")))
    ics_body <- str_replace(ics_body, "SUMMARY:.*", paste0("SUMMARY:", iconv(showtimes2$ScreenTitle[i], "UTF-8", "ASCII", sub = "")))
    ics_body <- str_replace(ics_body, "LOCATION:.*", paste0("LOCATION:", showtimes2$ScreenTheatre[i]))
    ics_body <- str_replace(ics_body, "DESCRIPTION:.*", paste0("DESCRIPTION:http:", showtimes2$ScreenTicketLink[i]))
    ics_events <- append(ics_events, ics_body)
  }
  ics_events <- append(ics_header, ics_events)
  ics_events <- append(ics_events, ics_footer)
  return(ics_events)
}

ScreeningCal(showtimes %>% filter(grepl('1', ScreenTheatre))) %>%
  write(., file = "datasets/AFI-Silver/Theater1.ics")
ScreeningCal(showtimes %>% filter(grepl('2', ScreenTheatre))) %>%
  write(., file = "datasets/AFI-Silver/Theater2.ics")
ScreeningCal(showtimes %>% filter(grepl('3', ScreenTheatre))) %>%
  write(., file = "datasets/AFI-Silver/Theater3.ics")
