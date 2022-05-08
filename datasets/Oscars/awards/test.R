library(tidyverse)
library(rvest)
library(jsonlite)
library(httr)
library(lubridate)
library(magrittr)
library(numform)

Streaming.Available <-
  rbind(read_csv("raw-lists/Prime-Docs.csv") %>%
          mutate(Service = "Prime") %>%
          mutate(Type = "Documentary"),
        read_csv("raw-lists/Prime-Films.csv") %>%
          mutate(Service = "Prime") %>%
          mutate(Type = "Feature Film")) %>%
  rbind(., read_csv("raw-lists/Prime-OscarShorts.csv") %>%
          mutate(Service = "Prime") %>%
          mutate(Type = "Shorts"))


AwardData <-
  OscarsTracking %>%
  left_join(., Streaming.Available, by=c("FilmID" = "IMDBid")) %>%
  filter(!is.na(FilmID)) %>%
  mutate(
    Seen = ifelse(is.na(Rating), "No", "Yes"),
    AwardCategory = ifelse(AwardCategory %in%
                             c("Best Short Subject, Animated Films",
                               "Best Animated Short Film",
                               "Best Short Subject, Cartoons"), "Best Short Film, Animated", AwardCategory),
    AwardCategory = ifelse(AwardCategory %in%
                             c("Best Sound Editing",
                               "Best Sound, Recording",
                               "Best Effects, Sound Effects",
                               "Best Achievement in Sound Editing",
                               "Best Achievement in Sound Mixing",
                               "Best Effects, Sound Effects Editing",
                               "Best Sound Mixing"), "Best Sound", AwardCategory),
    AwardCategory = ifelse(AwardCategory %in%
                             c("Best Art Direction-Interior Decoration, Black-and-White",
                               "Best Art Direction-Interior Decoration, Color",
                               "Best Art Direction-Set Decoration",
                               "Best Art Direction-Set Decoration, Black-and-White",
                               "Best Art Direction-Set Decoration, Black-and-White or Color",
                               "Best Art Direction-Set Decoration, Color",
                               "Best Achievement in Art Direction",
                               "Best Achievement in Production Design",
                               "Best Art Direction, Black-and-White",
                               "Best Art Direction",
                               "Best Art Direction, Color"), "Best Production Design", AwardCategory),
    AwardCategory = ifelse(AwardCategory %in%
                             c("Best Achievement in Makeup",
                               "Best Achievement in Makeup and Hairstyling",
                               "Best Makeup"), "Best Makeup and Hairstyling", AwardCategory),
    AwardCategory = ifelse(AwardCategory %in%
                             c("Best Assistant Director"), "Best Assistant Director", AwardCategory),
    AwardCategory = ifelse(AwardCategory %in% c("Best Achievement in Film Editing"), "Best Film Editing", AwardCategory),
    AwardCategory = ifelse(AwardCategory %in% c("Best Performance by an Actor in a Leading Role"), "Best Actor in a Leading Role", AwardCategory),
    AwardCategory = ifelse(AwardCategory %in% c("Best Performance by an Actor in a Supporting Role"), "Best Actor in a Supporting Role", AwardCategory),
    AwardCategory = ifelse(AwardCategory %in% c("Best Performance by an Actress in a Leading Role"), "Best Actress in a Leading Role", AwardCategory),
    AwardCategory = ifelse(AwardCategory %in% c("Best Performance by an Actress in a Supporting Role"), "Best Actress in a Supporting Role", AwardCategory),
    AwardCategory = ifelse(AwardCategory %in%
                             c("Best Foreign Language Film",
                               "Best International Feature Film",
                               "Best Foreign Language Film of the Year"), "Best International Feature", AwardCategory),
    AwardCategory = ifelse(AwardCategory %in%
                             c("Best Music, Scoring of a Dramatic or Comedy Picture",
                               "Best Music, Scoring of a Dramatic Picture",
                               "Best Music, Scoring",
                               "Best Music, Original Music Score",
                               "Best Music, Original Musical or Comedy Score",
                               "Best Music, Original Score",
                               "Best Music, Score",
                               "Best Music, Score - Substantially Original",
                               "Best Music, Original Song Score and Its Adaptation or Best Adaptation Score",
                               "Best Music, Original Score for a Motion Picture (not a Musical)",
                               "Best Music, Original Song",
                               "Best Music, Original Song Score",
                               "Best Music, Original Dramatic Score",
                               "Best Achievement in Music Written for Motion Pictures (Original Score)",
                               "Best Achievement in Music Written for Motion Pictures, Original Score",
                               "Best Music, Score of a Musical Picture (Original or Adaptation)",
                               "Best Music, Scoring Adaptation and Original Song Score",
                               "Best Music, Scoring of a Musical Picture",
                               "Best Music, Scoring of Music, Adaptation or Treatment",
                               "Best Music, Scoring Original Song Score and/or Adaptation",
                               "Best Achievement in Music Written for Motion Pictures (Original Song)",
                               "Best Achievement in Music Written for Motion Pictures, Original Song",
                               "Best Music, Substantially Original Score"), "Best Music", AwardCategory),
    AwardCategory = ifelse(AwardCategory %in%
                             c("Best Documentary, Short Subjects",
                               "Best Documentary Short Subject",
                               "Best Documentary, Short Subject",
                               "Best Documentary, Short Subjects"), "Best Short Film, Documentary", AwardCategory),
    AwardCategory = ifelse(AwardCategory %in%
                             c("Best Effects, Engineering Effects",
                               "Best Effects, Special Effects",
                               "Best Effects, Visual Effects",
                               "Best Achievement in Visual Effects",
                               "Best Effects, Special Visual Effects"), "Best Visual Effects", AwardCategory),
    AwardCategory = ifelse(AwardCategory %in%
                             c("Best Short Subject, Two-reel",
                               "Best Short Subject, One-reel",
                               "Best Short Subject, Novelty",
                               "Best Live Action Short Film",
                               "Best Short Subject, Live Action Subjects",
                               "Best Short Subject, Live Action Films",
                               "Best Short Subject, Comedy",
                               "Best Short Subject, Color"), "Best Short Film, Live Action", AwardCategory),
    AwardCategory = ifelse(AwardCategory %in%
                             c("Best Writing, Screenplay Adapted From Other Material",
                               "Best Writing, Screenplay Based on Material from Another Medium",
                               "Best Writing, Screenplay Based on Material Previously Produced or Published",
                               "Best Writing, Adapted Screenplay",
                               "Best Writing, Best Screenplay - Adapted",
                               "Best Writing, Original Story",
                               "Best Writing, Screenplay",
                               "Best Writing, Achievement",
                               "Best Writing, Original Screenplay",
                               "Best Writing, Motion Picture Story",
                               "Best Writing, Best Screenplay - Original",
                               "Best Writing, Story and Screenplay",
                               "Best Writing, Adaptation",
                               "Best Writing, Screenplay Written Directly for the Screen",
                               "Best Original Screenplay",
                               "Best Adapted Screenplay",
                               "Best Writing, Story and Screenplay - Written Directly for the Screen",
                               "Best Writing, Story and Screenplay Based on Factual Material or Material Not Previously Published or Produced",
                               "Best Writing, Story and Screenplay Based on Material Not Previously Published or Produced"), "Best Writing", AwardCategory))

# AwardSummary <-
#   AwardData %>%
#   dplyr::group_by(AwardCategory) %>%
#   dplyr::summarize(
#     Total.Y = n_distinct(FilmID[Seen == "Yes"]),
#     Total.N = n_distinct(FilmID[Seen == "No"]),
#     Total.Per = round(Total.Y/(Total.Y+Total.N), digits=2),
#     Prime.Y = n_distinct(FilmID[Seen == "Yes" & Service == "Prime"]),
#     Prime.N = n_distinct(FilmID[Seen == "No" & Service == "Prime"]),
#     Prime.Per = round(Prime.Y/(Prime.Y+Prime.N), digits=2))

# ShortsOnPrime <-
#   AwardData %>%
#   filter(Service == "Prime") %>%
#   filter(Type.y == "Shorts") %>%
#   select(AwardWinner, FilmID, FilmName, Year, Runtime, Poster, imdbRating, imdbVotes, Rating, Rated.Date) %>%
#   distinct(FilmID, .keep_all = TRUE)
