library(tidyverse)
library(spotifyr)
library(compmus)
library(gridExtra)

myclientid <- 'd7e2a7e91dc44321ae9ebe078fdf78e6'
myclientsecret <- '242e54cc07b6432db2ef073328971bbc'

Sys.setenv(SPOTIFY_CLIENT_ID=myclientid)
Sys.setenv(SPOTIFY_CLIENT_SECRET=myclientsecret)

Netherlands <- get_playlist_audio_features("", "37i9dQZEVXbKCF6dqVpDkS")
Germany <- get_playlist_audio_features("", "37i9dQZEVXbJiZcmkrIHGU")
Italy <- get_playlist_audio_features("", "37i9dQZEVXbIQnj7RRhdSX")
Spain <- get_playlist_audio_features("", "37i9dQZEVXbNFJfN1Vw8d9")

#Nederland
#Dat heb jij gedaan: "6lgIi3ixBsr4cMt3r19yX9?si=2f7d887ffd674c9f"
#Vluchtstrook: "63JrADDfrcjOaI6lyVkN3c?si=637af6292456481e"
#Je Blik Richting Mij: "3mafhqH8nGsvyZTN86IEOc?si=d68602bc50564294"
#Amsterdam: "1YR7VB3xx85Iad2n3soyMb?si=8c889e72e90a4ef8"



n3 <- get_tidy_audio_analysis("63JrADDfrcjOaI6lyVkN3c?si=637af6292456481e") %>%
  compmus_align(bars, segments) %>%                     # Change `bars`
  select(bars) %>%                                      #   in all three
  unnest(bars) %>%                                      #   of these lines.
  mutate(
    pitches =
      map(segments,
          compmus_summarise, pitches,
          method = "rms", norm = "euclidean"              # Change summary & norm.
      )
  ) %>%
  mutate(
    timbre =
      map(segments,
          compmus_summarise, timbre,
          method = "rms", norm = "euclidean"              # Change summary & norm.
      )
  ) %>%
  compmus_self_similarity(timbre, "angular") %>% 
  ggplot(
    aes(
      x = xstart + xduration / 2,
      width = xduration,
      y = ystart + yduration / 2,
      height = yduration,
      fill = d
    )
  ) +
  geom_tile() +
  coord_fixed() +
  scale_fill_viridis_c(guide = "none") +
  theme_classic()

n4 <- get_tidy_audio_analysis("3mafhqH8nGsvyZTN86IEOc?si=d68602bc50564294") %>%
  compmus_align(bars, segments) %>%                     # Change `bars`
  select(bars) %>%                                      #   in all three
  unnest(bars) %>%                                      #   of these lines.
  mutate(
    pitches =
      map(segments,
          compmus_summarise, pitches,
          method = "rms", norm = "euclidean"              # Change summary & norm.
      )
  ) %>%
  mutate(
    timbre =
      map(segments,
          compmus_summarise, timbre,
          method = "rms", norm = "euclidean"              # Change summary & norm.
      )
  ) %>%
  compmus_self_similarity(timbre, "angular") %>% 
  ggplot(
    aes(
      x = xstart + xduration / 2,
      width = xduration,
      y = ystart + yduration / 2,
      height = yduration,
      fill = d
    )
  ) +
  geom_tile() +
  coord_fixed() +
  scale_fill_viridis_c(guide = "none") +
  theme_classic()

n1 <- get_tidy_audio_analysis("1YR7VB3xx85Iad2n3soyMb?si=8c889e72e90a4ef8") %>%
  compmus_align(bars, segments) %>%                     # Change `bars`
  select(bars) %>%                                      #   in all three
  unnest(bars) %>%                                      #   of these lines.
  mutate(
    pitches =
      map(segments,
          compmus_summarise, pitches,
          method = "rms", norm = "euclidean"              # Change summary & norm.
      )
  ) %>%
  mutate(
    timbre =
      map(segments,
          compmus_summarise, timbre,
          method = "rms", norm = "euclidean"              # Change summary & norm.
      )
  ) %>%
  compmus_self_similarity(timbre, "angular") %>% 
  ggplot(
    aes(
      x = xstart + xduration / 2,
      width = xduration,
      y = ystart + yduration / 2,
      height = yduration,
      fill = d
    )
  ) +
  geom_tile() +
  coord_fixed() +
  scale_fill_viridis_c(guide = "none") +
  theme_classic()



#Spanje
#MAMMII: 1ri9ZUkBJVFUdgwzCnfcYs?si=77af4612914e4220
#Desperados: 6mmPpaltUZK7xjNlBPQQ0p?si=bf01a5e752e34c41

 
 
  labs(x = "", y = "")