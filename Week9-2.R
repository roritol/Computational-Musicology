library(tidyverse)
library(spotifyr)
library(compmus)

myclientid <- 'd7e2a7e91dc44321ae9ebe078fdf78e6'
myclientsecret <- '242e54cc07b6432db2ef073328971bbc'

Sys.setenv(SPOTIFY_CLIENT_ID=myclientid)
Sys.setenv(SPOTIFY_CLIENT_SECRET=myclientsecret)

bzt <-
  get_tidy_audio_analysis("5ZLkc5RY1NM4FtGWEd6HOE") %>% # Change URI.
  compmus_align(sections, segments) %>%                     # Change `bars`
  select(sections) %>%                                      #   in all three
  unnest(sections) %>%                                      #   of these lines.
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
  )

bzt %>%
  compmus_gather_timbre() %>%
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = basis,
      fill = value
    )
  ) +
  geom_tile() +
  labs(x = "Time (s)", y = NULL, fill = "Magnitude") +
  scale_fill_viridis_c() +                              
  theme_classic()

