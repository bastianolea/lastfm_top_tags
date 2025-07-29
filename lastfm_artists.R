library(readr)

# obtener ----
user <- "bastimapache"
# scrobbles <- get_scrobbles(user = user)
# artist_info <- get_library_info(user = user)

# readr::write_rds(scrobbles, "~/Downloads/scrobbles.rds")
# readr::write_rds(artist_info, "~/Downloads/artist_info.rds")

# cargar datos
scrobbles <- read_rds("scrobbles.rds")
artist_info <- read_rds("artist_info.rds")
# scrobbles <- read_csv("recenttracks-bastimapache-1753457720.csv")


# procesar ----
library(dplyr)

artist_info |> tibble()
scrobbles |> tibble()

library(lubridate)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggview)
library(scales)
library(ggstream)


scrobbles_2 <- scrobbles |> 
  tibble() |> 
  mutate(date = floor_date(date, "week"),
         date = as_date(date)) |>  
  mutate(artist = str_remove(artist, "&.*"),
         artist = str_remove(artist, "feat/..*"),
         artist = str_trim(artist))

top_artists <- scrobbles_2 |> 
  filter(year(date) >= 2024) |> 
  count(artist) |> 
  slice_max(n, n = 10) |> 
  pull(artist)

scrobbles_conteo <- scrobbles_2 |> 
  mutate(artist = if_else(artist %in% top_artists, artist, "Otros")) |> 
  count(date, artist)

scrobbles_conteo |> 
  distinct(artist)

scrobbles_conteo |> 
  filter(date > "2024-08-1") |> 
  filter(artist != "Otros") |>
  ggplot() +
  aes(date, n, fill = artist) +
  geom_stream(#bw = .7, 
    sorting = "onset",
    # sorting = "inside_out", 
              linewidth = 0.5, show.legend = F)


top_artists_b <- scrobbles_2 |> 
  filter(year(date) >= 2024) |> 
  count(artist) |> 
  slice_max(n, n = 30) |> 
  pull(artist)


artists_p <- scrobbles_2 |> 
  count(artist, name = "n_artist") |> 
  mutate(p_artist = n_artist/sum(n_artist),
         rand = sample(seq(0.001, 0.009, 0.001), n(), replace = T),
         p_artist = p_artist + rand) |> 
  arrange(desc(p_artist))



scrobbles_rank <- scrobbles_2 |> 
  count(date, artist) |> 
  left_join(artists_p) |> 
  group_by(date) |> 
  # filter(n > 1) |> 
  mutate(rank = dense_rank(n)) |> 
  filter(rank <= 5) |> 
  group_by(date) |> 
  arrange(date, rank, p_artist) |> 
  add_count(rank, name = "n_rank") |> 
  mutate(repetido = n_rank > 1) |> 
  fill(repetido, .direction = "up") |> 
  group_by(date, n_rank, repetido) |> 
  mutate(resolver = !repetido | repetido & p_artist == max(p_artist)) |> 
  filter(resolver) |>
  # filter(rank > 8) |> 
  print(n = 40)

scrobbles_rank |> 
  filter(date == "2025-07-24")


scrobbles_rank_2 <- scrobbles_rank |> 
  group_by(date) |>
  mutate(rank_day = n()) |>
  filter(rank_day == 5) |>
  ungroup()

scrobbles_rank_3 <- scrobbles_rank_2 |> 
  mutate(fecha = format(date, "%d/%m/%y"),
         fecha = forcats::fct_reorder(fecha, date))

scrobbles_rank_3 |> 
  filter(date > max(date)-months(4)) |> 
  ggplot() +
  aes(fecha, rank) +
  geom_tile(color = "white", linewidth = 0.2) +
  geom_text(aes(label = str_wrap(artist, 16)), color = "white", size = 3) +
  scale_y_continuous(transform = "reverse")

scrobbles_rank |> 
  filter(date == "2025-06-01")


# install.packages("ggbump")
library(ggbump)

scrobbles_rank_3 |> 
  filter(date > max(date)-months(4)) |> 
  ungroup() |> 
  # mutate(artist = if_else(artist %in% top_artists, artist, "Otros")) |>
  # filter(date > max(date)-weeks(10)) |> 
  ggplot(aes(date, rank, color = artist)) +
  geom_bump() +
  geom_label(aes(label = str_wrap(artist, 16)), color = "black", size = 3)
