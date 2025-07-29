# devtools::install_github("ppatrzyk/lastfmR")
# https://github.com/ppatrzyk/lastfmR

library(lastfmR)
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

scrobbles_tags <- scrobbles |> 
  tibble() |> 
  # distinct(artist) |> 
  left_join(artist_info |> select(artist, artist_tags))

scrobbles_tags_2 <- scrobbles_tags |> 
  separate(artist_tags, sep = "; ", 
           into = paste("artist_tags", 1:10, sep = "_"))

scrobbles_tags_3 <- scrobbles_tags_2 |> 
  pivot_longer(cols = starts_with("artist_tags"),
               names_to = "tags_n", values_to = "tags") |> 
  filter(!is.na(tags)) |> 
  mutate(tags = tolower(tags))

scrobbles_tags_3

scrobbles_tags_4 <- scrobbles_tags_3 |> 
  filter(tags != "seen live",
         !tags %in% c("metal", "electronic", "hardcore", "experimental")) |> 
  add_count(tags, name = "tag_n")

n_tags <-  15

top_tags <- scrobbles_tags_4 |> 
  distinct(tags, tag_n) |> 
  slice_max(tag_n, n = n_tags) |> 
  pull(tags)

top_tags

scrobbles_tags_4b <- scrobbles_tags_4 |> 
  filter(tags %in% top_tags) |> 
  group_by(date) |> 
  slice_max(tag_n, n = 1)

scrobbles_tags_5 <- scrobbles_tags_4b |> 
  # mutate(date = as_datetime(uts)) |> 
  mutate(date = floor_date(date, "week")) |> 
  count(date, tags)


# visualizar ----
library(ggplot2)
library(ggview)
library(scales)
library(stringr)

# install.packages("ggstream")
# https://github.com/davidsjoberg/ggstream
library(ggstream)

paleta <- c("#C490FF",
            "#62D4B6",
            "#223689",
            "#5911AC",
            "#894BD2",
            "#165159",
            "#3262B8"
)

color_base = "#5911AC"
color_fondo = col_mix(color_base, "black", amount = 0.8)
color_texto = col_mix(color_base, "white", amount = 0.7)
color_detalle = col_mix(color_base, "white", amount = 0.1) |> 
  col_saturate(-15)

show_col(c(color_base, color_fondo, color_texto, color_detalle))

theme_set(
  theme(panel.background = element_rect(color = color_fondo, fill = color_fondo),
        plot.background = element_rect(color = color_fondo, fill = color_fondo),
        legend.background = element_rect(color = color_fondo, fill = color_fondo),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        text = element_text(color = color_texto),
        legend.key.size = unit(3, "mm"),
        legend.key.spacing.y = unit(2, "mm"),
        legend.margin = margin(l = 2, t = 10),
        plot.margin = margin(4, 4, 4, 4, "mm"))
)

fechas <- c("2024-09-15", "2024-12-15", "2025-03-15", "2025-06-15")

scrobbles_tags_5 |> 
  filter(date > "2024-08-1") |> 
  ggplot() +
  aes(date, n, color = tags, fill = tags) +
  geom_stream(bw = .7, sorting = "inside_out", 
              color = color_fondo, size = 0.1, show.legend = F) +
  geom_point(size = 0, alpha = 0) +
  geom_stream_label(aes(label = case_when(tags %in% top_tags[1:7] ~ str_wrap(tags, 12),
                                          .default = "")),
                    bw = .7, sorting = "inside_out",
                    color = "white", size = 2, fontface = "bold", lineheight = 0.8,
                    hjust = 0.5, vjust = 0.5, n_grid = 200) +
  # escala de colores
  scale_fill_manual(values = colorRampPalette(paleta)(n_tags),
                    aesthetics = c("color", "fill")) +
  # escala horizontal de fechas
  annotate(geom = "text",
           x = as.POSIXct(fechas),
           label = format(as_date(fechas), "%m/%y"),
           y = -160, color = color_detalle, size = 3) +
  coord_cartesian(expand = F, clip = "off") +
  guides(fill = guide_none(),
         color = guide_legend(title = NULL, override.aes = list(size = 3, alpha = 1))) +
  labs(title = "Géneros musicales más escuchados",
       caption = paste0("Last.fm/user/", user)) +
  theme(plot.title = element_text(face = "bold", margin = margin(b = -10), hjust = 0),
        plot.caption = element_text(color = color_detalle, hjust = 1, size = 8, vjust = 0),
        plot.caption.position = "plot") +
  canvas(8, 5)

# guardar
# save_ggplot(plot = last_plot(), "lastfm_tags_bastimapache.png")
