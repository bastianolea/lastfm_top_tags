library(readr)
library(dplyr)
library(ggplot2)
library(ggview)
library(scales)
library(stringr)

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

# configuraciones ----
# configurar gráficos
# paleta de colores del gráfico
color_base = "#5911AC"
color_fondo = col_mix(color_base, "black", amount = 0.8)
color_texto = col_mix(color_base, "white", amount = 0.7)
color_detalle = col_mix(color_base, "white", amount = 0.1) |> 
  col_saturate(-15)

paleta <- c("#C490FF",
            "#62D4B6",
            "#223689",
            "#5911AC",
            "#894BD2",
            "#165159",
            "#3262B8"
)

paleta_generos <- c(
  "metal extremo" = "#5911AC",
  "metal" = "#894BD2",
  "noise" = "#223689",
  "clásica" = "#165159",
  "jazz" = "#4BB398",
  "hardcore" = "#C490FF",
  "rock" = "#3262B8",
  "otros" = "#6D9187")

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
        plot.margin = margin(4, 6, 4, 4, "mm"))
)

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


# tags
artist_tags <- artist_info |> 
  select(artist, artist_tags) |> 
  tibble() |> 
  # separar
  separate(artist_tags, sep = "; ", 
           into = paste("artist_tags", 1:10, sep = "_")) |> 
  # pivotar a largo
  pivot_longer(cols = starts_with("artist_tags"),
               names_to = "tags_n", values_to = "tags") |> 
  filter(!is.na(tags)) |> 
  # limpiar
  mutate(tags = tolower(tags)) |> 
  select(-tags_n) |> 
  # filtrar irrelevantes
  filter(tags != "seen live") |> # !tags %in% c("metal", "electronic", "hardcore", "experimental")) |> 
  # agregar conteo de cada tag
  add_count(tags, name = "tag_n") |>
  # dejar sólo la principal de cada artista
  group_by(artist) |>
  slice(1) |> 
  ungroup() |> 
  arrange(desc(tag_n))

# contar scrobbles por semana, y limpiar artistas
scrobbles_semana <- scrobbles |> 
  tibble() |> 
  mutate(date = floor_date(date, "week"),
         date = as_date(date)) |>  
  mutate(artist = str_remove(artist, "&.*"),
         artist = str_remove(artist, "feat/..*"),
         artist = str_trim(artist)) |> 
  # agregar tag de cada artista
  left_join(artist_tags)


# gráfico artistas semanal ----

# top 15 artistas más escuchados
top_artists <- scrobbles_semana |> 
  filter(year(date) >= 2024) |> 
  count(artist) |> 
  slice_max(n, n = 10) |> 
  pull(artist)

# conteo de artistas más escuchados
scrobbles_conteo <- scrobbles_semana |> 
  mutate(artist = if_else(artist %in% top_artists, artist, "Otros")) |> 
  count(date, artist) |> 
  arrange(desc(date))

fechas <- c("2024-09-15", "2024-12-15", "2025-03-15", "2025-06-15")

# gráfico
scrobbles_conteo |> 
  filter(date > "2024-08-1") |> 
  filter(artist != "Otros") |>
  ggplot() +
  aes(date, n, fill = artist, color = artist) +
  geom_stream(bw = .7, #sorting = "onset",
              color = color_fondo, linewidth = 0.1, 
              show.legend = F) +
  geom_point(size = 0, alpha = 0) +
  geom_stream_label(aes(label = artist),
                    bw = .7, #sorting = "onset",
                    color = "white", alpha = 1, size = 2, fontface = "bold", lineheight = 0.8,
                    hjust = 0.5, vjust = 0.5, n_grid = 200) +
  # escala de colores
  scale_fill_manual(values = colorRampPalette(paleta)(15),
                    aesthetics = c("color", "fill")) +
  # escala horizontal de fechas
  annotate(geom = "text",
           x = as_date(fechas), #as.POSIXct(fechas),
           label = format(as_date(fechas), "%m/%Y"),
           y = -58, color = color_detalle, size = 3) +
  coord_cartesian(expand = F, clip = "off") +
  guides(fill = guide_none(),
         color = guide_legend(title = NULL, override.aes = list(size = 3, alpha = 1))) +
  labs(title = "Artistas más escuchados",
       caption = paste0("Last.fm/user/", user)) +
  theme(plot.title = element_text(face = "bold", margin = margin(b = -10), hjust = 0),
        plot.caption = element_text(color = color_detalle, hjust = 1, size = 8, vjust = 0),
        plot.caption.position = "plot") +
  canvas(8, 5)

# guardar
save_ggplot(plot = last_plot(), "gráficos/lastfm_artists_bastimapache.png")


# gráfico de artistas por semana ----

# calcular porcentaje de artistas del total de scrobbles
artists_p <- scrobbles_semana |> 
  count(artist, name = "n_artist") |> 
  mutate(p_artist = n_artist/sum(n_artist)) |> 
  # desempatar artistas con mismo porcentaje
  mutate(p_artist = ifelse(lead(p_artist) == p_artist, 
                           p_artist+0.00001, p_artist)) |> 
  # azar para desempate
  mutate(azar = sample(seq(0.000001, 0.000009, 0.000001), n(), replace = T),
         p_artist = p_artist + azar) |> 
  arrange(desc(p_artist))

# crear ranking de más escuchados por semana
scrobbles_rank <- scrobbles_semana |> 
  # filter(date == "2025-07-13") |> 
  # agregar porcentaje total de artistas
  count(date, artist, tags, sort = T) |> 
  arrange(desc(date)) |> 
  left_join(artists_p) |> 
  # crear ranking semanal
  group_by(date) |> 
  arrange(desc(n), desc(p_artist)) |> 
  mutate(rank = 1:n()) |> 
  ungroup() |> 
  filter(rank <= 10) |> 
  # contar rankings semanales
  group_by(date) |> 
  mutate(n_ranks = n()) |> 
  filter(n_ranks == 10) |> 
  ungroup()

# agregar tags de artistas y ordenar
scrobbles_rank_2 <- scrobbles_rank |> 
  # filtro fechas
  filter(date > max(date)-months(2)) |> 
  # textos
  # agregar fecha formateada
  mutate(fecha = format(date, "%d/%m/%y"),
         fecha = forcats::fct_reorder(fecha, date)) |> 
  # cortar artistas
  mutate(artist_label = str_trunc(artist, 30),
         artist_label = str_wrap(artist_label, 16)) |> 
  # categorizar tags en grupos más generales
  mutate(tags2 = case_when(tags %in% c("metal", "post-metal","depressive black metal",
                                       "screamo", "nu metal", "progressive metal") ~ "metal",
                           tags %in% c("brutal death metal", "death metal", "slamming brutal death metal",
                                       "technical death metal", "black metal", "deathcore", "deathgrind") ~ "metal extremo",
                           tags %in% c("grindcore", "hardcore") ~ "hardcore",
                           tags %in% c("noise",  "noise rock", "sludge", "ambient", "drone", "power electronics") ~ "noise",
                           tags %in% c("piano", "classical") ~ "clásica",
                           tags %in% c("rock", "shoegaze", "post-punk", "classic rock", "progressive rock", "indie rock", "avant-garde") ~ "rock",
                           tags %in% c("jazz") ~ "jazz",
                           is.na(tags) ~ "clásica",
                           tags %in% c("flute", "folk", "dream pop", "experimental") ~ "otros",
                           .default = "otros")) |> 
  # ordenar géneros por frecuencia
  group_by(tags2) |> 
  mutate(tags_n = sum(n)) |> 
  ungroup() |> 
  mutate(tags2 = forcats::fct_reorder(tags2, tags_n)) |> 
  mutate(top = rank == 1)


scrobbles_rank_2 |> 
  ggplot() +
  aes(fecha, rank, alpha = rank, fill = tags2) +
  geom_tile(linewidth = 0.3, color = color_fondo) +
  # geom_tile(aes(linewidth = n), color = color_fondo) +
  geom_text(aes(label = artist_label,
                fontface = ifelse(top, "bold", "plain")), 
            size = 2.4, lineheight = 0.9,
            color = "white") +
  scale_y_continuous(transform = "reverse",
                     breaks = 1:10) +
  scale_alpha_continuous(range = c(1, 0.3)) +
  # scale_linewidth_continuous(range = c(4, 0.1)) +
  scale_fill_manual(values = paleta_generos) +
  coord_cartesian(expand = FALSE) +
  guides(alpha = guide_none(),
         fill = guide_legend(position = "top", reverse = T, nrow = 1, title = NULL)) +
  theme(axis.ticks = element_blank(),
        panel.grid = element_blank()) +
  labs(title = "Artistas más escuchados por semana",
       caption = paste0("Last.fm/user/", user)) +
  theme(plot.title = element_text(face = "bold", margin = margin(b = 1), hjust = 0),
        plot.caption = element_text(color = color_detalle, hjust = 1, size = 8),
        plot.caption.position = "plot") +
  theme(axis.text.y = element_text(face = "bold", hjust = 1, 
                                   margin = margin(r = 4)),
        axis.text.x = element_text(colour = color_detalle,
                                   margin = margin(t = 4, b = 10), size = 9)) +
  canvas(9, 6)

# guardar
save_ggplot(plot = last_plot(), "gráficos/lastfm_weekly_bastimapache.png")


# alernativo ----

# crear grupos que unan artistas que aparecen en semanas consecutivas,
# porque de lo contrario aparecen sus líneas encima de otros artistas cuando desaparecen una semana
scrobbles_rank_consecutivos <- scrobbles_rank_2 |> 
  # completar observaciones para detectar cuando un artista NO sale en una semana
  arrange(desc(date)) |> 
  complete(date, artist) |> 
  arrange(artist, date) |>
  # por artista, marcar apariciones consecutivas y agruparlas
  group_by(artist) |> 
  # si el artista pareció la semana pasada, marcar como TRUE para mantener su línea
  mutate(mantener = !is.na(rank) & !is.na(lag(rank)) | !is.na(rank) & !is.na(lead(rank))) |>
  group_by(artist) |> 
  # cada vez que deja de aparecer o vuelve a aparecer, cambiar el número
  mutate(cambio = mantener != lag(mantener, default = first(mantener)),
         grupo = cumsum(cambio)) |> 
  # agrupar las apariciones consecutivas con un grupo de nombre único, y si deja de aparecer, NA, pero si vuelve a aparecer, agrupar como una aparición concesutiva con un nombre de grupo único
  mutate(grupo = ifelse(mantener, paste(artist, grupo), NA)) |> 
  filter(!is.na(grupo))
  
# posición de las fechas para generar líneas verticales
pos_fechas <- seq_along(unique(scrobbles_rank_2$fecha))

library(showtext)

showtext_auto()
font_add_google("IBM Plex Sans")
showtext_opts(dpi = 300)

# gráfico
scrobbles_rank_2 |> 
  ggplot() +
  aes(fecha, rank, fill = tags2) +
  geom_tile(linewidth = 0.3, color = color_fondo) +
  # líneas verticales
  annotate("segment",
           x = c(0, pos_fechas, 10)-0.5, # considerando el cero y +1 al final
           xend = c(0, pos_fechas, 10)-0.5,
           y = 1-0.5, yend = 10+0.5,
           linewidth = 6, color = color_fondo) +
  # línea horizontal entre 1 y 2
  annotate("segment", x = 0.5, xend = max(pos_fechas)+0.5,
           y = 1+0.5, yend = 1+0.5,
           linewidth = 2, color = color_fondo) +
  # conectores entre columnas
  geom_step(data = scrobbles_rank_consecutivos,
            aes(group = grupo, color = tags2), # grupo especial que las corta si desaparecen una semana
            direction = "mid",
            position = position_dodge(width = 0.12), # movimiento lateral
            size = 2.4, linewidth = 0.8, alpha = 1) +
  # etiqueta de fondo al texto para tapar la línea conectora
  geom_label(aes(label = artist_label,
                 color = tags2,
                fontface = ifelse(top, "bold", "plain")),
            size = 2.4, lineheight = 0.9, label.size = 0, family = "IBM Plex Sans",
            show.legend = FALSE) +
  # # capa de degradado de transparencia sobre los recuadros
  geom_tile(fill = color_fondo, aes(alpha = rank)) +
  # texto sobre la transparencia para legibilidad
  geom_text(aes(label = artist_label,
                 fontface = ifelse(top, "bold", "plain"),
                size = ifelse(top, 2.7, 2.4)), 
            family = "IBM Plex Sans",
             lineheight = 0.9, alpha = 0.8,
             color = "white", show.legend = FALSE) +
  # invertir escala vertical
  scale_y_continuous(transform = "reverse",
                     breaks = 2:10,
                     expand = expansion(c(0, 0))) +
  # # escala de transparencia al revés
  scale_alpha_continuous(range = c(0, 0.8)) +
  # tamaño de las letras definido condicionalmente en geom_text()
  scale_size_identity() +
  # relleno de recuadros
  scale_fill_manual(values = paleta_generos,
                    aesthetics = c("fill", "color")) +
  # # punto destacado para la fila de top artistas
  annotate("point", size = 8, x = 0.33, y = 1, color = color_texto) +
  annotate("text", label = "1", size = 4, x = 0.33, y = 1, fontface = "bold", color = color_fondo) +
  # cortar eje horizontal para que no se corra con el punto destacado fuera del área el gráfico
  coord_cartesian(expand = TRUE, clip = "off",
                  xlim = c(1, max(pos_fechas))) +
  # leyendas
  guides(alpha = guide_none(), color = guide_none(),
         fill = guide_legend(position = "top", reverse = T, nrow = 1, title = NULL)) +
  labs(title = "Artistas más escuchados por semana",
       caption = paste0("Last.fm/user/", user)) +
  # tema
  theme(text = element_text(family = "IBM Plex Sans")) +
  theme(axis.ticks = element_blank(),
        panel.grid = element_blank()) +
  theme(plot.title = element_text(face = "bold", margin = margin(b = 1), hjust = 0),
        plot.caption = element_text(color = color_detalle, hjust = 1, size = 8),
        plot.caption.position = "plot") +
  theme(axis.text.y = element_text(face = "bold", hjust = 0.5, margin = margin(l = 2, r = -3)),
        axis.text.x = element_text(colour = color_detalle, margin = margin(t = 4, b = 10), size = 9)) +
  canvas(11, 6)

save_ggplot(plot = last_plot(), "gráficos/lastfm_weekly_bastimapache_b.png")

# install.packages("camcorder")
# library(camcorder)
# camcorder::gg_record(dir = "grabación",
#                      width = 11, height = 6)

# camcorder::gg_stop_recording()
# camcorder::gg_stop_recording()
# camcorder::gg_playback(#path = "grabación/")
#                        #name = "grabación", 
#   name = file.path(tempdir(), "recording", "vignette_gif.gif"),
#                        height = 900,
#                        last_image_duration = 30, loop = TRUE,
#                        last_as_first = TRUE)
# 
