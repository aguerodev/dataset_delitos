library(tidyverse)
library(janitor)
library(RcppRoll)
library(cowplot)
library(scales)
library(ggview)
library(systemfonts)


habitantes <- data.frame(
  fecha = 2014:2025,
  poblacion = c(4771778, 4828520, 4882723, 4933519, 4981349,
                5020970, 5051379, 5077667, 5104907, 5135912, 5164860,5191823)
)

df <- read_rds("data/delitos_2025-03-03.rds")

x <- df |>
  filter(delito == "Homicidio", fecha >= "2014-05-01", fecha <= "2025-01-31") |>
  count(date = update(fecha, day = 1)) |> 
  mutate(fecha = year(date)) |> 
  left_join(
    habitantes,
    by = join_by(fecha== fecha)
  ) |> 
  mutate(
    n = (n /poblacion) * 100000,
    n = roll_meanr(n, 5, fill = NA_real_)
  )


ggplot(
  data = x,
  mapping = aes(
    x = date,
    y = n
  )
) +
  annotate(
    "rect",
    xmin = as.Date("2014-05-08"),
    xmax = as.Date("2018-05-08"),
    ymin = -Inf,
    ymax = Inf,
    fill = "#E69F00",
    alpha = 0.1
  ) +
  annotate(
    "text",
    x = as.Date("2016-05-08"),
    y = Inf,
    label = "Luis Guillermo Solís Rivera",
    hjust = 0.5,
    vjust = -0.5,
    size = 4,
    family = "Roboto-Light"
  ) +
  annotate(
    "rect",
    xmin = as.Date("2018-05-08"),
    xmax = as.Date("2022-05-08"),
    ymin = -Inf,
    ymax = Inf,
    fill = "#56B4E9",
    alpha = 0.1
  ) +
  annotate(
    "text",
    x = as.Date("2020-05-08"),
    y = Inf,
    label = "Carlos Alvarado Quesada",
    hjust = 0.5,
    vjust = -0.5,
    size = 4,
    family = "Roboto-Light"
  ) +
  annotate(
    "rect",
    xmin = as.Date("2022-05-08"),
    xmax = as.Date("2026-05-08"), # Puedes ajustar la fecha final según tus datos
    ymin = -Inf,
    ymax = Inf,
    fill = "#009E73",
    alpha = 0.1
  ) +
  annotate(
    "text",
    x = as.Date("2024-05-08"),
    y = Inf,
    label = "Rodrigo Chaves Robles",
    hjust = 0.5,
    vjust = -0.5,
    size = 4,
    family = "Roboto-Light"
  ) +
  geom_line(linewidth = 0.8) +
  labs(
    tag = "Gráfico 1",
    title = "Tasa de homicidios en Costa Rica por períodos presidenciales, 2014-2024",
    subtitle = "(Por cada 100 000 habitantes)",
    caption = "1/ Se aplicó una media móvil de 5 períodos.\n2/ Los datos corresponden a tasas mensuales homicidios por cada 100 000 habitantes.\n\nFuente: Organismo de Investigación Judicial (OIJ), estadísticas policiales 2004-2024; Instituto Nacional de Estadística y Censos\n(INEC), estimaciones y proyecciones nacionales de población 1950-2100, 2024.",
    x = "Año",
    y = "Tasa de homicidios"
  ) +
  theme_minimal_vgrid(font_family = "Roboto") +
  coord_cartesian(clip = "off") +
  scale_x_date(
    minor_breaks = "1 year",
    date_breaks = "1 year",
    date_labels = "%Y",
    expand = expansion(add = 50)
  ) +
  theme(
    plot.tag = element_text(
      margin = margin(b = 60, l = 39),
      face = "plain"
    ),
    plot.margin = margin(30,15,15,15),
    plot.subtitle = element_text(
      margin = margin(b=35)
    ),
    plot.caption = element_text(
      hjust = 0
    ),
    axis.title.x = element_text(
      margin = margin(t = 10)
    )
  ) +
  canvas(width = 10, height = 5.62, dpi = 460)

ggsave("plot.jpg", width = 10, height = 5.62, dpi = 460)

