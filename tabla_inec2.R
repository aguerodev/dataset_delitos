library(gt)

# ==============================
# 1. CAMBIOS MANUALES (ESPECÍFICOS)
# ==============================

datos <- data.frame(
  actividades = c(
    "Total",
    "Cultivos Agro Industriales", "Café", "Caña de azúcar", "Palma Aceitera", "Naranja",
    "Frutas Frescas", "Banano", "Banano consumo nacional", "Melón", "Piña", "Sandía",
    "Granos Básicos", "Arroz granza", "Frijol", "Maíz",
    "Hortalizas", "Cebolla", "Papa"
  ),
  produccion_2022 = c(
    11205416,
    5770719, 428418, 3897888, 1227253, 217160,
    5221340, 2267500, 226750, 52228, 2618630, 56232,
    135573, 114343, 14524, 6706,
    77784, 45392, 32392
  ),
  separador = "",
  produccion_2023 = c(
    10751511,
    5103240, 461067, 3422767, 1219406, NA,  # NA representa ND (No disponible)
    5470740, 2302667, 230267, NA, 2937807, NA,
    96259, 76119, 12819, 7321,
    81272, 46789, 34483
  )
)

tabla_manual <- datos |>
  gt(rowname_col = "actividades") |>
  cols_label(
    produccion_2022 = "2022",
    separador = "",
    produccion_2023 = "2023"
  ) |>
  tab_stubhead(label = md("**Actividades**")) |>
  tab_header(
    title = html(
      "<div class='table-number'>CUADRO 3.10</div>
       <div class='table-title'><strong>Costa Rica. Producción de las principales actividades agropecuarias por año según actividades, 2022 - 2023</strong></div>"
    ),
    subtitle = md("(En toneladas métricas)<br><br>")
  ) |>
  # Alineaciones manuales específicas
  tab_style(
    style = cell_text(align = "left"),
    locations = cells_title()
  ) |>
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_column_labels()
  ) |>
  tab_style(
    style = cell_text(align = "right"),
    locations = cells_body(columns = where(is.numeric))
  ) |>
  # Destacar totales y algunos renglones (manual, según recomendaciones INEC)
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(rows = 1)
  ) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_stub(rows = 1)
  ) |>
  tab_style(
    style = list(cell_text(weight = "bold"), cell_fill(color = "#f2f1d9")),
    locations = cells_stub(c(1,2,7,13))
  ) |>
  tab_style(
    style = list(cell_text(weight = "bold"), cell_fill(color = "#f2f1d9")),
    locations = cells_body(rows = c(1,2,7,13))
  ) |>
  # Notas específicas de la tabla
  tab_footnote(
    md("Datos preliminares"),
    cells_column_labels(4)
  ) |>
  tab_footnote(
    md("El dato 2022 corresponde a la zafra 2021/22 y el dato 2023 a la zafra 2022/23."),
    cells_stub(rows = c("Caña de azúcar"))
  ) |>
  tab_footnote(
    md("Producción para exportación."),
    cells_stub(rows = c("Banano", "Melón", "Sandía"))
  )

# ==============================
# 2. AJUSTES GENERALES (COMUNES A TODAS LAS TABLAS)
# ==============================

tabla_final <- tabla_manual |>
  # Tipografía general
  opt_table_font(font = "ChollaSans") |>
  # Opciones generales de formato y estilo
  tab_options(
    heading.title.font.size = px(24),
    heading.subtitle.font.size = px(22),
    column_labels.font.size = px(24),
    stub.font.size = px(20),
    table.font.size = px(20),
    
    # Bordes principales
    table.border.left.width = px(0), 
    table.border.left.style = "none",
    table.border.right.width = px(0), 
    table.border.right.style = "none",
    table.border.top.width = px(0),
    table.border.bottom.width = px(0),
    
    # Bordes de encabezados (INEC recomienda separar claramente los encabezados)
    column_labels.border.top.width = px(0), 
    column_labels.border.top.color = "black",
    column_labels.border.bottom.width = px(2), 
    column_labels.border.bottom.style = "solid",
    column_labels.border.bottom.color = "black",
    heading.border.bottom.width = px(0), 
    column_labels.background.color = "#e3e1ba",
    column_labels.padding = px(10),
    data_row.padding.horizontal = px(10),
    
    # Bordes de cuerpo (uso moderado según INEC)
    stub.border.width = px(0),
    table_body.hlines.width = px(4),
    table_body.hlines.color = "white",
    table_body.border.top.color = "black",
    table_body.border.bottom.color = "black",
    table_body.border.bottom.width = px(2),
    
    # Otros parámetros
    footnotes.spec_ref = "^",
    footnotes.spec_ftr = " "
  ) |>
  # Formateo general de datos
  fmt_missing(missing_text = "ND") |>
  fmt_number(
    columns = where(is.numeric), 
    sep_mark = " ", 
    decimals = 0
  ) |>
  # Fuente y marcadores de notas al pie
  tab_source_note(
    md("Fuente: INEC, Costa Rica. [2012]. *Encuesta Nacional de Hogares Julio 2012. Resultados Generales*. 1 [3], p. 14.")
  ) |>
  opt_footnote_marks(
    marks = c("a/", "1/", "2/")  # Marcadores personalizados
  ) |>
  opt_css(
    "
      .table-title {
        line-height: 1.1; /* Ajusta este valor para mayor o menor espacio */
      }
      
      .table-number {
         margin-bottom: 15px;
      }
      
    "
  ) |> 
  # Ancho de columnas (ajuste según necesidades generales)
  cols_width(
    actividades ~ px(350),
    produccion_2022 ~ px(100),
    separador ~ px(50),
    produccion_2023 ~ px(100)
  )

# Visualizar la tabla final
tabla_final

# Guardar en HTML y luego en PNG
gt::gtsave(tabla_final, "overview_table.html")

webshot2::webshot(
  "overview_table.html",
  "overview_table.png",
  zoom = 2,
  expand = 15,
  selector = "table"
)
