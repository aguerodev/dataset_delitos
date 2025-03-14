## ========== BIBLIOTECAS ==========
library(gt)
library(dplyr)
library(systemfonts)

## ========== DATOS ==========
datos <- tibble(
  concepto = c(
    "Ingreso total del hogar",
    "Ingreso por trabajo",
    "Ingreso por salario",
    "Ingreso autónomo",
    "Ingreso renta de la propiedad",
    "Subsidios estatales y becas",
    "Otras transferencias"
  ),
  total_2011 = c(858974, 755110, 540394, 164671, 35945, 17001, 89827),
  urbana_2011 = c(1015375, 863505, 639480, 187105, 75284, 8303, 111200),
  rural_2011 = c(555471, 508650, 317140, 117118, 27353, 15318, 42988),
  total_2012 = c(919427, 748175, 570991, 177183, 87200, 10962, 73088),
  urbana_2012 = c(1088573, 858287, 671435, 191851, 87833, 8514, 127749),
  rural_2012 = c(632928, 533243, 357470, 175776, 33607, 15719, 59556)
)

## ========== CREACIÓN DE TABLA BASE ==========
tabla <- datos |> 
  gt(rowname_col = "concepto")

## ========== TÍTULOS Y ESTRUCTURA GENERAL ==========
tabla <- tabla |>
  # Título y subtítulo
  tab_header(
    title = md("**Cuadro 3.1**<br><br>**Costa Rica: Promedio de ingresos por hogar, por año y zona según fuente de ingreso, julio 2011 y julio 2012**"),
    subtitle = md("(Valores nominales)<br><br>")
  ) |>
  # Cabecera de filas
  tab_stubhead(label = md("**Fuente de ingresos**<br><br><br>")) |>
  # Fuente
  tab_source_note(md("Fuente: INEC, Costa Rica. [2012]. *Encuesta Nacional de Hogares Julio 2012. Resultados Generales*. 1 [3], p. 14."))

## ========== ETIQUETAS Y ESTRUCTURA DE COLUMNAS ==========
tabla <- tabla |>
  # Etiquetas de columnas
  cols_label(
    total_2011 = md("**Total**"), 
    urbana_2011 = md("**Urbana**"), 
    rural_2011 = md("**Rural**"),
    total_2012 = md("**Total**"), 
    urbana_2012 = md("**Urbana**"), 
    rural_2012 = md("**Rural**")
  ) |>
  # Agrupación de columnas
  tab_spanner(label = md("**Zona**"), columns = matches("(?<!total_)2011", perl = TRUE), id = "anio_2011") |>
  tab_spanner(label = md("**Zona**"), columns = matches("(?<!total_)2012", perl = TRUE), id = "anio_2012") |>
  tab_spanner(label = md("**2011**"), columns = contains("2011"), level = 2, id = "zona_2011") |>
  tab_spanner(label = md("**2012**"), columns = contains("2012"), level = 2, id = "zona_2012")

## ========== NOTAS AL PIE ==========
tabla <- tabla |>
  tab_footnote(
    md("Ingreso con imputación de valores no declarados y con ajuste por sub declaración."),
    cells_stub(rows = c("Ingreso por salario", "Ingreso autónomo", "Ingreso renta de la propiedad"))
  ) |>
  tab_footnote(
    md("Ingreso con imputación de valores no declarados y con ajuste por sub declaración en las transferencias por pensión; incluye transferencia no monetaria."),
    cells_stub(rows = "Otras transferencias")
  )

## ========== FORMATO DE DATOS ==========
tabla <- tabla |>
  # Formato de números
  fmt_number(columns = where(is.numeric), sep_mark = " ", decimals = 0) |>
  # Indentación
  tab_stub_indent(rows = c(3, 4), indent = 5)

## ========== ESTILOS DE CELDAS ==========
tabla <- tabla |>
  # Alineación
  tab_style(
    style = cell_text(align = "left"),
    locations = cells_title()
  ) |>
  tab_style(
    style = cell_text(align = "center"),
    locations = list(
      cells_body(columns = where(is.numeric)),
      cells_column_labels(),
      cells_column_spanners()
    )
  ) |>
  # Formato del texto
  tab_style(
    style = cell_text(size = px(18)),
    locations = cells_title(groups = "subtitle")
  ) |>
  # Negritas
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(rows = 1)
  ) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_stub(rows = 1)
  ) |>
  # Fuente
  opt_table_font(
    font = list(
      google_font(name = "ChollaSans")
    )
  )

## ========== OPCIONES DE TABLA ==========
tabla <- tabla |>
  tab_options(
    # Bordes principales
    table.border.left.width = px(0), 
    table.border.left.style = "none",
    table.border.right.width = px(0), 
    table.border.right.style = "none",
    table.border.top.width = px(0),
    table.border.bottom.width = px(0),
    
    # Bordes de cabeceras
    column_labels.border.top.width = px(2), 
    column_labels.border.top.color = "black",
    column_labels.border.bottom.width = px(2), 
    column_labels.border.bottom.style = "solid",
    column_labels.border.bottom.color = "black",
    heading.border.bottom.width = px(2), 
    heading.border.bottom.color = "black",
    
    # Bordes de cuerpo
    stub.border.width = px(0),
    table_body.hlines.width = px(0),
    table_body.border.top.color = "black",
    table_body.border.bottom.color = "black",
    table_body.border.bottom.width = px(2),
    
    # Otros parámetros
    footnotes.spec_ref = "^",
    footnotes.spec_ftr = " "
  )

## ========== MOSTRAR Y GUARDAR ==========
# Mostrar tabla
tabla

gtsave(
  tabla,
  filename = "tabla_alta_resolucion.png",
  vwidth = 2500,
  vheight = 2000,
  zoom = 3
)
