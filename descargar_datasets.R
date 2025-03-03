library(rvest)
library(selenider)
library(lubridate)
library(fs)
library(purrr)
library(janitor)
library(glue)

wait_for_download <- function(download_dir, filename, timeout = 60) {
  start_time <- Sys.time()
  file_path <- file.path(download_dir, filename)
  while (
    as.numeric(difftime(Sys.time(), start_time, units = "secs")) <= timeout
  ) {
    if (
      file.exists(file_path) &&
        length(list.files(download_dir, pattern = "\\.crdownload$")) == 0
    ) {
      return(TRUE)
    }
    Sys.sleep(1)
  }
  stop("Timeout")
}

descargar_dataset_pj <- function(
  date = lubridate::today(),
  download_dir = "data",
  filename = "Estadisticas.xls"
) {
  dir.create(download_dir, showWarnings = FALSE, recursive = TRUE)
  full_path <- file.path(download_dir, filename)

  session <- selenider_session(
    "chromote",
    timeout = 10,
    options = chromote_options(headless = TRUE)
  )
  session$driver$Browser$setDownloadBehavior(
    behavior = "allow",
    downloadPath = normalizePath(download_dir)
  )

  open_url("https://pjenlinea3.poder-judicial.go.cr/estadisticasoij/")

  start_date <- format(date, "%d/%m/%Y")
  end_date <- update(date, month = 12, day = 31)
  end_date <- if_else(end_date > today(), today(), end_date)
  end_date <- format(end_date, "%d/%m/%Y")

  execute_js_expr(
    "document.getElementById('txtFechaFinal').removeAttribute('readonly',0);"
  )
  execute_js_expr(sprintf(
    "document.getElementById('txtFechaFinal').value = '%s';",
    end_date
  ))
  execute_js_expr(
    "document.getElementById('txtFechaInicio').removeAttribute('readonly',0);"
  )
  execute_js_expr(sprintf(
    "document.getElementById('txtFechaInicio').value = '%s';",
    start_date
  ))

  execute_js_expr("document.getElementById('chbTodoPais').click();")
  execute_js_expr("document.getElementById('chbTodoDelitos').click();")
  execute_js_expr("document.getElementById('chbTodaVictima').click();")

  execute_js_expr("document.getElementById('btnExcel').click();")

  wait_for_download(download_dir, filename)

  new_filename <- paste0("delitos_", format(date, "%Y-%m-%d"), ".html")
  new_path <- file.path(download_dir, new_filename)
  fs::file_move(full_path, new_path)
  return(new_path)
}

.years <- ymd("2025-01-01") - years(0:11)

df <- map(
  .years,
  \(x) {
    .file <- descargar_dataset_pj(date = x)
    df <- read_html(.file) |>
      html_table()
    df <- df[[1]]
    return(df)
  },
  .progress = TRUE
) |>
  list_rbind()

df |>
  clean_names() |>
  mutate(
    across(where(is.character), str_to_title),
    fecha = ymd(fecha)
  ) |> 
  write_rds(glue("data/delitos_{today()}.rds"))