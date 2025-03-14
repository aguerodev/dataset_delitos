library(rvest)
accentless <- function( s ) {
  chartr(
    "áéóūáéíóúÁÉÍÓÚýÝàèìòùÀÈÌÒÙâêîôûÂÊÎÔÛãõÃÕñÑäëïöüÄËÏÖÜÿçÇ",
    "aeouaeiouAEIOUyYaeiouAEIOUaeiouAEIOUaoAOnNaeiouAEIOUycC",
    s );
}
page <- read_html("https://es.wikipedia.org/wiki/Anexo:Cantones_de_Costa_Rica") |> 
  html_table() |> 
  keep_at(1) |> 
  list_rbind() |> 
  select(3,8) |> 
  setNames(c("canton","poblacion")) |> 
  slice(-1) |> 
  mutate(
    poblacion = as.numeric(str_remove(poblacion,"\\s")),
    canton = str_to_lower(accentless(canton)),
    canton = str_replace(canton, "vazquez de coronado","vasquez de coronado"),
    canton = str_replace(canton, "leon cortes castro","leon cortes")
  )



x <- df |>
  filter(
    year(fecha) %in% c(2014, 2024),
    delito == "Homicidio"
  ) |>
  count(canton, fecha = year(fecha)) |> 
  mutate(
    fecha = as.integer(fecha),
    canton = accentless(str_to_lower(canton))
    ) |> 
  left_join(
    y = page,
    by = join_by(canton == canton)
  ) |> 
 mutate(
   p = (n/poblacion)*100000,
   color = if_else(p>50, "red","blue")
 ) 

x

ggplot(
  data = x,
  mapping = aes(
    x = fecha,
    y = p,
    group = canton,
    color = color
  )
) +
  geom_line() 
