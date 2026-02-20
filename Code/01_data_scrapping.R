# ------------------------------------------------------------- #
# ------------------Web scraping --------------------- 
# ------------------------------------------------------------- #
#Se revisa Html,pero las bases no aparecen directamente. 
# El contenido se carga dinámicamente utilizando el atributo "w3-include-html"
#Por lo que se scrapea directamente los archivos html que contienen las tablas

urls <- paste0(
  "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_",
  1:10,
  ".html"
)

#guardar en listas cada tabla

lista_tablas <- lapply(urls, function(x) {
  read_html(x) %>%
    html_element("table") %>%
    html_table()
})

saveRDS(
  lista_tablas,
  file = file.path(path_raw, "lista_tablas_raw.rds")
)

#validar que los nombres de las columnas conincidan antes de pegar

nombres_columnas <- lapply(lista_tablas, names)
ref <- nombres_columnas[[1]]
sapply(nombres_columnas, function(x) identical(x, ref))

#Todos los nombres coinciden

#Unir las bases
base_completa <- bind_rows(lista_tablas)