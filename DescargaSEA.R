#Descarga de datos SEA

#Carga de librerias
library(rvest) # Get data from web
library(httr)  # Also useful to get data on web
library(stringr) # Hand   le of strings
library(dplyr) # Handle data and use of pipelines
library(xlsx) # Read and write excel files
library(sf) # Read and handle kmz
library(lubridate) #handle with dates
  
#Fijar directorio de trabajo
setwd("/home/vcaquilpan/Documentos/R Scripts/SEA/v2/WebScraping-SEA/Datos")

  #1) Descarga informacion general.

#Extraer url principal de la seccion "Busqueda" (Ejemplo: https://seia.sea.gob.cl/busqueda/buscarProyectoAction.php?_paginador_refresh=0&_paginador_fila_actual=1)
url_principal <- "https://seia.sea.gob.cl/busqueda/buscarProyectoAction.php?_paginador_refresh=0&_paginador_fila_actual="

#Extraer numero total de paginas 
paginas <-read_html(paste0(url_principal,"1")) %>% html_node(css = "#info_resultado") %>% html_text() %>% 
  sub(pattern = ".*páginas: ",replacement = "") %>% gsub(pattern = "[[:space:]]",replacement = "") %>% 
  gsub(pattern = ",",replacement = "") %>% as.integer()

#Extraccion de información general. Se extraen las siguientes variables: N° de proyecto, Nombre de proyecto, Tipo, Región, Tipologia, Titular, inversion,
#Fecha de presentación, Estado y URL.
for (i in 1:paginas) {
  lectura <- read_html(paste0(url_principal,i))
  #Informacion General
  informacion <- lectura  %>% html_node(css = '.tabla_datos') %>% html_table()
  informacion <- informacion[-1,]
  #Extraccion urls proyectos
  urls <- lectura %>% html_node(xpath = "/html/body/div[1]/div[1]/div/div[3]/div[4]/div/table/tbody")
  urls <- urls %>% html_children() %>% html_children() %>% html_children() %>% html_attr("href")
  urls <- gsub(pattern = ".*&modo=ficha",replacement = NA,urls)
  urls <- urls[!(is.na(urls) | urls == "#")]
  informacion$url <- urls 
  #Agrupacion de datos
  if (exists("Tabla")) {
    Tabla <- rbind(Tabla,informacion)
  } else {
     Tabla <- informacion
  }
  #Imprimir que está corriendo bien
  print(paste0("informacion general pagina: ",i))
}

#Eliminar variables que ya no se usaran
rm(informacion,lectura,i,urls)
#Eliminar columna Mapa, ya que no entrega información
Tabla$Mapa <- NULL
#Obtener id de cada proyecto
Tabla$id <- Tabla$url %>% gsub(pattern = "\\D",replacement = "")

#2) Lectura de coordenadas. El SEA dispone de un archivo KMZ de coordenadas
#validadas. Vamos a leer dicho archivo.
coords <- read_sf("proyectos.kmz")

#Desde la columna descripción podemo utilizar el id de cada proyecto. 
Localizacion <- as.data.frame(coords$description,col.names = "description") %>% rename("description" = "coords$description")
Localizacion$proyecto <- Localizacion$description %>% sub(pattern = "</b>.*",replacement = "") %>% sub(pattern = ".*<b>",replacement = "")
Localizacion$url_mapa <- Localizacion$description %>% sub(pattern = '"><.*',replacement = "") %>% sub(pattern = '.*src="',replacement = "")                              
Localizacion$id <- Localizacion$url_mapa %>% sub(pattern = "&name=0",replacement = "")  %>% gsub(pattern = "\\D",replacement = "")
Localizacion <- Localizacion %>% select(-c(description,proyecto))

#Agregamos las coordendas
Localizacion$Longitud <- st_coordinates(coords)[,1]
Localizacion$Latitud <- st_coordinates(coords)[,2]

#Unir datos de localizacion a información general
Tabla_general <- Tabla %>% left_join(Localizacion,by = "id")

#Remover objetos sobrantes
rm(coords,Localizacion)

#La URL del mapa nos deriva a una pagina que tiene un parametro de interes mas
#que no está en los datos extraidos anteriormente, que corresponde a la fecha de cuando fue
#calificado cada proyecto. Para esto se tiene que hacer una consulta pagina a pagina.
for (k in 1:nrow(Tabla_general)) {
  lectura <- read_html(paste0("https://seia.sea.gob.cl/mapa/info.php?id=",format(Tabla_general$id[k],scientific = FALSE),"&name=0")) %>% html_node(xpath = "/html/body/table") %>% html_table()
  Tabla_general$Fecha_Calificado[k] <- lectura %>% filter(X1 == "Calificado:") %>%  select(c(X2)) %>% as.character() 
  print(paste0("Estado y Fecha RCA:",k))
  }

#Se identifica que no todos los proyectos tienen fecha de "Calificado", los cuales corresponden principalmente a los proyectos desistidos o abandonados, por lo que 
#en los casos en donde no haya existido ese campo se reemplaza el resultado por defecto de "character(0)" por "".
Tabla_general$Fecha_Calificado[Tabla_general$Fecha_Calificado == "character(0)"] <- ""

#3) Ahora revisamos que información podemos obtener de las fichas de proyectos.
#Viendo algunos proyectos, la fecha "Calificado" al parecer corresponde a la fecha en
#la cual se realizó la notificación de la RCA, obviamente lo que es posterior a la 
#emisión de la RCA. En el link URL_MAPA aparece el valor de inversion como cifra completa, 
#sin embargo en la ficha aparece en Millones de dolares. Para esto se considera "." como separador
#de miles y "," como separador de decimales. 

#Se analizan algunas variables a extraer, entre las que se proponen:
# Cantidad de documentos en la sección: "Evaluacion Ambiental"      
# Cantidad de Adendas realizadas disponibles en la sección: "Evaluación Ambiental"

# A continuacion se identifica el link que nos lleva a cada una de las fichas y que nos permite obtener el número de documentos en la sección "Evaluacion ambiental"
#y tambien el número de Adendas. Se detecta que ademas de "Adenda", también se encuentra "Adenda complementaria" como documento. Ambos fueron agregados en este caso.

url_evaluacion <- "https://seia.sea.gob.cl/seia-web/fichaProceso.php?id_expediente="
#Realizamos la extraccion de informacion
for (p in 1:nrow(Tabla_general)) {
lectura_docs <- read_html(paste0("https://seia.sea.gob.cl/seia-web/fichaProceso.php?id_expediente=",format(Tabla_general$id[p], scientific = FALSE)))
documentos <- lectura_docs  %>% html_nodes(css = ".tabla_datos_linea")%>% html_table() %>% as.data.frame()
#Se detecta que hay casos en los que la tabla es distinta que es un caso en donde un titular desistió al principio del proceso.
if (nrow(documentos) > 0) {
  Tabla_general$n_documentos[p] <-  documentos %>% nrow()
  Tabla_general$n_adendas[p] <- documentos %>%  filter(Documento == "Adenda" | Documento == "Adenda complementaria") %>% .$Documento %>% length()
} else {
  documentos <- lectura_docs %>%  html_node(css = ".tabla_datos") %>% html_table() %>% as.data.frame()
  Tabla_general$n_documentos[p] <-  documentos %>% nrow()
  Tabla_general$n_adendas[p] <- 0
} 
  print(paste0("Adendas y documentos: ",p))
}
  
# 4) Además que es posible obtener la cantidad de actividades realizadas descritas en la seccion "participacion ciudadana".
# El siguiente link da cuenta de las actividades de participación ciudadana y se analiza proyecto a proyecto.
url_p <- "https://seia.sea.gob.cl/expediente/xhr_pac.php?id_expediente="

# Existe un expediente (6357086) que presenta problemas, ya que es el unico que presenta dos tablas, por lo tanto se genera una excepcion para dicho
# expediente
  
#Se realiza la extraccion
for (m in 1:nrow(Tabla_general)) {
  if (Tabla_general$id[m] != "6357086") {
    Tabla_general$n_participacion[m] <- read_html(paste0(url_p,Tabla_general$id[m])) %>% html_nodes(css = ".tabla_datos_linea") %>% html_table(fill = TRUE) %>% as.data.frame() %>% nrow()
    if (Tabla_general$n_participacion[m] > 0) {Tabla_general$n_participacion[m] <- Tabla_general$n_participacion[m] - 1}
  } else {
    Tabla_general$n_participacion[m] <-  read_html(paste0(url_p,Tabla_general$id[m])) %>% html_nodes(css = "table.tabla_datos_linea:nth-child(5)") %>% html_table(fill = TRUE) %>% as.data.frame() %>% nrow()
  }
  print(paste0("Participacion: ",m))
}

#Analizando un poco más sobre la información disponible en el SEA, se realizó una búsqueda de un proyecto: "Edificio Mirador Azul", y se encontraron dos resultados
#con este mismo nombre, pero de diferente titular, no obstante se encuentran en el mismo estado de "Aprobado", y presentan fechas muy similares entre si. 

#5) A cada ficha de proyecto se va a extraer la tipologia de proyecto, que a diferencia de la anterior, en este caso ahora corresponde a una descripción, no a una sigla.
#por otro lado se extraera ademas la descripcion del proyecto. 

url_ficha <- "https://seia.sea.gob.cl/expediente/ficha/fichaPrincipal.php?modo=normal&id_expediente="

#Se detecta problema en el proyecto id "10061" y "100816" y en varios otros proyectos, por lo que se realiza un TryCatch para que el loop no se detenga,
#y a su vez detecte todos los errores. 
Tabla_general$Tipo_proyecto <- ""
Tabla_general$Descripcion <- ""

for (g in 1:nrow(Tabla_general)) {
  tryCatch(
    expr = {
    lectura_ficha <- read_html(paste0(url_ficha,Tabla_general$id[g]))
    Tabla_general$Tipo_proyecto[g] <- lectura_ficha %>% html_node(".tabla_datos_linea") %>% html_table(fill = TRUE) %>% filter(X1 == "Tipo de Proyecto") %>% 
    select(X2) %>% as.character()
    Tabla_general$Descripcion[g] <- lectura_ficha %>% html_node(".tabla_datos_linea") %>% html_table(fill = TRUE) %>% filter(X1 == "Descripción del Proyecto") %>% 
      select(X2) %>% as.character()
    print(paste0("Descripcion: ",g))
    },error = function(e){
      print(paste0("Descripcion error: ",g))
  })
}

#Se detectó que en algunos casos, se extrajo el contenido xml que incluye tanto el texto como el contenido base que no nos interesa. Estos casos son identificados 
#los cuales empiezan con el simbolo "<". A todos esos casos, se les elimina la parte xml. No obstante, es un poco más complejo extraer la información de valor en estos
#casos.
summary(grepl(pattern = "^<",Tabla_general$Descripcion))

#Remover objetos sin uso
rm(lectura_docs,lectura_ficha,g,k,m,p)

#6) Limpieza general del archivo

#Solo dejamos las columnas que nos interesan
Tabla_general <- Tabla_general %>% select(-c(No,url_mapa))

#Ordenamos los nombres de las columnas
Tabla_general <- Tabla_general %>% rename('name' = Nombre,
                         'type' = Tipo,
                         'region' = Región,
                         'owner' = Titular,
                         'typology' = Tipología,
                         'investment' = `Inversión(MMU$)`,
                         'entry_date' = `FechaPresentaciónFecha deIngreso(*)`,
                         'state' = Estado,
                         'main_url' = url,
                         'id_project' = id,
                         'longitude' = Longitud,
                         'latitude' = Latitud,
                         'n_docs' = n_documentos,
                         'n_addendum' = n_adendas,
                         'n_participatory' = n_participacion,
                         'typology_des' = Tipo_proyecto,
                         'description' = Descripcion,
                         'qualification_date' = Fecha_Calificado) %>% 
  select(name,type,region,owner,typology,typology_des,investment,entry_date,state,qualification_date,id_project,latitude,longitude,n_docs,n_addendum,n_participatory,description,main_url)

#Arreglar columna qualification_date
vector_mes<- c('ene','feb','mar','abr','may','jun','jul','ago','sep','oct','nov','dic')
Tabla_general$qualification_date[!is.na(Tabla_general$qualification_date)] <- paste0(str_sub(Tabla_general$qualification_date[!is.na(Tabla_general$qualification_date)] ,start = 1,end = 2),"/",Tabla_general$qualification_date[!is.na(Tabla_general$qualification_date)] %>% str_extract(pattern = '[[:alpha:]]{3}+') %>% match(vector_mes),"/",str_sub(Tabla_general$qualification_date[!is.na(Tabla_general$qualification_date)] ,start = 8, end = 11))
Tabla_general$qualification_date[Tabla_general$qualification_date == "ch/NA/er(0"] <- ""

#Transformar fechas al mismo formato
Tabla_general$entry_date <- as.Date(Tabla_general$entry_date,format = "%d/%m/%Y")
Tabla_general$qualification_date <-  as.Date(Tabla_general$qualification_date,format = "%d/%m/%Y")

#Imprimir Tabla resultante
write_csv(Tabla_general,"projects.csv")
      
# Referencias
# https://rtask.thinkr.fr/installation-of-r-4-0-on-ubuntu-20-04-lts-and-tips-for-spatial-packages/
# https://www.datacamp.com/community/tutorials/r-web-scraping-rvest?utm_source=adwords_ppc&utm_campaignid=1455363063&utm_adgroupid=65083631748&utm_device=c&utm_keyword=&utm_matchtype=b&utm_network=g&utm_adpostion=&utm_creative=332602034364&utm_targetid=aud-517318241987:dsa-429603003980&utm_loc_interest_ms=&utm_loc_physical_ms=9047105&gclid=EAIaIQobChMIs56vj9Km7wIVjIKRCh1JrwLcEAAYASAAEgLVqvD_BwE
          