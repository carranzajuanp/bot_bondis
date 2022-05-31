rm(list = ls())
library(ROAuth)
library(dplyr)
library(httr)
library(jsonlite)
library(stringr)
library(osmdata)
library(tmaptools)
library(tmap)
setwd("~/")
api_key <- ""
api_secret <- ""
bearer_token <- ""
access_token <- ""
access_token_secret <- ""
res = GET("https://cordobus.apps.cordoba.gob.ar/tracking/api/internos-activos-ahora/?format=json")
data = fromJSON(rawToChar(res$content))
datos <- flatten(data$results$features, recursive = TRUE)
rm(data)
datos$y = as.numeric(substr(datos$geometry.coordinates, 3, 10))
datos$x = substr(datos$geometry.coordinates, 14, 22)
datos$x = as.numeric(gsub(")", "", datos$x))
datos = subset(datos, is.na(x)==F & is.na(y)==F)
datos = st_as_sf(datos, coords = c("y","x"))
datos = st_set_crs(datos, 4326)
datos$geometry.coordinates = NULL
# i = 2
for ( i in 2:3) {
  url = paste0("https://cordobus.apps.cordoba.gob.ar/tracking/api/internos-activos-ahora/?format=json&page=", i)
  res = GET(url)
  data = fromJSON(rawToChar(res$content))
  aux <- flatten(data$results$features, recursive = TRUE)
  rm(data)
  aux$y = as.numeric(substr(aux$geometry.coordinates, 3, 10))
  aux$x = substr(aux$geometry.coordinates, 14, 22)
  aux$x = as.numeric(gsub(")", "", aux$x))
  aux = subset(aux, is.na(x)==F & is.na(y)==F)
  aux = st_as_sf(aux, coords = c("y","x"))
  aux = st_set_crs(aux, 4326)
  aux$geometry.coordinates = NULL 
  datos = rbind(datos, aux)
}
datos = subset(datos, datos$properties.linea != "Desconocido")
datos = dplyr::rename(datos, 'Tipo de bondi' = properties.adaptado)
datos$`Tipo de bondi` = ifelse(datos$`Tipo de bondi`==T, "Adaptado", "Comun")
mapa = mapview(datos, zcol = "`Tipo de bondi`"
tmap_save(mapa, "bondis.png")
lineas = st_drop_geometry(datos) %>% 
  group_by(properties.linea) %>% 
  summarise(bondis = n()) %>% 
  arrange(desc(bondis)) %>% 
  filter(bondis == max(bondis))
tuit = paste0("En este momento circulan por la Ciudad de Córdoba ", nrow(datos),
              " bondis. La línea con más unidades en la calle es la ", lineas[1,1],
              ", con ", lineas[[1,2]], " colectivos. Hay en la calle ", table(datos$`Tipo de bondi`)[[1]], 
              " coches adaptados para personas en silla de ruedas (", round(table(datos$`Tipo de bondi`)[[1]]*100 /
                nrow(datos),1), "%).")
rbot_token <- rtweet::create_token(
  app = "bondis_cordoba",
  consumer_key = api_key,
  consumer_secret = api_secret,
  access_token = access_token,
  access_secret = access_token_secret,
  set_renv = FALSE
)
rtweet::post_tweet(status = paste0("TEST3__", tuit, "__TEST3"), media = "bondis.png" , token = rbot_token)

                   
