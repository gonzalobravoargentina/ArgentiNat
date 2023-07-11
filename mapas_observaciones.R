library(readr)
obs <- read_csv("observations-gonzalobravopatagonia.csv")

#Sacar observaciones que no sean de Argentina
obs <- obs[obs$latitude <= -37, ]

library(leaflet)
library(leaflet.extras)
# Crear el mapa
mapa1 <- leaflet(obs,options = leafletOptions(
  attributionControl=FALSE,zoomControl = FALSE, scaleControl = FALSE))%>%# add different provider tiles
  addProviderTiles(
    "Esri.WorldImagery") %>%# Agregar los iconos de las localidades
  addCircleMarkers(
    data = obs, 
    ~longitude, 
    ~latitude,
    weight = 0.5,
    col = 'red',
    fillColor = 'red',
    radius = 4,
    fillOpacity = 0.5,
    stroke = T )
  

mapa2 <- leaflet(obs,options = leafletOptions(
  attributionControl=FALSE,zoomControl = FALSE))%>%# add different provider tiles
  addProviderTiles(
    "Esri.WorldImagery") %>%# Agregar los iconos de las localidades
  addCircleMarkers(
    data = obs, 
    ~longitude, 
    ~latitude,
    weight = 0.5,
    col = 'red',
    fillColor = 'red',
    radius = 4,
    fillOpacity = 0.5,
    stroke = T,clusterOptions = markerClusterOptions())

mapa3 <- leaflet(obs,options = leafletOptions(
  attributionControl=FALSE,zoomControl = FALSE))%>%# add different provider tiles
  addProviderTiles(
    "Esri.WorldImagery") %>%# Agregar los iconos de las localidades
  addCircleMarkers(
    data = obs, 
    ~longitude, 
    ~latitude,
    weight = 0.5,
    col = 'red',
    fillColor = 'red',
    radius = 4,
    fillOpacity = 0.5,
    stroke = T,clusterOptions = markerClusterOptions())

mapa4 <- leaflet(obs,options = leafletOptions(
  attributionControl=FALSE,zoomControl = FALSE))%>%# add different provider tiles
  addProviderTiles(
    "Esri.WorldImagery") %>%# Agregar los iconos de las localidades
  addCircleMarkers(
    data = obs, 
    ~longitude, 
    ~latitude,
    weight = 0.5,
    col = 'red',
    fillColor = 'red',
    radius = 4,
    fillOpacity = 0.5,
    stroke = T,clusterOptions = markerClusterOptions())


library(htmltools)
library(shiny)
# Crear el panel con los dos mapas
panel <- fluidRow(
  column(width = 6, style = "margin-bottom: 20px;", mapa1),
  column(width = 6, style = "margin-bottom: 20px;", mapa2),
  column(width = 6, style = "margin-bottom: 20px;", mapa3),
  column(width = 6, style = "margin-bottom: 20px;", mapa4)
)

# Visualizar el panel
shinyApp(
  ui = fluidPage(panel),
  server = function(input, output) {}
)


#Estadisticas
especies <- unique(obs$taxon_species_name)
especies <- especies[!is.na(especies)]#elimina NA
especies <- as.data.frame(especies)
GBIF <- unique(obs$quality_grade)

# Filtrar y obtener las especies únicas con "research" en quality_grade
GBIF <- subset(obs, quality_grade == "research")$taxon_species_name

library(ggplot2)
library(RColorBrewer)

# Calcular la frecuencia de los nombres
frecuencia <- table(obs$taxon_phylum_name)

# Crear un dataframe con los datos de frecuencia
df <- data.frame(Phylum = names(frecuencia), Frecuencia = frecuencia)

colnames(df) <- c("Phylum_En","Var1","Frecuencia")
# Ordenar el dataframe por frecuencia descendente
df <- df[order(df$Frecuencia, decreasing = TRUE), ]

#traducir al espanol 
df$Phylum <- c("Moluscos", "Equinodermos", "Cordados", "Cnidarios", "Artrópodos",
                     "Poríferos", "Rodofitas", "Ocrofitas", "Clorofitas", "Anélidos",
                     "Briozoos", "Platelmintos", "Braquiópodos", "Ctenóforos", "Nemertinos")



# Generar una paleta de colores gradualmente variados
colores <- colorRampPalette(colors = c("blue", "green", "orange", "red"))(length(df$Phylum))

# Crear el gráfico de torta con la paleta de colores personalizada
ggplot(df, aes(x = "", y = Frecuencia, fill = Phylum)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Gráfico de Torta - Frecuencia por Phylum") +
  scale_fill_manual(values = colores[1:length(df$Phylum)]) +  # Aplicar la paleta de colores personalizada
  theme_minimal() +
  theme(legend.position = "bottom")


library(plotly)
# Crear el gráfico de torta con plot_ly
p <- plot_ly(df, labels = df$Phylum, values = df$Frecuencia, type = "pie") %>%
  layout(title = "Gráfico de Torta - Frecuencia por Phylum")


frecuenciaChordata <- table(obs$taxon_phylum_name,obs$taxon_class_name)
unique(obs$observed_on)
