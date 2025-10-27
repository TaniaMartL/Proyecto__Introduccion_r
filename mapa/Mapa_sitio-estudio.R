##########################################################################
#Nombre del script:Mapa del Área de estudio 
#Autor:Tania Martínez León 
#Proposito: Realizar el mapa del area de muestreo de Q. macdougallii
#Fecha: 26/10/25
#Datos de contacto:tanleo.m@gmail.com
#Notas
##########################################################################
# Paso 1 Librerias ####
# Vector de paquetes necesarios
paquetes <- c("ggplot2",    # Visualización de datos
              "sf",         # Archivos vectoriales
              "RColorBrewer", # Paletas de colores
              "terra",      # Maneja archivos raster
              "dbplyr",     # Manipulación de bases de datos
              "cowplot",    # Unión de gráficas
              "ggspatial",  # Rosa, la escala
              "magick",     # Fotos dentro de gráficos
              "patchwork",  # Une gráficas
              "ggnewscale", # Agrega nuevas paletas de colores para sobreponer capas en la composición del mapa
              "tidyterra")  # Sobreponer capa hillshade sobre DEM
# Instalar paquetes si no están instalados
#install.packages(paquetes, dependencies = TRUE)

# Cargar librerias
library(ggplot2) 
library(sf) 
library(RColorBrewer)
library(terra)
library(dbplyr) 
library(cowplot) 
library(ggspatial) 
library(magick) 
library(patchwork) 
library(ggnewscale)
library(tidyterra)

# Paso 2. Importar archivos vectoriales ####
# 2.1 Cargar la capa de México #####
Mexico <- st_read("destdv250k_2gw.shp")
dim(Mexico)
names(Mexico)
head(Mexico)
class(Mexico)


### 2.2 cargar capa de oaxaca
oaxaca <- st_read("oaxaca.shp")
dim(oaxaca)
names(oaxaca)
head(oaxaca)
class(oaxaca)
crs(oaxaca, proj = TRUE)
plot(oaxaca)


# 2.3 Cargar capa con los municipios del distrito de ixtlan#####
ixtlan <- st_read("ixtlan_dis_mun.shp")
dim(ixtlan)
names(ixtlan)
plot(ixtlan)
crs(ixtlan, proj = TRUE)


# 2.4 Cargar capa de puntos de muestreo
sitios <- st_read("Sitios.shp")
dim(sitios)
names(sitios)
crs(ixtlan, proj = TRUE)

# 3. Importar rásters
# 3.1 Importar Modelo Digital de elevación
elevacion<-rast("DEM_ixtlan.tif")
elevacion
plot(elevacion)
summary(elevacion)
dim(elevacion)
class(elevacion)
elevacion$DEM_ixtlan
crs(ixtlan, proj = TRUE)

# 3.2 Crear un hillshade a partir del modelo digital de elevación
# Un Hillshade es una representación de un modelo digital de elevación (DEM) que utiliza sombreado para simular la luz del sol y crear un efecto tridimensional que muestra la topografía.
# 3.2.1  Calcular atributos
# La función terra::terrain() permite calcular la pendiente y la orientación a partir del DEM. Estas son esenciales para la capa Hillshade 
terrain_attributes <- terrain(elevacion, v = c("slope", "aspect"), unit = "radians")
slope <- terrain_attributes$slope
aspect <- terrain_attributes$aspect
# # La función terra::shade() utiliza la pendiente y la orientación calculadas, junto con un ángulo y una dirección del sol especificados, para crear el ráster de sombreado
hillshade <- shade(slope, aspect, angle = 45, direction = 0) 
names(hillshade) <- "shades" # Renombrar
# 3.2.2 Visualizar el sombreado 
# Graficar el sombreado directamente. Más adelante, se combinará con la capa DEM
plot(hillshade, col = gray.colors(256, start = 0, end = 1), legend = FALSE) 

# 3.3 Convertir el ráster de elevación en un data frame
df_Ele<- as.data.frame(elevacion, xy = TRUE)
summary(df_Ele)
class(df_Ele)

# 4. Visualizaciones
# 4.1 Visualizar ráster
ggplot(data = df_Ele,aes(x = x, 
                         y = y)) +
  geom_raster(aes(fill = DEM_ixtlan))

# 4.1.2 Creando paleta propia
ggplot(data = df_Ele,aes(x = x, 
                         y = y)) +
  geom_raster(aes(fill = DEM_ixtlan))+
  scale_fill_gradientn(colours = c("#698B22", "#FFC125","darkred"  ))


# 4.2 Añadir capas vectoriales #######
# Crear paleta de color para la capa de sitios
pal_col <- c('darkorchid4', 'royalblue4')

# 4.2.1 Añadir 2 Capas vectoriales
ggplot(data =ixtlan) +
  geom_sf(color='black', fill=NA) +
  xlab("Longitud") + ylab("Latitud") +
  ggtitle("Zona de estudio",
          subtitle = "Sitios de Muestreo")+
 geom_sf(data=sitios, color = 'black', aes(fill=Sitio))+
  scale_fill_manual(values=pal_col) 

# Mapa del País con estado de Oaxaca

mv1 <- ggplot(data =Mexico) +
  geom_sf(color='black', fill=NA) +
  xlab("Longitud") + ylab("Latitud") +
  ggtitle("Macrolocalización")+
  geom_sf(data=oaxaca, color = 'black', fill='black')+
  theme_bw()
mv1

# Mapa de Oaxaca con el distrito de Ixtlán
mv2 <- ggplot(data =oaxaca) +
  geom_sf(color='black', fill=NA) +
  xlab("Longitud") + ylab("Latitud") +
  ggtitle("Distrito de Ixtlán de Juárez", )+
  theme(plot.title = element_text(size = 12))+
  geom_sf(data=ixtlan, color = 'black', fill='black')+
  theme_bw()
mv2

# 4.3 Añadir 3 capas 2 raster y 1 vectorial
# Añadir rosa de los vientos y escala 
m <-ggplot()+
  geom_raster(data = df_Ele,aes(x = x, 
                                   y = y,fill = DEM_ixtlan))+
  scale_fill_gradientn(colours = c("#698B22", "#FFC125","darkred"), name = "Elevación (msnm)")+
  xlab("Longitud") + ylab("Latitud") +
  ggtitle("Zona de estudio",
          subtitle = "Sitios de Muestreo de Q. macdougallii")+
  annotation_scale(location = "br", width_hint = 0.5) + # añadir escala
  annotation_north_arrow(location = "br", which_north = "true",  # añadir rosa de los vientos 
                         pad_x = unit(0.75, "cm"), pad_y = unit(0.5, "cm"),
                         style = north_arrow_fancy_orienteering)+
  new_scale_fill()+ # Permite agregar una nueva paleta de color para la siguiente capa 
  geom_spatraster(data = hillshade, aes(fill = shades), alpha = 0.3) + # Ajustar la transparencia con alpha
  scale_fill_gradientn(colors = hcl.colors(100, "Grays"), na.value = NA, guide = "none") +
  new_scale_fill()+ # Permite agregar una nueva paleta de color para la siguiente capa
  geom_sf(data=sitios, color = 'black', aes(fill=Sitio))+
  scale_fill_manual(values=pal_col)+
  theme_linedraw()


#Cambiar posición de rosa de los vientos  "bl" (bottom-left), "br" (bottom-right), "tl" (top-left), "tr" (top-right)
m


# 5.1 Mapa completo 

mapa <- ((mv1 / mv2 + plot_layout(guides = 'keep')) | m) + plot_layout(guides = 'collect')
mapa

# 5.1 Guardar mapa

ggsave("Mapa_Final.jpg", plot = mapa, width = 11, height = 6,dpi = 300)
