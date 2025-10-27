##########################################################################
#Nombre del script: Análisis de variables morfológicas y ambientales de Q. macdougallii
#Autor:Tania Martínez León 
#Proposito: Realizar el análisis de variables morfológicas y ambientales de Q. macdougallii
#Fecha: 26/10/25
#Datos de contacto:tanleo.m@gmail.com
#Notas
##########################################################################
# 1.	Cargar paquetes necesarios 
# Vector de paquetes necesarios
paquetes <- c( ",tidyverse", # manipulación de datos
               "ggplot2",    # Visualización de datos
               "geodata",    # descargar datos climáticos 
               "terra",      # Extraer valores de variables ambientales descargados para coordenadas especificadas
               "factoextra", # Extraer y visualizar los resultados de análisis de datos multivariados
               "ggcorrplot", # Visualizar  matrices de correlación utilizando ggplot2
               "devtools",     # 
               "ggbiplot",   # grafica de PCA
               "car",        # funciones y herramientas para el análisis de regresión
               "psych",      # PCA
               "patchwork")  # Une gráficas

# Instalar paquetes si no están instalados
#install.packages(paquetes, dependencies = TRUE)
#----------------
#paquetes necesarios 
library(tidyverse)
library(ggplot2)
library(geodata)
library(terra)
library(factoextra)
library(ggcorrplot)
library(devtools)
library(ggbiplot)
library(car)
library(psych)
library(patchwork)
# 2. importar las variables de Q. macdougalli  y explorar la base de datos 
# La base de datos es parte de un proyecto en desarrollo de Alfonso-Corrado et al., que tiene por objetivo conocer la distribución y estructura poblacional de Q. macdougalli, encino endémico de Oaxaca. Tiene 210 observaciones y 10 variables, como son DAP, Altura, diámetro de copa y coordenadas. Las variables fueron obtenidas entre el 29 de febrero y 6 de septiembre del 2024.
df <- read_csv(file = "dataframe_Quercusm.csv") 
dim(df)
summary(df)
# Eliminar filas con valores faltantes 
df_filt <- na.omit(df)
dim(df_filt)
summary(df_filt)
# Agregar una nueva variable que contenga el promedio del diametro de copa 
df_filt$diametro_copa_mm <- (df_filt$diametro_copa1_m +df_filt$diametro_copa2_m)/2
#convertir a factor la variable lugar
class(df_filt$lugar)
df_filt$lugar <- as.factor(df_filt$lugar)         
class(df_filt$lugar)
summary(df_filt$lugar)
# 3. Calcular la media y desviación estandar por lugar 
# calcular logaritmo de las variables para normalizar y estabilizar varianza
df_filt$H_log <-log(df_filt$Altura_m) 
df_filt$D_log <-log(df_filt$Diametro_cm)
df_filt$C_log <-log(df_filt$diametro_copa_mm)
# # La función aggregate() se utiliza para calcular estadísticas de resumen para subconjuntos de datos. Permite agrupar datos según una o más variables y aplicar una función específica.
aggregate(cbind(H_log, D_log, C_log) ~ lugar, data=df_filt, FUN=function(x) c(mean=mean(x), sd=sd(x)))
## 4. graficar las variables por lugar 
# establecer paleta de colores
pal <- c('#8B3A62', '#7CCD7C')
#Gráfico de Altura
H <- ggplot(df_filt, aes(x=lugar, y=H_log, fill = lugar)) +
  geom_boxplot() +
  geom_jitter(width=0.15) + # permite vizualizar los puntos de datos individuales
  scale_fill_manual(values=pal)+
  labs(title = "Altura",
x = "Lugar", # Etiqueta del eje X
y = "Log de Altura") # Etiqueta del eje Y
H
#Gráfico de DAP
D <- ggplot(df_filt, aes(x=lugar, y=D_log, fill = lugar)) + 
  geom_boxplot() + 
  geom_jitter(width=0.15)+ # permite vizualizar los puntos de datos individuales
  scale_fill_manual(values=pal)+ 
  labs(title = "Diámetro a altura del pecho",
       x = "Lugar", # Etiqueta del eje X
       y = "Log de DAP") # Etiqueta del eje Y
D
#Gráfico de Diámetro de copa
Cr <- ggplot(df_filt, aes(x=lugar, y=C_log, fill = lugar)) +
  geom_boxplot() +
  geom_jitter(width=0.15)+ # permite vizualizar los puntos de datos individuales
  scale_fill_manual(values=pal)+
  labs(title = "Diámetro de copa",
       x = "Lugar", # Etiqueta del eje X
       y = "Log de Diámetro de copa") # Etiqueta del eje Y
Cr
# unir las gráficas con patchwork
#Aplicar un tema común a todos los gráficos y titulos globales al gráfico
var <- wrap_plots((H /D) | Cr) &
  plot_annotation(
    title = "Variables morfológicas de Q. macdougallii ",
    subtitle = "Datos de distribución y estructura poblacional de Q. macdougallii",
    caption = "Fuente: Alfonso-Corrado etal, en prensa") &
  theme_bw()

var
# Guardar gráfico
ggsave ("Gráfico_variables.png", plot = var, width = 12, height = 6)

## 5. Gráfica de dispersión para explorar la relación entre variables morfológicas 
# Gráfica de relación entre DAP y altura 
D_H <- ggplot(df_filt, aes(x = D_log, y = H_log)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "#458B74")+
  labs(title = "Relación entre Diámetro a altura del pecho y Altura",
      x = "Log DAP", # Etiqueta del eje X
      y = "Log Altura") + # Etiqueta del eje Y
theme_bw()+
  facet_wrap(~ lugar) # separa las variables por lugar
  D_H
  
# Gráfica de relación entre DAP y Diámetro de Copa 
D_C <- ggplot(df_filt, aes(x = D_log, y = C_log)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "#8B3E2F")+
  labs(title = "Relación entre Diámetro a altura del pecho y Diámetro de Copa",
       x = "Log DAP", # Etiqueta del eje X
       y = "Log de Diámetro de copa ") + # Etiqueta del eje Y
  theme_bw()+
  facet_wrap(~ lugar) # separa las variables por lugar
D_C
#Unir los gráficos 
dispersion <- (D_H /D_C) + # unir verticalmente 
  plot_annotation(
    title = "Relación entre Variables morfológicas de Q. macdougallii ",
    subtitle = "Datos de distribución y estructura poblacional de Q. macdougallii",
    caption = "Fuente: Alfonso-Corrado etal, en prensa") 

dispersion
## Guardar gráfico 
ggsave ("Gráfico_dispersión.png", plot = dispersion, width = 8, height = 6)

# 6. Descargar variables ambientales 
# descargar variables climáticas de worldclim
# bio_data <- worldclim_country("Mexico", var = "bio", res = 0.3, path = tempdir())
# Renombrar las variables para mayor claridad
#crear un vector vacio para meterlo en un loop
#v_names <- vector()
#for(i in 1:19){
  #v_names[i] <- paste0("bio_", sprintf("%02d", i)) #pegar los números a el prefijo bio
#}
#asignar nombre

# names(bio_data) <- v_names
#seleccionar las coordenadas del dataframe para extraer las variables climáticas 
#coord <- df_filt %>%
  #dplyr::select(Longitud, Latitud)
## Extrarer las variables climáticas por individuos georrefenciados 
#extracted_bio <- terra::extract(bio_data, coord)
#cambiar los valores de Id para que coincidan con el dataframe original 
#extracted_bio$ID <- (extracted_bio$ID=df_filt$Id)  
#Guardar las variables climáticas
#write_csv(extracted_bio, "bio_coor.csv")
# importar el dataframe con las variables climáticas 
df_bio <- read_csv(file = "bio_coor.csv") 
summary(df_bio)

#Descargar una capa raster de altitud
#alt <- worldclim_country("Mexico",var="elev", res=0.3, path=tempdir())

### extraer la elevación de para cada individuo georreferenciado
#extracted_alt <- terra::extract(alt, coord)
#cambiar los valores de Id para que coincidan con el dataframe original 
#extracted_alt$ID <- (extracted_alt$ID=df_filt$Id)  
#Rename a single column
#extracted_alt <- extracted_alt %>%
# rename(elevacion = MEX_wc2.1_30s_elev )
##guardar dataframe de elavacion 
#write_csv(extracted_alt, "elevacion.csv")
### importar datos de elevacion 
alt <- read_csv("elevacion.csv")
### unir el data frame de las variables climáticas con la elevación 
df_env <- left_join(df_bio, alt, by="ID")

### 7.  Análisis estadísticos 

# 7.1 de levene
# La prueba de Levene es un análisis estadístico que evalúa si las varianzas de dos o más grupos son iguales
# Si el p-valor es menor que el nivel de significancia (generalmente 0.05), indica que las varianzas son diferentes 
# Prueba de levene para log de Altura
leve_res_H <- leveneTest(H_log ~ lugar, data = df_filt)
print(leve_res_H)
# Prueba de levene para log de Diámetro a altura del pecho
leve_res_D <- leveneTest(D_log ~ lugar, data = df_filt)
print(leve_res_D)
# Prueba de levene para log de Diámetro de Copa
leve_res_C <- leveneTest(C_log ~ lugar, data = df_filt)
print(leve_res_C)
# Esto indica un resultado estadísticamente significativo, lo que significa que se rechaza la hipótesis nula de varianzas iguales. Las varianzas se consideran desiguales entre los sitios

## 7.2Prueba de t de Welch
#La prueba t de Welch es una prueba estadística que se utiliza para comparar la media de dos grupos en situaciones donde los tamaños de muestra y/o las varianzas no son iguales.
# Prueba t de Welch  para log de Altura
t_H <- t.test(H_log ~ lugar, data = df_filt, var.equal = FALSE)
print(t_H)
# Prueba t de Welch  para log de Diámetro a altura de copa
t_D <- t.test(D_log ~ lugar, data = df_filt, var.equal = FALSE)
print(t_D)
# Prueba t de Welch  para log de Diámetro de Copa
t_C <- t.test(C_log ~ lugar, data = df_filt, var.equal = FALSE)
print(t_C)
# Dado que p-valor es menor que el nivel de significancia, se  rechaza la hipótesis nula de medias iguales

# 8. Análisis de Multivariado
# 8.1 Análisis de correlación de las variables climáticas, para seleccionar las variables para el PCA

mat_cor <- cor(subset(df_env, select = -c(ID)))
# Gráficar mátriz de correlación 
ggcorrplot(mat_cor, #matriz de correlación
           hc.order = FALSE, #si se ordena por un m?todo (HCLUST) o por default
           type = "lower", #si se muestra toda o una secci?n
           lab = TRUE, #si se agregan los d?gitos
           lab_size = 3, #tama?o de los d?gitos
           colors= c("blue", "white", "red") #vector de colores
)
# seleccionar las viarables bio_01 (temperatura media anual), bio_12 (precipitación anual) y elevación 

# unir dataframes con varibles morfológicas y ambientales 
df_all <- left_join(df_env, df_filt, by=join_by("ID"=="Id"))
summary(df_all)
# seleccionar variables para el PCA 
var_pca <- dplyr::select (df_all,bio_01,bio_12,elevacion, H_log, D_log, C_log, lugar)

# 8.2 Análisis de correlación de las variables
mat_cor_var <- cor(var_pca [,-7]) # se exlcuye la varaible lugar

# Gráficar mátriz de correlación 
cm <- ggcorrplot(mat_cor_var, #matriz de correlaci?n
           hc.order = FALSE, #si se ordena por un m?todo (HCLUST) o por default
           type = "lower", #si se muestra toda o una secci?n
           lab = TRUE, #si se agregan los d?gitos
           lab_size = 3, #tama?o de los d?gitos
           colors= c("blue", "white", "red") #vector de colores
)
# Agregar titulos 
cmt <- cm + labs(title = "Mátriz de Correlación de Variables morfológicas y ambientales de Q. macdougallii",
          subtitle = "Datos de distribución y estructura poblacional de Q. macdougallii",
          caption = "Fuente: Alfonso-Corrado etal, en prensa")
cmt
# Se observa una fuerte correlación entre las variables morfológicas 

# 8.3 PCA 
# El PCA reduce la dimensionalidad de los datos al crear nuevas variables, llamadas componentes principales, que son combinaciones lineales de las variables originales.
# Facilita la visualización de datos de alta dimensionalidad en un gráfico de 2 o 3 dimensiones

# Calculo del PCA
        pc <- prcomp(var_pca[,-7],
             center = TRUE,
             scale. = TRUE)
attributes(pc)
print(pc)
summary(pc)

### crear un dataframe con los valores del summary(pc), que se usaran para graficar la varianza explicada 
pca_summary <- data.frame(
  Components = c("1", "2", "3", "4", "5","6"),
  ExplainedVariance= c(46.25, 32.77, 16.01, 03.60, 01.373, 00.0010),
  CumulativeExplainedVariance = c(46.25, 79.02, 95.03, 98.63, 99.999, 100.0000))

# Gráfica de la varianza explicada 
plot_explained_variance <- ggplot(pca_summary, aes(x = Components)) +
  geom_bar(aes(y = ExplainedVariance), stat = "identity", fill = "steelblue", color = "black") +
  geom_text(aes(y = ExplainedVariance, label = paste0(round(ExplainedVariance, 2), "%")), vjust = -0.5, size = 5, color = "black") + # etiqueta de la varianza explicada por ejes 
  ylim(0, 101) + # limite en y del gráfico
  labs(
    title = "Varianza explicada por Componente Principal",
    x = "Componente Principal",
    y = "Porcentaje de Varianza Explicada"
  ) +
  theme_minimal()
plot_explained_variance 
# Gráfica de la Varianza explicada acumulada
plot_cumulative_explained_variance <- ggplot(pca_summary, aes(x = Components)) +
  geom_line(aes(y = CumulativeExplainedVariance), color = "orange", group = 1) +
  geom_point(aes(y = CumulativeExplainedVariance), color = "orange") +
  geom_text(aes(y = CumulativeExplainedVariance, label = paste0(round(CumulativeExplainedVariance, 2), "%")), vjust = -0.5, size = 5, color = "#000000", nudge_x = 0.15) +
  scale_y_continuous(name = "Varianza explicada acumulada (%)") +
  ylim(0, 101) + # limite en y del gráfico
  labs(
    title = "Varianza explicada acumulada",
    x = "Componente Principal"
  ) +
  theme_minimal()
plot_cumulative_explained_variance

# Unir los gráficos de varianza explicada
var_exp <-  (plot_explained_variance +plot_cumulative_explained_variance)+
  plot_annotation(
    title = "Varianza Explicada (PCA)",
    subtitle = "Datos de distribución y estructura poblacional de Q. macdougallii",
    caption = "Fuente: Alfonso-Corrado etal, en prensa"
)
var_exp
### guardar grafico de varianza explicada
ggsave ("Varianza Explicada.png", plot = var_exp, width = 8, height = 6)

##### Gráfico PCA 
# guardar en un vector los datos de sitio
y <- var_pca %>% select(lugar)

#guardar en un vector los 2 primeros vectores
componentes <- data.frame(pc$x[, 1:2], sitio = y)
colors <- c("#38812F", "#A30000")

plot_pca <- ggplot(componentes, aes(x = PC1, y = PC2, color = lugar)) +
  geom_point(alpha = 0.8, size = 4) +
  labs(title = "Variables morfológicas y ambientales de Q. macdougallii proyectadas en los primeros 2 Componentes Principales",
       subtitle = "Datos de distribución y estructura poblacional de Q. macdougallii",
       caption = "Fuente: Alfonso-Corrado etal, en prensa") +
  scale_color_manual(values = colors) +
  theme_minimal()

plot_pca

## guardar gráfico de PCA
ggsave ("PCA.png", plot = plot_pca, width = 8, height = 6)

# Gráfico de PCA con ejes de variables 
g <- ggbiplot(pc,
              obs.scale = 1,
              var.scale = 1,
              groups = var_pca$lugar,
              ellipse = TRUE,
              circle = TRUE,
              ellipse.prob = 0.68)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal',
               legend.position = 'top')
g <- g + plot_annotation(
  title = "Análisis de Componenetes Principales (PCA)",
  subtitle = "Datos de distribución y estructura poblacional de Q. macdougallii",
  caption = "Fuente: Alfonso-Corrado etal, en prensa"
)
g
## guardar gráfico de PCA
ggsave ("PCA_2.png", plot = g, width = 6, height = 8)

####Conclusiones 
# El análisis de t de welch sugiere que existen diferencias entre las tres variables morfológicas por sitios  
# 	Las gráficas de PCA muestran cierta separación entre individuos por sitios, sin embargo, se observa una alta correlación entre las variables morfológicas. Por tanto, se sugiere que en futuros análisis se explore la relación de otras variables morfológicas (ej. Morfología de la hoja) con variables ambientales 
