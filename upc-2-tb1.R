# Reiniciar
rm(data)
rm(data_new1)
rm(data_Origen)
rm(df_no_NA)
rm(canceladas)
rm(graf)
rm(graf2)
rm(data_TA)
rm(data.limpia)

# Instalar y cargar las librerías necesarias
install.packages("ggplot2")
install.packages("dplyr")
library(ggplot2)
library(dplyr)

#------------------------------------------------------------------
#------------------------------------------------------------------
#Cargar data
#------------------------------------------------------------------
#------------------------------------------------------------------

setwd("D:/FABRIZIO/Documents/Downloads")
data <- read.csv ("hotel_bookings.csv", header = TRUE, sep = ",", 
                  stringsAsFactors = FALSE)
data_Origen <- data # respaldo

#------------------------------------------------------------------
#------------------------------------------------------------------
#Inspeccionar data
#------------------------------------------------------------------
#------------------------------------------------------------------

View(data)  #Ver data
names(data) #Ver todas los nombres de columnas
ncol(data) #Dimension del conjunto (cant. de variables)
str(data)   #Estructura del conjunto y tipos de datos
head(data)  #Ver 6 primeras filas/registros

#------------------------------------------------------------------
#------------------------------------------------------------------
#Pre-Procesar Datos
#------------------------------------------------------------------
#------------------------------------------------------------------

#Convertir a Factor
data$arrival_date_month <- factor(data$arrival_date_month, levels = c("January", "February", 
  "March", "April", "May", "June", "July", "August", "September", "October", "November",
  "December"), ordered = TRUE)

#------------------------------------------------------------------
# Identificar datos faltantes (NA)
# Columnas con NA
unlist(lapply(data, function(x) any(is.na(x))))
# Cuantos NA por columna
sapply(data, function(x) sum(is.na(x)))
# Funcion para hallar solo columnas con NA
valor_NA <- function(x){
  sum = 0
  for(i in 1:ncol(x))
  {
    sumatoria = colSums(is.na(x[i]))
    if(sumatoria != 0){
      cat("En la columna",colnames(x[i]),"total de valores NA:",sumatoria,"\n")
    }
  }
}
valor_NA(data)


#------------------------------------------------------------------
# Tratamiento de datos faltantes        
# Reemplazar datos NA por la media
cambiar_NA <- function(x){
  col_numericas <- sapply(x, is.numeric)
  for(col in names(x)[col_numericas]){
    x[is.na(x[,col]), col] <- mean(x[,col], na.rm = TRUE)
  }
  return (x)
}
data <- cambiar_NA(data)
# Probar si existen valores NA
valor_NA(data)

#------------------------------------------------------------------
# Identificar datos atípicos (Outliers)
# Verificar atipicos en bloxplot
# Ejemplo con la variable stays_in_week_nights
boxplot(data$stays_in_week_nights, main = 
          "[stays_in_week_nights]\n antes del tratamiento", boxwex = 0.5)
# Mostrar valores atípicos
# Ejemplo con la variable stays_in_week_nights
sum(boxplot(data$stays_in_week_nights)$out)
# Cantidad de valores atípicos
# Ejemplo con la variable stays_in_week_nights
sum(!sapply(boxplot(data$stays_in_week_nights)$out, is.null))
# Imprimir variables numéricas y su cant. de datos atípicos
imp_outliers <- function(x){
  col_numericas <- sapply(x, is.numeric)
  for(col in names(x)[col_numericas]){
    total <- sum(!sapply(boxplot(x[col])$out, is.null))
    if(total != 0){
      cat("Para la columna ", col, ", su total de valores atípicos es: ", total, "\n")
    }
  }
}
imp_outliers(data)

#------------------------------------------------------------------
# Tratamiento de datos atípicos (Outliers)
# Metodo de los cuantiles
# 1) valor atipico por debajo del 5% -> media
# 2) valor atipico por encima del 95% -> mediana
tratar_outliers <- function(x, removeNA = TRUE){
  col_numericas <- sapply(x, is.numeric)
  for(col in names(x)[col_numericas]){
    cuantiles <- quantile(x[[col]], c(0.05, 0.95), na.rm = removeNA)
    x[[col]][x[[col]]<cuantiles[1]] <- mean(x[[col]], na.rm = removeNA)
    x[[col]][x[[col]]>cuantiles[2]] <- median(x[[col]], na.rm = removeNA)
  }
  return(x)
}
data_TA <- tratar_outliers(data)

# Revisión de la nueva data
boxplot(data_TA$stays_in_week_nights, main = "[stays_in_week_nights]\n despues del tratamiento", boxwex = 0.5)
imp_outliers(data_TA)
# Se obserba que aún existen datos atípicos; sin embargo, estos datos 
# poseen una separación casi mínima, lo cual es el objetivo del tratamiento.
# Por ejemplo, en el caso de la variable adulto, su intervalo de datos iba 
# desde el 0 hasta el 55. Pero después del tratamiento, su intervalo es del 
# 1 al 3. Debido a este intervalo pequeño, el comando boxplot toma los valores
# 1 y 3 como valores atípicos; sin embargo, la reducción de la separación
# fue exitosa.
# Las siguientes 3 lineas evidencian lo mencionado:
par(mfrow = c(1,2))
boxplot(data$adults, main = "[Adulto] sin tratamiento", boxwex = 0.5)
boxplot(data_TA$adults, main = "[Adulto] con tratamiento", boxwex = 0.5)
par(mfrow = c(1, 1))

#------------------------------------------------------------------
#------------------------------------------------------------------
# Visualizacion de datos
#------------------------------------------------------------------
#------------------------------------------------------------------

#------------------------------------------------------------------
# Pregunta clave 1
# ¿Cuántas reservas se realizan por tipo de hotel? ¿Qué tipo de hotel prefiere la gente?

# Filtrar los datos para las reservas no canceladas
data_no_canceladas <- subset(data_TA, reservation_status == "Check-Out")

# Crear una tabla de frecuencia de reservas no canceladas por tipo de hotel
tabla_frecuencia_hotel <- table(data_no_canceladas$hotel)

# Mostrar los resultados en la consola
print(tabla_frecuencia_hotel)

# Crear el gráfico de barras
barplot(tabla_frecuencia_hotel,
        col = c("lightblue", "lightgreen"),  # Colores personalizados
        main = "Total de Reservas No Canceladas por Tipo de Hotel",
        xlab = "Tipo de Hotel",
        ylab = "Total de Reservas No Canceladas",
        ylim = c(0, max(tabla_frecuencia_hotel) * 1.1),  # Ajustar el límite del eje y
        border = "black")  # Bordes de las barras

# Conclusión
if (tabla_frecuencia_hotel["City Hotel"] > tabla_frecuencia_hotel["Resort Hotel"]) {
  cat("La preferencia de los huéspedes es por el City Hotel.\n")
} else {
  cat("La preferencia de los huéspedes es por el Resort Hotel.\n")
}


#------------------------------------------------------------------
# Pregunta clave 2
# ¿Está aumentando la demanda con el tiempo?
data_TA$reservation_status_date <- as.Date(data_TA$reservation_status_date, format = "%Y-%m-%d")

# Extraer el año
data_TA$año <- format(data_TA$reservation_status_date, "%Y")

# Filtrar los datos para los años 2015 a 2017 y las reservas no canceladas
data_filtrada <- subset(data_TA, año %in% c("2015", "2016", "2017") & reservation_status == "Check-Out")

# Crear una tabla de frecuencia de reservas no canceladas por año
tabla_frecuencia_no_canceladas <- table(data_filtrada$año)

# Crear el gráfico de barras
barplot(tabla_frecuencia_no_canceladas, 
        col = "steelblue", 
        main = "Total de Reservas No Canceladas por Año (2015-2017)", 
        xlab = "Año", 
        ylab = "Total de Reservas No Canceladas",
        ylim = c(0, max(tabla_frecuencia_no_canceladas) * 1.1),  # Ajustar el límite del eje y
        names.arg = c("2015", "2016", "2017"))  # Etiquetas para el eje x
# Mostrar los resultados en la consola
print(tabla_frecuencia_no_canceladas)

# PARA CITY HOTEL TOTAL DE RESERVAS ANUAL

# Convertir la columna de fecha a tipo fecha
data_TA$reservation_status_date <- as.Date(data_TA$reservation_status_date, format = "%Y-%m-%d")

# Extraer el año
data_TA$año <- format(data_TA$reservation_status_date, "%Y")

# Filtrar los datos para el City Hotel y las reservas no canceladas
data_city_hotel <- subset(data_TA, hotel == "City Hotel" & reservation_status == "Check-Out")

# Crear una tabla de frecuencia de reservas no canceladas por año
tabla_frecuencia_city_hotel <- table(data_city_hotel$año)

# Crear el gráfico de barras básico
barplot(tabla_frecuencia_city_hotel,
        col = c("lightblue", "lightgreen", "lightcoral"),  # Colores personalizados
        border = "black",  # Bordes negros para las barras
        main = "Total de Reservas No Canceladas del City Hotel por Año",  # Título
        xlab = "Año",  # Etiqueta del eje x
        ylab = "Total de Reservas No Canceladas",  # Etiqueta del eje y
        ylim = c(0, max(tabla_frecuencia_city_hotel) * 1.2),  # Ajustar los límites del eje y
        names.arg = names(tabla_frecuencia_city_hotel),  # Etiquetas de los años
        cex.names = 1.2,  # Tamaño de las etiquetas
        cex.main = 1.5,  # Tamaño del título
        cex.lab = 1.3)  # Tamaño de las etiquetas de los ejes

print(tabla_frecuencia_city_hotel)

# RESORT HOTEL

# Convertir la columna de fecha a tipo fecha
data_TA$reservation_status_date <- as.Date(data_TA$reservation_status_date, format = "%Y-%m-%d")

# Extraer el año
data_TA$año <- format(data_TA$reservation_status_date, "%Y")

# Filtrar los datos para el Resort Hotel y las reservas no canceladas
data_resort_hotel <- subset(data_TA, hotel == "Resort Hotel" & reservation_status == "Check-Out")

# Crear una tabla de frecuencia de reservas no canceladas por año
tabla_frecuencia_resort_hotel <- table(data_resort_hotel$año)

# Crear el gráfico de barras
barplot(tabla_frecuencia_resort_hotel,
        col = c("lightblue", "lightgreen", "lightcoral"),  # Colores personalizados
        main = "Total de Reservas No Canceladas del Resort Hotel por Año",
        xlab = "Año",
        ylab = "Total de Reservas No Canceladas",
        ylim = c(0, max(tabla_frecuencia_resort_hotel) * 1.1),  # Ajustar el límite del eje y
        names.arg = names(tabla_frecuencia_resort_hotel),  # Etiquetas para el eje x
        border = "black")  # Bordes de las barras

print(tabla_frecuencia_resort_hotel)


#------------------------------------------------------------------
# Pregunta clave 3
# ¿Cuáles son las temporadas de reservas (alta, media, baja)?
reservas_no_canceladas <- subset(data, is_canceled == 0)

# Contar las reservas por mes
reservas_por_mes <- table(reservas_no_canceladas$arrival_date_month)

# Mostrar la cantidad de reservas por mes
print(reservas_por_mes)

# Crear un gráfico de barras para visualizar la cantidad de reservas por mes
barplot(reservas_por_mes,
        col = "skyblue",
        main = "Cantidad de Reservas por Mes (No Canceladas)",
        xlab = "Mes",
        ylab = "Cantidad de Reservas",
        las = 2)  # 'las = 2' hace que las etiquetas de los meses se muestren en vertical


#------------------------------------------------------------------
# Pregunta clave 4
# ¿Cuándo es menor la demanda de reservas?

# Identificar el mes con menor demanda
mes_menor_demanda <- names(which.min(reservas_por_mes))
cat("El mes con menor demanda de reservas es:", mes_menor_demanda, "\n")


#------------------------------------------------------------------
# Pregunta clave 5
# ¿Cuántas reservas incluyen niños y/o bebés?

# Verificar si hay valores NA en las columnas 'children' y 'babies'
sum(is.na(data_Origen$children))  # Verifica cuántos valores NA hay en 'children'
sum(is.na(data_Origen$babies))    # Verifica cuántos valores NA hay en 'babies'

# Reemplazar los valores NA en 'children' y 'babies' por 0 (suponiendo que NA significa 0)
data_Origen$children[is.na(data_Origen$children)] <- 0
data_Origen$babies[is.na(data_Origen$babies)] <- 0

# Asegurarse de que las columnas 'children' y 'babies' sean numéricas
data_Origen$children <- as.numeric(data_Origen$children)
data_Origen$babies <- as.numeric(data_Origen$babies)

# Crear una nueva columna que identifique si la reserva incluye niños, bebés o ambos
kids_data <- data_Origen %>%
  mutate(
    reservation_type = case_when(
      children > 0 & babies > 0 ~ "Con niños y bebés",
      children > 0 & babies == 0 ~ "Solo niños",
      children == 0 & babies > 0 ~ "Solo bebés",
      TRUE ~ "Sin niños ni bebés"
    )
  )

# Asegurarse de que la nueva columna `reservation_type` sea de tipo factor o texto
data_Origen$reservation_type <- as.factor(data_Origen$reservation_type)

# Pregunta: ¿Cuántas reservas incluyen niños y/o bebés?
# Gráfico de barras para visualizar la cantidad de reservas según tipo (niños, bebés o ambos)
ggplot(kids_data, aes(x = reservation_type)) +
  geom_bar(fill = "lightblue") +
  labs(x = "Tipo de Reserva",
       y = "Cantidad de Reservas",
       title = "Reservas que Incluyen Niños y/o Bebés") +
  theme_minimal()

# Crear una tabla resumen para ver los números exactos de cada tipo de reserva
resumen_reservas <- kids_data %>%
  group_by(reservation_type) %>%
  summarise(count = n())

print(resumen_reservas)

# Calcular el total de reservas que incluyen niños y/o bebés
total_con_ninos_o_bebes <- resumen_reservas %>%
  filter(reservation_type != "Sin niños ni bebés") %>%
  summarise(total = sum(count))

print(total_con_ninos_o_bebes)
nrow(data_TA)

#------------------------------------------------------------------
# Pregunta clave 6
# ¿Es importante contar con espacios de estacionamiento?
# Crear una nueva columna indicando si hay estacionamiento o no
hotel_data <- data_TA %>%
mutate(estacionamiento = ifelse(required_car_parking_spaces > 0, "Con Estacionamiento", "Sin Estacionamiento"))

# Gráfico de barras de reservas con/sin estacionamiento
ggplot(hotel_data, aes(x = estacionamiento, fill = estacionamiento)) +
  geom_bar() +
  labs(title = "Distribución de Reservas con y sin Espacios de Estacionamiento",
       x = "Tipo de Reserva",
       y = "Cantidad de Reservas") +
  theme_minimal()

# Calcular la tasa de cancelación por tipo de estacionamiento
tasa_cancelacion <- hotel_data %>%
  group_by(estacionamiento) %>%
  summarise(tasa_cancelacion = mean(is_canceled))

# Gráfico de tasa de cancelación
ggplot(tasa_cancelacion, aes(x = estacionamiento, y = tasa_cancelacion, fill = estacionamiento)) +
  geom_col() +
  labs(title = "Tasa de Cancelación para Reservas con y sin Estacionamiento",
       x = "Tipo de Reserva",
       y = "Tasa de Cancelación") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()

# Calcular el promedio de solicitudes especiales
solicitudes_especiales <- hotel_data %>%
  group_by(estacionamiento) %>%
  summarise(promedio_solicitudes = mean(total_of_special_requests))

# Gráfico de solicitudes especiales
ggplot(solicitudes_especiales, aes(x = estacionamiento, y = promedio_solicitudes, fill = estacionamiento)) +
  geom_col() +
  labs(title = "Promedio de Solicitudes Especiales con y sin Estacionamiento",
       x = "Tipo de Reserva",
       y = "Promedio de Solicitudes Especiales") +
  theme_minimal()



#------------------------------------------------------------------
# Pregunta clave 7
# Cancelaciones por mes
par(mfrow = c(1, 1))
rm(canceladas)
canceladas <- data_TA[data_TA$is_canceled == 1, ] #Filtrar

# Reservaciones canceladas por mes diferenciando entre años
cuenta2 = table(canceladas$arrival_date_year, canceladas$arrival_date_month)
graf2 <- barplot(cuenta2, beside = TRUE, 
        col = c("blue", "red", "green"), #Cada año posee un color
        main = "Cancelados por mes y año", 
        ylab = "Cantidad de cancelaciones", 
        xlab = "Mes de cancelacion", 
        legend = rownames(cuenta2),
        args.legend = list(title = "Año", x = "topright"),
        las = 2,
        ylim = c(0, max(cuenta2) * 1.2)) 
text(graf2, cuenta2 + 2, labels = as.vector(cuenta2), cex = 0.8, pos = 3)

# Reservaciones canceladas por mes sin deferenciar años
cuenta = table(canceladas$arrival_date_month)
graf <- barplot(cuenta, col=c("red"), main = "Cancelados por mes",las = 3,
                ylab = "Cantidad de cancelaciones", 
                xlab = "Mes de cancelacion",
                ylim = c(0, max(cuenta) * 1.2))
text(graf, cuenta + 2, labels = as.vector(cuenta), cex = 0.8, pos = 3)


# EXTRA
# Busqueda de cadenas de caracteres "NULL". No es espacio en blanco.
valor_Null <- function(x){
  sum = 0
  for(i in 1:ncol(x))
  {
    sumatoria <- sum(x[i] == "NULL", na.rm = TRUE)
    if(sumatoria != 0){
      cat("En la columna",colnames(x[i]),"total de valores NA:",sumatoria,"\n")
    }
  }
}
valor_Null(data)

# Hallar las medias de las columnas numericas
todas_MEAN <- function(x, removeNA = TRUE){
  col_numericas <- sapply(x, is.numeric)
  for(col in names(x)[col_numericas])
  {
    cat("En la columna",colnames(x[col]),"promedio:", mean(x[[col]]),"\n")
  }
}
todas_MEAN(data)


# Obtener data modificada
write.csv(data_TA, "hotel_bookings_updt.csv", row.names = FALSE)
