install.packages("readr")
install.packages("ggplot2")
install.packages("dplyr")

library(ggplot2)
library(cowplot)
library(patchwork)
library(dplyr)

hotel_bookings <- read.csv("hotel_bookings.csv", header = TRUE, stringsAsFactors = FALSE)

#numero de filas
nrow(hotel_bookings)

#numero de columnas
ncol(hotel_bookings)

#estructura de los datos
str(hotel_bookings)

#resumen de los datos
summary(hotel_bookings)

#Valores faltantes por columna
colSums(is.na(hotel_bookings))

#eliminiacion de filas con valores faltantes
hotel_bookings <- na.omit(hotel_bookings)
colSums(is.na(hotel_bookings))

#valores atipicos
boxplot(hotel_bookings$columna)


columna_num <- hotel[, sapply(hotel, is.numeric)]

boxplot(numeric_cols)
for (col in names (hotel)){
  if(is.numeric(hotel[[col]])){
    boxplot(hotel[[col]], main = col)
  }
}

numeric_cols <-new_hotel[,sapply(new_hotel, is.numeric)]

boxplot(numeric_cols)
for (col in names(new_hotel)) {
  if(is.numeric(new_hotel[[col]])){
    boxplot(new_hotel[[col]], main=col)
  }
}

resumen_hoteles <- summarise(group_by(hotel_bookings, hotel), total_reservas = n())

ggplot(resumen_hoteles, aes(x = hotel, y = total_reservas, fill = hotel)) +
  geom_bar(stat = "identity") +
  labs(title = "Cantidad de reservas por hotel",
       x = "Tipo de Hotel",
       y = "total de Reservas") +
  scale_fill_manual(values = c("City Hotel" = "red", "Resort Hotel" = "blue")) +
  theme_minimal()


resumen_hoteles <- summarise(
  group_by(hotel_bookings, hotel, arrival_date_year), 
  total_reservas = n()
)

ggplot(resumen_hoteles, aes(x = arrival_date_year, y = total_reservas, fill = hotel)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Reservas Totales por Año y Tipo de Hotel",
       x = "Año",
       y = "Total de Reservas") +
  scale_fill_manual(values = c("City Hotel" = "red", "Resort Hotel" = "blue")) +
  theme_minimal()

resumen_reservas <- summarise(group_by(hotel_bookings, arrival_date_year, arrival_date_month, hotel),
                              total_reservas = n())

ggplot(resumen_reservas, aes(x = arrival_date_month, y = total_reservas, color = hotel, group = hotel)) +
  geom_line() +
  facet_wrap(~ arrival_date_year, ncol = 1) +
  labs(title = "Evolución de reservas por mes y año",
       x = "Mes",
       y = "Número de reservas") +
  scale_color_manual(values = c("City Hotel" = "#FC8D62", "Resort Hotel" = "#66C2A5")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

hotel_bookings$con_ninos_bebes <- ifelse(hotel_bookings$children > 0 | hotel_bookings$babies > 0, 
                                         "SI", 
                                         "NO")



reservas_ninos <- aggregate(hotel_bookings$hotel, 
                            by = list(hotel_bookings$con_ninos_bebes), 
                            FUN = length)

colnames(reservas_ninos) <- c("con_ninos_bebes", "total_reservas")

ggplot(reservas_ninos, aes(x = con_ninos_bebes, y = total_reservas, fill = con_ninos_bebes)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Cantidad de Reservas con y sin Niños/Bebés",
    x = "Tipo de Reserva",
    y = "Total de Reservas"
  ) +
  
   theme_minimal()

hotel_bookings$requiere_estacionamiento <- ifelse(hotel_bookings$required_car_parking_spaces > 0, 
                                                  "Requiere estacionamiento", 
                                                  "No requiere estacionamiento")

reservas_estacionamiento <- aggregate(hotel_bookings$hotel, 
                                      by = list(hotel_bookings$requiere_estacionamiento), 
                                      FUN = length)

colnames(reservas_estacionamiento) <- c("requiere_estacionamiento", "total_reservas")

ggplot(reservas_estacionamiento, aes(x = requiere_estacionamiento, y = total_reservas, fill = requiere_estacionamiento)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Cantidad de Reservas que Requieren o No Estacionamiento",
    x = "Requerimiento de Estacionamiento",
    y = "Total de Reservas"
  ) +
 
  theme_minimal()

resumen_hoteles <- summarise(group_by(hotel_bookings, arrival_date_month, hotel),
                             total_cancelaciones = sum(is_canceled))

ggplot(resumen_hoteles, aes(x = arrival_date_month, y = total_cancelaciones, fill = hotel)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total de Cancelaciones por Hotel y Mes",
       x = "Mes",
       y = "Total de Cancelaciones") +
  scale_fill_manual(values = c("City Hotel" = "#66C2A5", "Resort Hotel" = "#FCFF62")) +
  theme_minimal()

write.csv(data, "HotelBookingLimpio.csv", row.names = TRUE)

getwd()