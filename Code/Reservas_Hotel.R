install.packages("readr")
install.packages("ggplot2")

library(ggplot2)

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

ggplot(resumen_hoteles, aes(x = hotel, y = total_reservas, fill = hotel)) +
  geom_bar(stat = "identity") +
  labs(title = "Cantidad de reservas por hotel",
       x = "Tipo de Hotel",
       y = "total de Reservas") +
  theme_minimal()
ggplot(resumen_hoteles, aes(x = hotel, y = total_reservas, fill = hotel)) +
  geom_bar(stat = "identity") +
  labs(title = "Cantidad de reservas por hotel",
       x = "Tipo de Hotel",
       y = "total de Reservas") +
  scale_fill_manual(values = c("City Hotel" = "blue", "Resort Hotel" = "red")) + # Colores personalizados
  theme_minimal()



getwd()