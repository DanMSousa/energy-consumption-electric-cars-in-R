setwd("D:/Projetos/BigDataAnalyticsComReMAzureLearning/Cap21/Projeto1")
getwd()

install.packages("readxl")
install.packages("dplyr")
install.packages("sqldf")
install.packages("ggplot2")
library(readxl)
library(dplyr)
library(sqldf)
library(ggplot2)

# Pacotes para visualizar a análise de correlação
install.packages('corrgram')
install.packages('corrplot')
library(corrplot)
library(corrgram)

path_file_excel = "Dataset_of_electric_passenger_cars_with_their_specifications/FEV-data-Excel.xlsx"

worksheets = excel_sheets(path_file_excel)
worksheets

df_cars = read_excel(path_file_excel)
View(df_cars)

# renomear nomes das colunas:
new_col_names <- gsub(" ", "_", colnames(df_cars))
colnames(df_cars) <- new_col_names
new_col_names_two <- gsub("\\[", "", colnames(df_cars))
colnames(df_cars) <- new_col_names_two
new_col_names_three <- gsub("\\]", "", colnames(df_cars))
colnames(df_cars) <- new_col_names_three
new_col_names_four <- gsub("-", "", colnames(df_cars))
colnames(df_cars) <- new_col_names_four
new_col_names_five <- gsub("/", "_", colnames(df_cars))
colnames(df_cars) <- new_col_names_five
new_col_names_six <- gsub("\\(", "_", colnames(df_cars))
colnames(df_cars) <- new_col_names_six
new_col_names_seven <- gsub("\\)", "_", colnames(df_cars))
colnames(df_cars) <- new_col_names_seven

View(df_cars)


sum(is.na(df_cars))
colSums(is.na(df_cars))

str(df_cars)

names(df_cars)
#


 # data frame sem NAs para gerar médias:
df_cars_with_no_NA = na.omit(df_cars)

View(df_cars_with_no_NA)
View(df_cars)
#







## Trocar variéveis categóricas por números e para tipo numérico:


# Type of Brakes
unique(df_cars$Type_of_brakes)

df_cars$Type_of_brakes[which(df_cars$Type_of_brakes == "disc (front + rear)")] <- 1
df_cars$Type_of_brakes[which(df_cars$Type_of_brakes == "disc (front) + drum (rear)")] <- 2

df_cars_with_no_NA$Type_of_brakes[which(df_cars_with_no_NA$Type_of_brakes == "disc (front + rear)")] <- 1
df_cars_with_no_NA$Type_of_brakes[which(df_cars_with_no_NA$Type_of_brakes == "disc (front) + drum (rear)")] <- 2


unique(df_cars$Type_of_brakes)
View(df_cars)
View(df_cars_with_no_NA)
str(df_cars_with_no_NA)

brakes_as_numeric <- as.numeric(df_cars_with_no_NA$Type_of_brakes)

df_cars_with_no_NA$Type_of_brakes <- brakes_as_numeric
df_cars$Type_of_brakes <- brakes_as_numeric

str(df_cars_with_no_NA)
View(df_cars_with_no_NA)
#

# Drive_type
str(df_cars)
unique(df_cars$Drive_type)
df_cars$Drive_type[which(df_cars$Drive_type == "4WD")] <- 1
df_cars$Drive_type[which(df_cars$Drive_type == "2WD (rear)")] <- 2
df_cars$Drive_type[which(df_cars$Drive_type == "2WD (front)")] <- 3

df_cars_with_no_NA$Drive_type[which(df_cars_with_no_NA$Drive_type == "4WD")] <- 1
df_cars_with_no_NA$Drive_type[which(df_cars_with_no_NA$Drive_type == "2WD (rear)")] <- 2
df_cars_with_no_NA$Drive_type[which(df_cars_with_no_NA$Drive_type == "2WD (front)")] <- 3

unique(df_cars)
View(df_cars)
View(df_cars_with_no_NA)
str(df_cars_with_no_NA)

drive_types_numeric <- as.numeric(df_cars_with_no_NA$Drive_type)
drive_types_numeric2 <- as.numeric(df_cars$Drive_type)

df_cars_with_no_NA$Drive_type <- drive_types_numeric
df_cars$Drive_type <- drive_types_numeric2

str(df_cars_with_no_NA)
View(df_cars_with_no_NA)

str(df_cars)
View(df_cars)

#







## colocar média da coluna em valores NA:

# Type_of_brakes
mean_type_of_brakes = mean(df_cars_with_no_NA$Type_of_brakes)
ceiling(mean_type_of_brakes)
df_cars$Type_of_brakes[which(is.na(df_cars$Type_of_brakes))] <- ceiling(mean_type_of_brakes)

numeric_type_of_brakes <- as.numeric(df_cars$Type_of_brakes)
df_cars$Type_of_brakes <- numeric_type_of_brakes

View(df_cars)
View(df_cars_with_no_NA)

colSums(is.na(df_cars))
#

# Maximum_load_capacity_kg
str(df_cars)
mean_max_load_capacity_kg <- mean(df_cars_with_no_NA$Maximum_load_capacity_kg)
mean_max_load_capacity_kg
df_cars$Maximum_load_capacity_kg[which(is.na(df_cars$Maximum_load_capacity_kg))] <- ceiling(mean_max_load_capacity_kg)

View(df_cars)
View(df_cars_with_no_NA)

colSums(is.na(df_cars))
#

# Acceleration_0100_kph_s:
str(df_cars)
mean_acceleration_0100_kph_s <- mean(df_cars_with_no_NA$Acceleration_0100_kph_s)
mean_acceleration_0100_kph_s
df_cars$Acceleration_0100_kph_s[which(is.na(df_cars$Acceleration_0100_kph_s))] <- ceiling(mean_acceleration_0100_kph_s)

View(df_cars)
View(df_cars_with_no_NA)

colSums(is.na(df_cars))
#

# mean__Energy_consumption_kWh_100_km
str(df_cars)
mean_mean__Energy_consumption_kWh_100_km <- mean(df_cars_with_no_NA$mean__Energy_consumption_kWh_100_km)
mean_mean__Energy_consumption_kWh_100_km

df_cars$mean__Energy_consumption_kWh_100_km[which(is.na(df_cars$mean__Energy_consumption_kWh_100_km))] <- mean_mean__Energy_consumption_kWh_100_km

View(df_cars)
View(df_cars_with_no_NA)

colSums(is.na(df_cars))
#

# Permissable_gross_weight_kg 
str(df_cars)
mean_permissable_gross_weight_kg <- mean(df_cars_with_no_NA$Permissable_gross_weight_kg)
mean_permissable_gross_weight_kg

df_cars$Permissable_gross_weight_kg[which(is.na(df_cars$Permissable_gross_weight_kg))] <- mean_permissable_gross_weight_kg

View(df_cars)
View(df_cars_with_no_NA)

colSums(is.na(df_cars))
#

# Boot_capacity__VDA__l
str(df_cars)
mean_boot_capacity__VDA__l <- mean(df_cars_with_no_NA$Boot_capacity__VDA__l)
mean_boot_capacity__VDA__l

df_cars$Boot_capacity__VDA__l[which(is.na(df_cars$Boot_capacity__VDA__l))] <- mean_boot_capacity__VDA__l

View(df_cars)
View(df_cars_with_no_NA)

colSums(is.na(df_cars))
#

is.na(df_cars)
#

summary(df_cars$Engine_power_KM)






# VERIFICAÇÃO DAS CORRELAÇÕES E RELEVANCIA PARA AS COLUNAS EM RELAÇÃO A MÉDIA DE CONSUMO: 


# correlação da potencia elétrica e a média de consumo por carro - Engine_power_KM:
correlation_mean_energy_power <- cor(df_cars$mean__Energy_consumption_kWh_100_km, df_cars$Engine_power_KM)
correlation_mean_energy_power # abaixo de 0,6

regression <- lm(df_cars$mean__Energy_consumption_kWh_100_km ~ df_cars$Engine_power_KM)
summary(regression) # p-valor: 0.000128 ***
?lm
?plot

z = plot(
  x=df_cars$Engine_power_KM,
  y=df_cars$mean__Energy_consumption_kWh_100_km, main="Energy Power x Média Consumo",
  xlab="Potencia Elétrica", ylab="Média de consumo", pch=1,
) # nao parece ter uma correlação, remover energy power da predição
grid(z) #aplicando grid ao gráfico
abline(regression)


# correlação preço mínimo bruto e média de consumo - Minimal_price__gross__PLN:
str(df_cars)

correlation_mean_Minimal_price__gross__PLN <- cor(df_cars$mean__Energy_consumption_kWh_100_km, df_cars$Minimal_price__gross__PLN)
correlation_mean_Minimal_price__gross__PLN # perto de 0,7

regression <- lm(df_cars$mean__Energy_consumption_kWh_100_km ~ df_cars$Minimal_price__gross__PLN)
summary(regression) # p-valor: 2.29e-08 ***
?lm
?plot

z = plot(
  x=df_cars$Minimal_price__gross__PLN,
  y=df_cars$mean__Energy_consumption_kWh_100_km, main="Minimal Price Gross x Média Consumo",
  xlab="Preço Mínimo Bruto", ylab="Média de consumo", pch=1,
) # nao parece ter uma grande correlação, talvez remover Minimal Price Gross da predição
grid(z) #aplicando grid ao gráfico
abline(regression)


# correlação entre torque máximo e média de consumo - Maximum_torque_Nm
str(df_cars)

correlation_mean_Maximum_torque_Nmr <- cor(df_cars$mean__Energy_consumption_kWh_100_km, df_cars$Maximum_torque_Nm)
correlation_mean_Maximum_torque_Nmr # abaixo de 0,5

regression <- lm(df_cars$mean__Energy_consumption_kWh_100_km ~ df_cars$Maximum_torque_Nm)
summary(regression) # 1.59e-05 ***
?lm
?plot

z = plot(
  x=df_cars$Maximum_torque_Nm,
  y=df_cars$mean__Energy_consumption_kWh_100_km, main="Torque Máximo x Média Consumo",
  xlab="Máximo de Torque", ylab="Média de consumo", pch=1,
) # nao parece ter uma grande correlação, talvez remover Minimal Price Gross da predição
grid(z) #aplicando grid ao gráfico
abline(regression)


# Correlação entre Tipo de Freio e a média de consumo -  Type_of_brakes
str(df_cars)

correlation_mean_Type_of_brakesr <- cor(df_cars$mean__Energy_consumption_kWh_100_km, df_cars$Type_of_brakes)
correlation_mean_Type_of_brakesr # CORRELAÇÃO NEGATIVA

regression <- lm(df_cars$mean__Energy_consumption_kWh_100_km ~ df_cars$Type_of_brakes)
summary(regression) # p-valor: 0.292
?lm
?plot

z = plot(
  x=df_cars$Type_of_brakes,
  y=df_cars$mean__Energy_consumption_kWh_100_km, main="Tipo de Freio x Média Consumo",
  xlab="Tipo de Freio", ylab="Média de consumo", pch=1,
) # nao parece ter uma grande correlação, remover
grid(z) #aplicando grid ao gráfico
abline(regression)


# correlação entre tipo de drive e média de consumo - Drive_type:
str(df_cars)

correlation_mean_Drive_type <- cor(df_cars$mean__Energy_consumption_kWh_100_km, df_cars$Drive_type)
correlation_mean_Drive_type # CORRELAÇÃO NEGATIVA : -0,5211

regression <- lm(df_cars$mean__Energy_consumption_kWh_100_km ~ df_cars$Drive_type)
summary(regression) # p-valor: 6.31e-05 ***

z = plot(
  x=df_cars$Drive_type,
  y=df_cars$mean__Energy_consumption_kWh_100_km, main="Tipo de Freio x Média Consumo",
  xlab="Drive_type", ylab="Média de consumo", pch=1,
) # nao parece ter uma grande correlação, remover
grid(z) #aplicando grid ao gráfico
abline(regression)


# correlação entre capacidade da bateria por quilometro por hora e média de consumo - Battery_capacity_kWh
str(df_cars)

correlation_mean_Battery_capacity_kWh <- cor(df_cars$mean__Energy_consumption_kWh_100_km, df_cars$Battery_capacity_kWh)
correlation_mean_Battery_capacity_kWh # correlação um pouco acima de 0,6

regression <- lm(df_cars$mean__Energy_consumption_kWh_100_km ~ df_cars$Battery_capacity_kWh)
summary(regression) # p-valor: 1.11e-06 ***

z = plot(
  x=df_cars$Battery_capacity_kWh,
  y=df_cars$mean__Energy_consumption_kWh_100_km, main="Battery_capacity_kWh x Média Consumo",
  xlab="Battery_capacity_kWh", ylab="Média de consumo", pch=1,
) # nao parece ter uma grande correlação, remover
grid(z) #aplicando grid ao gráfico
abline(regression)


# correlação entre a faixa de WLTP e média de consumo - Range__WLTP__km
str(df_cars)

correlation_mean_Range__WLTP__km <- cor(df_cars$mean__Energy_consumption_kWh_100_km, df_cars$Range__WLTP__km)
correlation_mean_Range__WLTP__km # correlação um pouco acima de 0 : 0,1213

regression <- lm(df_cars$mean__Energy_consumption_kWh_100_km ~ df_cars$Range__WLTP__km)
summary(regression) # p-valor: 0.387

z = plot(
  x=df_cars$Range__WLTP__km,
  y=df_cars$mean__Energy_consumption_kWh_100_km, main="Range__WLTP__km x Média Consumo",
  xlab="Range__WLTP__km", ylab="Média de consumo", pch=1,
) # nao parece ter uma correlação, remover
grid(z) #aplicando grid ao gráfico
abline(regression)


# correlação entre distancia entre pneus em cm e a média de consumo - Wheelbase_cm
str(df_cars)

correlation_mean_Wheelbase_cm <- cor(df_cars$mean__Energy_consumption_kWh_100_km, df_cars$Wheelbase_cm)
correlation_mean_Wheelbase_cm # correlação um pouco perto de 0,7 : 0,674

regression <- lm(df_cars$mean__Energy_consumption_kWh_100_km ~ df_cars$Wheelbase_cm)
summary(regression) # p-valor: 3.16e-08 ***

z = plot(
  x=df_cars$Wheelbase_cm,
  y=df_cars$mean__Energy_consumption_kWh_100_km, main="Wheelbase_cm x Média Consumo",
  xlab="Wheelbase_cm", ylab="Média de consumo", pch=1,
) # nao parece ter uma grande correlação, remover
grid(z) #aplicando grid ao gráfico
abline(regression)


# correlação entre comprimento e a média de consumo do carro - Length_cm
str(df_cars)

correlation_mean_Length_cm <- cor(df_cars$mean__Energy_consumption_kWh_100_km, df_cars$Length_cm)
correlation_mean_Length_cm # correlação um pouco perto de 0,65 : 0,66623

regression <- lm(df_cars$mean__Energy_consumption_kWh_100_km ~ df_cars$Length_cm)
summary(regression) # p-valor: 5.18e-08 ***

z = plot(
  x=df_cars$Length_cm,
  y=df_cars$mean__Energy_consumption_kWh_100_km, main="Length_cm x Média Consumo",
  xlab="Length_cm", ylab="Média de consumo", pch=1,
)
grid(z) #aplicando grid ao gráfico
abline(regression)


# correlação entre largura e a média de consumo do carro - Width_cm
str(df_cars)

correlation_mean_Width_cm <- cor(df_cars$mean__Energy_consumption_kWh_100_km, df_cars$Width_cm)
correlation_mean_Width_cm # correlação um pouco perto de ZERO : 0,371

regression <- lm(df_cars$mean__Energy_consumption_kWh_100_km ~ df_cars$Width_cm)
summary(regression) # p-valor: 0.0062 **

z = plot(
  x=df_cars$Width_cm,
  y=df_cars$mean__Energy_consumption_kWh_100_km, main="Width_cm x Média Consumo",
  xlab="Width_cm", ylab="Média de consumo", pch=1,
)
grid(z) #aplicando grid ao gráfico
abline(regression)



# correlação entre altura e a média de consumo do carro - Height_cm
str(df_cars)

correlation_mean_Height_cm <- cor(df_cars$mean__Energy_consumption_kWh_100_km, df_cars$Height_cm)
correlation_mean_Height_cm # correlação um pouco perto de ZERO : 0,3289925

regression <- lm(df_cars$mean__Energy_consumption_kWh_100_km ~ df_cars$Height_cm)
summary(regression) # p-valor: 0.0162 *

z = plot(
  x=df_cars$Height_cm,
  y=df_cars$mean__Energy_consumption_kWh_100_km, main="Height_cm x Média Consumo",
  xlab="Height_cm", ylab="Média de consumo", pch=1,
)
grid(z) #aplicando grid ao gráfico
abline(regression)


# Correlação entre peso mínimo vazio e média de consumo - Minimal_empty_weight_kg:
str(df_cars)

correlation_mean_Minimal_empty_weight_kg <- cor(df_cars$mean__Energy_consumption_kWh_100_km, df_cars$Minimal_empty_weight_kg)
correlation_mean_Minimal_empty_weight_kg # correlação acima de 0,7 : 0,75778

regression <- lm(df_cars$mean__Energy_consumption_kWh_100_km ~ df_cars$Minimal_empty_weight_kg)
summary(regression) # p-valor: 5.06e-11 ***

z = plot(
  x=df_cars$Minimal_empty_weight_kg,
  y=df_cars$mean__Energy_consumption_kWh_100_km, main="Minimal_empty_weight_kg x Média Consumo",
  xlab="Minimal_empty_weight_kg", ylab="Média de consumo", pch=1,
)
grid(z) #aplicando grid ao gráfico
abline(regression)


# correlação entre peso permissível bruto e a média de consumo do carro - Permissable_gross_weight_kg
str(df_cars)

correlation_mean_Permissable_gross_weight_kg <- cor(df_cars$mean__Energy_consumption_kWh_100_km, df_cars$Permissable_gross_weight_kg)
correlation_mean_Permissable_gross_weight_kg # correlação acima de 0,8 : 0,85759

regression <- lm(df_cars$mean__Energy_consumption_kWh_100_km ~ df_cars$Permissable_gross_weight_kg)
summary(regression) # p-valor: 2.42e-16 ***

z = plot(
  x=df_cars$Permissable_gross_weight_kg,
  y=df_cars$mean__Energy_consumption_kWh_100_km, main="Permissable_gross_weight_kg x Média Consumo",
  xlab="Permissable_gross_weight_kg", ylab="Média de consumo", pch=1,
)
grid(z) #aplicando grid ao gráfico
abline(regression)


# correlação entre máxima capacidade de carga e média de consumo do carro - Maximum_load_capacity_kg
str(df_cars)

correlation_mean_Maximum_load_capacity_kg <- cor(df_cars$mean__Energy_consumption_kWh_100_km, df_cars$Maximum_load_capacity_kg)
correlation_mean_Maximum_load_capacity_kg # correlação próxima de 0,7 : 0,7014771

regression <- lm(df_cars$mean__Energy_consumption_kWh_100_km ~ df_cars$Maximum_load_capacity_kg)
summary(regression) # p-valor: 2.42e-16 ***

z = plot(
  x=df_cars$Maximum_load_capacity_kg,
  y=df_cars$mean__Energy_consumption_kWh_100_km, main="Maximum_load_capacity_kg x Média Consumo",
  xlab="Maximum_load_capacity_kg", ylab="Média de consumo", pch=1,
)
grid(z) #aplicando grid ao gráfico
abline(regression)


# correlação entre número de assentos e a média de consumo - Number_of_seats
str(df_cars)

correlation_mean_Number_of_seats <- cor(df_cars$mean__Energy_consumption_kWh_100_km, df_cars$Number_of_seats)
correlation_mean_Number_of_seats # correlação próxima de 0 : 0,2042503

regression <- lm(df_cars$mean__Energy_consumption_kWh_100_km ~ df_cars$Number_of_seats)
summary(regression) # p-valor: 0.142 

z = plot(
  x=df_cars$Number_of_seats,
  y=df_cars$mean__Energy_consumption_kWh_100_km, main="Number_of_seats x Média Consumo",
  xlab="Number_of_seats", ylab="Média de consumo", pch=1,
)
grid(z) #aplicando grid ao gráfico
abline(regression)


# correlação entre o número de portas e a média de consumo do carro - Number_of_doors
str(df_cars)

correlation_mean_Number_of_doors <- cor(df_cars$mean__Energy_consumption_kWh_100_km, df_cars$Number_of_doors)
correlation_mean_Number_of_doors # correlação próxima de 0 : 0,2042503

regression <- lm(df_cars$mean__Energy_consumption_kWh_100_km ~ df_cars$Number_of_doors)
summary(regression) # p-valor: 0.142 

z = plot(
  x=df_cars$Number_of_doors,
  y=df_cars$mean__Energy_consumption_kWh_100_km, main="Number_of_doors x Média Consumo",
  xlab="Number_of_doors", ylab="Média de consumo", pch=1,
)
grid(z) #aplicando grid ao gráfico
abline(regression)


# correlação tamanho do pneu e a média de consumo do carro - Tire_size_in
str(df_cars)

correlation_mean_Tire_size_in <- cor(df_cars$mean__Energy_consumption_kWh_100_km, df_cars$Tire_size_in)
correlation_mean_Tire_size_in # correlação próxima de 0,4 : 0,394135

regression <- lm(df_cars$mean__Energy_consumption_kWh_100_km ~ df_cars$Tire_size_in)
summary(regression) # p-valor: 0.0035 **

z = plot(
  x=df_cars$Tire_size_in,
  y=df_cars$mean__Energy_consumption_kWh_100_km, main="Tire_size_in x Média Consumo",
  xlab="Tire_size_in", ylab="Média de consumo", pch=1,
)
grid(z) #aplicando grid ao gráfico
abline(regression)


# correlação entre máximo de velocidade em km por hora e a média de consumo do carro - Maximum_speed_kph
str(df_cars)

correlation_mean_Maximum_speed_kph <- cor(df_cars$mean__Energy_consumption_kWh_100_km, df_cars$Maximum_speed_kph)
correlation_mean_Maximum_speed_kph # correlação próxima de 0,4 : 0,4266974

regression <- lm(df_cars$mean__Energy_consumption_kWh_100_km ~ df_cars$Maximum_speed_kph)
summary(regression) # p-valor: 0.00144 **

z = plot(
  x=df_cars$Maximum_speed_kph,
  y=df_cars$mean__Energy_consumption_kWh_100_km, main="Maximum_speed_kph x Média Consumo",
  xlab="Maximum_speed_kph", ylab="Média de consumo", pch=1,
)
grid(z) #aplicando grid ao gráfico
abline(regression)


# correlação entre capacidade de inicialização e média de consumo deo carro - Boot_capacity__VDA__l
str(df_cars)

correlation_mean_Boot_capacity__VDA__l <- cor(df_cars$mean__Energy_consumption_kWh_100_km, df_cars$Boot_capacity__VDA__l)
correlation_mean_Boot_capacity__VDA__l # correlação próxima de 0,6 : 0,5867678

regression <- lm(df_cars$mean__Energy_consumption_kWh_100_km ~ df_cars$Boot_capacity__VDA__l)
summary(regression) # p-valor: 3.88e-06 ***

z = plot(
  x=df_cars$Boot_capacity__VDA__l,
  y=df_cars$mean__Energy_consumption_kWh_100_km, main="Boot_capacity__VDA__l x Média Consumo",
  xlab="Boot_capacity__VDA__l", ylab="Média de consumo", pch=1,
)
grid(z) #aplicando grid ao gráfico
abline(regression)


# correlação entre aceleração a cada 100 km por h e média de consumo do carro - Acceleration_0100_kph_s
str(df_cars)

correlation_mean_Acceleration_0100_kph_s <- cor(df_cars$mean__Energy_consumption_kWh_100_km, df_cars$Acceleration_0100_kph_s)
correlation_mean_Acceleration_0100_kph_s # CORRLAÇÃO NEGATIVA próxima de -0,5 : -0.4642759

regression <- lm(df_cars$mean__Energy_consumption_kWh_100_km ~ df_cars$Acceleration_0100_kph_s)
summary(regression) # p-valor: 0.000462 ***

z = plot(
  x=df_cars$Acceleration_0100_kph_s,
  y=df_cars$mean__Energy_consumption_kWh_100_km, main="Acceleration_0100_kph_s x Média Consumo",
  xlab="Acceleration_0100_kph_s", ylab="Média de consumo", pch=1,
)
grid(z) #aplicando grid ao gráfico
abline(regression)


# correlação entre máximo DC de Carregemtno e média de consumo do carro - Maximum_DC_charging_power_kW
str(df_cars)

correlation_mean_Maximum_DC_charging_power_kW <- cor(df_cars$mean__Energy_consumption_kWh_100_km, df_cars$Maximum_DC_charging_power_kW)
correlation_mean_Maximum_DC_charging_power_kW # correlaçãoA próxima de 0,6 : 0.5781921

regression <- lm(df_cars$mean__Energy_consumption_kWh_100_km ~ df_cars$Maximum_DC_charging_power_kW)
summary(regression) # p-valor: 5.79e-06 ***

z = plot(
  x=df_cars$Maximum_DC_charging_power_kW,
  y=df_cars$mean__Energy_consumption_kWh_100_km, main="Maximum_DC_charging_power_kW x Média Consumo",
  xlab="Maximum_DC_charging_power_kW", ylab="Média de consumo", pch=1,
)
grid(z) #aplicando grid ao gráfico
abline(regression)




# matriz de correlação entre as colunas de dados numéricos que irão ser usadas no algoritmo de previsão. Usei esta para ver se os campos se relacionam bem não apenas com o target(média de consumo):
str(df_cars)
numeric_columns <- sapply(df_cars, is.numeric)
head(numeric_columns)
View(numeric_columns)


corr_simple <- function(data=df,sig=0.5){
  #convert data to numeric in order to run correlations
  #convert to factor first to keep the integrity of the data - each value will become a number rather than turn into NA
  df_cor <- data %>% mutate_if(is.character, as.factor)
  df_cor <- df_cor %>% mutate_if(is.factor, as.numeric)
  #run a correlation and drop the insignificant ones
  corr <- cor(df_cor)
  #prepare to drop duplicates and correlations of 1     
  corr[lower.tri(corr,diag=TRUE)] <- NA 
  #drop perfect correlations
  corr[corr == 1] <- NA 
  #turn into a 3-column table
  corr <- as.data.frame(as.table(corr))
  #remove the NA values from above 
  corr <- na.omit(corr) 
  #select significant values  
  corr <- subset(corr, abs(Freq) > sig) 
  #sort by highest correlation
  corr <- corr[order(-abs(corr$Freq)),] 
  #print table
  print(corr)
  #turn corr back into matrix in order to plot with corrplot
  mtx_corr <- reshape2::acast(corr, Var1~Var2, value.var="Freq")
  
  #plot correlations visually
  corrplot(mtx_corr, is.corr=FALSE, tl.col="black", na.label=" ")
}

corr_simple(df_cars[,numeric_columns])




## ALGORITMO DE PREVISÃO

# motivo escolha das colunas para o algoritmo de pervisão:

# campos: Minimal_price__gross__PLN + Engine_power_KM + Maximum_torque_Nm + Drive_type + Battery_capacity_kWh + 
#         Wheelbase_cm + Length_cm + Width_cm + Minimal_empty_weight_kg + Permissable_gross_weight_kg + Maximum_load_capacity_kg +
#         Tire_size_in + Maximum_speed_kph + Boot_capacity__VDA__l + Acceleration_0100_kph_s + Maximum_DC_charging_power_kW
#(campos escolhidos com base na correlação e p-valor. P-valor com 2 ou 3 estrelas e correlação acima de 0,4 bem próximos de 5)
 
# Após anáilise da matriz de correlação entre os campos, remover: Drive_type , Acceleration_0100_kph_s , porque eles se relacionam neutro ou muito mau com a maioria dos outros campos,
# apesar de terem uma leve boa relação com a variável target(média de consumo)

# Campos finais, que tem boa relação com o target e neutro ou boa relação com os outros campos:
# Minimal_price__gross__PLN + Engine_power_KM + Maximum_torque_Nm + Battery_capacity_kWh + 
#         Wheelbase_cm + Length_cm + Width_cm + Minimal_empty_weight_kg + Permissable_gross_weight_kg + Maximum_load_capacity_kg +
#         Tire_size_in + Maximum_speed_kph + Boot_capacity__VDA__l + Maximum_DC_charging_power_kW
#

# divisão do dataset para treino e teste:

# make reproducible
set.seed(1)

# create ID column
df_cars$id <- 1:nrow(df_cars)
View(df_cars)


# 80% para treino:
train_df_cars <- df_cars %>% dplyr::sample_frac(0.80)
View(train_df_cars)

# 20% para teste:
test_df_cars  <- dplyr::anti_join(df_cars, train_df_cars, by = 'id')
View(test_df_cars)

model_learn_cars <- lm(mean__Energy_consumption_kWh_100_km ~ Minimal_price__gross__PLN + Engine_power_KM + Maximum_torque_Nm + Battery_capacity_kWh + 
                         Wheelbase_cm + Length_cm + Width_cm + Minimal_empty_weight_kg + Permissable_gross_weight_kg + Maximum_load_capacity_kg +
                         Tire_size_in + Maximum_speed_kph + Boot_capacity__VDA__l + Maximum_DC_charging_power_kW, data = train_df_cars)
summary(model_learn_cars)

# interpretação/explicação do modelo:



?lm


# Checar as suposições no modelo:

# Obter os resíduos:
residuals <- resid(model_learn_cars)
residuals

# Gráfico de Resíduos vs Valores Ajustados
ggplot(train_df_cars, aes(x = predict(model_learn_cars), y = residuals)) +
  geom_point() +
  geom_smooth(se = FALSE, method = 'loess') +
  ggtitle("Resíduos x Valores Ajustados") +
  xlab("Valores Ajustados") +
  ylab("Resíduos") # realmente mostrou os resíduos bem espalhados. Isso é um bom sinal para o modelo.

# rlang::last_trace() . Executei para ver o erro que fazia o gráfico não aparecer. Pois eu estava usando o df_cars em vez de o train_df_cars que gerou o modelo


# Histograma dos resíduos:
ggplot(train_df_cars, aes(x = residuals)) +
  geom_histogram(binwidth = 1, fill = 'blue', alpha = 0.7) +
  ggtitle("Histograma dos Resíduos") +
  xlab("Resíduos") # O gráfico está um pouco semelhante com uma distribuição normal, mas acredito que pode-se melhorar/diminuir a taxa de erros.

# QQ-Plot
ggplot(train_df_cars, aes(sample = residuals)) +
  geom_qq() +
  geom_qq_line() +
  ggtitle("QQ-Plot dos Resíduos") +
  xlab("Quantis Teóricos") +
  ylab("Quantis Amostrais") # pontos muito próximos da linha diagonal, mas tem uma certa quantidade um pouco longe, o que mostra que podemos diminuir a taxa de erros.





# Salvar modelo localmente:
save(model_learn_cars, file = "model_mean_consume_eletric_cars.RDATA")





# carregar modelo e testar com dataset de teste com 20% dos dados:

model_loaded = load("model_mean_consume_eletric_cars.RDATA")
summary(model_loaded)
View(test_df_cars)

predictions = predict(model_learn_cars, newdata = test_df_cars)

# Exibir previsões
cat("Esperamos que os carros, nessa ordem, consuma em média:", as.integer(predictions))

# comparação do previsto com os targests no data frame de teste:
# dados previstos:             25        23        15         18          22         18         23         18          15         28         18
# dados no frame de teste:     23.85     15.60     17.50      15.40       21.20      15.90      24.10      16.50       14.00      28.20      25.90


# podemos ver que os valores previstos, em sua maioria, estão sempre muito próximos dos valores de testes a serem previstos(alvos)







# PS:
# talvez seja interessante gerar o modelo sem os valores NAs em vez de colocar a média; e/ou
# talvez seja interessante também remover a coluna width_cm.
# com mais esses dois pontos, ou somente, pode ser que o modelo diminua os erros e preveja melhor os resultados.




