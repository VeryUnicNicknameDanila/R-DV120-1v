library(factoextra)
library(readr)
library(dplyr)
library(tidyverse)
#считали таблицу
table = read_csv("https://www.dropbox.com/s/erhs9hoj4vhrz0b/eddypro.csv?dl=1",
                 skip=1, comment = "[")
#убрали самую первую строку значений, тк она пустая
table <- table[-1,]
#приравняли значения -9999 к NA
table[table == -9999] = NA
#оставили строки ночные
table <- table %>% filter(daytime == "T")
#оставили строки 2013 года
table <- table %>% filter(year(date) == 2013)
#оставили строки осени
table <- table %>% filter(month(date) %in% c(03, 04, 05))
#оставили строки только числовые
table = table %>% select(where(is.numeric))
#оставили столбцы без тех что относятся к co2
table =-table %>% select(-contains("co2"), co2_flux)
#создали таблицу var_data со значениями дисперсии каждого показателя
var_data = table %>% summarise_all( ~var(.x,na.rm=T))
#там где результат получился NA, заменяем на 0
var_data[is.na(var_data)] = 0
#создаем таблицу cpa_data используя var_data чтобы исключить показатели с нулевой дисперсией
cpa_data = table[,as.logical(var_data != 0)]
#создали корреляционную таблицу на основе cpa_data исключив столбцы содержащие NA
cor_matrix = cor(na.exclude(cpa_data))
#создали тепловую карту корреляционной матрицы
heatmap(cor_matrix)
#сделали выборку значений из матрицы для CO2
co2_cor = as.numeric(cor_matrix[83,])
#проименовали эту выборку
names(co2_cor) = names(cor_matrix[83,])
#выбрали только те значения, что входят в диапазон коэффициента влияния
cpa_dataf = cpa_data[,co2_cor > 0.1 | co2_cor < -.1]
#Чтобы не возникало ошибок со специальными символами, переименуем некоторые переменные
table = table %>% rename(z_sub_d__L = `(z-d)/L`)
table = table %>% rename_with(
  ~gsub("-","_sub_",.x, fixed = T))
table = table %>% rename_with(
  ~gsub("*","_star",.x, fixed = T))
table = table %>% rename_with(
  ~gsub("%","_p",.x, fixed = T))
table = table %>% rename_with(
  ~gsub("/","__",.x, fixed = T))

cpa_dataf = cpa_dataf %>% rename(z_sub_d__L = `(z-d)/L`)
cpa_dataf = cpa_dataf %>% rename_with(
  ~gsub("-","_sub_",.x, fixed = T))
cpa_dataf = cpa_dataf %>% rename_with(
  ~gsub("*","_star",.x, fixed = T))
cpa_dataf = cpa_dataf %>% rename_with(
  ~gsub("%","_p",.x, fixed = T))
cpa_dataf = cpa_dataf %>% rename_with(
  ~gsub("/","__",.x, fixed = T))
#произвели анализ основных компонентов
data_pca = prcomp(na.exclude(cpa_dataf),scale=TRUE)
#отобразили розу ветров для влияния различных показателей
fviz_pca_var(data_pca,repel = TRUE, col.var = "steelblue")
#создали модель линейной регрессии исходя из показателей, максимально приближенных к co2_flux по влиянию
model = lm(co2_flux ~ air_heat_capacity+air_pressure+u_unrot+Tdew+air_heat_capacity+h2o_var, table)
#вывели сводную по получившейся модели
summary(model)

formula = paste(c("co2_flux ~ ", paste(names(cpa_dataf)[-38], collapse = "+")), collapse = "")
formula = as.formula(formula)

model_first = lm(formula, cpa_dataf)
summary(model_first)
formula2 = formula(co2_flux ~ wind_dir + u_spikes + h2o...128  + w_spikes + u_rot + air_molar_volume + h2o_var + qc_LE)
model2 = lm(formula2, cpa_dataf)
summary(model2)
formula3 = formula(co2_flux ~ wind_dir + u_spikes + h2o...128  + w_spikes + u_rot + h2o_var + qc_LE)
model3 = lm(formula3, cpa_dataf)
summary(model3)
anova(model3)


