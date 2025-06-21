
library(daltoolbox)
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(readxl)
library(tseries)    
library(forecast)   




dados <- read_excel("finland_data.xlsx")




categorias_remover <- c(
  "total", "desconhecido", "educação básica",
  "qualificação de nível pós-abrangente",
  "programas e qualificações genéricas"
)

dados_filtrados <- dados |>
  filter(!tolower(field) %in% tolower(categorias_remover))




ggplot(dados_filtrados, aes(x = year, y = value, color = field)) +
  geom_line(size = 1.2) +
  geom_point(size = 1.5) +
  labs(title = "Evolução da Taxa de Conclusão por Área",
       x = "Ano", y = "Taxa de Conclusão",
       color = "Área") +
  theme_minimal() +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  guides(color = guide_legend(nrow = 2))




ggplot(dados_filtrados, aes(x = reorder(field, value, median), y = value)) +
  geom_boxplot(fill = "steelblue", color = "black") +
  labs(title = "Distribuição da Taxa de Conclusão por Área",
       x = "Área de Conhecimento", y = "Taxa de Conclusão") +
  theme_minimal() +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1))




ggplot(dados_filtrados, aes(x = year, y = value)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "steelblue", size = 2) +
  facet_wrap(~field, scales = "free_y") +
  labs(title = "Evolução Temporal por Área",
       x = "Ano", y = "Taxa de Conclusão") +
  theme_minimal() +
  theme(text = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1))




dados_filtrados |>
  group_by(field) |>
  summarise(
    media = mean(value, na.rm = TRUE),
    mediana = median(value, na.rm = TRUE),
    desvio_padrao = sd(value, na.rm = TRUE),
    minimo = min(value, na.rm = TRUE),
    maximo = max(value, na.rm = TRUE),
    n = n()
  ) |>
  arrange(desc(media))




data_total <- dados |>
  filter(tolower(field) == "total") |>
  group_by(year) |>
  summarise(value = sum(value, na.rm = TRUE))

ggplot(data_total, aes(x = year, y = value)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "steelblue", size = 2) +
  labs(title = "Evolução do Total por Ano",
       x = "Ano", y = "Valor Total") +
  theme_minimal() +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1))




area_exemplo <- c("Engenharia, manufatura e construção", 
                  "Negócios, administração e direito", 
                  "Saúde e bem-estar")

serie_area <- dados_filtrados |>
  filter(tolower(field) == tolower(area_exemplo)) |>
  arrange(year)


Acf(serie_area$value, main = paste("ACF -", area_exemplo))
Pacf(serie_area$value, main = paste("PACF -", area_exemplo))




adf_result <- adf.test(serie_area$value)
print(adf_result)




diff_serie <- diff(serie_area$value)

plot.ts(diff_serie, main = paste("Série Diferenciada -", area_exemplo),
        ylab = "Diferença da Taxa de Conclusão", xlab = "Tempo")
