library(daltoolbox)
library(dplyr)
library(ggplot2)

# -----------------------------
# 🎯 Escolher a área
# -----------------------------
area_exemplo <- "ciências sociais, jornalismo e informação"

dados_area <- dados_filtrados |>
  filter(tolower(field) == tolower(area_exemplo)) |>
  arrange(year)

# -----------------------------
# 🔗 Criar a série temporal
# -----------------------------
serie <- ts(dados_area$value, start = min(dados_area$year), frequency = 1)

# -----------------------------
# 🔥 Definir o período de treino e teste
# -----------------------------
n_total <- length(serie)
n_treino <- n_total - 3

serie_treino <- serie[1:n_treino]
serie_teste  <- serie[(n_treino + 1):n_total]

# -----------------------------
# 🚀 Modelos
# -----------------------------

## ARIMA
model_arima <- prep.series_arima()
model_arima <- fit(model_arima, serie_treino)
pred_arima <- predict(model_arima, n.ahead = 3)

## MLP
model_mlp <- prep.series_mlp(hidden_neurons = 5)
model_mlp <- fit(model_mlp, serie_treino)
pred_mlp <- predict(model_mlp, n.ahead = 3)

## SVR
model_svr <- prep.series_svr()
model_svr <- fit(model_svr, serie_treino)
pred_svr <- predict(model_svr, n.ahead = 3)

# -----------------------------
# 📉 Avaliar erros
# -----------------------------
## Dados reais do teste
real <- as.numeric(serie_teste)

## Calcular RMSE
rmse <- function(real, pred) sqrt(mean((real - pred)^2))

erros <- data.frame(
  Modelo = c("ARIMA", "MLP", "SVR"),
  RMSE = c(
    rmse(real, pred_arima),
    rmse(real, pred_mlp),
    rmse(real, pred_svr)
  )
)

print(erros)

# -----------------------------
# 📊 Plotar resultados
# -----------------------------
anos <- c(time(serie))

df_plot <- data.frame(
  ano = anos,
  real = c(serie_treino, real),
  arima = c(serie_treino, pred_arima),
  mlp = c(serie_treino, pred_mlp),
  svr = c(serie_treino, pred_svr)
)

ggplot(df_plot, aes(x = ano)) +
  geom_line(aes(y = real, color = "Real"), size = 1.2) +
  geom_line(aes(y = arima, color = "ARIMA"), linetype = "dashed", size = 1.1) +
  geom_line(aes(y = mlp, color = "MLP"), linetype = "dotted", size = 1.1) +
  geom_line(aes(y = svr, color = "SVR"), linetype = "dotdash", size = 1.1) +
  scale_color_manual(values = c("Real" = "blue", "ARIMA" = "red", "MLP" = "green", "SVR" = "orange")) +
  labs(
    title = paste("Previsão da Taxa de Conclusão -", area_exemplo),
    x = "Ano", y = "Taxa de Conclusão",
    color = "Série"
  ) +
  theme_minimal() +
  theme(text = element_text(size = 14))
