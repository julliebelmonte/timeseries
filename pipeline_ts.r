library(daltoolbox)
library(dplyr)
library(ggplot2)

# ---------------------------
# Filtrar os dados
# ---------------------------
area_exemplo <- c("Engenharia, manufatura e construção", 
                  "Negócios, administração e direito", 
                  "Saúde e bem-estar")

dados_area <- finland_data |>
  filter(tolower(field) %in% tolower(area_exemplo)) |>
  arrange(year)

# ---------------------------
# Série temporal
# ---------------------------
serie <- ts_data(dados_area$value)

# Separar treino e teste
samp <- ts_sample(serie, test_size = 3)

# Projeção para MLP e SVR
io_train <- ts_projection(samp$train)
io_test  <- ts_projection(samp$test)

# ---------------------------
# Modelo ARIMA
# ---------------------------
model_arima <- ts_arima()
model_arima <- fit(model_arima, x = NULL, y = samp$train)

# Ajuste no treino (o próprio treino)
adjust_arima <- samp$train

# Predição
pred_arima <- predict(model_arima, x = NULL, steps_ahead = 3)

# Avaliação
ev_arima <- evaluate(model_arima, samp$test, pred_arima)

# ---------------------------
# Modelo MLP
# ---------------------------
model_mlp <- ts_mlp()
model_mlp <- fit(model_mlp, x = io_train$input, y = io_train$output)

adjust_mlp <- predict(model_mlp, io_train$input)
pred_mlp <- predict(model_mlp, x = io_test$input[1, ], steps_ahead = 3)

ev_mlp <- evaluate(model_mlp, as.vector(io_test$output), as.vector(pred_mlp))

# ---------------------------
# Modelo SVR
# ---------------------------
model_svr <- ts_svr()
model_svr <- fit(model_svr, x = io_train$input, y = io_train$output)

adjust_svr <- predict(model_svr, io_train$input)
pred_svr <- predict(model_svr, x = io_test$input[1, ], steps_ahead = 3)

ev_svr <- evaluate(model_svr, as.vector(io_test$output), as.vector(pred_svr))

# ---------------------------
# Tabela de Erros
# ---------------------------
erros <- data.frame(
  Modelo = c("ARIMA", "MLP", "SVR"),
  MSE = c(ev_arima$mse, ev_mlp$mse, ev_svr$mse)
)

print(erros)

# ---------------------------
# Plotando os resultados
# ---------------------------

yvalues <- c(io_train$output, io_test$output)

# Plot ARIMA
plot_ts_pred(y = yvalues, 
             yadj = adjust_arima, 
             ypre = pred_arima) + 
  theme(text = element_text(size = 14)) +
  labs(title = "Previsão da Taxa de Conclusão - ARIMA")

# Plot MLP
plot_ts_pred(y = yvalues, 
             yadj = adjust_mlp, 
             ypre = pred_mlp) + 
  theme(text = element_text(size = 14)) +
  labs(title = "Previsão da Taxa de Conclusão - MLP")

# Plot SVR
plot_ts_pred(y = yvalues, 
             yadj = adjust_svr, 
             ypre = pred_svr) + 
  theme(text = element_text(size = 14)) +
  labs(title = "Previsão da Taxa de Conclusão - SVR")

length(samp$train)
length(samp$test)
any(is.na(serie))
