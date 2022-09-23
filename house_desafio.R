
library(tidyverse)
library(tidymodels)
library(ggcorrplot)
library(generics)
library(parsnip)
library(fastrep)
options(scipen=999) 

#
dados<- read.csv("house_data.csv",h=T) 

glimpse(dados)

summary(dados)
#falar das variaveis 

attach(dados)

#verificando as correlações
dados |> 
  select_if(is.numeric) |> 
  cor() 

#corr fraca com price: id, condition, yr_built, zipcode, long, 
#sqft_lot15


#retirando colunas
#df<-dados[,c(-1,-2,-12,-13,-14,-16,-17,-18,-19,-20,-21)]

df <- dados |> 
  select(-id, -date, -yr_renovated, -floors,
         - sqft_basement, -zipcode) |> 
  as_tibble() 

head(df)
attach(df)

## Analise exploratoria inicial 

summary(df) 

#correlação

#dev.off()
# Plot
corr <- round(cor(df), 1)
ggcorrplot(corr,
           type = "lower",
           lab = TRUE,
           colors = c("blue", "white", "red"),
           title="Correlação", 
           ggtheme=theme_bw)

#ajuste do modelo

# fit0 = lm(price~bedrooms + bathrooms+sqft_living + sqft_lot + floors+
#             waterfront+ view + condition + yr_built, data = df)
# summary(fit0)
# 
# 
# fit = lm(price~bedrooms + bathrooms+ sqft_living + sqft_lot + floors+
#            waterfront+ view + condition , data = df)
# summary(fit)
# 
# step(fit0)

#

fit_a= lm(price ~ ., data = df)


fit_a |> 
  summary()


#dividindo o banco de dados
#treino e teste
set.seed(1909)

df_split <- rsample::initial_split(df, prop = 0.80)
#17290/4323/21613

# usa 80% dos dados para treinamento
df_train <- rsample::training(df_split)

# usa 20% dos dados para validacao
df_test  <-  rsample::testing(df_split)

#
df_rec1 = df_train |> 
  recipe(price ~.)  #define o pre processamento

df_rl =
  linear_reg() %>% 
  set_engine("lm") # define o modelo a ser usado

#aa

#treino 
df_work = workflow() |> 
  add_model(df_rl) |> 
  add_recipe(df_rec1) #junta tudo em lugar so

#ajuste na amostra de teste
final_lm_res <- last_fit(df_work, df_split) # ajuste tudo no teste já


collect_metrics(final_lm_res) |> 
  #rename to portuguese
  fastrep::tbl("Resumo nos dados de teste") # coleta as metricas no teste

collect_predictions(final_lm_res) |> 
  #rename to portuguese
  fastrep::tbl("")


#falta o r2


#lm_fit <- fit(df_work, df_train) # ajusta o modelo nos dados de treino

#
ames_train_res <- predict(lm_fit, new_data = df_train %>% select(-price))
ames_train_res

ames_train_res <- bind_cols(ames_train_res, df_train %>% select(price))
ames_train_res

ggplot(ames_train_res, aes(x = price, y = .pred)) + 
  # Create a diagonal line:
  geom_abline(lty = 2) + 
  geom_point(alpha = 0.5) + 
  labs(y = "Predicted  Price ", x = " Price ") +
  # Scale and size the x- and y-axis uniformly:
  coord_obs_pred()

#ali
df_test |> 
  select(price) |> 
  bind_cols(predict(lm_fit, df_test)) |> 
  ggplot(aes(price, .pred)) +
  geom_point() # predito vs real, tem que ser aproximar da retya y = x


#metricas para reg linear 
rmse(ames_train_res, truth = price, estimate = .pred)
metrics <- metric_set(rmse, rsq, mae) #rmse peq, r2 grand, erro abs medio peq

metrics(ames_train_res, truth = price, estimate = .pred)

#fim cap 8





point_plot = function(df, var){
  df |> 
    ggplot(aes(price, {{ var }})) +
    geom_point()
}


point_plot(df, lat) # ver o comportamentod ass variaveis





ggplot(df_test, aes(x = price,
                    y = df_test)) +
  geom_point() +
  geom_smooth(method = "lm")





#graficos de desempenho: predicao vs valores observados, r2





```


