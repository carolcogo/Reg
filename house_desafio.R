
library(tidyverse)
library(tidymodels)
library(ggcorrplot)
library(generics)
library(parsnip)
library(fastrep)
#options(scipen=999) 

#importando banco de dados
dados<- read.csv("house_data.csv",h=T) 

glimpse(dados)

#resumo das variaveis
dados |> 
summary()

attach(dados)

#verificando as correlações
dados |> 
  select_if(is.numeric) |> 
  cor() 

#corr fraca com price: id, condition, yr_built, zipcode, long, 
#sqft_lot15


#retirando algumas colunas
df <- dados |> 
  select(-id, -date, -yr_renovated, -floors,
         - sqft_basement, -zipcode) |> 
  as_tibble() 

head(df)
attach(df)

## Analise inicial 
#correlação

# Plot
corr <- round(cor(df), 1)
ggcorrplot(corr,
           type = "lower",
           lab = TRUE,
           colors = c("blue", "white", "red"),
           title="Correlação", 
           ggtheme=theme_bw)

#ajuste do modelo

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

####
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
final_lm_res <- last_fit(df_work, df_split) 

# Coleta as metricas no teste

collect_metrics(final_lm_res) |> 
  fastrep::tbl("Resumo nos dados de teste")


lm_fit <- fit(df_work, df_train) # ajusta o modelo nos dados de treino


point_plot = function(df, var){
  df |> 
    ggplot(aes(price, {{ var }})) +
    geom_point()
}

# ver o comportamento das variaveis
point_plot(df, lat) 

point_plot(df,long)


#graficos de desempenho: predicao vs valores observados
df_test |> 
  select(price) |> 
  bind_cols(predict(lm_fit, df_test)) |> 
  ggplot(aes(price, .pred)) +
  geom_point() 





