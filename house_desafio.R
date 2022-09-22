
library(tidyverse)
library(tidymodels)
library(ggcorrplot)
library(generics)
library(parsnip)
options(scipen=999) 

#
dados<- read.csv("house_data.csv",h=T) 

glimpse(dados)

summary(dados)

attach(dados)

#verificando as correlações
dados |> 
  select_if(is.numeric) |> 
  cor() 

#corr fraca com price: id, condition, yr_built, zipcode, long, 
#sqft_lot15


#retirando colunas id, date, grade, sqft_above,sqft_basement, 
#yr_renovated, zipcode, lat, long, sqft_living15, sqft_lot15


df<-dados[,c(-1,-2,-12,-13,-14,-16,-17,-18,-19,-20,-21)]

head(df)
attach(df)

## Analise exploratoria inicial 

summary(df) 

#correlação
df |>
  cor() 

#dev.off()
# Plot
ggcorrplot(corr,
           type = "lower",
           lab = TRUE,
           colors = c("blue", "white", "red"),
           title="Correlação", 
           ggtheme=theme_bw)

#ajuste do modelo

fit0 = lm(price~bedrooms + bathrooms+sqft_living + sqft_lot + floors+
            waterfront+ view + condition + yr_built, data = df)
summary(fit0)


fit = lm(price~bedrooms + bathrooms+ sqft_living + sqft_lot + floors+
           waterfront+ view + condition , data = df)
summary(fit)

step(fit0)


#treino e teste
set.seed(1909)

df_split <- rsample::initial_split(df, prop = 0.80)
#17290/4323/21613

# usa 80% dos dados para treinamento
df_train <- rsample::training(df_split)

# usa 20% dos dados para validacao
df_test  <-  rsample::testing(df_split)

# valores preditos

pv <-predict(fit,df_test) # na amostra de validacao

a<-df_test %>% 
  select(price) %>% 
  bind_cols(predict(fit,df_test))

#metricas para reg linear 

#graficos de desempenho: predicao vs valores observados, r2












