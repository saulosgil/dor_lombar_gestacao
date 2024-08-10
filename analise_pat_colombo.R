# Pacotes
library(readxl)
library(tidyverse)
library(corrplot)

# Carrega os dados
dados_pat_colombo <- read_excel("C:/Users/saulo/OneDrive/Área de Trabalho/dados_pat_colombo.xlsx")

# Filtra as linhas com apenas as observações
df <- dados_pat_colombo[1:53,]

# Ajusta as variáveis
df <-
  df |>
  mutate(
    score_dor = as.numeric(score_dor),
    imc_pre_gestacional = as.numeric(imc_pre_gestacional),
    imc_pos_gestacional = as.numeric(imc_pos_gestacional),
    'estado_conjugal_com _1_sem_2' = as.factor(imc_pos_gestacional),
    renda = as.factor(renda),
    uso_medicamento_sim_1_n_2 = as.factor(uso_medicamento_sim_1_n_2),
    tabaco_sim_1_n_2 = as.factor(tabaco_sim_1_n_2),
    atvfisica_sim_1_n_2 = as.factor(atvfisica_sim_1_n_2)
  ) |>
  as.data.frame()

# Resumo
glimpse(df)
skimr::skim(df)

# prevalencia de dor lombar
freq_lombar <- df |>
  group_by(local_dor)  |>
  summarise(n = n()) |>
  mutate(freq = n / sum(n) * 100) |>
  ungroup() |>
  arrange(freq)

freq_lombar

freq_lombar |>
  ggplot(aes(x = local_dor,
             y = freq,
             fill = local_dor,
             label = round(freq, 1))) +
  geom_col() +
  geom_text(position = position_stack(vjust = 0.5)) +
  theme_classic()

# Prevalencia da classificação de dor
freq_class_dor <- df |>
  group_by(classificacao)  |>
  summarise(n = n()) |>
  mutate(freq = n / sum(n) * 100) |>
  ungroup() |>
  arrange(freq)

freq_class_dor

freq_class_dor |>
  ggplot(aes(x = fct_reorder(classificacao, freq,.desc = TRUE), classificacao,
             y = freq,
             fill = classificacao,
             label = round(freq, 1))) +
  geom_col() +
  geom_text(position = position_stack(vjust = 0.5)) +
  theme_classic()

# grafico de dispersão
df |>
  ggplot(aes(x = ganho_peso_gestacional,
             y = score_dor,
             colour = classificacao)) +
  geom_point(size = 2)+
  theme_classic()

# grafico de dispersão - fit
df |>
  ggplot(aes(x = ganho_peso_gestacional,
             y = score_dor)) +
  geom_point(size = 2)+
  geom_smooth(method = "lm")+
  theme_classic()

# Correlações
df_numeric <- df |>
  select(where(is.numeric))

M <- cor(df_numeric)

corrplot::corrplot(M, type="upper", order="hclust")


# modelos
## modelo não ajustado
model <- lm(score_dor ~ ganho_peso_gestacional, df)
summary(model)

# modelo ajustado
modelo_ajustado <- lm(score_dor ~ ganho_peso_gestacional + idade + imc_pre_gestacional + atvfisica_sim_1_n_2, df)
summary(modelo_ajustado)


