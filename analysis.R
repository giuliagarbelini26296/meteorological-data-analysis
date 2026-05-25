

library(tidyverse)

dados <- read_delim(
  "data/INMET_SE_SP_A771_SAO PAULO - INTERLAGOS_01-01-2025_A_31-12-2025.CSV",
  delim = ";",
  skip = 8,
  locale = locale(encoding = "Latin1")
)

colnames(dados)

dados_limpos <- dados %>%
  rename(
    precipitacao = `PRECIPITAÇÃO TOTAL, HORÁRIO (mm)`,
    temperatura = `TEMPERATURA DO AR - BULBO SECO, HORARIA (°C)`,
    umidade = `UMIDADE RELATIVA DO AR, HORARIA (%)`,
    vento = `VENTO, VELOCIDADE HORARIA (m/s)`
  ) %>%
  mutate(
    precipitacao = as.numeric(str_replace(precipitacao, ",", ".")),
    temperatura = as.numeric(str_replace(temperatura, ",", ".")),
    umidade = as.numeric(str_replace(umidade, ",", ".")),
    vento = as.numeric(str_replace(vento, ",", ".")),
    Data = as.Date(Data, format = "%Y/%m/%d")
  )

glimpse(dados_limpos)

dados_limpos <- dados %>%
  rename(
    precipitacao = `PRECIPITAÇÃO TOTAL, HORÁRIO (mm)`,
    temperatura = `TEMPERATURA DO AR - BULBO SECO, HORARIA (°C)`,
    umidade = `UMIDADE RELATIVA DO AR, HORARIA (%)`,
    vento = `VENTO, VELOCIDADE HORARIA (m/s)`
  ) %>%
  mutate(
    precipitacao = as.numeric(str_replace(precipitacao, ",", ".")),
    
    temperatura = as.numeric(str_replace(temperatura, ",", ".")) / 10,
    
    umidade = as.numeric(str_replace(umidade, ",", ".")),
    
    vento = as.numeric(str_replace(vento, ",", ".")),
    
    Data = as.Date(Data, format = "%Y-%m-%d")
  )

summary(dados_limpos$temperatura)

ggplot(dados_limpos, aes(x = Data, y = temperatura)) +
  geom_line() +
  labs(
    title = "Temperatura do Ar em São Paulo - Interlagos (2025)",
    subtitle = "Dados do INMET",
    x = "Data",
    y = "Temperatura (°C)"
  ) +
  theme_minimal()

temperatura_diaria <- dados_limpos %>%
  group_by(Data) %>%
  summarise(
    temperatura_media = mean(temperatura, na.rm = TRUE)
  )


ggplot(temperatura_diaria, aes(x = Data, y = temperatura_media)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Temperatura Média Diária em São Paulo - Interlagos",
    subtitle = "Dados meteorológicos do INMET (2025)",
    x = "Data",
    y = "Temperatura Média (°C)"
  ) +
  theme_minimal()

precipitacao_diaria <- dados_limpos %>%
  group_by(Data) %>%
  summarise(
    precipitacao_total = sum(precipitacao, na.rm = TRUE)
  )


ggplot(precipitacao_diaria, aes(x = Data, y = precipitacao_total)) +
  geom_col() +
  labs(
    title = "Precipitação Diária em São Paulo - Interlagos",
    subtitle = "Dados meteorológicos do INMET (2025)",
    x = "Data",
    y = "Precipitação (mm)"
  ) +
  theme_minimal()

ggsave(
  "images/temperatura_media_diaria.png",
  width = 10,
  height = 6
)

library(lubridate)

dados_limpos <- dados_limpos %>%
  mutate(
    mes = month(Data, label = TRUE, abbr = FALSE)
  )

ggplot(dados_limpos, aes(x = mes, y = temperatura)) +
  geom_boxplot() +
  labs(
    title = "Distribuição Mensal da Temperatura",
    subtitle = "São Paulo - Interlagos (2025)",
    x = "Mês",
    y = "Temperatura (°C)"
  ) +
  theme_minimal()

ggplot(dados_limpos, aes(x = mes, y = temperatura)) +
  geom_boxplot() +
  labs(
    title = "Distribuição Mensal da Temperatura",
    subtitle = "Dados meteorológicos do INMET - 2025",
    x = "Mês",
    y = "Temperatura (°C)"
  ) +
  theme_minimal(base_size = 14)