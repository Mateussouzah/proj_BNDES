library(tidyverse)
library(magrittr)
library(stringr)
install.packages("magrittr")

bndes <- read_delim(file = "C:/Users/mateu/Downloads/operacoes_n_automaticas.csv",
                    delim = ";",escape_double = FALSE,
                    locale = (locale(decimal_mark = ",",
                              grouping_mark = ".")))
?locale

bndes <- read_csv2("C:/Users/mateu/Downloads/operacoes_n_automaticas.csv")

bndes %>%
  group_by(cliente) %>%

  view()
paste("R$", format(453453540, decimal.mark = ",", big.mark = ".", nsmall = 2)

)

?select
library(prettydoc)

View(bndes)

bndes %>%
  group_by(cliente) %>%

  arrange(desc(valor_contratado_reais)) %>%
  View()

bndes %>%
  mutate(ano = lubridate::year(data_da_contratacao)) %>%
  group_by(ano) %>%
  summarise(total_ano = sum(valor_contratado_reais)) %>%
  view()

?group_by

bndes %>%
  group_by(cliente) %>%
  summarise(total_cliente = sum(valor_contratado_reais)) %>%
  arrange(desc(total_cliente)) %>%
  slice(1:8) %>%
  mutate(cliente = forcats::fct_reorder(cliente, total_cliente)) %>%
   ggplot() +
  geom_col(aes(x = total_cliente, y = cliente, fill = cliente),
           show.legend = FALSE) +
  geom_label(aes(x = total_cliente/2 , y = cliente, label = total_cliente)) +
  coord_cartesian(ylim = c(0, 10)) +
  ggthemr::ggthemr("fresh")$theme


?ggthemr

options(OutDec=",")

grafico <- bndes %>%
  mutate(ano = lubridate::year(data_da_contratacao)) %>%
  group_by(ano) %>%
  summarise(total_ano = sum(valor_contratado_reais)) %>%
  mutate(total_ano = round(total_ano, 2)) %>%
ggplot(aes(x = ano, y = rouond(total_ano, 2))) +
  geom_line() +
  geom_point() +
  plotly::ggplotly(grafico)

bndes %>%
  group_by(inovacao) %>%
  count() %>%
  ggplot() +
  geom_col(aes(x = inovacao, y = n)) +
  geom_label(aes(x = inovacao , y = n/2, label = n)) +
  ggthemr::ggthemr("fresh")$theme +
  labs( x = "Inovação",
              title = "Grafico de Inovação",
        subtitle = "Classificação de projetos em inovador ou não")


bndes %>%
  group_by(inovacao) %>%
  count() %>%
  pie(x = 19001, 1022)

bndes %>%
  summarise(media_juros = mean(prazo_carencia_meses))


bndes %>%
  group_by(porte_do_cliente) %>%
  count() %>%
  mutate(porte_do_cliente = forcats::fct_reorder(porte_do_cliente, n)) %>%
  ggplot() +
  geom_col(aes(x = porte_do_cliente, y = n)) +
  geom_label(aes(x = porte_do_cliente , y = n/2, label = n)) +
  ggthemr::ggthemr("dust")$theme +
  labs(
        title = "Tamanho do cliente",
        subtitle = "Classificação do tamanho das empresas financiadas")

knitr::include_graphics("https://external-preview.redd.it/uafMLScOUutOCmzRcIu2h4lgTa6o2tiHAjgP_p8aUuI.gif?format=mp4&s=9706b2c3cac21a723dda178b8aa34c519b14d3a4")

