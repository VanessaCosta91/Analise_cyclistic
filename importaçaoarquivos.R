install.packages("tidyverse")
install.packages("data.table")
install.packages("lubridate")
install.packages("janitor")
install.packages("skimr")
install.packages("psych")
install.packages("plotly")
install.packages("scales")
install.packages("ggplot2")
install.packages("rmarkdown")
install.packages("knitr")

library(tidyverse)
library(data.table)
library(lubridate)
library(janitor)
library(skimr)
library(psych)
library(dplyr)
library(plotly)
library(ggplot2)
library(scales)
library(rmarkdown)
library(knitr)

#importação dos arquivos com dados das viagens dos ultimos dozes meses, salvos do diretorio local
trip_122023 <- read_csv("C:\\Users\\vanes\\OneDrive\\Área de Trabalho\\dados_v2\\202312-divvy-tripdata.csv")
trip_012024 <- read_csv("C:\\Users\\vanes\\OneDrive\\Área de Trabalho\\dados_v2\\202401-divvy-tripdata.csv")
trip_022024 <- read_csv("C:\\Users\\vanes\\OneDrive\\Área de Trabalho\\dados_v2\\202402-divvy-tripdata.csv")
trip_032024 <- read_csv("C:\\Users\\vanes\\OneDrive\\Área de Trabalho\\dados_v2\\202403-divvy-tripdata.csv")
trip_042024 <- read_csv("C:\\Users\\vanes\\OneDrive\\Área de Trabalho\\dados_v2\\202404-divvy-tripdata.csv")
trip_052024 <- read_csv("C:\\Users\\vanes\\OneDrive\\Área de Trabalho\\dados_v2\\202405-divvy-tripdata.csv")
trip_062024 <- read_csv("C:\\Users\\vanes\\OneDrive\\Área de Trabalho\\dados_v2\\202406-divvy-tripdata.csv")
trip_072024 <- read_csv("C:\\Users\\vanes\\OneDrive\\Área de Trabalho\\dados_v2\\202407-divvy-tripdata.csv")
trip_082024 <- read_csv("C:\\Users\\vanes\\OneDrive\\Área de Trabalho\\dados_v2\\202408-divvy-tripdata.csv")
trip_092024 <- read_csv("C:\\Users\\vanes\\OneDrive\\Área de Trabalho\\dados_v2\\202409-divvy-tripdata.csv")
trip_102024 <- read_csv("C:\\Users\\vanes\\OneDrive\\Área de Trabalho\\dados_v2\\202410-divvy-tripdata.csv")
trip_112024 <- read_csv("C:\\Users\\vanes\\OneDrive\\Área de Trabalho\\dados_v2\\202411-divvy-tripdata.csv")

#junção dos arquivos mensais em um unico arquivo
trip_ano <- bind_rows(trip_122023,trip_012024,trip_022024,trip_032024,trip_042024,trip_052024,trip_062024,trip_072024,trip_082024,trip_092024,trip_102024,trip_112024)

head(trip_ano)

#transformar data em formatato datetime e calcular tempo de viagem
trip_ano <- trip_ano %>%
  mutate(
    started_at = ymd_hms (started_at),
    ended_at = ymd_hms(ended_at),
    tempo_trip = as.numeric(difftime(ended_at,started_at,units="mins"))
  )

str(trip_ano)

#contar quantidade de viagens por modelo de bicicleta e tipo de membro
tipos_bicicletas <- trip_ano %>%
  group_by(member_casual, rideable_type) %>%
  summarise(total_viagens_modelo = n())

print(tipos_bicicletas)

#extrair ia da semana da data
trip_ano <- trip_ano %>%
  mutate(
    dia_semana_num = wday(started_at),  # Número do dia da semana
    dia_semana = case_when(
      dia_semana_num == 1 ~ "Domingo",
      dia_semana_num == 2 ~ "Segunda-feira",
      dia_semana_num == 3 ~ "Terça-feira",
      dia_semana_num == 4 ~ "Quarta-feira",
      dia_semana_num == 5 ~ "Quinta-feira",
      dia_semana_num == 6 ~ "Sexta-feira",
      dia_semana_num == 7 ~ "Sábado"
    )
  )

trip_ano_com_na_start <- trip_ano %>%
  filter(is.na(started_at))

#contar total de viagens por dia da semana e tipo de usuario
viagens_dia_semana <- trip_ano %>%
  filter(!is.na(dia_semana) & !is.na(member_casual)) %>%
  group_by(member_casual,dia_semana) %>%
  summarise(Total_viagens_por_semana = n())

print(viagens_dia_semana)

#culcular media de tempo por tipo de usuario
tempo_medio_usuario <- trip_ano %>%
  group_by(member_casual) %>%
  summarise(tempo_medio = mean(tempo_trip, na.rm = TRUE))

print(tempo_medio_usuario)

#contar quantidade total de viagens
quant_viagens <- trip_ano %>%
  group_by(member_casual) %>%
  summarise(total_viagens = n())

#grafico de barras representando quantidade de viagens por modelo  e usuario
grafico_modelo <- ggplot(tipos_bicicletas, aes(x = rideable_type, member_casual, y = total_viagens_modelo, fill = member_casual)) +
            geom_bar(stat = "identity", position = "dodge") +
            labs(title = "Total de viagens por tipo de usuário e modelo", x = "Modelos de bicicletas", y = "Número de viagens", fill = "Tipo de usuário") +
            theme_light()

ggsave("grafico_viagens_modelo.png", plot=grafico_modelo, height = 5, dpi=300)

#ordenar semana
viagens_dia_semana$dia_semana <- factor(viagens_dia_semana$dia_semana, 
                                        levels = c("Segunda-feira", "Terça-feira", "Quarta-feira", "Quinta-feira", "Sexta-feira", "Sábado", "Domingo"))

#grafico de linhas representando quantidade de viagens por dia da semana
grafico_dia_semana <- ggplot(viagens_dia_semana, aes(x = dia_semana, y = Total_viagens_por_semana, group=member_casual, color = member_casual)) +
                      geom_line() +
                      labs(title = "Total de viagens por tipo de usuário e dia da semana", x = "Dia da Semana", y = "Número de viagens", color = "Tipo de usuário") +
                      scale_y_continuous(labels = scales::label_number(accury =  1))+
                      theme_light()+
                      theme(axis.text.x = element_text(angle=90, hjust=1),
                            legend.position = "top")


ggsave("grafico_viagens_dia_semana.png", plot=grafico_dia_semana, height = 5, dpi=300)

#grafico de barras que representa media tempo de viagem por tipo de usuario
grafico_media_tempo_viagem <- ggplot(tempo_medio_usuario, aes(x=member_casual, y = tempo_medio, fill=member_casual))+
                              geom_bar(stat="identity") +
                              labs(title = "Tempo média da viagem por tipo de usuário", x = "Tipo de usuário", y = "Tempo Médio", fill = "Tipo de usuário")+
                              theme_light()+
                              theme(legend.position = "top")

ggsave("grafico_media_tempo_viagem.png", plot=grafico_media_tempo_viagem , height = 5, dpi=300)

#gráfico pizza com representa quantidade total de viagens
grafico_viagens_por_usuario <- ggplot(quant_viagens, aes(x="", y= total_viagens, fill= member_casual))+
                               geom_bar(stat="identity", width=1) +
                               coord_polar(theta = "y")+
                               geom_text(aes(label=total_viagens),
                                         position = position_stack(vjust=0.5))+
                                 labs(title= "Quantidade de viagens por tipo de usuário", fill = "Tipo de usuário")+
                                theme_light() +
                                theme(
                                  panel.background = element_rect(fill = "white"),
                                  panel.grid = element_blank(),  
                                  axis.text = element_blank(), 
                                  axis.title = element_blank() 
                                )

ggsave("grafico_quantidade_viagem.png", plot=grafico_viagens_por_usuario, height = 5, dpi=300)