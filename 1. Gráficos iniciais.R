library(RCurl)

x <- getURL("https://raw.githubusercontent.com/dssg-pt/covid19pt-data/master/data.csv")
data_covid <- read.csv(text = x)

# A alternativa para abrir ficheiro csv do github seria:
# library(readr)
# urlfile = "https://raw.githubusercontent.com/dssg-pt/covid19pt-data/master/data.csv"
# data_covid <- read.csv(url(urlfile))

w <- getURL("https://raw.githubusercontent.com/dssg-pt/covid19pt-data/master/vacinas_detalhe.csv")
data_vacina <- read.csv(text = w)

library(ggplot2)

# _____________________________________________________________________________

# SARA

# Vamos fazer um gráfico que avalia a % de novos casos por faixa etária 
# Temos então que somar as colunas confirmados_m_idade + confirmados_f_idade para ter o número total de confirmados
# por cada faixa etária
# Criamos uma função que faça a soma da soma dos valores de uma coluna mais a soma dos valores da outra coluna

somatorio_colunas<-function(x,y){(sum(x, na.rm=TRUE)+ sum(y, na.rm=TRUE))}

# Aplicamos essa função às duas colunas que queremos, neste caso, somar os casos entre homens e mulheres 
# numa determinada idade 

confirmados_0_9<-somatorio_colunas(mydata$confirmados_0_9_f, mydata$confirmados_0_9_m)
confirmados_0_9

confirmados_10_19<-somatorio_colunas(mydata$confirmados_10_19_f, mydata$confirmados_10_19_m)
confirmados_10_19

confirmados_20_29<-somatorio_colunas(mydata$confirmados_20_29_f, mydata$confirmados_20_29_m)
confirmados_20_29

confirmados_30_39<- somatorio_colunas(mydata$confirmados_30_39_f, mydata$confirmados_30_39_m)
confirmados_30_39

confirmados_40_49<-somatorio_colunas(mydata$confirmados_40_49_f, mydata$confirmados_40_49_m)
confirmados_40_49

confirmados_50_59<- somatorio_colunas(mydata$confirmados_50_59_f, mydata$confirmados_50_59_m)
confirmados_50_59

confirmados_60_69<-somatorio_colunas(mydata$confirmados_60_69_f, mydata$confirmados_60_69_m)
confirmados_60_69

confirmados_70_79<-somatorio_colunas(mydata$confirmados_70_79_f,mydata$confirmados_70_79_m)
confirmados_70_79

confirmados_80_plus<-somatorio_colunas(mydata$confirmados_80_plus_f, mydata$confirmados_80_plus_m)
confirmados_80_plus

# Fazemos o somatorio dos confirmados totais

confirmados_total<-sum(mydata$confirmados, na.rm = TRUE)

# Criamos uma formula para a % para aplicar a cada uma das idades

percent<-function(x){(x/confirmados_total)*100}

percent_0_9<-percent(confirmados_0_9)
percent_0_9

percent_10_19<-percent(confirmados_10_19)
percent_20_29<-percent(confirmados_20_29)
percent_30_39<-percent(confirmados_30_39)
percent_40_49<-percent(confirmados_40_49)
percent_50_59<-percent(confirmados_50_59)
percent_60_69<-percent(confirmados_60_69)
percent_70_79<-percent(confirmados_70_79)
percent_80_plus<-percent(confirmados_80_plus)

#Vamos criar um histograma com estes valores, escolhido através do plotly
library(plotly)
percent_casos_idade<- plot_ly(x = c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80+"), y = c(percent_0_9, percent_10_19, percent_20_29,percent_30_39,percent_40_49,percent_50_59,percent_60_69,percent_70_79,percent_80_plus), type = "bar")
percent_casos_idade


#2ª forma, utilizar a última linha do my data para as percentagens : A FORMA CORRETA!!!!
percent2<-function(x,y){(((mydata[580,x])+(mydata[580,y]))/(mydata[580,"confirmados"]))*100}

percent_0_9<-percent2("confirmados_0_9_f", "confirmados_0_9_m")
percent_10_19<-percent2("confirmados_10_19_f","confirmados_10_19_m")
percent_20_29<-percent2("confirmados_20_29_f","confirmados_20_29_m")
percent_30_39<-percent2("confirmados_30_39_f","confirmados_30_39_m")
percent_40_49<-percent2("confirmados_40_49_f","confirmados_40_49_m")
percent_50_59<-percent2("confirmados_50_59_f","confirmados_50_59_m")
percent_60_69<-percent2("confirmados_60_69_f","confirmados_60_69_m")
percent_70_79<-percent2("confirmados_70_79_f","confirmados_70_79_m")
percent_80_plus<-percent2("confirmados_80_plus_f","confirmados_80_plus_m")

library(ggplot2)
percent_casos_idade<- plot_ly(x = c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80+"), y = c(percent_0_9, percent_10_19, percent_20_29,percent_30_39,percent_40_49,percent_50_59,percent_60_69,percent_70_79,percent_80_plus), type = "bar") %>% layout(title= "% Confirmados Por Faixa Etária", xaxis=list(title="IDADES"), yaxis=list(title="%")) %>% percent_casos_idade[color=bars]
percent_casos_idade

# ____________________________________________________________________________


# Agora vamos tentar criar outro tipo de gráficos pelo plotly, para vermos a evolução dos casos em função
# do tempo. 
# Primeiro vamos criar uma nova coluna para mudar o formato das datas para data e não character.

data_covid$data2 <- as.Date(data_covid$data, format = "%d-%m-%Y")

# NOTA: Como tivemos um erro à frente no trabalho (apareceu um vazio no gráfico), e percebemos que se devia
# a alguns NA no dia 25-04-2021, vamos optar por dar a essas linhas os valores do dia anterior.

data_covid[425, 23:40] = data_covid[424, 23:40]

# Depois temos de criar novas colunas com a soma dos masculinos e femininos de cada idade.

data_covid$confirmados_0_9 <- data_covid$confirmados_0_9_f + data_covid$confirmados_0_9_m
data_covid$confirmados_10_19 <- data_covid$confirmados_10_19_f + data_covid$confirmados_10_19_m
data_covid$confirmados_20_29 <- data_covid$confirmados_20_29_f + data_covid$confirmados_20_29_m
data_covid$confirmados_30_39 <- data_covid$confirmados_30_39_f + data_covid$confirmados_30_39_m
data_covid$confirmados_40_49 <- data_covid$confirmados_40_49_f + data_covid$confirmados_40_49_m
data_covid$confirmados_50_59 <- data_covid$confirmados_50_59_f + data_covid$confirmados_50_59_m
data_covid$confirmados_60_69 <- data_covid$confirmados_60_69_f + data_covid$confirmados_60_69_m
data_covid$confirmados_70_79 <- data_covid$confirmados_70_79_f + data_covid$confirmados_70_79_m
data_covid$confirmados_80_plus <- data_covid$confirmados_80_plus_f + data_covid$confirmados_80_plus_m

# A seguir vamos criar mais uma coluna e passar essa soma para percentagem.

data_covid$percent_0_9 <- (data_covid$confirmados_0_9 / data_covid$confirmados) * 100
data_covid$percent_10_19 <- (data_covid$confirmados_10_19 / data_covid$confirmados) * 100
data_covid$percent_20_29 <- (data_covid$confirmados_20_29 / data_covid$confirmados) * 100
data_covid$percent_30_39 <- (data_covid$confirmados_30_39 / data_covid$confirmados) * 100
data_covid$percent_40_49 <- (data_covid$confirmados_40_49 / data_covid$confirmados) * 100
data_covid$percent_50_59 <- (data_covid$confirmados_50_59 / data_covid$confirmados) * 100
data_covid$percent_60_69 <- (data_covid$confirmados_60_69 / data_covid$confirmados) * 100
data_covid$percent_70_79 <- (data_covid$confirmados_70_79 / data_covid$confirmados) * 100
data_covid$percent_80_plus <- (data_covid$confirmados_80_plus / data_covid$confirmados) * 100

library(plotly)

# Para criar um stacked area chart com os valores inteiros vamos usar as colunas com os confirmados totais
# por cada faixa etária.

fig <- plot_ly(data_covid, x = data_covid$data2, y = data_covid$confirmados_0_9, name = '0-9', type = 'scatter', mode = 'none', stackgroup = 'one', fillcolor = '#ff00a9')
fig <- fig %>% add_trace(y = data_covid$confirmados_10_19, name = '10-19', fillcolor = '#fb9f9f')
fig <- fig %>% add_trace(y = data_covid$confirmados_20_29, name = '20-29', fillcolor = '#ff0065')
fig <- fig %>% add_trace(y = data_covid$confirmados_30_39, name = '30-39', fillcolor = '#ffbfd3')
fig <- fig %>% add_trace(y = data_covid$confirmados_40_49, name = '40-49', fillcolor = '#fb5858')
fig <- fig %>% add_trace(y = data_covid$confirmados_50_59, name = '50-59', fillcolor = '#ff71ce')
fig <- fig %>% add_trace(y = data_covid$confirmados_60_69, name = '60-69', fillcolor = '#b967ff')
fig <- fig %>% add_trace(y = data_covid$confirmados_70_79, name = '70-79', fillcolor = '#8b3a3a')
fig <- fig %>% add_trace(y = data_covid$confirmados_80_plus, name = '80+', fillcolor = '#cd8c95')
fig <- fig %>% layout(title = 'Evolução temporal dos casos por grupo etário',
                      xaxis = list(title = "Tempo",
                                   showgrid = FALSE),
                        yaxis = list(title = "Nº Casos",
                                   showgrid = FALSE))
fig

# Para criar um stacked area chart com percentagens vamos usar as colunas com as percentagens
# de cada faixa etária.


fig <- plot_ly(data_covid, x = data_covid$data2, y = data_covid$percent_0_9, name = '0-9', type = 'scatter', mode = 'none', stackgroup = 'one', groupnorm = 'percent', fillcolor = '#ff48c4')
fig <- fig %>% add_trace(y = data_covid$percent_10_19, name = '10-19', fillcolor = '#2bd1fc')
fig <- fig %>% add_trace(y = data_covid$percent_20_29, name = '20-29', fillcolor = '#f3ea5f')
fig <- fig %>% add_trace(y = data_covid$percent_30_39, name = '30-39', fillcolor = '#c04df9')
fig <- fig %>% add_trace(y = data_covid$percent_40_49, name = '40-49', fillcolor = '#ff3f3f')
fig <- fig %>% add_trace(y = data_covid$percent_50_59, name = '50-59', fillcolor = '#05ffa1')
fig <- fig %>% add_trace(y = data_covid$percent_60_69, name = '60-69', fillcolor = '#0084ff')
fig <- fig %>% add_trace(y = data_covid$percent_70_79, name = '70-79', fillcolor = '#44bec7')
fig <- fig %>% add_trace(y = data_covid$percent_80_plus, name = '80+', fillcolor = '#ebdada')
fig <- fig %>% layout(title = 'Evolução percentual de casos por grupo etário',
                      xaxis = list(title = "Tempo",
                                   showgrid = FALSE),
                      yaxis = list(title = "Nº Casos",
                                   showgrid = FALSE,
                                   ticksuffix = '%'))
fig

# ___________________________________________________________________________


# Mas queremos não usar valores cumulativos, mas sim os valores inteiros de novos casos por faixa etária.
# Para isso, temos de usar a função diff(as.matrix(df)) que nos vai dar a diferença entre as linhas de uma
# data frame, ou das colunas que quisermos se no sitio da df pusermos so as colunas que nos interessam.
# df2<-data.frame(diff(as.matrix(df)))

# Primeiro criamos uma nova tabela em que apenas usamos a primeira linha das colunas que nos interessam, 
# e depois criar outra em que usamos a formula para obter uma tabela só com os casos novos por faixa etária.

casos_novos <- data_covid[1, c(94, 95, 96, 97, 98, 99, 100, 101, 102, 103)]

casos_novos2 <- data.frame(diff(as.matrix(data_covid[,c(95, 96, 97, 98, 99, 100, 101, 102, 103)])))

# Depois vamos unir a coluna data da data frame original à nova tabela dos casos novos 2 por faixa etária.
casos_novos2 <- cbind(data_covid[2:582, 94], casos_novos2)

# Para mudar o nome das colunas.
colnames(casos_novos2) <- c("Data2", "Casos_novos_0_9", "Casos_novos_10_19", "Casos_novos_20_29", "Casos_novos_30_39", "Casos_novos_40_49", "Casos_novos_50_59", "Casos_novos_60_69", "Casos_novos_70_79", "Casos_novos_80_plus")
colnames(casos_novos) <- c("Data2", "Confirmados_novos", "Casos_novos_0_9", "Casos_novos_10_19", "Casos_novos_20_29", "Casos_novos_30_39", "Casos_novos_40_49", "Casos_novos_50_59", "Casos_novos_60_69", "Casos_novos_70_79", "Casos_novos_80_plus")

# Depois vamos juntar as linhas das duas novas tabelas, para termos a tabela completa com o primeiro dia, e
# unimos depois a coluna dos novos confirmados totais da tabela original.
casos_novos <- rbind(casos_novos, casos_novos2)
casos_novos <- cbind(casos_novos$Data2, data_covid$confirmados_novos, casos_novos[, 2:10])


# Agora vamos calcular as percentagens dos casos novos e adicionar logo colunas com esses valores.

casos_novos$perc_novos_0_9 <- (casos_novos$Casos_novos_0_9 / casos_novos$Confirmados_novos) * 100
casos_novos$perc_novos_10_19 <- (casos_novos$Casos_novos_10_19 / casos_novos$Confirmados_novos) * 100
casos_novos$perc_novos_20_29 <- (casos_novos$Casos_novos_20_29 / casos_novos$Confirmados_novos) * 100
casos_novos$perc_novos_30_39 <- (casos_novos$Casos_novos_30_39 / casos_novos$Confirmados_novos) * 100
casos_novos$perc_novos_40_49 <- (casos_novos$Casos_novos_40_49 / casos_novos$Confirmados_novos) * 100
casos_novos$perc_novos_50_59 <- (casos_novos$Casos_novos_50_59 / casos_novos$Confirmados_novos) * 100
casos_novos$perc_novos_60_69 <- (casos_novos$Casos_novos_60_69 / casos_novos$Confirmados_novos) * 100
casos_novos$perc_novos_70_79 <- (casos_novos$Casos_novos_70_79 / casos_novos$Confirmados_novos) * 100
casos_novos$perc_novos_80_plus <- (casos_novos$Casos_novos_80_plus / casos_novos$Confirmados_novos) * 100


# Para fazer o gráfico com os casos novos, usamos a mesma função do plotly.

fig <- plot_ly(casos_novos, x = casos_novos$Data2, y = casos_novos$Casos_novos_0_9, name = '0-9', type = 'scatter', mode = 'none', stackgroup = 'one', groupnorm = 'percent', fillcolor = '#5f9ea0')
fig <- fig %>% add_trace(y = casos_novos$Casos_novos_10_19, name = '10-19', fillcolor = '#deb887')
fig <- fig %>% add_trace(y = casos_novos$Casos_novos_20_29, name = '20-29', fillcolor = '#cd5b45')
fig <- fig %>% add_trace(y = casos_novos$Casos_novos_30_39, name = '30-39', fillcolor = '#ffb90f')
fig <- fig %>% add_trace(y = casos_novos$Casos_novos_40_49, name = '40-49', fillcolor = '#6495ed')
fig <- fig %>% add_trace(y = casos_novos$Casos_novos_50_59, name = '50-59', fillcolor = '#e9967a')
fig <- fig %>% add_trace(y = casos_novos$Casos_novos_60_69, name = '60-69', fillcolor = '#6e8b3d')
fig <- fig %>% add_trace(y = casos_novos$Casos_novos_70_79, name = '70-79', fillcolor = '#7b68ee')
fig <- fig %>% add_trace(y = casos_novos$Casos_novos_80_plus, name = '80+', fillcolor = '#ffe4e1')
fig <- fig %>% layout(title = 'Evolução percentual de casos novos por grupo etário',
                      xaxis = list(title = "Data",
                                   showgrid = FALSE),
                      yaxis = list(title = "%",
                                   showgrid = FALSE,
                                   ticksuffix = '%'))
fig


install.packages("zoo")
install.packages("tidyverse")
install.packages("lubridate")
install.packages("fpp2")
library(zoo)
library(tidyverse)
library(lubridate)
library(fpp2)

# Para deixarmos de ter negativos e para termos uns picos menos acentuados, vamos usar a função rollmean().

casos_novos$Rollmean_0_9 <- rollmean(casos_novos$Casos_novos_0_9, k = 7, "center", fill = NA)
casos_novos$Rollmean_10_19 <- rollmean(casos_novos$Casos_novos_10_19, k = 7, "center", fill = NA)
casos_novos$Rollmean_20_29 <- rollmean(casos_novos$Casos_novos_20_29, k = 7, "center", fill = NA)
casos_novos$Rollmean_30_39 <- rollmean(casos_novos$Casos_novos_30_39, k = 7, "center", fill = NA)
casos_novos$Rollmean_40_49 <- rollmean(casos_novos$Casos_novos_40_49, k = 7, "center", fill = NA)
casos_novos$Rollmean_50_59 <- rollmean(casos_novos$Casos_novos_50_59, k = 7, "center", fill = NA)
casos_novos$Rollmean_60_69 <- rollmean(casos_novos$Casos_novos_60_69, k = 7, "center", fill = NA)
casos_novos$Rollmean_70_79 <- rollmean(casos_novos$Casos_novos_70_79, k = 7, "center", fill = NA)
casos_novos$Rollmean_80_plus <- rollmean(casos_novos$Casos_novos_80_plus, k = 7, "center", fill = NA)

casos_novos$Rollmean_perc_0_9 <- rollmean(casos_novos$perc_novos_0_9, k = 7, "center", fill = NA)
casos_novos$Rollmean_perc_10_19 <- rollmean(casos_novos$perc_novos_10_19, k = 7, "center", fill = NA)
casos_novos$Rollmean_perc_20_29 <- rollmean(casos_novos$perc_novos_20_29, k = 7, "center", fill = NA)
casos_novos$Rollmean_perc_30_39 <- rollmean(casos_novos$perc_novos_30_39, k = 7, "center", fill = NA)
casos_novos$Rollmean_perc_40_49 <- rollmean(casos_novos$perc_novos_40_49, k = 7, "center", fill = NA)
casos_novos$Rollmean_perc_50_59 <- rollmean(casos_novos$perc_novos_50_59, k = 7, "center", fill = NA)
casos_novos$Rollmean_perc_60_69 <- rollmean(casos_novos$perc_novos_60_69, k = 7, "center", fill = NA)
casos_novos$Rollmean_perc_70_79 <- rollmean(casos_novos$perc_novos_70_79, k = 7, "center", fill = NA)
casos_novos$Rollmean_perc_80_plus <- rollmean(casos_novos$perc_novos_80_plus, k = 7, "center", fill = NA)


# Tentativa de criar função que depois fosse criar uma linha no dia em que a vacinação iniciou.
#
# vline <- function(x = 0, color = "navy") {
#   list(
#     type = "line", 
#     y0 = 0, 
#     y1 = 100, 
#     yref = "paper",
#     x0 = x, 
#     x1 = x, 
#     line = list(color = color)
#   )
# }


# Depois fazemos novamente o scatter plotly mas com os valores médios da rollmean.

fig <- plot_ly(casos_novos, x = casos_novos$Data2, y = casos_novos$Rollmean_perc_0_9, name = '0-9', type = 'scatter', mode = 'none', stackgroup = 'one', groupnorm = 'percent', fillcolor = '#5f9ea0')
fig <- fig %>% add_trace(y = casos_novos$Rollmean_perc_10_19, name = '10-19', fillcolor = '#deb887')
fig <- fig %>% add_trace(y = casos_novos$Rollmean_perc_20_29, name = '20-29', fillcolor = '#cd5b45')
fig <- fig %>% add_trace(y = casos_novos$Rollmean_perc_30_39, name = '30-39', fillcolor = '#ffb90f')
fig <- fig %>% add_trace(y = casos_novos$Rollmean_perc_40_49, name = '40-49', fillcolor = '#6495ed')
fig <- fig %>% add_trace(y = casos_novos$Rollmean_perc_50_59, name = '50-59', fillcolor = '#e9967a')
fig <- fig %>% add_trace(y = casos_novos$Rollmean_perc_60_69, name = '60-69', fillcolor = '#6e8b3d')
fig <- fig %>% add_trace(y = casos_novos$Rollmean_perc_70_79, name = '70-79', fillcolor = '#7b68ee')
fig <- fig %>% add_trace(y = casos_novos$Rollmean_perc_80_plus, name = '80+', fillcolor = '#ffe4e1')
fig <- fig %>% layout(title = 'Evolução percentual de casos novos por grupo etário', 
                      xaxis = list(title = "Data", 
                                   showgrid = FALSE), 
                      yaxis = list(title = "%", 
                                   showgrid = FALSE, 
                                   ticksuffix = '%'),
                      paper_bgcolor = "#f5f5f5",
                      plot_bgcolor = "#f5f5f5"
                      )

fig


# Para guardar um gráfico como html 
htmlwidgets::saveWidget(as_widget(fig), "index.html")


# Para tentar fazer um gráfico com linhas em vez de áreas

fig <- plot_ly(casos_novos, x = casos_novos$Data2, y = casos_novos$Rollmean_0_9, name = "0-9", type = 'scatter', mode = 'lines', color = "#458b74")
fig <- fig %>% add_trace(y = casos_novos$Rollmean_10_19, name = '10-19', color = "#ff7f24") 
fig <- fig %>% add_trace(y = casos_novos$Rollmean_20_29, name = '20-29', color = "#cd3333") 
fig <- fig %>% add_trace(y = casos_novos$Rollmean_30_39, name = '30-39', color = "#556b2f") 
fig <- fig %>% add_trace(y = casos_novos$Rollmean_40_49, name = '40-49', color = "#5f9ea0") 
fig <- fig %>% add_trace(y = casos_novos$Rollmean_50_59, name = '50-59', color = "#8b2252") 
fig <- fig %>% add_trace(y = casos_novos$Rollmean_60_69, name = '60-69', color = "#ff7f50") 
fig <- fig %>% add_trace(y = casos_novos$Rollmean_70_79, name = '70-79', color = "#483d8b") 
fig <- fig %>% add_trace(y = casos_novos$Rollmean_80_plus, name = '80+', color = "#cd8162") 
fig <- fig %>% layout(title = 'Evolução de casos por grupo etário', legend = list(title = list(text = 'Grupo etário')),
         xaxis = list(dtick = "M1", tickformat="%b\n%Y",
                      ticklabelmode="period"), width = 1000)
fig <- fig %>%
  layout(
    xaxis = list(title = "Data",
                 zerolinecolor = '#1a1a1a',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
    yaxis = list(title = "NºCasos",
                 zerolinecolor = '#1a1a1a',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
    plot_bgcolor = '#f5f5f5',
    paper_bgcolor = '#f5f5f5')

fig


# Tentativa de criar gráfico dos internados com linhas
#
# fig <- plot(ggplot(casos_novos, aes(x=data, y=internados_totais)) + 
#               geom_line(color="salmon", size=1, )+ 
#              labs(title="Evolução no Nº de Doentes Internados", x="Data", y="Nº de Internados", color='red'))+ 
#   geom_vline(xintercept =as.numeric(as.Date(c("2021-01-11"))), linetype= c("solid"), color = "Navy",  size = 1) + 
#   theme(panel.background = element_rect(fill = 'LightCyan4', colour = 'black'), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
#   geom_text(aes(x=mydata2[321,10], label="primeira vacina", y=5000), colour="MistyRose1", angle=90, vjust = 1.2, size=4, family = 'arial')



# ___________________________________________________________________________


#SARA
##1ª forma de traçar linhas no ggplot
#data_vacina<-as.Date(c("2021-01-11"))
#data_vacina<-which(mydata2$data%in%data_vacina)
# ggplot + geom_vline(xintercept = as.numeric(mydata2$data[data_vacina]), col= "red", lwd=1)

fig4<-plot(ggplot(mydata2, aes(x=data, y=internados_totais)) + 
geom_line(color="salmon", size=1, )+ labs(title="Evolução no Nº de Doentes Internados", x="Data", y="Nº de Internados", color='red'))  + 
geom_vline(xintercept =as.numeric(as.Date(c("2021-01-11"))), linetype= c("solid"), color = "MistyRose1",  size = 1) + 
theme(panel.background = element_rect(fill = 'LightCyan4', colour = 'black'), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
geom_text(aes(x=mydata2[321,10], label="primeira vacina", y=5000), colour="MistyRose1", angle=90, vjust = 1.2, size=4, family = 'arial')
fig4
teste
