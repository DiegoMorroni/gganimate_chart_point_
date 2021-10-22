##evolução preço camarão 7b
##autor: Diego Morroni
##data de criação: 18/10¹2021
##ultima edição: 21/10/2021
##################################################################################
# setando diretorio
setwd("C:/Users/diego morroni/Desktop/portfolio")

# pacotes necessarios
#ordenar dados
install.packages("dplyr")
#alterar formato data
install.packages("lubridate")
#gráfico
install.packages("ggplot")
#grafico interativo
install.packages("gganimate")
install.packages("gifski")

#carregando dados
dados<- read.csv("relatorio30_.csv", h= T, dec=",", sep= ";")
# alterando formato de dados
dados$Pescado<- as.factor(dados$Pescado)
dados$kg.no.Período<-as.numeric(dados$kg.no.Período)
dados$valor.estimado.no.período<-as.numeric(dados$valor.estimado.no.período)# retirar o R$ antes de carregar 


#filtragem e segmentação do periodo, escolha das espécies
dados_sort<-dados%>%
  filter( Ano != 2016 & Ano != 2021, Pescado == "Camarão-sete-barbas")%>%
  group_by(Ano, Mês, Município)%>%
  summarise(valor_kg = mean(valor.estimado.no.período/kg.no.Período))
  

#modificando formato data
library(lubridate) 
dados_sort$mes_2<-month(as.numeric(dados_sort$Mês), label = T)      


##################################################################################
# gráfico animado padrao GGPlot - valor acumulado modo 2
library(plotly)
library(gganimate)

 #grafico_animado
   plot<-ggplot(dados_sort)+
   geom_point(aes(x=mes_2, y=valor_kg, color= Município),stat= "identity",size= 2)+labs(title= "variação do preço do camarão Ano:{frame_time}",x= " ", y="preço do kg do camarão (R$)")+ transition_time(Ano)
 
animate(plot, renderer = gifski_renderer())
 anim_save("plot.gif")
  