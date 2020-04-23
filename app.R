





library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)

Donnees<-read.csv("data_ulule_2019-1.csv",header = TRUE,sep = ";")
attach(Donnees)







##	Exclure du p?rim?tre les campagnes annul?es

data<-Donnees[order(country),] %>%
  filter(is_cancelled==FALSE)


##	Se restreindre au p?rim?tre des 8 pays ayant le plus de campagnes au total


data_2 <- subset(data, country == "FR" | country=="BE" | country=="IT" | country=="CA" | country=="ES" | country=="CH" | country=="DE" | country=="GB")




data_3<-data_2 %>% 
  select(c(3,7,14))

data_4 <-data_2[,-c(3,7,14)]


taux_change <- 0.9

n<-nrow(data_3)

for (i in 1:n) {
  
  if(data_3$currency[i] != "EUR")
  {
    data_3$amount_raised[i] <- data_3$amount_raised[i] * taux_change
    data_3$goal[i] <- data_3$goal[i] * taux_change
    data_3$currency[i] <- "EUR"
  }
  
}

data_final <- cbind(data_4,data_3)



#changer le format des dates
data_final$date_end<-as.POSIXct(data_final$date_end)

data_final$date_start<-as.POSIXct(data_final$date_start)

data_final<- data_final %>%
  filter(data_final$date_end < ymd("2018-12-31"))


data_final["annee"] = year(data_final$date_start)



data_final['count'] = 1 


count = data_final %>% 
  filter(!is.na(data_final$category))%>% 
  group_by(`annee`, `category`) %>% 
  summarise(count=  sum(count))



proportion = data_final %>% 
  filter(!is.na(data_final$category))%>% 
  group_by(`annee`, `category`) %>% 
  summarise(prop=  sum(`goal_raised`)/sum(count))

moyenne = data_final %>% 
  filter(data_final$goal_raised=="TRUE" & !is.na(data_final$category))%>% 
  group_by(`annee`, `category`) %>% 
  summarise(moyenne=mean(`amount_raised`, na.rm = TRUE))

donnee<-merge(proportion,moyenne, by=c("annee","category"), all=TRUE)
donnee <- merge(donnee,count,by=c("annee","category"), all=TRUE)


##PARTI SHINY




library(shiny)



library(shinydashboard)

ui <- (pageWithSidebar(
  
  
  headerPanel(h3("Visualisation des donnees Ulule", align = "center", style = "color: black",  height  = 250, width = 250)),
  sidebarPanel(style="background-color: #99CCFF;",
               
               radioButtons("suivi", "Suivie", choices = c("Nombre-total-de-campagnes-crees"="count", "Proportion-de-campagnes-financees"="proportion", "Montant-moyen-des-campagnes-financees"="moyenne")),
               selectInput("categorie", "categorie:", unique(donnee$category))
  ),
  mainPanel(
    
    
    plotOutput("plot")
  )
  
))

server <- function(input, output) {
  
  
  
  output$plot <- renderPlot({
    suivi<-input$suivi
    categorie<-input$categorie
    plot(as.integer(donnee[donnee$category==input$categorie, "annee"]), donnee[donnee$category==input$categorie, input$suivi], main= input$categorie, xlab=" evolution par annee", ylab=input$suivi,col.main="blue",col="red",col.lab="darkblue",type="l")
  })
  
}


shinyApp(ui, server)



