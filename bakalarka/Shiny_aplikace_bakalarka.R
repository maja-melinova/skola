#install.packages("tidyverse")
#install.packages("plotrix")
#install.packages("ggplot2")

library(shiny)
library(plotrix) 
library(plyr)
library(ggplot2)
library("reshape2") 
library(DT)
library(dplyr)
#library(dplyr)


###############################################################################
# Data ------------------------------------------------------------------------
###############################################################################

#-------------------------------------------------------------------------------
#Ruleta - zjednodusena
#-------------------------------------------------------------------------------
ruletaFunkce <- function(n){
  
  data_ruleta = data.frame(matrix(nrow = n, ncol = 0)) 
  data_ruleta$Cislo <- sample(0:36, n, replace = TRUE, prob=rep(1/37, times=37)) 
  
  #sude -> 2; liche -> 1; nula -> 0
  data_ruleta$SudeLiche[data_ruleta$Cislo %% 2 == 1] <- 1
  data_ruleta$SudeLiche[data_ruleta$Cislo %% 2 == 0] <- 2
  data_ruleta$SudeLiche[data_ruleta$Cislo == 0] <- 0
  
  #cerna -> 1; cervena -> 2; zelena -> 0
  
  barva <- function(cislo){
    vysledek <- c()
    for(x in cislo){
      if(x == 0){
        vysledek <- c(vysledek, 0)
      } else if(x == 2 || x == 4 || x == 6 || x == 8 || x == 10 || x == 11 || x == 13 || x == 15 || x == 17 || x == 20 || x == 22 || x == 24 || x == 26 || x == 28 || x == 29 || x == 31 || x == 33 || x == 35){
        vysledek <- c(vysledek, 1)
      } else {
        vysledek <- c(vysledek, 2)
      }
    }
    return(vysledek)
  }
  
  data_ruleta$Barva <- barva(data_ruleta$Cislo)
  
  #nula -> 0; male -> 1; velke -> 2
  data_ruleta$MaleVelke[data_ruleta$Cislo <= 18] <- 1
  data_ruleta$MaleVelke[data_ruleta$Cislo > 18] <- 2
  data_ruleta$MaleVelke[data_ruleta$Cislo == 0] <- 0
  
  #sazka - cislo -> 1, sude/liche -> 2, barva -> 3, mala/velka -> 4
  data_ruleta$Sazka <- sample(1:4, n, replace = TRUE, prob = c(0.1, 0.3, 0.3, 0.3))
  
  sazka <- function(typ){
    sazim <- c()
    for(x in typ){
      if(x == 1){
        sazim <- c(sazim, sample(0:36, 1, replace = TRUE))
      } else if(x == 2 || x == 3 || x == 4){
        sazim <- c(sazim, sample(1:2, 1, replace = TRUE))
      }
    }
    return(sazim)
  }
  
  data_ruleta$Sazka2 <- sazka(data_ruleta$Sazka)
  
  #hrac -> 0; kasino -> 1
  for(i in 1:n){
    if(data_ruleta$Sazka[i] == 1 && data_ruleta$Sazka2[i] == data_ruleta$Cislo[i]){
      data_ruleta$Vyhra[i] <- 0
    } else if(data_ruleta$Sazka[i] == 2 && data_ruleta$Sazka2[i] == data_ruleta$SudeLiche[i]){
      data_ruleta$Vyhra[i] <- 0
    } else if(data_ruleta$Sazka[i] == 3 && data_ruleta$Sazka2[i] == data_ruleta$Barva[i]) {
      data_ruleta$Vyhra[i] <- 0 
    } else if(data_ruleta$Sazka[i] == 4 && data_ruleta$Sazka2[i] == data_ruleta$MaleVelke[i]) {
      data_ruleta$Vyhra[i] <- 0 
    } else {
      data_ruleta$Vyhra[i] <- 1
    }
  }
  
  data_ruleta$Prumer <- data_ruleta[1,7]
  
  for(i in 1:n){
    data_ruleta[i,8] <- mean(data_ruleta[1:i,7])
  }
  
  return(data_ruleta)
}


#-------------------------------------------------------------------------------
#Bernoulliho pokus
#-------------------------------------------------------------------------------

bernoulliFunkce <- function(p = 0.5, n){
  p <- p
  
  data_bernoulli = data.frame(matrix(nrow = n, ncol = 0)) 
  data_bernoulli$Hodnota <- sample(0:1, n, replace = TRUE, prob = c(1-p, p))
  data_bernoulli$Prumer <- data_bernoulli[1,1]
  
  for(i in 2:n){
    data_bernoulli[i,2] <- mean(data_bernoulli[1:i,1])
  }
  
  return(data_bernoulli)
}

#-------------------------------------------------------------------------------
#Odhad pi metodou Monte Carlo
#-------------------------------------------------------------------------------

piFunkce <- function(n){
  data_pi <- data.frame(matrix(nrow = n, ncol = 0))
  data_pi$x <- runif(n, min = -1, max = 1)
  data_pi$y <- runif(n, min = -1, max = 1)
  data_pi$Vzdalenost <- data_pi$x^2 + data_pi$y^2
  
  data_pi$Kruh <- data_pi$Vzdalenost <= 1
  #data_pi$OdhadPi <- 0
  
  odhad_pi <- 4*(sum(data_pi$Kruh)/n)
  return(odhad_pi)
}

#-------------------------------------------------------------------------------
#Demo data - podíl chlapců a děvčat při narození
#-------------------------------------------------------------------------------

demo_funkce <- function(type){
  # narozeni celkem --------------------------------------------------------------
  data_narozeni_celkem <- read.csv("narozeni_celkem.csv", sep = ";")
  rownames(data_narozeni_celkem) <- data_narozeni_celkem[,1]
  data_narozeni_celkem <- data_narozeni_celkem[,2:16]
  
  nazvySloupcu_c <- c()
  for(i in 7:21){
    ifelse(i < 10, nazvySloupcu_c[i-6] <- sprintf("c0%d",i), nazvySloupcu_c[i-6] <- sprintf("c%d",i))
  }
  
  colnames(data_narozeni_celkem) <- nazvySloupcu_c
  
  pocetNA_c <- setNames(rowSums(is.na(data_narozeni_celkem)), rownames(data_narozeni_celkem))
  pocetNA_c <- pocetNA_c[pocetNA_c > 7]
  
  
  # narozeni muzi ----------------------------------------------------------------
  data_narozeni_muzi <- read.csv("narozeni_muzi.csv", sep = ";")
  rownames(data_narozeni_muzi) <- data_narozeni_muzi[,1]
  data_narozeni_muzi <- data_narozeni_muzi[,2:16]
  
  nazvySloupcu_m <- c()
  for(i in 7:21){
    ifelse(i < 10, nazvySloupcu_m[i-6] <- sprintf("m0%d",i), nazvySloupcu_m[i-6] <- sprintf("m%d",i))
  }
  
  colnames(data_narozeni_muzi) <- nazvySloupcu_m
  
  pocetNA_m <- setNames(rowSums(is.na(data_narozeni_muzi)), rownames(data_narozeni_muzi))
  pocetNA_m <- pocetNA_m[pocetNA_m > 7]
  
  #-------------
  
  radky_moc_NA <- intersect(names(pocetNA_c),names(pocetNA_m))
  #radky_moc_NA -- řádky, kde je více než půl hodnot NA (a shodují se v tabulce "celkem" i "muzi")
  
  indexy_moc_NA <- c()
  
  for(i in 1:length(radky_moc_NA)){
    indexy_moc_NA[i] <- which(radky_moc_NA[i] == rownames(data_narozeni_celkem))
  }
  
  data_narozeni_celkem <- data_narozeni_celkem[-indexy_moc_NA,]
  data_narozeni_muzi <- data_narozeni_muzi[-indexy_moc_NA,]
  
  #------------
  # nahrazeni chybějících hodnot řádkovým medianem
  median_c <- apply(data_narozeni_celkem, 1, median, na.rm=TRUE)
  
  for(i in 1:nrow(data_narozeni_celkem)){
    for(j in 1:ncol(data_narozeni_celkem)){
      if(is.na(data_narozeni_celkem[i, j])){
        data_narozeni_celkem[i, j] <- median_c[i]
      }
    }
  }
  
  median_m <- apply(data_narozeni_muzi, 1, median, na.rm=TRUE)
  
  for(i in 1:nrow(data_narozeni_muzi)){
    for(j in 1:ncol(data_narozeni_muzi)){
      if(is.na(data_narozeni_muzi[i, j])){
        data_narozeni_muzi[i, j] <- median_m[i]
      }
    }
  }
  
  data_podil_narozenych_m <- data.frame(matrix(nrow = nrow(data_narozeni_celkem), ncol = ncol(data_narozeni_celkem)))
  rownames(data_podil_narozenych_m) <- rownames(data_narozeni_celkem)
  
  nazvySloupcu_p <- c()
  for(i in 7:21){
    ifelse(i < 10, nazvySloupcu_p[i-6] <- sprintf("p0%d",i), nazvySloupcu_p[i-6] <- sprintf("p%d",i))
  }
  
  data_podil_narozenych_m <- data_narozeni_muzi/data_narozeni_celkem
  colnames(data_podil_narozenych_m) <- nazvySloupcu_p
  
  if(type == "c"){
    return(data_narozeni_celkem)
  }
  if(type == "m"){
    return(data_narozeni_muzi)
  }
  if(type == "p"){
    return(data_podil_narozenych_m)
  }
}

###############################################################################
# Uživatelské rozhraní -------------------------------------------------------
###############################################################################

ui <- fluidPage(
  titlePanel(
    p( 
      h2("Zákon velkých čísel", style="margin-left:15px"),
      h5("Podpora výuky základů teorie pravděpodobnosti pomocí R-kových Shiny aplikací", style="color:grey;margin-left:15px")
    ), windowTitle = "Zákon velkých čísel"
  ),
#-------------------------------------------------------------------------------
# Hlavní panel
#-------------------------------------------------------------------------------
  mainPanel(width = 7, style="zoom: 80%",
    tabsetPanel(
      id = "tabselected",
      type = "tabs",
      tabPanel("Bernoulliho pokus", value = 1),
      tabPanel("Ruleta", value = 2),
      tabPanel("Odhad π", value = 3),
      tabPanel("Podíl narozených chlapců", value = 4)
    ),
#-------------------------------------------------------------------------------
# HP - Bernoulliho pokus
    conditionalPanel(condition="input.tabselected==1",
      h3("Bernoulliho pokus"),
      sliderInput(
        inputId = "bernoulli_pokusy",
        label = "Počet pokusů:",
        min = 1,
        max = 5000,
        value = 2500,
        width = "100%"
      ),
      splitLayout(
        sliderInput(
          inputId = "bernoulli_p",
          label = "Pravděpodobnost p (úspěchu):",
          min = 0,
          max = 1,
          value = 0.5,
          width = "100%"
        ),
        sliderInput(
          inputId = "bernoulli_opakovani",
          label = "Počet opakování:",
          min = 1,
          max = 5,
          value = 3,
          width = "100%"
        )
      ),br(), br(),
      mainPanel(plotOutput(outputId = "bernoulliGraf"), width="100%")
    ),
#-------------------------------------------------------------------------------
# HP - Ruleta
    conditionalPanel(condition="input.tabselected==2",
      h3("Ruleta"),
      splitLayout(
        mainPanel(width="100%",
          sliderInput(
            inputId = "ruleta_pokusy",
            label = "Počet pokusů:",
            min = 1,
            max = 2000,
            value = 1000,
            width = "95%"
          ),
          sliderInput(
            inputId = "ruleta_opakovani",
            label = "Počet opakování:",
            min = 1,
            max = 5,
            value = 3,
            width = "95%"
          )),
        mainPanel(tableOutput("ruletaTabulka"), width="100%", style="margin-left:20px")),
        br(), br(),
        mainPanel(plotOutput(outputId = "ruletaGraf"), width="100%")
      ),
#-------------------------------------------------------------------------------
# HP - Výpočet pi
    conditionalPanel(condition="input.tabselected==3",
      h3("Odhad π metodou Monte Carlo"),
      mainPanel(width="100%",
                  sliderInput(
                    inputId = "pi_pokusy",
                    label = "Počet bodů pro výpočet jednoho π:",
                    min = 1,
                    max = 10000,
                    value = 5000,
                    width = "100%"
                  ),
                  sliderInput(
                    inputId = "pi_opakovani",
                    label = "Počet opakování:",
                    min = 1,
                    max = 1000,
                    value = 500,
                    width = "100%"
                  )),
      br(),br(),
      mainPanel(plotOutput(outputId = "piGraf"), width="100%")
    ),
#-------------------------------------------------------------------------------
# HP - Podíl narozených chlapců
    conditionalPanel(condition="input.tabselected==4",
      h3("Podíl narozených chlapců"),
      mainPanel(dataTableOutput("demoTabulka"), width = "100%"), br(),
      mainPanel(plotOutput(outputId = "demoGraf", height = "300px"), width="100%")
    )
  ),
#-------------------------------------------------------------------------------
# Postranní panel
#-------------------------------------------------------------------------------
  sidebarPanel(width = 5, style = "height: 83vh; overflow-y: auto",
#-------------------------------------------------------------------------------
# PP - Bernoulliho pokus
    conditionalPanel(condition="input.tabselected==1",
      h3("Zdrojový kód aplikace"),
      htmltools::includeMarkdown("bernoulli.Rmd"),
    ),
#-------------------------------------------------------------------------------
# PP - Ruleta
    conditionalPanel(condition="input.tabselected==2",
      h3("Zdrojový kód aplikace"),
      htmltools::includeMarkdown("ruleta.Rmd"),
    ),
#-------------------------------------------------------------------------------
# PP - Výpočet pi
    conditionalPanel(condition="input.tabselected==3",
      h3("Zdrojový kód aplikace"),
      htmltools::includeMarkdown("pi.Rmd"),
    ),
#-------------------------------------------------------------------------------
# PP - Podíl narozených chlapců
    conditionalPanel(condition="input.tabselected==4",
      h3("Zdrojový kód aplikace"),
      htmltools::includeMarkdown("demo.Rmd"),
    )
  )
)

###############################################################################
# Serverová logika -----------------------------------------------------------
###############################################################################

server <- function(input, output){
#-------------------------------------------------------------------------------
#bernoulli  
  output$bernoulliGraf <- renderPlot({
    data <- data.frame(matrix(nrow = input$bernoulli_pokusy, ncol = input$bernoulli_opakovani))

    for(i in 1:input$bernoulli_opakovani){
      x <- bernoulliFunkce(input$bernoulli_p, input$bernoulli_pokusy)
      data[i] <- x[2] 
    }
    
    data$index <- 1:input$bernoulli_pokusy
    data_long <- melt(data, id = "index")
    
    x_pozice <- 0.25
    
    if(input$bernoulli_p < 0.4){
      x_pozice <- 0.75
    }
    
    ggplot(data_long, aes(x = data_long$index, y = data_long$value, color = variable)) + 
      geom_line(size = 1)+
      xlab("Počet pokusů") + ylab("Podíl úspěchů") + ylim(0,1)+
      geom_hline(yintercept=input$bernoulli_p,linetype = "dashed")+
      scale_colour_discrete(labels= paste(1:length(unique(data_long$variable) ), ". řada pokusů"))+
      theme(legend.position = c(0.5, x_pozice),axis.text = element_text(size = 15), axis.title = element_text(size = 15), legend.text = element_text(size=15))+
      labs(color=NULL)
    })
#-------------------------------------------------------------------------------
#ruleta  
  output$ruletaGraf <- renderPlot({
    data <- data.frame(matrix(nrow = input$ruleta_pokusy, ncol = input$ruleta_opakovani))
    
    for(i in 1:input$ruleta_opakovani){
      x <- ruletaFunkce(input$ruleta_pokusy)
      data[i] <- x[8] 
    }
    
    data$index <- 1:input$ruleta_pokusy
    data_long <- melt(data, id = "index")
    
    ggplot(data_long, aes(x = data_long$index, y = data_long$value, color = variable)) + 
      geom_line(size = 1)+
      xlab("Počet pokusů") + ylab("Podíl úspěchů") + ylim(0,1)+ 
      geom_hline(yintercept=0.5,linetype = "dashed")+
      scale_colour_discrete(labels= paste(1:length(unique(data_long$variable) ), ". řada pokusů"))+
      theme(legend.position = c(0.5, 0.25),axis.text = element_text(size = 15), axis.title = element_text(size = 15), legend.text = element_text(size=15))+
      labs(color=NULL) +
      annotate(geom="text", x = input$ruleta_pokusy*(5/6), y = 0.7, label="Vyhrává kasino", size = 5) +
      annotate(geom="text", x = input$ruleta_pokusy*(5/6), y = 0.3, label="Vyhrává hráč", size = 5)
  })
  output$ruletaTabulka = renderTable({
    data_ukazka <- ruletaFunkce(5)[,1:7]
  })
#-------------------------------------------------------------------------------
#odhad pi
  output$piGraf <- renderPlot({
    data <- data.frame(matrix(nrow = input$pi_opakovani, ncol = 0))
    data$index <- 1:input$pi_opakovani
    data$odhadPi <- 0
    
    for(i in 1:input$pi_opakovani){
      data[i, 2] <- piFunkce(input$pi_pokusy)
    }
    
    data$Prumer <- data[1,2]
    
    for(i in 2:input$pi_opakovani){
      data[i,3] <- mean(data[1:i,2])
    }
    
    ggplot(data, aes(x = index, y = Prumer, color = "#800020")) + 
      geom_line(size = 1)+ 
      xlab("Počet opakování") + ylab("Vypočítaná hodnota π")+
      geom_hline(yintercept=pi,linetype = "dashed")+
      theme(legend.position = "none",axis.text = element_text(size = 15), axis.title = element_text(size = 15))+
      labs(color=NULL)+
      geom_text( 
        data=data %>% filter(index == input$pi_opakovani), # Filter data first
        aes(label=round(Prumer, digits = 4)),
        size = 5,
        vjust = ifelse(data[input$pi_opakovani,3] > pi, -0.7, 1.5)
      )
  })

#Podil narozenych chlapcu
  output$demoTabulka <- renderDataTable({
    demo_data <- round(demo_funkce("p"), digits = 4)
  }, options = list(pageLength = 5, scrollX = TRUE))
  
  output$demoGraf <- renderPlot({
    data_podil_narozenych_m <- demo_funkce("p")
    data <- data.frame(matrix(nrow = ncol(data_podil_narozenych_m), ncol = 0))
    
    podil_narozenych_chlapcu <- c()
    
    for(i in 1:ncol(data_podil_narozenych_m)){
      podil_narozenych_chlapcu[i] <- mean(as.matrix(data_podil_narozenych_m[,i]))
    }
    
    podil_narozenach_devcat <- 1-podil_narozenych_chlapcu
    pocet_chlapcu_na_100_devcat <- round(podil_narozenych_chlapcu/podil_narozenach_devcat*100, digits = 3)
    
    data$pocetChlapcu <- pocet_chlapcu_na_100_devcat
    data$index <- c(2007:2021)
    
    ggplot(data, aes(x = index, y = pocetChlapcu, color = pocetChlapcu)) + 
      geom_line(size = 1)+ 
      xlab("Rok") + ylab("Počet chlapců na 100 děvčat")+
      theme(legend.position = "none",axis.text = element_text(size = 15), axis.title = element_text(size = 15), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      labs(color=NULL)+
      scale_x_continuous(breaks = scales::pretty_breaks(n = 15))
  })
}

###############################################################################
# Zavolání aplikace ----------------------------------------------------------
###############################################################################

shinyApp(ui, server)







