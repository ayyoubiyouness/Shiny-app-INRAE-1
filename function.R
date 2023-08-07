library(markdown)
library(shiny)
library(ggplot2)
library(readxl)
library(ggcorrplot)
library(tidyverse)
library(plotly)
library(highcharter)
library(shinydashboard)
library(shinyjs)
library(shinyBS)
library(shinycssloaders)
library(shinyWidgets)
library(shinydashboardPlus)
library(DT)

calculate_mean_by_cycle <- function(data, variable) {
  result <- as.data.frame(by(data[[variable]], list(data$cycle), mean))
  return(result)
}
# Fonctions utiles pour la répartition des graphes : 
quinzine <- function(data){
  satap <-as.data.frame(data$mean) 
  satap2 <-as.data.frame(data$min) 
  satap3 <- as.data.frame(data$max)
  
  values <- as.data.frame(c(sum(satap[1:15,]),sum(satap[16:31,]) , sum(satap[32:45,]), sum(satap[46:60,]),
                            sum(satap[61:75,]), sum(satap[76:91,]), sum(satap[92:106,]), sum(satap[107:121,]), 
                            sum(satap[122:136,]), sum(satap[137:152,]), sum(satap[153:167,]), sum(satap[168:182,]),
                            sum(satap[183:197,]),sum(satap[198:213,]),sum(satap[214:228,]),sum(satap[229:244,]),
                            sum(satap[245:259,]),sum(satap[260:274,]),sum(satap[275:289,]),sum(satap[290:305,]),
                            sum(satap[306:320,]),sum(satap[321:335,]),sum(satap[336:350,]),sum(satap[351:366,])))
  values1 <- as.data.frame(c(sum(satap2[1:15,]),sum(satap2[16:31,]) , sum(satap2[32:45,]), sum(satap2[46:60,]),
                             sum(satap2[61:75,]), sum(satap2[76:91,]), sum(satap2[92:106,]), sum(satap2[107:121,]), 
                             sum(satap2[122:136,]), sum(satap2[137:152,]), sum(satap2[153:167,]), sum(satap2[168:182,]),
                             sum(satap2[183:197,]),sum(satap2[198:213,]),sum(satap2[214:228,]),sum(satap2[229:244,]),
                             sum(satap2[245:259,]),sum(satap2[260:274,]),sum(satap2[275:289,]),sum(satap2[290:305,]),
                             sum(satap2[306:320,]),sum(satap2[321:335,]),sum(satap2[336:350,]),sum(satap2[351:366,])))
  values2 <- as.data.frame(c(sum(satap3[1:15,]),sum(satap3[16:31,]) , sum(satap3[32:45,]), sum(satap3[46:60,]),
                             sum(satap3[61:75,]), sum(satap3[76:91,]), sum(satap3[92:106,]), sum(satap3[107:121,]), 
                             sum(satap3[122:136,]), sum(satap3[137:152,]), sum(satap3[153:167,]), sum(satap3[168:182,]),
                             sum(satap3[183:197,]),sum(satap3[198:213,]),sum(satap3[214:228,]),sum(satap3[229:244,]),
                             sum(satap3[245:259,]),sum(satap3[260:274,]),sum(satap3[275:289,]),sum(satap3[290:305,]),
                             sum(satap3[306:320,]),sum(satap3[321:335,]),sum(satap3[336:350,]),sum(satap3[351:366,])))
  mois <- as.data.frame(c("j1", "j2", "f1", "f2", "ms1", "ms2", "av1", "av2", "ma1", "ma2", "ju1", "ju2", "jl1", "jl2", "ao1", "ao2", 
                          "s1", "s2", "o1", "o2", "n1", "n2", "d1", "d2") )
  donnee <- cbind(mois, values, values1, values2)
  colnames(donnee) <- c("Date", "mean", "min", "max")
  return(donnee)
}

mois <- function(data){
  satap <-as.data.frame(data$mean) 
  satap2 <-as.data.frame(data$min) 
  satap3 <- as.data.frame(data$max)
  
  values1 <- as.data.frame(c(sum(satap[1:31,]), sum(satap[32:60,]), 
                             sum(satap[61:91,]), sum(satap[92:121,]), 
                             sum(satap[122:152,]), sum(satap[153:182,]), 
                             sum(satap[183:213,]),sum(satap[214:244,]),
                             sum(satap[245:274,]),sum(satap[275:305,]),
                             sum(satap[306:335,]),sum(satap[336:366,]))) 
  values2 <- as.data.frame(c(sum(satap2[1:31,]), sum(satap2[32:60,]), 
                             sum(satap2[61:91,]), sum(satap2[92:121,]), 
                             sum(satap2[122:152,]), sum(satap2[153:182,]), 
                             sum(satap2[183:213,]),sum(satap2[214:244,]),
                             sum(satap2[245:274,]),sum(satap2[275:305,]),
                             sum(satap2[306:335,]),sum(satap2[336:366,]))) 
  values3 <- as.data.frame(c(sum(satap3[1:31,]), sum(satap3[32:60,]), 
                             sum(satap3[61:91,]), sum(satap3[92:121,]), 
                             sum(satap3[122:152,]), sum(satap3[153:182,]), 
                             sum(satap3[183:213,]),sum(satap3[214:244,]),
                             sum(satap3[245:274,]),sum(satap3[275:305,]),
                             sum(satap3[306:335,]),sum(satap3[336:366,]))) 
  
  mois <- as.data.frame(c("janvier", "février", "mars", "avril", "mai", "juin", "juillet", "aout", "septembre", "octobre", "novembre", "décembre"))
  donnee <- cbind(mois, values1, values2, values3 )
  colnames(donnee) <- c("Date", "mean", "min", "max")
  return(donnee)
}


saison <- function(data) {
  satap <-as.data.frame(data$mean) 
  satap2 <-as.data.frame(data$min) 
  satap3 <- as.data.frame(data$max)
  
  values <-  as.data.frame(c(sum(satap[c(1:60,336:366),]), 
                             sum(satap[c(61:152),]),
                             sum(satap[c(153:244),]),
                             sum(satap[c(245:335),])))
  values1 <- as.data.frame(c(sum(satap2[c(1:60,336:366),]), 
                             sum(satap2[c(61:152),]),
                             sum(satap2[c(153:244),]),
                             sum(satap2[c(245:335),])))
  values2 <-  as.data.frame(c(sum(satap3[c(1:60,336:366),]), 
                              sum(satap3[c(61:152),]),
                              sum(satap3[c(153:244),]),
                              sum(satap3[c(245:335),])))
  
  saison <- c("Hiver", "Printemps", "Ete", "Automne")
  donnee <- cbind(saison, values, values1, values2)
  colnames(donnee) <- c("Date", "mean", "min", "max")
  return(donnee)
}

