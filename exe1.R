library(esquisse)
options("esquisse.viewer" = "browser")
library(outliers)
library(corrplot)
library(janitor)
library(tidyverse)
library(lubridate)
library(reshape2)
library(arules)
View(base)
base <- read.csv("~/faculdade/matérias/tcc/DADOS GUSTAVO MINERAÇÃO1.csv", sep=",",h=T,na.strings = "",stringsAsFactors = T)
enem <- read.csv("~/faculdade/matérias/tcc/enem.csv",h=T,sep=";", stringsAsFactors = T)
vest <- read.csv("~/faculdade/matérias/tcc/vest.csv",h=T,sep=";", stringsAsFactors = T)

vest <- clean_names(vest)
vest$pontuacao <- as.character(vest$pontuacao)
vest$pontuacao <- str_replace_all(as.character(vest$pontuacao), "[,]", ".")
vest$pontuacao <- as.numeric(vest$pontuacao)
vest$pontuacao[vest$pontuacao=="NULL"] <-  NA
vest$vestibular <- vest$pontuacao
vest<- vest[,-c(3,4)]
vest <- distinct(vest)
vest.a <-  subset(vest,data_ingresso!="NULL")
vest.a$ingresso <- mdy(vest.a$data_ingresso)
vest.a$ingresso <- year(vest.a$ingresso)
vest.a$ra <- as.factor(vest.a$ra)
vest.b <- vest.a[,c(1,3:4)]
vest.b$vest[vest.b$vestibular<70&vest.b$vestibular>=60] <- "razoavel"
vest.b$vest[vest.b$vestibular<90&vest.b$vestibular>=70] <- "bom"
vest.b$vest[vest.b$vestibular>=90&vest.b$vestibular<=100] <- "excelente"
vest.b$vest[vest.b$vestibular<60] <- "ruim"
vest.b <- vest.b[,c(1,3,4)]
write.csv(vest.b,"vestibular.csv")




base <- clean_names(base)
base$x <- NULL

base$cr[base$cr=="NULL"] <- NA
base$cr<- as.numeric(as.character(base$cr))
base$cr <- round(base$cr,0)
base$ultimo_semestre_cursado <- str_sub(base$ultimo_semestre_cursado, end = -3) 
base$idade<- dmy(base$data_nascimento)
base$idade<-as.numeric(base$ultimo_semestre_cursado) - year(base$idade)
base$media_final[base$media_final=="NULL"] <- NA
base$media_final <- as.numeric(as.character(base$media_final))
base <- base[!is.na(base$media_final),]









