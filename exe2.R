dados1 <- read.csv("vestibular.csv", sep=",",h=T,na.strings = "",stringsAsFactors = T)
dados1$ra <- as.factor(as.character(dados1$ra))
dados1$X <- NULL


dados2 <- read.csv("Alunos cidade1.csv", sep=",",h=T,na.strings = "",stringsAsFactors = T)
dados2 <- clean_names(dados2)
dados2$ra <- as.factor(as.character(dados2$ra))
dados2$x <- NULL
dados2$endereco[dados2$endereco=="NULL"] <- NA
dados2$cidade[dados2$cidade=="NULL"] <- NA
dados2 <- read.csv("Alunos cidade1.csv", sep=",",h=T,na.strings = "",stringsAsFactors = T)
dados2$cidade <- rm_accent(dados2$cidade)
dados2$cidade <- str_to_lower(dados2$cidade)
dados2$cidade <- str_trim(dados2$cidade)


dados3 <- read.csv("base.csv", sep=",",h=T,na.strings = "",stringsAsFactors = T)
dados3 <- clean_names(dados3)
dados3$ra <- as.factor(as.character(dados3$ra))










dados4 <- left_join(dados2,dados3, by="ra")
dados4 <- left_join(dados4,dados1,by="ra")
dados4$cr <- as.character(dados4$cr)
dados4$cr <- as.numeric(dados4$cr)
dados4 <- subset(dados4,status_no_curso=="ABANDONO"|status_no_curso=="CONCLUIDO"|status_no_curso=="MATRICULADO"|status_no_curso=="TRANSFERÊNCIA INTERNA"|status_no_curso=="JUBILADO")
dados4 <- droplevels(dados4)


dados = dados4 %>% filter(!is.na(cr)) %>% group_by(ra) %>% summarise(cr = mean(cr)) 
dados4 = dados4 %>%
  filter() %>%
  left_join(dados, by = "ra")
vet = which(!is.na(dados4$cr.y) & is.na(dados4$cr.x))
dados42 = dados4[vet,]
dados42 = dados42 %>% select(-cr.x) %>% rename(cr = cr.y)
dados4 = dados4[-vet,]
dados4 = dados4 %>% select(-cr.y) %>% rename(cr = cr.x)
dados4 = rbind(dados4,dados42)
rm(dados42)

dados4$idadenew[dados4$idade>15 & dados4$idade<=29] <- "jovem"
dados4$idadenew[dados4$idade>29 & dados4$idade<=59] <- "adulto"
dados4$idadenew[dados4$idade>=60] <- "idoso"
#dados4$pc[dados4$curso=="DIREITO"] <- (dados4$ultimo_semestre_cursado - dados4$ingresso)/5*100
dados4$cidade <- str_trim(dados4$cidade)
dados4 <- dados4[dados4$ra!="07.2.00660",]
dados4 <-read.csv("dados4.csv", sep=",",h=T,na.strings = "",stringsAsFactors = T)
dados4$ingresso[str_sub(dados4$ra,1,2)== "08"] <- 2008
dados4$ingresso[str_sub(dados4$ra,1,2)== "09"] <- 2009
dados4$ingresso[str_sub(dados4$ra,1,2)== "10"] <- 2010
dados4$ingresso[str_sub(dados4$ra,1,2)== "11"] <- 2011
dados4$ingresso[str_sub(dados4$ra,1,2)== "12"] <- 2012
dados4$ingresso[str_sub(dados4$ra,1,2)== "13"] <- 2013
dados4$ingresso[str_sub(dados4$ra,1,2)== "14"] <- 2014
dados4$ingresso[str_sub(dados4$ra,1,2)== "15"] <- 2015
dados4$ingresso[str_sub(dados4$ra,1,2)== "16"] <- 2016
dados4$ingresso[str_sub(dados4$ra,1,2)== "17"] <- 2017
dados4$ingresso[str_sub(dados4$ra,1,2)== "18"] <- 2018
dados4$ingresso[str_sub(dados4$ra,1,2)== "19"] <- 2019
dados4$ingresso[str_sub(dados4$ra,1,2)== "20"] <- 2020
set.seed(1)
dados4 <- subset(dados4,ingresso!=2020)
dados4$pc <- dados4$ultimo_semestre_cursado - dados4$ingresso +1
dados4 <- dados4[dados4$ingresso>=2015,]
trat <- read.csv("trat.csv",stringsAsFactors = T,sep = ",",header = T)
dados4 <- left_join(dados4,trat)
dados4 <- dados4 %>% group_by(ra,cidade,sexo,curso,status_no_curso,bolsa,tipo_ingresso,cr,estado_civil,ultimo_semestre_cursado,idade,ingresso,vest,idadenew,pc,ultimo) %>% summarise()

#///////////////////////////////////////CRIANDO A COLUNA PORCENTAGEM DE CONCLUSÃO /////////////////////////////////////////////////////////////////////////


xpgestao <- dados4 %>% filter(curso=="gestao comercial" & str_sub(ultimo,start=6,end=6)== "1"  ) %>% mutate(pc1=(pc-0.5)/2.5*100)
dados4 <- rbind(dados4,xpgestao)
xpgestao2 <- dados4 %>% filter(curso=="gestao comercial" & str_sub(ultimo,start=6,end=6)== "2"  ) %>% mutate(pc1=pc/2.5*100)
dados4 <- rbind(dados4,xpgestao2)







xpdireito <- dados4 %>% filter(curso=="direito" & str_sub(ultimo,start=6,end=6)== "1"  ) %>% mutate(pc1=(pc-0.5)/5*100)
dados4 <- rbind(dados4,xpdireito)
xpdireito2 <- dados4 %>% filter(curso=="direito" & str_sub(ultimo,start=6,end=6)== "2"  ) %>% mutate(pc1=pc/5*100)
dados4 <- rbind(dados4,xpdireito2)



xpagro <- dados4 %>% filter(curso=="agronegocio" & str_sub(ultimo,start=6,end=6)== "1"  ) %>% mutate(pc1=(pc-0.5)/3*100)
xpagro2 <- dados4 %>% filter(curso=="agronegocio" & str_sub(ultimo,start=6,end=6)== "2"  ) %>% mutate(pc1=pc/3*100)
dados4 <- rbind(dados4,xpagro)
dados4 <- rbind(dados4,xpagro2)



xpadmin <- dados4 %>% filter(curso=="administracao" & str_sub(ultimo,start=6,end=6)== "1"  ) %>% mutate(pc1=(pc-0.5)/4*100)
xpadmin2 <- dados4 %>% filter(curso=="administracao" & str_sub(ultimo,start=6,end=6)== "2"  ) %>% mutate(pc1=pc/4*100)
dados4 <- rbind(dados4,xpadmin)
dados4 <- rbind(dados4,xpadmin2)





xppedagogia <- dados4 %>% filter(curso=="pedagogia" & str_sub(ultimo,start=6,end=6)== "1"  ) %>% mutate(pc1=(pc-0.5)/4*100)
xppedagogia2 <- dados4 %>% filter(curso=="pedagogia" & str_sub(ultimo,start=6,end=6)== "2"  ) %>% mutate(pc1=pc/4*100)
dados4 <- rbind(dados4,xppedagogia)
dados4 <- rbind(dados4,xppedagogia2)


xpproducao <- dados4 %>% filter(curso=="producao publicitaria" & str_sub(ultimo,start=6,end=6)== "1"  ) %>% mutate(pc1=(pc-0.5)/2.5*100)
xpproducao2 <- dados4 %>% filter(curso=="producao publicitaria" & str_sub(ultimo,start=6,end=6)== "2"  ) %>% mutate(pc1=pc/2.5*100)
dados4 <- rbind(dados4,xpproducao)
dados4 <- rbind(dados4,xpproducao2)


xpsistemas<- dados4 %>% filter(curso=="sistemas de informacao" & str_sub(ultimo,start=6,end=6)== "1"  ) %>% mutate(pc1=(pc-0.5)/4*100)
xpsistemas2<- dados4 %>% filter(curso=="sistemas de informacao" & str_sub(ultimo,start=6,end=6)== "2"  ) %>% mutate(pc1=pc/4*100)
dados4 <- rbind(dados4,xpsistemas)
dados4 <- rbind(dados4,xpsistemas2)

xpcontabil<- dados4 %>% filter(curso=="ciencias contabeis" & str_sub(ultimo,start=6,end=6)== "1"  ) %>% mutate(pc1=(pc-0.5)/4*100)
xpcontabil2<- dados4 %>% filter(curso=="ciencias contabeis" & str_sub(ultimo,start=6,end=6)== "2"  ) %>% mutate(pc1=pc/4*100)
dados4 <- rbind(dados4,xpcontabil)
dados4 <- rbind(dados4,xpcontabil2)
dados4 <- dados4[complete.cases(dados4$pc1),]

dados4 <- dados4 %>% group_by(ra,cidade,sexo,curso,status_no_curso,bolsa,tipo_ingresso,cr,estado_civil,ultimo_semestre_cursado,idade,ingresso,vest,idadenew,pc,ultimo,pc1) %>% summarise()
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


#/////////////////////////TRANFORMANDO PENCENTUAL DE CONCLUSÃO EM VALOR CATEGORICO/////////////////////////////////////////////////////

dados4$percentual_conclusao[dados4$pc1<25] <- "menor que 25% de conclusao"
dados4$percentual_conclusao[dados4$pc1 >=25 & dados4$pc1<50] <- "de 25 e 49 % de conclusao"
dados4$percentual_conclusao[dados4$pc1>=50 & dados4$pc1<75] <- "de 50 e 74% de conclusao"
dados4$percentual_conclusao[dados4$pc1>=75 & dados4$pc1 <100] <- "de 75 a 99% de conclusao"
dados4$percentual_conclusao[dados4$pc1==100] <- "100% concluido"
dados4$percentual_conclusao[dados4$pc1>100 & dados4$pc1<=150] <- "de 100 a 150% de conclusão"
dados4$percentual_conclusao[dados4$pc1>150] <- "maior que 150% de conclusão"



#///////////////////NORMALIZANDO VALORES DA BOLSA/////////////////////////////

dados4$bolsa <- str_to_lower(dados4$bolsa)
dados4$bolsa <- rm_accent(dados4$bolsa)
dados4$bolsa <- str_trim(dados4$bolsa)
dados4$bolsa[str_sub(dados4$bolsa,start=1,end=18)=="bolsa pontualidade"] <- "bolsa pontualidade"
dados4$bolsa[dados4$tipo_ingresso=="ENEM"] <- "bolsa mérito enem" 
dados4$bolsa[str_sub(dados4$bolsa,start=1,end=16)=="bolsa reingresso"] <- "bolsa reingresso"
dados4$bolsa[dados4$bolsa=="bolsa fies antiga"] <- "fies"
dados4$bolsa[str_sub(dados4$bolsa,start=1,end=12)=="bolsa social"] <- "bolsa social"
dados4$bolsa[dados4$ra=="15.1.33937"] <- "bolsa de indicacao curso pedagogia"
dados4$bolsa[str_sub(dados4$bolsa,start=1,end=18)=="bolsa pontualidade"|str_sub(dados4$bolsa,start=1,end=14)=="bolsa aprovada"| str_sub(dados4$bolsa,start=1,end=8)=="bolsa 5%"|str_sub(dados4$bolsa,start=1,end=7)=="bolsa 1"|str_sub(dados4$bolsa,start=1,end=10)=="bolsa desc"] <- "sem bolsa"
dados4$bolsa[str_sub(dados4$bolsa,start=1,end=13)=="bolsa empresa"] <- "bolsas empresariais"
dados4$bolsa[str_sub(dados4$bolsa,start=1,end=13)=="institucional"] <- "bolsas institucionais"
dados4$bolsa[str_sub(dados4$bolsa,start=1,end=11)=="bolsa trans"] <- "bolsa transferencia de curso"
dados4$bolsa[str_sub(dados4$bolsa,start=1,end=11)=="bolsa prefe"] <- "bolsa prefeitura"


#////////////////////////////////////////////////////////////////////////////




df2 <- dados4
#dim(df2)
df2 <- subset(df2, status_no_curso!="JUBILADO")
#dim(df2)
#/////////////////////////////////
df2 <- df2 %>% group_by(ra,cidade,sexo,bolsa,curso,status_no_curso,tipo_ingresso,cr,estado_civil,ultimo_semestre_cursado,idade,ingresso,vest,idadenew,pc,ultimo,pc1,percentual_conclusao) %>% summarise()
df2 <- df2 %>% group_by(ra,cidade,sexo,curso,status_no_curso,tipo_ingresso,cr,estado_civil,ultimo_semestre_cursado,idade,ingresso,vest,idadenew,pc,ultimo,pc1,percentual_conclusao) %>% mutate(duplicado=n()>1)
View(df2[df2$duplicado,])


df1 <-  df2 %>% filter(duplicado & bolsa == "sem bolsa") %>% as.data.frame()
df2 <- anti_join(df2,df1)

#////////////////////////////////


df2 <- df2 %>% group_by(ra,cidade,sexo,bolsa,curso,status_no_curso,tipo_ingresso,cr,estado_civil,ultimo_semestre_cursado,idade,ingresso,vest,idadenew,pc,ultimo,pc1,percentual_conclusao) %>% summarise()
df2 <- df2 %>% group_by(ra,cidade,sexo,curso,status_no_curso,tipo_ingresso,cr,estado_civil,ultimo_semestre_cursado,idade,ingresso,vest,idadenew,pc,ultimo,pc1,percentual_conclusao) %>% mutate(duplicado=n()>1)
View(df2[df2$duplicado,])

df3 <- df2 %>% filter(duplicado & bolsa == "laurea maxima") %>% as.data.frame()
df2 <- anti_join(df2,df3)


#////////////////////////////////

df2$bolsa[df2$tipo_ingresso=="FIES"] <- "fies"
df2$bolsa[df2$tipo_ingresso=="PROUNI"] <- "prouni"
df2$bolsa[df2$bolsa=="pic"|df2$bolsa=="proex"] <- "bolsas institucionais"

#///////////////////////////////
df2 <- df2 %>% group_by(ra,cidade,sexo,bolsa,curso,status_no_curso,tipo_ingresso,cr,estado_civil,ultimo_semestre_cursado,idade,ingresso,vest,idadenew,pc,ultimo,pc1,percentual_conclusao) %>% summarise()
df2 <- df2 %>% group_by(ra,cidade,sexo,curso,status_no_curso,tipo_ingresso,cr,estado_civil,ultimo_semestre_cursado,idade,ingresso,vest,idadenew,pc,ultimo,pc1,percentual_conclusao) %>% mutate(duplicado=n()>1)
View(df2[df2$duplicado,])

df4 <- df2 %>% filter(duplicado & bolsa == "bolsa transferencia de curso") %>% as.data.frame()
df2 <- anti_join(df2,df4)

#///////////////////////////////
df2 <- df2 %>% group_by(ra,cidade,sexo,bolsa,curso,status_no_curso,tipo_ingresso,cr,estado_civil,ultimo_semestre_cursado,idade,ingresso,vest,idadenew,pc,ultimo,pc1,percentual_conclusao) %>% summarise()
df2 <- df2 %>% group_by(ra,cidade,sexo,curso,status_no_curso,tipo_ingresso,cr,estado_civil,ultimo_semestre_cursado,idade,ingresso,vest,idadenew,pc,ultimo,pc1,percentual_conclusao) %>% mutate(duplicado=n()>1)
View(df2[df2$duplicado,])

df5 <- df2 %>% filter(duplicado & bolsa == "fies") %>% as.data.frame()
df2 <- anti_join(df2,df5)

#///////////////////////////////
df2 <- df2 %>% group_by(ra,cidade,sexo,bolsa,curso,status_no_curso,tipo_ingresso,cr,estado_civil,ultimo_semestre_cursado,idade,ingresso,vest,idadenew,pc,ultimo,pc1,percentual_conclusao) %>% summarise()
df2 <- df2 %>% group_by(ra,cidade,sexo,curso,status_no_curso,tipo_ingresso,cr,estado_civil,ultimo_semestre_cursado,idade,ingresso,vest,idadenew,pc,ultimo,pc1,percentual_conclusao) %>% mutate(duplicado=n()>1)
View(df2[df2$duplicado,])

df6 <- df2 %>% filter(duplicado & bolsa == "bolsas institucionais") %>% as.data.frame()
df2 <- anti_join(df2,df6)

#//////////////////////////////
df2 <- df2 %>% group_by(ra,cidade,sexo,bolsa,curso,status_no_curso,tipo_ingresso,cr,estado_civil,ultimo_semestre_cursado,idade,ingresso,vest,idadenew,pc,ultimo,pc1,percentual_conclusao) %>% summarise()
df2 <- df2 %>% group_by(ra,cidade,sexo,curso,status_no_curso,tipo_ingresso,cr,estado_civil,ultimo_semestre_cursado,idade,ingresso,vest,idadenew,pc,ultimo,pc1,percentual_conclusao) %>% mutate(duplicado=n()>1)
View(df2[df2$duplicado,])

df7 <- df2 %>% filter(duplicado & bolsa == "bolsa monitoria") %>% as.data.frame()
df2 <- anti_join(df2,df7)

#/////////////////////////////
df2 <- df2 %>% group_by(ra,cidade,sexo,bolsa,curso,status_no_curso,tipo_ingresso,cr,estado_civil,ultimo_semestre_cursado,idade,ingresso,vest,idadenew,pc,ultimo,pc1,percentual_conclusao) %>% summarise()
df2 <- df2 %>% group_by(ra,cidade,sexo,curso,status_no_curso,tipo_ingresso,cr,estado_civil,ultimo_semestre_cursado,idade,ingresso,vest,idadenew,pc,ultimo,pc1,percentual_conclusao) %>% mutate(duplicado=n()>1)
View(df2[df2$duplicado,])

df8 <- df2 %>% filter(duplicado & bolsa == "bolsa egresso ") %>% as.data.frame()
df2 <- anti_join(df2,df8)

#/////////////////////////////
df2 <- df2 %>% group_by(ra,cidade,sexo,bolsa,curso,status_no_curso,tipo_ingresso,cr,estado_civil,ultimo_semestre_cursado,idade,ingresso,vest,idadenew,pc,ultimo,pc1,percentual_conclusao) %>% summarise()
df2 <- df2 %>% group_by(ra,cidade,sexo,curso,status_no_curso,tipo_ingresso,cr,estado_civil,ultimo_semestre_cursado,idade,ingresso,vest,idadenew,pc,ultimo,pc1,percentual_conclusao) %>% mutate(duplicado=n()>1)
View(df2[df2$duplicado,])

df9 <- df2 %>% filter(duplicado & bolsa == "bolsa funcionário") %>% as.data.frame()
df2 <- anti_join(df2,df9)

#///////////////////////////
df2 <- df2 %>% group_by(ra,cidade,sexo,bolsa,curso,status_no_curso,tipo_ingresso,cr,estado_civil,ultimo_semestre_cursado,idade,ingresso,vest,idadenew,pc,ultimo,pc1,percentual_conclusao) %>% summarise()
df2 <- df2 %>% group_by(ra,cidade,sexo,curso,status_no_curso,tipo_ingresso,cr,estado_civil,ultimo_semestre_cursado,idade,ingresso,vest,idadenew,pc,ultimo,pc1,percentual_conclusao) %>% mutate(duplicado=n()>1)
View(df2[df2$duplicado,])

df10 <- df2 %>% filter(duplicado & bolsa == "bolsa prefeitura") %>% as.data.frame()
df2 <- anti_join(df2,df10)

#//////////////////////////
df2 <- df2 %>% group_by(ra,cidade,sexo,bolsa,curso,status_no_curso,tipo_ingresso,cr,estado_civil,ultimo_semestre_cursado,idade,ingresso,vest,idadenew,pc,ultimo,pc1,percentual_conclusao) %>% summarise()
df2 <- df2 %>% group_by(ra,cidade,sexo,curso,status_no_curso,tipo_ingresso,cr,estado_civil,ultimo_semestre_cursado,idade,ingresso,vest,idadenew,pc,ultimo,pc1,percentual_conclusao) %>% mutate(duplicado=n()>1)
View(df2[df2$duplicado,])

df11 <- df2 %>% filter(duplicado & bolsa == "bolsa social") %>% as.data.frame()
df2 <- anti_join(df2,df11)

#/////////////////////////
df2 <- df2 %>% group_by(ra,cidade,sexo,bolsa,curso,status_no_curso,tipo_ingresso,cr,estado_civil,ultimo_semestre_cursado,idade,ingresso,vest,idadenew,pc,ultimo,pc1,percentual_conclusao) %>% summarise()
df2 <- df2 %>% group_by(ra,cidade,sexo,curso,status_no_curso,tipo_ingresso,cr,estado_civil,ultimo_semestre_cursado,idade,ingresso,vest,idadenew,pc,ultimo,pc1,percentual_conclusao) %>% mutate(duplicado=n()>1)
View(df2[df2$duplicado,])

df12 <- df2 %>% filter(duplicado & bolsa == "bolsa reingresso") %>% as.data.frame()
df2 <- anti_join(df2,df12)

#/////////////////////////
df2 <- df2 %>% group_by(ra,cidade,sexo,bolsa,curso,status_no_curso,tipo_ingresso,cr,estado_civil,ultimo_semestre_cursado,idade,ingresso,vest,idadenew,pc,ultimo,pc1,percentual_conclusao) %>% summarise()
df2 <- df2 %>% group_by(ra,cidade,sexo,curso,status_no_curso,tipo_ingresso,cr,estado_civil,ultimo_semestre_cursado,idade,ingresso,vest,idadenew,pc,ultimo,pc1,percentual_conclusao) %>% mutate(duplicado=n()>1)
View(df2[df2$duplicado,])

df13 <- df2 %>% filter(duplicado & bolsa == "bolsa integral") %>% as.data.frame()
df2 <- anti_join(df2,df13)

#////////////////////////
df2 <- df2 %>% group_by(ra,cidade,sexo,bolsa,curso,status_no_curso,tipo_ingresso,cr,estado_civil,ultimo_semestre_cursado,idade,ingresso,vest,idadenew,pc,ultimo,pc1,percentual_conclusao) %>% summarise()
df2 <- df2 %>% group_by(ra,cidade,sexo,curso,status_no_curso,tipo_ingresso,cr,estado_civil,ultimo_semestre_cursado,idade,ingresso,vest,idadenew,pc,ultimo,pc1,percentual_conclusao) %>% mutate(duplicado=n()>1)
View(df2[df2$duplicado,])

df2 <- df2 %>% group_by(ra,curso,cidade,sexo,status_no_curso,bolsa,tipo_ingresso,cr,estado_civil,idade,vest,ingresso,idadenew) %>% mutate(duplicado=n()>1)
df14 <- df2 %>% filter(duplicado & pc1new == pc1) %>% as.data.frame()
df2 <- anti_join(df2,df14)

#///////////////////////
df2 <- df2 %>% group_by(ra,cidade,sexo,bolsa,curso,status_no_curso,tipo_ingresso,cr,estado_civil,ultimo_semestre_cursado,idade,ingresso,vest,idadenew,pc,ultimo,pc1,percentual_conclusao) %>% summarise()
df2 <- df2 %>% group_by(ra,cidade,sexo,bolsa,curso,status_no_curso,tipo_ingresso,cr,estado_civil,ultimo_semestre_cursado,idade,ingresso,idadenew,pc,ultimo,pc1,percentual_conclusao) %>% mutate(duplicado=n()>1)
View(df2[df2$duplicado,])

df15 <- df2 %>% filter(duplicado & vest == "ruim") %>% as.data.frame()
df2 <- anti_join(df2,df15)

#//////////////////////
df2 <- df2 %>% group_by(ra,cidade,sexo,bolsa,curso,status_no_curso,tipo_ingresso,cr,estado_civil,ultimo_semestre_cursado,idade,ingresso,vest,idadenew,pc,ultimo,pc1,percentual_conclusao) %>% summarise()
df2 <- df2 %>% group_by(ra,cidade,sexo,bolsa,curso,tipo_ingresso,cr,estado_civil,ultimo_semestre_cursado,idade,ingresso,vest,idadenew,pc,ultimo,pc1,percentual_conclusao) %>% mutate(duplicado=n()>1)
View(df2[df2$duplicado,])

df16 <- df2 %>% filter(duplicado & status_no_curso=="MATRICULADO" & pc1>100) %>% as.data.frame()
df2 <- anti_join(df2,df16)


#////////////////////
df2 <- df2 %>% group_by(ra,cidade,sexo,bolsa,curso,status_no_curso,tipo_ingresso,cr,estado_civil,ultimo_semestre_cursado,idade,ingresso,vest,idadenew,pc,ultimo,pc1,percentual_conclusao) %>% summarise()
df2 <- df2 %>% group_by(ra,cidade,sexo,curso,status_no_curso,tipo_ingresso,cr,estado_civil,ultimo_semestre_cursado,idade,ingresso,vest,idadenew,pc,ultimo,pc1,percentual_conclusao) %>% mutate(duplicado=n()>1)
View(df2[df2$duplicado,])

df17 <- df2 %>% filter(duplicado & bolsa == "bolsa educa mais brasil") %>% as.data.frame()
df2 <- anti_join(df2,df17)

#//////////////////
df2 <- df2 %>% group_by(ra,cidade,sexo,bolsa,curso,status_no_curso,tipo_ingresso,cr,estado_civil,ultimo_semestre_cursado,idade,ingresso,vest,idadenew,pc,ultimo,pc1,percentual_conclusao) %>% summarise()
df2 <- df2 %>% group_by(ra,cidade,sexo,curso,status_no_curso,tipo_ingresso,cr,estado_civil,ultimo_semestre_cursado,idade,ingresso,vest,idadenew,pc,ultimo,pc1,percentual_conclusao) %>% mutate(duplicado=n()>1)
View(df2[df2$duplicado,])

df18 <- df2 %>% filter(duplicado & bolsa == "prouni") %>% as.data.frame()
df2 <- anti_join(df2,df18)


df2 <- df2 %>% group_by(ra,cidade,sexo,bolsa,curso,status_no_curso,tipo_ingresso,cr,estado_civil,ultimo_semestre_cursado,idade,ingresso,vest,idadenew,pc,ultimo,pc1,percentual_conclusao) %>% summarise()
































df2$bolsa[df2$ra=="13.1.05614"] <- "prouni"

df2$bolsa[df2$ra=="15.1.35619"] <- "bolsa estagio"
df2$bolsa[df2$ra=="18.1.56489"] <- "bolsa educa mais brasil"
df2$bolsa[df2$ra=="18.1.56497"] <- "bolsa quero bolsa"
df2$bolsa[df2$ra=="19.1.59276"] <- "prouni"












