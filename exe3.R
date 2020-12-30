
df2 <- subset(df2,status_no_curso!="TRANSFERÊNCIA INTERNA")








df2 <- df2 %>% group_by(ra,cidade,sexo,bolsa,curso,status_no_curso,tipo_ingresso,cr,estado_civil,ultimo_semestre_cursado,ingresso,vest,idadenew,pc,pc1,percentual_conclusao) %>% summarise()
df2 <- df2 %>% group_by(ra) %>% mutate(duplicado=n()>1,mini=min(pc1))
View(df2[df2$duplicado,])
df16 <- df2 %>% filter(duplicado & status_no_curso=="MATRICULADO" & pc1>100) %>% as.data.frame()
df2 <- anti_join(df2,df16)


df2 <- df2 %>% group_by(ra,cidade,sexo,bolsa,curso,status_no_curso,tipo_ingresso,cr,estado_civil,ultimo_semestre_cursado,ingresso,vest,idadenew,pc,pc1,percentual_conclusao) %>% summarise()
df2 <- df2 %>% group_by(ra) %>% mutate(duplicado=n()>1,mini=min(pc1))
View(df2[df2$duplicado,])












dfduplicado <- df2[df2$duplicado,]
dfduplicado1<- dfduplicado[c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,36,41,43,45,47,49,51,53,55,57,59,61,63,65,67,69,71,75,77,79,81,85,87,89,93,95,97,99,101,103,105,107,109,111,
                             113,115,117,119,121,123,125,127,129,131,137,139,141,143,145,147,149,151,154,156,158,160,162,164,170),]
df2 <- anti_join(df2,dfduplicado1)

df2 <- df2 %>% group_by(ra,cidade,sexo,bolsa,curso,status_no_curso,tipo_ingresso,cr,estado_civil,ultimo_semestre_cursado,ingresso,vest,idadenew,pc,pc1,percentual_conclusao) %>% summarise()
df2 <- df2 %>% group_by(ra) %>% mutate(duplicado=n()>1,mini=min(pc1))
View(df2[df2$duplicado,])
df2 <- df2 %>% group_by(ra,cidade,sexo,bolsa,curso,status_no_curso,tipo_ingresso,cr,estado_civil,ultimo_semestre_cursado,ingresso,vest,idadenew,pc,pc1,percentual_conclusao) %>% summarise()
df2$cidade <- as.character(df2$cidade)
df2$cidade[df2$cidade=="acailandia" | df2$cidade=="alto parnaiba" | df2$cidade=="amarante do maranhao" | df2$cidade=="baixa grande do ribeiro" |df2$cidade=="bom jesus" |df2$cidade=="buriticupu" |df2$cidade=="codo" |df2$cidade=="grajau" |df2$cidade=="mirador" |df2$cidade=="nova iorque" | df2$cidade=="pastos bons" |df2$cidade=="porto franco" |df2$cidade=="santa filomena" | df2$cidade=="santa ines" | df2$cidade=="sao bento"  | df2$cidade=="sao luis" | df2$cidade=="sao pedro dos crentes" | df2$cidade=="teresina"| df2$cidade=="6"  ] <- "balsas"
df2$cidade[df2$cidade=="arame" | df2$cidade=="sao domingos do azeitao" | df2$cidade=="coroata" |df2$cidade=="estreito" | df2$cidade=="fazenda"] <- "balsas"
df2 <- droplevels(df2)












df2$cr <- as.character(df2$cr)
df2$cr <- as.numeric(df2$cr)
df2$cr1[df2$cr<60] <- "ruim"
df2$cr1[df2$cr>=60 & df2$cr<70] <- "razoavel"
df2$cr1[df2$cr>=70 & df2$cr<80] <- "bom"
df2$cr1[df2$cr>=80] <- "excelente"
df_tratado <- df2[,-c(1,8,10,14,15)]
df_tratado$cidade <- as.factor(df_tratado$cidade)
df_tratado$bolsa <- as.factor(df_tratado$bolsa)
df_tratado$ingresso <- as.factor(df_tratado$ingresso)
df_tratado$percentual_conclusao <- as.factor(df_tratado$percentual_conclusao)
df_tratado$cr1 <- as.factor(df_tratado$cr1)






































df18new <- unique(df17new)


df22 <- df22 %>% group_by(ra,sexo,status_no_curso,bolsa,tipo_ingresso,cr,estado_civil,idade,ingresso,vest,idadenew,pc1,percentual_conclusao,mini,transferencia_interna) %>% mutate(cont=n()>1)
df18 <- df22 %>% filter(cont & cr1=="bom")
df22 <- anti_join(df22,df18)

