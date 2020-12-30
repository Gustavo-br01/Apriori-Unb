
df_tratado <- as.data.frame(df_tratado)



trans <- as(df_tratado,"transactions")


itemFrequencyPlot(trans, top=30)



regras <- apriori(trans, parameter = list(supp = 0.012, conf = 0.8,target="rules"),
                  appearance = list (default="lhs",rhs="status_no_curso=ABANDONO"))




subsetRules <- which(colSums(is.subset(regras, regras)) > 1) # get subset rules in vector
length(subsetRules)  #> 3913
regras <- regras[-subsetRules] # remove subset rules. 


inspect(regras)


plot(regras,method = "doubledecker")
