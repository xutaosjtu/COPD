# TODO: Add comment
# 
# Author: xutao
###############################################################################


require("nlme")

model1 = c("SEX", "AGE", "BMI")
model2 = c(model1, "PACKYEARS")
model3 = c(model2, "FEV1_SCR_PRE","FEV1FVC_SCR_PRE")

metabolites = colnames(data.normalized)
data = data.frame( COPD = COPD.data$COPD, COPD.data[, c(model1, "visit", "id", "exercise")])

rst.Disease = NULL
rst.interaction = NULL
for(metabo in metabolites){
	metabo = data.normalized[ which(data$exercise == 3),metabo] - data.normalized[ which(data$exercise == 4),metabo]
	
	model = lme( metabo ~ COPD * SEX  + AGE + BMI, #+ PACKYEARS + FEV1_SCR_PRE + FEV1FVC_SCR_PRE, 
			data = data[which(data$exercise == 1),],
			na.action = na.omit,
			random = ~1|id/visit, 
			#subset = which(data$exercise == 1)
	)
	
	model.anova = anova(model)
	rst.Disease = rbind(rst.Disease,model.anova[2,])
	rst.interaction = rbind(rst.interaction, model.anova[dim(model.anova)[1],])
}
rownames(rst.Disease) = metabolites;
rownames(rst.interaction) = metabolites;

write.csv(rst.Disease, file = "disease differences TP4_TP3_model 3.csv")
write.csv(rst.interaction, file = "disease sex interaction at TP4_TP3_model 3.csv")
