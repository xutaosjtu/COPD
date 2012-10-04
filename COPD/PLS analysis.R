# TODO: Add comment
# 
# Author: tao.xu
###############################################################################

data$visit = rep(c(rep(1,4), rep(3, 4)), 46)
data$exercise = rep(c(1:4), 46*2)

require(caret)
require(pls)

mode1 = scan(what = character())
COPD
SEX
AGE
BMI
WEIGHT
PACKYEARS
WPEAKRT_SCR
FEV1_SCR_PRE
FEV1_SCR_POST
FEV1FVC_SCR_PRE
FEV1FVC_SCR_POST
FEV1_PP_SCR_PRE
FEV1_PP_SCR_POST
FVC_SCR_PRE
FVC_SCR_POST

index = which(colnames(data) %in% c("id",mode1))
tmp = data[,-index]
tmp = as.matrix(tmp)
tmp.na = apply(tmp, 2, function(x) length(which(is.na(x))))
tmp = tmp[, which(tmp.na<100)]
tmp = apply(tmp, 2, function(x) {
			x[which(is.na(x))] <- min(x, na.rm = T)
			return(x)
		}
)
tmp = log(tmp)
#tmp = na.omit(tmp)

#tmp = na.omit(tmp)
#tmppls = plsda(as.matrix(tmp), as.factor(data$COPD), ncomp = 3)
tmp = data.frame(tmp, COPD = as.numeric(data$COPD))

S4pls<-plsr(COPD ~ . , data=tmp)

color = rep(0, dim(data)[1])
color = greenred(24)[c(c(1:4)*2,c(18,20, 22, 24)) ]
plot(S4pls$scores[,1:3],col=color[interaction(data$exercise,data$COPD)], pch=19, main = "PLS plot of COPD vs non COPD")
#plot(S4pls$scores[,1:2],col = data$COPD)



tmp = data.frame(tmp, exercise = data$exercise)

S4pls<-plsr(exercise ~ . , data=tmp)

plot(S4pls$scores[,1:3],col=color[interaction(data$exercise,data$COPD)], pch=19, main = "PLS plot of different exercise stage")



tmp = data.frame(tmp, exercise_COPD = as.numeric(interaction(data$exercise,data$COPD)))

S4pls<-plsr(exercise_COPD ~ . , data=tmp)

plot(S4pls$scores[,1:3],col=color[interaction(data$exercise,data$COPD)], pch=19, main = "PLS plot of exercise COPD interaction")


























