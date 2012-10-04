# TODO: Add comment
# 
# Author: tao.xu
###############################################################################

require(caret)

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
tmp = data.frame(tmp, COPD = as.numeric(data$COPD))

#tmp = na.omit(tmp)

tmppls = plsda(as.matrix(tmp), as.factor(data$COPD), ncomp = 3)

S4pls<-plsr(COPD ~ . , data=tmp)
plot(S4pls$scores,col=interaction(data$COPD, data$visit, data$exercise ),pch=19)
plot(S4pls$scores,col = data$COPD)