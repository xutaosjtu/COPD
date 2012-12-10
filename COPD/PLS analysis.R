# TODO: Add comment
# 
# Author: tao.xu
###############################################################################

COPD.data$visit = rep(c(rep(1,4), rep(3, 4)), 46)
COPD.data$exercise = rep(c(1:4), 46*2)

require(caret)
require(pls)
require(gplots)

#tmp = na.omit(tmp)

#tmp = na.omit(tmp)
#tmppls = plsda(as.matrix(tmp), as.factor(data$COPD), ncomp = 3)
#PLS of COPD
data = data.frame(data.normalized, COPD = as.numeric(COPD.data$COPD), exercise = as.numeric(COPD.data$exercise))

subset= which(COPD.data$COPD == "C=N")
S4pls<-plsr(exercise ~ . , data=data, subset = subset)#, subset = which(COPD.data$exercise == 3)
S4pls<-plsda(x=data.normalized, COPD.data$exercise, ncomp = 10)


#color = rep(0, dim(data)[1])
color = greenred(24)[c(c(4:1)*2,c(18,20, 22, 24)) ]
#plot(S4pls$scores[,1:2],col=color[c(4,8)][interaction(COPD.data$exercise[which(COPD.data$exercise == 1)],COPD.data$COPD[which(COPD.data$exercise == 1)])], pch=c(17, 19)[COPD.data$SEX[which(COPD.data$exercise == 1)]], main = "PLS plot of COPD vs non COPD")
pdf("Exercise non-COPD.pdf", height = 5, width = 10)
plot(S4pls$scores[,c(1,2)],col = color[interaction(COPD.data$exercise[subset],COPD.data$COPD[subset])], pch=c(17, 19)[COPD.data$SEX[which(COPD.data$exercise == 1)]], main = "Exercise: NonCOPD", xlim = c(-6, 4))
dev.off()

#PLS of exercises
data = data.frame(data.normalized, exercise = COPD.data$exercise)

S4pls<-plsr(exercise ~ . , data=data)

plot(S4pls$scores[,1:3],col=color[interaction(COPD.data$exercise,COPD.data$COPD)], pch=c(17, 19)[COPD.data$SEX], main = "PLS plot of different exercise stage")



data = data.frame(data.normalized, exercise_COPD = as.numeric(interaction(COPD.data$exercise,COPD.data$COPD)))

S4pls<-plsr(exercise_COPD ~ . , data=data)

plot(S4pls$scores[,1:3],col=color[data$exercise_COPD], pch=c(17, 19)[COPD.data$SEX], main = "PLS plot of exercise COPD interaction")

################	explained variance of outcome	################
yve <- 100 * drop(R2(S4pls, estimate = "train", intercept = FALSE)$val) 























