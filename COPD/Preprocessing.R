# TODO: Add comment
# 
# Author: tao.xu
###############################################################################


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
visit
exercise

index = which(colnames(COPD.data) %in% c("id",mode1))
tmp = COPD.data[,-index]
tmp = as.matrix(tmp)
tmp.na = apply(tmp, 2, function(x) length(which(is.na(x))))
tmp = tmp[, which(tmp.na<100)]
tmp = apply(tmp, 2, function(x) {
			x[which(is.na(x))] <- min(x, na.rm = T)
			return(x)
		}
)
tmp = log(tmp)
data.normalized = tmp