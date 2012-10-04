# TODO: Add comment
# 
# Author: tao.xu
###############################################################################


rst = NULL;
ltmp = NULL
for(i in 1:209){
	belowlod = which(data[[i]]$lborres == "<LOD")
	d = (data[[i]]$lbdrvres) 
	d [belowlod ]= NA
	tmp = tapply (d, INDEX = interaction(data[[i]]$usubjid, data[[i]]$visitnum, data[[i]]$lbtptnum), function(x) {mean(x, na.rm = T)})
	#ltmp = c(ltmp, length(tmp))
	tmp = data.frame(id = names(tmp), tmp)
	colnames(tmp) = c("id", as.vector(data[[i]]$lbtest)[1])
	if(i == 1){
		rst= tmp
	}
	else {
		rst = merge(rst, tmp, by = "id", all = T)
	}
#	rst = cbind(rst, tmp)
}

rst$id = apply(rst, 1, function(x) substr(x[1], 1, 16))
visit = rep(c(rep(1,4), rep(3, 4)), 46)
exercise = rep(1:4, 46*2)

tmp = merge(rst, demographics, by.x = "id", by.y = "experiment")

unlist(lapply(data, function(x) x$usubjid[1]))

lapply(data, dim)


