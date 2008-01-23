`classical` <-
function(data){
	p <- colMeans(data) # p-values
	b <- pb(data)       # point biserial
	data.frame(p_values = p, Point_Biserial = b)
 }

