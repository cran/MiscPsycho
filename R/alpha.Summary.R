`alpha.Summary` <-
function(columns){
result <- numeric(ncol(columns))
n      <- ncol(columns)
for(i in 1:length(columns)){
result[i] <- alpha(columns[-i])
dat <- data.frame(Item = 1:n, alpha = result)
}
cat("Below is what alpha *would be* if the item were removed", '\n', '\n')
dat
}

