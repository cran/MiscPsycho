plot.jml <- function(x, ask = TRUE, all = TRUE, item, ...){
	par(ask = ask)
	xvals <- seq(from=-5, to =5, by=.01)
	params <- coef(x)
	L <- length(params)
	tmp <- data.frame(x$model.frame)
	tmp$Raw.Score <- rowSums(tmp)
	mle <- summary(scoreCon(coef(x)))$coef[c(1,3)]
	tmp <- merge(tmp, mle, by='Raw.Score', sort = FALSE)
	if(all){
		for(i in 1:L ){
			exp <- 1/(1 + exp(params[i] - xvals))
			tmp2 <- data.frame(prop.table(table(tmp[, i+1], tmp$Estimate), margin=2))
			prop.correct <- tmp2[tmp2[,1]==1, 2:3]
			names(prop.correct)[1:2] <- c('Estimate', 'prop')
			prop.correct$Estimate <- as.numeric(levels(prop.correct$Estimate))
			plot(xvals, exp, type='l', ylim = c(0,1), xlab = 'Ability', ylab = 'Proportion Correct', ...)
			points(prop.correct$Estimate, prop.correct$prop, ...)
			title(main = paste("Plot of Item", i))
		}
	} else {
		par(ask = FALSE)
		i <- item
		exp <- 1/(1 + exp(params[i] - xvals))
		tmp2 <- data.frame(prop.table(table(tmp[, i+1], tmp$Estimate), margin=2))
		prop.correct <- tmp2[tmp2[,1]==1, 2:3]
		names(prop.correct)[1:2] <- c('Estimate', 'prop')
		prop.correct$Estimate <- as.numeric(levels(prop.correct$Estimate))
		plot(xvals, exp, type='l', ylim = c(0,1), xlab = 'Ability', ylab = 'Proportion Correct', ...)
		points(prop.correct$Estimate, prop.correct$prop, ...)
		title(main = paste("Plot of Item", i))
	}	
	par(ask = FALSE)
}
