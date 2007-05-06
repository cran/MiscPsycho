`rasch` <-
function(b,theta){
   1 / (1 + exp(outer(b,theta,'-')))
   }

