`like.mat` <-
function(x,b,theta){
   rasch(b, theta)^x * (1 - rasch(b,theta))^(1-x)
   }

