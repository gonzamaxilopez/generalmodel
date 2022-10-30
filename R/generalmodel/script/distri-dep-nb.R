

n=1000;#host population size

k=0.7;
m=seq(0.01,100, by=0.01) ;

alpha=0.574;
sigma=0.08
#z=exp(-sigma);

output2 <- matrix(ncol=5, nrow=length(m));

for (j in (1:length(m))) {
  v<-rnbinom(n=n,size=k,mu=m[j]);
  output <- matrix(ncol=5, nrow=n);
  s1 <- 0;
  s2 <- 0;
  for (i in (1:n)) {
    output[i,1] <- v[i]#carga parásitaria
    output[i,2] <- rbinom(1,v[i],1-alpha);#carga de machos
    output[i,3] <- v[i]-output[i,2];#carga de hembras
    
    
    egg <- rpois(1, output[i,3]*(exp(-sigma))^(v[i]-1));# egg production 
    output[i,4] <- egg;#output[i,3]*(exp(-sigma))^(v[i]-1);#producción de huevos
    if (output[i,2]>0) {
      output[i,5] <- egg;#output[i,3]*(exp(-sigma))^(v[i]-1);#producción de huevos fertiles
    } else output[i,5] <- 0;
  
    s1 <- s1+output[i,4];
    s2 <- s2+output[i,5];
  }
  output2[j,1] <- m[j];# mean parasite burden per host
  output2[j,2] <- s1/n;# mean egg production per host 
  output2[j,3] <- s2/n;# mean infective egg production per host
  output2[j,4] <- s2/s1;# mating probability
  output2[j,5] <- s1/(n*alpha*m[j]) # Mean effective contribution per female parasite  
}



output <- data.frame(output)
write.table(output2, file = "simucarga-dep-nb.txt", sep = "\t",row.names = FALSE, col.names = T)