

n=1000;

k=0.7;
m=seq(0.01,100, by=0.01) ;

alpha=0.574;
beta=1-alpha;
sigma=0.08

output2 <- matrix(ncol=5, nrow=length(m));

for (j in (1:length(m))) {
  output <- matrix(ncol=5, nrow=n);
  s1 <- 0;
  s2 <- 0;
  for (i in (1:n)) {
    output[i,2] <- rnbinom(n=1,size=beta*k,mu=beta*m[j]);#carga de machos
    output[i,3] <- rnbinom(n=1,size=alpha*k,mu=alpha*m[j]);#carga de hembras
    output[i,1] <- output[i,2]+output[i,3]#carga parásitaria
    output[i,4] <- output[i,3]*(exp(-sigma))^(output[i,1]-1);#producción de huevos
    if (output[i,2]>0) {
      output[i,5] <- output[i,3]*(exp(-sigma))^(output[i,1]-1);#producción de huevos fertiles
    } else output[i,5] <- 0;
    
    s1 <- s1+output[i,4];
    s2 <- s2+output[i,5];
  }
  output2[j,1] <- m[j]; 
  output2[j,2] <- s1/n;
  output2[j,3] <- s2/n;
  output2[j,4] <- s2/s1;
  output2[j,5] <- s1/(n*alpha*m[j])   
}



output <- data.frame(output)
write.table(output2, file = "simucarga-ind.txt", sep = "\t",row.names = F, col.names = T)