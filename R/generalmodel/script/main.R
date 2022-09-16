

n=1000;

k=0.7;
m=2;

alpha=0.5;
sigma=0.07


v<-rnbinom(n=n,size=k,mu=m);

output <- matrix(ncol=4, nrow=n);
for (i in (1:n)) {
  output[i,1] <- v[i]
  output[i,2] <- rbinom(1,v[i],alpha);
  output[i,3] <- v[i]-output[i,2];
  if (output[i,2]>0) {
    output[i,4] <- output[i,3]*(exp(-sigma))^(v[i]-1)
  } else output[i,4] <- 0
    
}




output <- data.frame(output)
write.table(output, file = "simucarga.txt", sep = "\t",row.names = F, col.names = T)