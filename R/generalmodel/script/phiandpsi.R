# plot of psi and phi
m<-seq(0.01,100,0.01);
k<-0.7;
gama<-0.08;
z<-exp(-gama);
pi<-0.3;

a<-0.574;
b<-1-a;

psipo<- exp(m*(z-1));
phipo<- 1-exp(-1*m*z*b); 

psinb<-(1-(m/k)*(z-1))^(-k-1);
phinb<-1-((1-(m/k)*(a*z-1))/(1-(m/k)*(z-1)))^(-k-1); 

phinb_ind<-1-((1+(m/k))/(1-(m/k)*(z-1)))^(-b*k); 

#psizig<-(1-(m/(1-pi))*(z-1))^(-2);
#phizig<-1-((1-(m/(1-pi))*(a*z-1))/(1-(m/(1-pi))*(z-1)))^(-2); 

psizip<-exp((m/(1-pi))*(z-1));
phizip<-1-exp(-1*(m/(1-pi))*(z*b)); 

psizinb<-(1-(m/(k*(1-pi)))*(z-1))^(-k-1);
phizinb<-1-((1-(m/(k*(1-pi)))*(a*z-1))/(1-(m/(k*(1-pi)))*(z-1)))^(-k-1); 



plot(m,psipo)
plot(m,psizip)
plot(m,phizip)

#output
outdata<-data.frame(m=m,psipo=psipo,phipo=phipo,psinb=psinb,phinb=phinb,phinb_ind=phinb_ind,psizip=psizip,phizip=phizip,psizinb=psizinb,phizinb=phizinb)
write.table(outdata, file = "plotphiandpsi(a=0.574).txt", sep = "\t",row.names = F, col.names = T)

