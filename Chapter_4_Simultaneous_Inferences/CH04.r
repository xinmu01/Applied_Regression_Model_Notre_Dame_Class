# Compare the critical values used in the Bonferroni, Scheffe,
# and working-hotelling procedure for n= 20, 50 and g ranged from 2 to 10

g=seq(2,10,1)
n=c(20,50)
gn=expand.grid(g,n)
alpha=0.05
B= qt(1-alpha/(2*gn[,1]),gn[,2]-2)
W= sqrt(2*qf(1-alpha,2,gn[,2]-2))
S= sqrt(gn[,1]*qf(1-alpha,gn[,1],gn[,2]-2))

## These plots show that the critical value B are going to be larger when g is larger. This means the 
## Bonferroni method is more conservative
par(mfrow=c(1,2),mar=c(2,2,1,1))
for(i in 1:length(n)){
  plot(g,B[gn[,2]==n[i]],type="l", ylim=c(2,5),lwd=2,lty=1)
  lines(g,W[gn[,2]==n[i]],type="l", col='red',lwd=2,lty=2)
  lines(g,S[gn[,2]==n[i]],type="l", col='blue',lwd=2,lty=3)
  legend(2,5,c("B","WH","S"),lty=1:3,lwd=c(2,2,2),col=c('black','red','blue'))
}
