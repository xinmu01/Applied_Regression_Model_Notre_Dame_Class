# generate a random sample
# direct methods
# discrete RV
# example 1: # generate discrete RV X with P(X=i)=0.1*i, i=1,2,3,4.
x <- 1:4          # define x values taking probabilities
p <- 0.1 * 1:4    # define probabilities
cp <- cumsum(p)  # cumulative sum of probabilities
cp
U <- runif(1)  # draw uniform random number
ii <- 1;
while (U > cp[ii]) {
  ii <- ii + 1
}
xi <- x[ii]
c(U, xi)  # generated u and xi
# example 2: # generate binomial samples
# X from binom(10,.8). # of trials is 10, probability of sucess is 0.8
# draw 10 uniform random numbers
m <- 10
p <- 0.8
U <- runif(m)  # draw m uniform random number
X <- sum( (U < p) )
X

# or in one step
X <- sum(runif(m) < p)
X
n <- 20
m <- 10
p <- 0.8
U <- matrix(runif(m*n), nrow=n)  # draw uniform random numbers
X <- apply(U < p, MARGIN=1, sum)
X
# indirect method
# Rejection sampling diagram
# example 3: Beta distribution with Uniform envelope
x <- seq(0, 1, length=2e2)
y.u <- dunif(x, 0, 1)*3
y.b <- dbeta(x, 6, 3)
plot(x, y.u, type="l", col = "blue", lwd=3, xlim=range(x), ylim=c(0,3),
      main="Rejection sampling diagram", xlab = "x", ylab = "y")
abline(h = 0, col = "gray75")
points(x, y.b, type="l", col = "red", lwd=5)
lines(x=c(0.6,0.6), y=c(0,3))
text(x=0.6, y=2.5, labels="reject", pos=2)
text(x=0.6, y=0.5, labels="accept", pos=4)
text(x=0.15, y=2.9, labels="e(x) = 3 * Uniform(0,1)", pos=1, col="blue")
text(x=0.15, y=0.1, labels="f(x) = Beta(6, 3)", pos=3, col = "red")
# data example for rejection sampling
R <- 1e5                # number of rejection samples
e <- runif(R,0,1)       # sample from enveloping function
accept <- rep("No", R)  # initialize samples as "No" accept
U <- runif(R, 0, 1)     # sample from uniform distribution

# accept if the height of the envelope is less than the height of the function
#   e(Y) * U * scale <= f(Y)
accept[ ( dunif(e, 0, 1) * U * 3 <= dbeta(e, 6, 3) ) ] <- "Yes"

# put into a data.frame for plotting
sam <- data.frame(e, accept = factor(accept, levels = c("Yes","No")))

# plot a stacked histogram
library(ggplot2)
p <- ggplot(sam, aes(x = e))
p <- p + geom_histogram(aes(fill = accept), binwidth = 1/101)
print(p)


