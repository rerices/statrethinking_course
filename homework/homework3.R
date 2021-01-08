library(rethinking)
library(dagitty)
data(foxes)
d <- foxes

# Scaling of Variables

d$W <- scale(foxes$weight)
d$A <- scale(foxes$area)

# Question 1

m3.1 <- quap(
    alist(
        W ~ dnorm(mu, sigma),
        mu <- a + bA*A,
        a ~ dnorm(0, 0.2),
        bA ~ dnorm(0, 0.5),
        sigma ~ dexp(1) 
    ), data = d
)

prior <- extract.prior(m3.1)
mu <- link(m3.1, post = prior, data = list(A = c(-2,2)))
plot(NULL, xlim = c(-2,2), ylim = c(-2,2))
for(i in 1:50) lines(c(-2,2), mu[i,], col = col.alpha("black", 0.4) )
