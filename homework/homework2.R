library(rethinking)
## Question 1

data(Howell1)
d <- Howell1
d2 <- d[ d$age >= 18 , ]
xbar <- mean(d2$weight)
m4.3 <- quap(
    alist(
        height ~ dnorm( mu , sigma ) ,
        mu <- a + b*( weight - xbar ) ,
        a ~ dnorm( 178 , 20 ) ,
        b ~ dlnorm( 0 , 1 ) ,
        sigma ~ dunif( 0 , 50 )
    ) ,
    data=d2 )

dat <- data.frame( weight=c(45,40,65,31,53) )
h_sim <- sim( m4.3 , data=dat )
Eh <- apply(h_sim,2,mean)
h_ci <- apply(h_sim,2,PI,prob=0.89)

dat$Eh <- Eh
dat$L89 <- h_ci[1,]
dat$U89 <- h_ci[2,]
round(dat,1)

## Question 2

d$log_weight <- log(d$weight)
xbar <- mean(d$log_weight)
m2 <- quap(
    alist(
        height ~ dnorm( mu , sigma ) ,
        mu <- a + b*( log_weight - xbar ) ,
        a ~ dnorm( 178 , 20 ) ,
        b ~ dlnorm( 0 , 1 ) ,
        sigma ~ dunif( 0 , 50 )
    ) ,
    data=d )
plot( d$weight , d$height , col=col.alpha(rangi2,0.7) )
x_seq <- log(1:60)
mu <- sim( m2 , data=list(log_weight=x_seq) )
mu_mean <- apply(mu,2,mean)
mu_ci <- apply(mu,2,PI,0.99)
lines( exp(x_seq) , mu_mean )
shade( mu_ci , exp(x_seq) )


## Question 3

library(rethinking)
data(Howell1)
d <- Howell1
d$weight_s <- ( d$weight - mean(d$weight) )/sd(d$weight)
d$weight_s2 <- d$weight_s^2
m4.5 <- quap(
    alist(
        height ~ dnorm( mu , sigma ) ,
        mu <- a + b1*weight_s + b2*weight_s2 ,
        a ~ dnorm( 178 , 20 ) ,
        b1 ~ dlnorm( 0 , 1 ) ,
        b2 ~ dnorm( 0 , 1 ) ,
        sigma ~ dunif( 0 , 50 )
    ) ,
    data=d )

set.seed(45)
prior <- extract.prior( m4.5 )
precis( prior )

w_seq <- seq( from=min(d$weight_s) , to=max(d$weight_s) ,
              length.out=50 )
w2_seq <- w_seq^2
mu <- link( m4.5 , post=prior ,
            data=list( weight_s=w_seq , weight_s2=w2_seq ) )

plot( NULL , xlim=range(w_seq) , ylim=c(55,270) ,
      xlab="weight (std)" , ylab="height" )
for ( i in 1:50 ) lines( w_seq , mu[i,] , col=col.alpha("black",0.5) )
-2
