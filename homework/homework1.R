library(rethinking)

## Question 1

p_grid <- seq(from=0 , to=1, length.out=1000)
prior <- rep(1 , 1000)
prob_data <- dbinom(8, size=15, prob=p_grid)
posterior <- prob_data * prior
posterior <- posterior / sum(posterior)
set.seed(100)
samples <- sample(p_grid, prob=posterior, size=1e4, replace=TRUE)

## Question 2

p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- c( rep( 0 , 500 ) , rep( 1 , 500 ) )
prob_data <- dbinom( 8 , size=15 , prob=p_grid )
posterior <- prob_data * prior
posterior <- posterior / sum(posterior)
set.seed(100)
samples2 <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )

dens( samples , xlab="p" , xlim=c(0,1) , ylim=c(0,6) )
dens( samples2 , add=TRUE , lty=2 )
abline( v=0.7 , col="red" )


## Question 3

set.seed(100)
N <- 20
p_true <- 0.7
W <- rbinom( 1 , size=N , prob=p_true )
p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- rep( 1 , 1000 )
prob_data <- dbinom( W , size=N , prob=p_grid )
posterior <- prob_data * prior
posterior <- posterior / sum(posterior)
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )
PI99 <- PI( samples , 0.99 )
as.numeric( PI99[2] - PI99[1] )

f <- function( N ) {
    p_true <- 0.7
    W <- rbinom( 1 , size=N , prob=p_true )
    p_grid <- seq( from=0 , to=1 , length.out=1000 )
    prior <- rep( 1 , 1000 )
    prob_data <- dbinom( W , size=N , prob=p_grid )
    posterior <- prob_data * prior
    posterior <- posterior / sum(posterior)
    samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )
    PI99 <- PI( samples , 0.99 )
    as.numeric( PI99[2] - PI99[1] )
}

Nlist <- c( 20 , 50 , 100 , 200 , 500 , 1000 , 2000 )
Nlist <- rep( Nlist , each=100 )
width <- sapply( Nlist , f )
plot( Nlist , width )
abline( h=0.05 , col="red" )