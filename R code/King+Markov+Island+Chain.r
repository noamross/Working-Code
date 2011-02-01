##########################################################
# King Markov, island dictator example
# this is a simple metropolis algorithm
num.visits <- 20000
population <- 1:10
current.island <- sample( 1:10 , size=1 )
visits <- {}
par(mfrow=c(2,1))
for ( i in 1:num.visits ) {
    visits[i] <- current.island
    proposed.island <- current.island + sample( c(-1,1) , size=1 )
    proposed.island <- ifelse( proposed.island > 10 , 1 , ifelse( proposed.island < 1 , 10 , proposed.island ) )
    population.proposed <- population[proposed.island]
    population.current <- population[current.island]
    prob.move <- population.proposed / population.current
    current.island <- ifelse( runif(1) < prob.move , proposed.island , current.island )
    if ( i > 10 & i/50==floor(i/50) ) {
        lo <- max(1,i-500)
        hi <- min(num.visits,i+2)
        plot( visits , type="l" , xlab="time step" , ylab="location" , xlim=c(lo,hi) )
        plot( density(visits, adjust=0.5) , xaxp=c(1,10,9) , xlab="island" , main="" )
    }
}