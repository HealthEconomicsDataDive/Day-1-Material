#################
#  Uncertainty  #
#################

# Firstly let's define the number of simulations to run and the column names
n.PSA=1000
c.names<-c('ID', 'Beta','Normal')

# Now create some space to fill in the blanks
p.transition<-array(NA,dim=c(n.PSA,3),
                    dimnames=list(NULL,c.names))


# I want to make the first column the simulation numbers from 1:1000
p.transition[,1]<- 1:n.PSA

# I want to make the second column a random value from the beta distribution with parameters 1 and 2 as specified
p.transition[,2]<- rbeta(n.PSA, 20, 50)

# Is what we have done reasonable?
range(p.transition[,2])
mean(p.transition[,2])

# We could plot these random samples
plot(p.transition[,2])

# There are nicer ways to make graphs
library(ggplot2)

qplot(p.transition[,1],p.transition[,2], geom=c("point"))

# Let's do this for the normal distribution too and add into column 3
p.transition[,3]<- rnorm(n.PSA, -0.1,0.3)
qplot(p.transition[,1],p.transition[,3], geom=c("point"))


# To plot the density function, ggplot likes things as a data frame
data<-as.data.frame(p.transition)

# Look at the different shapes and the x axis values for the types of distributions
ggplot(data, aes(x = Beta)) +
  geom_density()

ggplot(data, aes(x = Normal)) +
  geom_density()



