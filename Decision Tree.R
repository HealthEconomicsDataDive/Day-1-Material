#################
# Decision Tree #
#################

# Firstly, let's define the number of treatments we are looking at and their names
n.treatments<-2
t.names<-c("New Drug","Old Drug")

# We want to look at the costs, QALYS and probabilities for health states.

# First create some blanks to fill in
c.successful<-c.unsuccessful<-rep(NA,n.treatments)
q.successful<-q.unsuccessful<-rep(NA,n.treatments)
p.successful<-p.unsuccessful<-rep(NA,n.treatments)

# And we can name the vectors to correspond to the Drug New drug and Old Drug to keep us calculating correctly
names(c.successful)<-names(c.unsuccessful)<-names(q.successful)<-
  names(q.unsuccessful)<-names(p.successful)<-names(p.unsuccessful)<-t.names


# Now let's start with adding the cost information 

# Cost of treatment 
c.treat<-c(2000,150)

# Cost inputs for each treatment over lifetime horizon
c.successful[1]<-10000
c.unsuccessful[1]<-20000 
c.successful[2]<-5000
c.unsuccessful[2]<-10000

# Let's add in the QALY information 

# QALY inputs for each treatment over lifetime horizon
q.successful[1]<-30
q.unsuccessful[1]<-15
q.successful[2]<-25
q.unsuccessful[2]<-23

# Check and see how the costs and qalys are looking
c.successful
c.unsuccessful
q.successful
q.unsuccessful


# Let's add in the probabilities 

# Probabilities of successful and unsuccessful on new treatment
p.successful[1]<-0.7
p.unsuccessful[1]<-1-(p.successful[1])

# Probabilities of successful and unsuccessful on old treatment
p.successful[2]<-0.95
p.unsuccessful[2]<-1-(p.successful[2])

# Again let's check
p.successful
p.unsuccessful



# Again create some blank cells to fill in for the calculations of costs and QALYs
incremental.costs<-incremental.effects<-costs<-effects<-rep(NA,n.treatments)

# And we can name the vectors so we again have Drug A and Drug B the right way around
incremental.costs<-incremental.effects<-names(costs)<-names(effects)<-t.names


# Calculate the total costs and effects so we can calculate the ICER
costs<-c.treat+p.successful*c.successful+p.unsuccessful*c.unsuccessful

effects<-p.successful*q.successful+p.unsuccessful*q.unsuccessful

# Do these look right?
costs
effects


# Calculate the incremental costs and QALYs
# We use [2] as we are using the old drug as the reference, and want to see the incremental cost per QALY of the new drug
incremental.costs<-costs-costs[2]
incremental.effects<-effects-effects[2]

# Calculate the incremental cost effectiveness ratio
ICER<-incremental.costs/incremental.effects


