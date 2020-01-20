#################
# Markov Model  #
#################

# Firstly, let's define the number of treatments we are looking at and their names
n.treatments<-2
t.names<-c("Drug A","Drug B")

# Let's also define the number of states we are looking at and their names
n.states<-2
s.names<-c("Disease","No Disease")

# In a Markov model we want to specify the number of cycles - we will run a loop over this many cycles later
n.cycles<-12



# Now let's start with adding the cost information 

c.matrix<-array(NA,dim=c(n.treatments, n.states),
                dimnames=list(t.names,s.names))

c.matrix["Drug A","Disease"]<-150
c.matrix["Drug A","No Disease"]<-50

c.matrix["Drug B","Disease"]<-200
c.matrix["Drug B","No Disease"]<-100



# Let's add in the QALY information 

q.matrix<-array(NA,dim=c(n.treatments,n.states),
                dimnames=list(t.names,s.names))

q.matrix["Drug A","Disease"]<-0.73
q.matrix["Drug A","No Disease"]<-0.75

q.matrix["Drug B","Disease"]<-0.74
q.matrix["Drug B","No Disease"]<-0.75




# Let's add in the probabilities 

p.matrix<-array(NA,dim=c(n.treatments,n.states,n.states),
                dimnames=list(t.names,s.names,s.names))


# The matrix containing transition probabilities for Drug A

p.matrix["Drug A","Disease","No Disease"]<-0.85
p.matrix["Drug A","Disease","Disease"]<-1-p.matrix["Drug A","Disease","No Disease"]

p.matrix["Drug A","No Disease","No Disease"]<-0.95
p.matrix["Drug A","No Disease","Disease"]<-1-p.matrix["Drug A","No Disease","No Disease"]


# The matrix containing transition probabilities for Drug B

p.matrix["Drug B","Disease","No Disease"]<-0.95
p.matrix["Drug B","Disease","Disease"]<-1-p.matrix["Drug B","Disease","No Disease"]

p.matrix["Drug B","No Disease","No Disease"]<-0.975
p.matrix["Drug B","No Disease","Disease"]<-1-p.matrix["Drug B","No Disease","No Disease"]





# We want to store the information for each cycle

cycle.info<-array(NA,dim=c(n.treatments,n.cycles,n.states),
                      dimnames=list(t.names,NULL,s.names))

# Everyone starts without the disease 
cycle.info[,1,"No Disease"]<-1
cycle.info[,1,"Disease"]<-0

# We add up the costs and QALYs for each cycle at a time for each drug 

cycle.costs<-array(NA,dim=c(n.treatments,n.cycles),
                   dimnames=list(t.names,NULL))
cycle.QALYs<-array(NA,dim=c(n.treatments,n.cycles),
                   dimnames=list(t.names,NULL))


# The total costs and QALYs need to also be stored

total.costs<-array(NA,dim=c(n.treatments),
                   dimnames=list(t.names))
total.QALYs<-array(NA,dim=c(n.treatments),
                   dimnames=list(t.names))





for(i.treatment in 1:n.treatments)
{

    for(i.cycle in 2:n.cycles)
    {
      
      cycle.info[i.treatment,i.cycle,]<-
        cycle.info[i.treatment,i.cycle-1,]%*%
        p.matrix[i.treatment,,]
    }
    
   
    cycle.QALYs[i.treatment,]<-
      cycle.info[i.treatment,,]%*%q.matrix[i.treatment,]
    
    cycle.costs[i.treatment,]<-
      cycle.info[i.treatment,,]%*%c.matrix[i.treatment,]
    
    
    total.costs[i.treatment]<-sum(cycle.costs[i.treatment,])
    
    total.QALYs[i.treatment]<-sum(cycle.QALYs[i.treatment,])
  }



# Let's look at the results

# Incremental costs and QALYs of Drug B compared to Drug A
incremental.costs<-total.costs["Drug B"]-total.costs["Drug A"]
incremental.QALYs<-total.QALYs["Drug B"]-total.QALYs["Drug A"]

# Calculate the incremental cost effectiveness ratio
ICER<-incremental.costs/incremental.QALYs




