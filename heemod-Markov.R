#
# heemod example
# 2 state model
# drug A vs drug B
#
# Nathan Green
# Imperial College London


library(heemod)


#######################
# single intervention #
#######################

mat_drugA <-
  define_transition(
    state_names = c("healthy", "disease"),
    0.95, C,
    0.85, C
  )

plot(mat_drugA)

healthy_drugA <-
  define_state(
    cost = 50,
    qaly = 0.75
  )
healthy_drugA

disease_drugA <-
  define_state(
    cost = 150,
    qaly = 0.73
  )


# combine information

strat_drugA <-
  define_strategy(
    transition = mat_drugA,
    healthy = healthy_drugA,
    disease = disease_drugA
  )


# simulation

res_mod <- run_model(
  drugA = strat_drugA,
  cycles = 12,
  cost = cost,
  effect = qaly
)

res_mod
plot(res_mod)
heemod::get_counts(res_mod)


####################
# drug A vs drug B #
####################

mat_drugB <-
  define_transition(
    state_names = c("healthy", "disease"),
    0.975, C,
    0.95, C
  )


# define states with multiple strategies

state_healthy <-
  define_state(
    cost = dispatch_strategy(
      drugA = 50,
      drugB = 100
    ),
    qaly = dispatch_strategy(
      drugA = 0.75,
      drugB = 0.75
    )
  )
state_healthy

state_disease <-
  define_state(
    cost = dispatch_strategy(
      drugA = 150,
      drugB = 200
    ),
    qaly = dispatch_strategy(
      drugA = 0.73,
      drugB = 0.74
    )
  )
state_disease

# combine for strategy

strat_drugA <-
  define_strategy(
    transition = mat_drugA,
    healthy = state_healthy,
    disease = state_disease
  )

strat_drugB <-
  define_strategy(
    transition = mat_drugB,
    healthy = state_healthy,
    disease = state_disease
  )

# run model

res_mod <-
  run_model(
    init = c(1, 0),
    method = "end", 
    drugA = strat_drugA,
    drugB = strat_drugB,
    cycles = 12,
    cost = cost,
    effect = qaly
  )

summary(res_mod)

plot(res_mod)
plot(res_mod, type = "values", panel = "by_value")#, free_y = TRUE)

heemod::get_counts(res_mod)
heemod::get_values(res_mod)

