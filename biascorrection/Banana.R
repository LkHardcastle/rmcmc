sapply(list.files("R", full.names = T), source)
library(mvtnorm)

set.seed(23852L)
dimension = 2
target_distribution <- ~ (-(x^2 + y^2) / 8 - (x^2 - y)^2 - (x - 1)^2 / 100)


set.seed(876287L)
proposal <- barker_proposal()
results1 <- sample_chain(
  target_distribution = target_distribution,
  initial_state = rnorm(dimension),
  n_warm_up_iteration = 5000,
  n_main_iteration = 5000,
  proposal = proposal,
  adapters = list(scale_adapter(), shape_adapter("variance"))
)

set.seed(87627L)
proposal <- barker_proposal()
results2 <- sample_chain(
  target_distribution = target_distribution,
  initial_state = rnorm(dimension),
  n_warm_up_iteration = 5000,
  n_main_iteration = 5000,
  proposal = proposal,
  adapters = list(scale_adapter(), variance_corrected_naive_shape_adapter())
)

set.seed(876287L)
proposal <- barker_proposal()
results3 <- sample_chain(
  target_distribution = target_distribution,
  initial_state = rnorm(dimension),
  n_warm_up_iteration = 5000,
  n_main_iteration = 5000,
  proposal = proposal,
  adapters = list(scale_adapter(), variance_corrected_shape_adapter())
)

set.seed(876287L)
proposal <- barker_proposal()
results4 <- sample_chain(
  target_distribution = target_distribution,
  initial_state = rnorm(dimension),
  n_warm_up_iteration = 5000,
  n_main_iteration = 5000,
  proposal = proposal,
  adapters = list(scale_adapter(), shape_adapter("covariance"))
)

set.seed(876287L)
proposal <- barker_proposal()
results5 <- sample_chain(
  target_distribution = target_distribution,
  initial_state = rnorm(dimension),
  n_warm_up_iteration = 5000,
  n_main_iteration = 5000,
  proposal = proposal,
  adapters = list(scale_adapter(), covariance_naive_diagonal_corrected_shape_adapter())
)

set.seed(876287L)
proposal <- barker_proposal()
results6 <- sample_chain(
  target_distribution = target_distribution,
  initial_state = rnorm(dimension),
  n_warm_up_iteration = 5000,
  n_main_iteration = 5000,
  proposal = proposal,
  adapters = list(scale_adapter(), covariance_quenouille_alternating_shape_adapter())
)

par(mfrow = c(2,3))
index = 1
plot(results1$traces[,index], type = "l")
plot(results2$traces[,index], type = "l")
plot(results3$traces[,index], type = "l")
plot(results4$traces[,index], type = "l")
plot(results5$traces[,index], type = "l")
plot(results6$traces[,index], type = "l")

par(mfrow = c(2,3))
plot(results1$traces[,1],results1$traces[,2])
plot(results2$traces[,1],results2$traces[,2])
plot(results3$traces[,1],results3$traces[,2])
plot(results4$traces[,1],results4$traces[,2])
plot(results5$traces[,1],results5$traces[,2])
plot(results6$traces[,1],results6$traces[,2])

