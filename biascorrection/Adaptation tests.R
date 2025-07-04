sapply(list.files("R", full.names = T), source)

set.seed(876287L)
dimension <- 3
scales <- exp(rnorm(dimension))
target_distribution <- list(
  log_density = function(x) -sum((x / scales)^2) / 2,
  gradient_log_density = function(x) -x / scales^2
)
proposal <- barker_proposal()
results1 <- sample_chain(
  target_distribution = target_distribution,
  initial_state = rnorm(dimension),
  n_warm_up_iteration = 10000,
  n_main_iteration = 10000,
  proposal = proposal,
  adapters = list(scale_adapter(), shape_adapter("variance"))
)

set.seed(876287L)
dimension <- 3
scales <- exp(rnorm(dimension))
target_distribution <- list(
  log_density = function(x) -sum((x / scales)^2) / 2,
  gradient_log_density = function(x) -x / scales^2
)
proposal <- barker_proposal()
results2 <- sample_chain(
  target_distribution = target_distribution,
  initial_state = rnorm(dimension),
  n_warm_up_iteration = 10000,
  n_main_iteration = 10000,
  proposal = proposal,
  adapters = list(scale_adapter(), variance_corrected_naive_shape_adapter())
)

set.seed(876287L)
dimension <- 3
scales <- exp(rnorm(dimension))
target_distribution <- list(
  log_density = function(x) -sum((x / scales)^2) / 2,
  gradient_log_density = function(x) -x / scales^2
)
proposal <- barker_proposal()
results3 <- sample_chain(
  target_distribution = target_distribution,
  initial_state = rnorm(dimension),
  n_warm_up_iteration = 10000,
  n_main_iteration = 10000,
  proposal = proposal,
  adapters = list(scale_adapter(), variance_corrected_shape_adapter())
)

set.seed(876287L)
dimension <- 3
scales <- exp(rnorm(dimension))
target_distribution <- list(
  log_density = function(x) -sum((x / scales)^2) / 2,
  gradient_log_density = function(x) -x / scales^2
)
proposal <- barker_proposal()
results4 <- sample_chain(
  target_distribution = target_distribution,
  initial_state = rnorm(dimension),
  n_warm_up_iteration = 10000,
  n_main_iteration = 10000,
  proposal = proposal,
  adapters = list(scale_adapter(), covariance_naive_diagonal_corrected_shape_adapter())
)

set.seed(876287L)
dimension <- 3
scales <- exp(rnorm(dimension))
target_distribution <- list(
  log_density = function(x) -sum((x / scales)^2) / 2,
  gradient_log_density = function(x) -x / scales^2
)
proposal <- barker_proposal()
results5 <- sample_chain(
  target_distribution = target_distribution,
  initial_state = rnorm(dimension),
  n_warm_up_iteration = 10000,
  n_main_iteration = 10000,
  proposal = proposal,
  adapters = list(scale_adapter(), covariance_diagonal_corrected_shape_adapter())
)

set.seed(876287L)
dimension <- 3
scales <- exp(rnorm(dimension))
target_distribution <- list(
  log_density = function(x) -sum((x / scales)^2) / 2,
  gradient_log_density = function(x) -x / scales^2
)
proposal <- barker_proposal()
results6 <- sample_chain(
  target_distribution = target_distribution,
  initial_state = rnorm(dimension),
  n_warm_up_iteration = 10000,
  n_main_iteration = 10000,
  proposal = proposal,
  adapters = list(scale_adapter(), covariance_quenouille_alternating_shape_adapter())
)

par(mfrow = c(2,3))
plot(results1$traces[,1], type = "l")
plot(results2$traces[,1], type = "l")
plot(results3$traces[,1], type = "l")
plot(results4$traces[,1], type = "l")
plot(results5$traces[,1], type = "l")
plot(results6$traces[,1], type = "l")


