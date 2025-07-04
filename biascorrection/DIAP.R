sapply(list.files("R", full.names = T), source)

diapdata <- read.csv("DIA_trainingset_RDKit_descriptors.csv", header = T)

diapcovs <- diapdata[,2:198]

diap <- diapdata$Label

X <- diapcovs

X <- data.matrix(X)

Y <- diap

logpi_log <- function(betas) {
  # Only take negative values, and set positive values to 0.
  # This allows underflow but not overflow.
  b <- (X %*% betas - sign(X %*% betas)*(X %*% betas))/2
  # Only take positive values, and set negative values to 0.
  # This allows underflow but not overflow.
  d <- (X %*% betas + sign(X %*% betas)*(X %*% betas))/2
  # Calculate the posterior
  sum( (-Y)*(-b + log(exp(b) + exp(-X %*% betas + b))) ) +
    sum( (Y-1)*(d + log(exp(-d) + exp(X %*% betas - d))) ) - 0.5*sum(betas^2)}

grad_logpi_log <- function(betas) {
  Xb <- X %*% betas  # Linear predictor

  # Stable sigmoid computation
  sigmoid <- 1 / (1 + exp(-Xb))

  # Gradient contributions
  grad_likelihood <- t(X) %*% (Y-sigmoid)  # X^T (σ(Xβ) - Y)
  grad_prior <- -betas  # -β (from the normal prior)

  # Total gradient
  grad <- as.vector(grad_likelihood) + grad_prior
  return(grad)
}

dimension <- ncol(diapcovs)

target_distribution <- list(
  log_density = logpi_log,
  gradient_log_density = grad_logpi_log
)

set.seed(876287L)
proposal <- barker_proposal()
results1 <- sample_chain(
  target_distribution = target_distribution,
  initial_state = rnorm(dimension),
  n_warm_up_iteration = 50000,
  n_main_iteration = 50000,
  proposal = proposal,
  adapters = list(scale_adapter(), shape_adapter("variance"))
)

set.seed(876287L)
proposal <- barker_proposal()
results2 <- sample_chain(
  target_distribution = target_distribution,
  initial_state = rnorm(dimension),
  n_warm_up_iteration = 50000,
  n_main_iteration = 50000,
  proposal = proposal,
  adapters = list(scale_adapter(), variance_corrected_naive_shape_adapter())
)

set.seed(876287L)
proposal <- barker_proposal()
results3 <- sample_chain(
  target_distribution = target_distribution,
  initial_state = rnorm(dimension),
  n_warm_up_iteration = 50000,
  n_main_iteration = 50000,
  proposal = proposal,
  adapters = list(scale_adapter(), variance_corrected_shape_adapter())
)

set.seed(876287L)
proposal <- barker_proposal()
results4 <- sample_chain(
  target_distribution = target_distribution,
  initial_state = rnorm(dimension),
  n_warm_up_iteration = 50000,
  n_main_iteration = 50000,
  proposal = proposal,
  adapters = list(scale_adapter(), shape_adapter("covariance"))
)

set.seed(876287L)
proposal <- barker_proposal()
results5 <- sample_chain(
  target_distribution = target_distribution,
  initial_state = rnorm(dimension),
  n_warm_up_iteration = 50000,
  n_main_iteration = 50000,
  proposal = proposal,
  adapters = list(scale_adapter(), covariance_naive_diagonal_corrected_shape_adapter())
)

set.seed(876287L)
proposal <- barker_proposal()
results6 <- sample_chain(
  target_distribution = target_distribution,
  initial_state = rnorm(dimension),
  n_warm_up_iteration = 50000,
  n_main_iteration = 50000,
  proposal = proposal,
  adapters = list(scale_adapter(), covariance_quenouille_alternating_shape_adapter())
)

par(mfrow = c(2,3))
plot(results1$traces[,8], type = "l")
plot(results2$traces[,8], type = "l")
plot(results3$traces[,8], type = "l")
plot(results4$traces[,8], type = "l")
plot(results5$traces[,8], type = "l")
plot(results6$traces[,8], type = "l")
