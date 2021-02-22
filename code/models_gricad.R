library(tidyverse)
library(brms)

# imports data and removes the first column
df <- read.csv("/bettik/PROJECTS/pr-deeppredspeech/R/data/df.csv")[-1] %>%
    # removing RTs equal to zero
    # filter(RT != 0) %>%
    # removing RTs below 300ms
    filter(RT > 300) %>%
    # converts RTs from ms to seconds
    mutate(RT = RT / 1000) %>%
    # removing transgenders as we do exploratory analysis included sex as predictor
    filter(Gender != "FtM" ) %>%
    # removing participants who are neither above the cutoff for autism on the AQ, nor on the ADOS or ADI
    filter(Group == "TD" | Group == "ASD" & (AQ > 31 | ADOS_score_total > 6 | ADI == 1) ) %>%
    # removing participant who is less than 50 % correct
    filter(ID != "114")

# recoding the binary predictors using sum contrasts (-0.5 vs. 0.5)
df <- df %>%
    mutate(
        GroupC = ifelse(Group == "TD", -0.5, 0.5),
        CongC = ifelse(Congruency == "ICG", -0.5, 0.5),
        PrimeC = ifelse(Prime == "LSF", -0.5, 0.5)
        )

# defining the model formula (one "linear model" per parameter)
formula <- brmsformula(
    # drift rate (delta)
    RT | dec(ACC) ~ 1 + GroupC * CongC * PrimeC + (1 | ID) + (1 | Target),
    # boundary separation parameter (alpha)
    bs ~ 1 + GroupC + (1 | ID) + (1 | Target),
    # non-decision time (tau)
    ndt ~ 1 + GroupC + (1 | ID) + (1 | Target),
    # starting point or bias (beta)
    bias ~ 1 + GroupC * PrimeC + (1 | ID) + (1 | Target)
    )

# defining the priors
priors <- c(
    # priors for the intercepts
    prior("normal(0, 5)", class = "Intercept"),
    prior("normal(0, 1)", class = "Intercept", dpar = "bs"),
    prior("normal(0, 1)", class = "Intercept", dpar = "ndt"),
    prior("normal(0, 1)", class = "Intercept", dpar = "bias"),
    # priors for the slopes
    prior("normal(0, 1)", class = "b"),
    # priors on the SD of the varying intercepts
    prior("exponential(1)", class = "sd")
    )

# specify initial values to help the model start sampling (with small variation)
# from https://github.com/paul-buerkner/Bayesian-IRT-paper/blob/master/Bayesian-IRT.R
# https://discourse.mc-stan.org/t/function-to-specify-initial-values-in-brms-model-has-dimension-mismatch/8692
chains <- 8 # number of chains
epsilon <- 0.1 # variability in starting value for the NDT intercept
get_init_value <- function(x) list(Intercept_ndt = rnorm(n = 1, mean = x, sd = epsilon) )
inits_drift <- replicate(chains, get_init_value(-3), simplify = FALSE)

# number of requested cores on GRICAD
n_cores <- length(readLines(Sys.getenv("OAR_NODEFILE") ) )
print(paste(n_cores, "cores used on GRICAD") )

# fitting the model
varying_effects <- brm(
    formula,
    data = df,
    # specifying the family and link functions for each parameter
    family = wiener(
        link = "identity", link_bs = "log",
        link_ndt = "log", link_bias = "logit"
        ),
    # comment this line to use default priors
    prior = priors,
    # list of initialisation values
    inits = inits_drift,
    init_r = 0.05,
    warmup = 2000, iter = 5000,
    chains = chains, cores = n_cores,
    control = list(adapt_delta = 0.99, max_treedepth = 15),
    # saves the model (as .rds) or loads it if it already exists
    file = "/bettik/PROJECTS/pr-deeppredspeech/R/models/wiener_varying_effects_prime_bias.rds",
    # for prior predictive checking
    # sample_prior = "only"
    # or for further hypothesis testing
    sample_prior = TRUE
    )
