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

# number of requested cores on GRICAD
n_cores <- length(readLines(Sys.getenv("OAR_NODEFILE") ) )
print(paste(n_cores, "cores used on GRICAD, starting model comparison") )

# importing the varying-effects model
varying_effects <- readRDS(
    "/bettik/PROJECTS/pr-deeppredspeech/R/models/wiener_varying_effects.rds"
    )

# importing the varying-effects model + effect of prime on response bias
varying_effects_prime <- readRDS(
    "/bettik/PROJECTS/pr-deeppredspeech/R/models/wiener_varying_effects_prime_bias.rds"
    )

# performing model comparison via approximate LOO-CV
varying_effects <- add_criterion(varying_effects, "waic")
print("first model done")
varying_effects_prime <- add_criterion(varying_effects_prime, "waic")
print("second model done")

# displays the model comparison table
LOOCV <- loo_compare(varying_effects, varying_effects_prime, criterion = "waic")

# computes pseudo-BMA weights
# weights <- loo_model_weights(varying_effects, varying_effects_prime)

# saves the model comparison table
saveRDS(LOOCV, file = "/bettik/PROJECTS/pr-deeppredspeech/R/models/model_comparison.rds")
