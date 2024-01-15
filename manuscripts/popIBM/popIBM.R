# popIBM model - cleaned for publication in Communications in medicine titled:
# "Digital twin simulation modelling shows that
# mass testing and local lockdowns effectively controlled COVID-19 in Denmark"
# further commented for submission to ECDC

# This is a detailed, individual-based simulation model of SARS-CoV-2 transmission
# to evaluate mass testing and local lockdowns during the Alpha wave in Denmark
# in counterfactual scenarios.

# Denmark tested ten times more than median country in EU during alpha wave
# local lockdown was based on observed 7 day incidence in parish/municipality

# Please note that the model is coded using data.table
# this has a specific syntax, please see: r-datatable.com for an introduction

library(doParallel)
library(data.table)

###########################################################
#### Parameters that can be sensibly changed by user ####
##########################################################

# The number of repetitions - 100 in the publication
n_samples <- 1

# Are automatic lockdowns activated in the model?
activate_lockdown <- TRUE

# Fraction of tests available compared to observed, 1 = mass testing, 0.1 = limited testing
frac_n_tests <- 0.1

# Set number of cores to be used by the foreach package
registerDoParallel(cores = 1)

# Set number of threads to be used by data.table, likely do not work well with doParallel
setDTthreads(1)

# Print which elements are loaded
load_info <- FALSE

# The model cannot run without the input of data. - not submitted
# many parameters enter through this file -- including the basic structure of the individuals (a data.table)
# load("./popibm_init.Rdata", verbose = load_info)

# Simulation dates
new_end_date <- as.Date("2021-06-30")
end_times <- as.numeric(new_end_date) - as.numeric(as.Date("2020-01-01"))
times <- seq(start_denmark, end_times, 1)

# Proportion of transmission within municipality
w_municipality <- 0.9

# Seasonal factor (relative to estimate from Sweden)
season_fac <- 0.8

##############################################################
#### Parameters beyond this point should NOT be changed ######
##############################################################

## Parameters related to vaccines ###################

# Relative time of vaccination effects:
# First dose reaches effectiveness after 14 days
# Second dose is administered such that it takes effect 28 days after first dose
breaks_vac <- c(14, 14 + 28, Inf)

# Vaccination groups to output
n_vac_groups_out <- length(breaks_vac)

# Reduction factor on transmission when effectively vaccinated
red_transmission_vac <- ifelse(exists("input_red_transmission_vac"), input_red_transmission_vac, 0.1)

# Reduction factor on risk going to hospital when effectively vaccinated
red_hospital_risk <- 0.25

# Maximal number of vaccination doses in the simulation - depend on end time
n_max_doses <- 3



## Demographics #####################################

# Individuals are stored in the data.table called "ibm"
# Each line in the data.table is equivalent to a person

# Number of parishes in the input
n_parish <- ibm[, uniqueN(parish_id)]

# Number of municipalities in the input
n_municipality <- ibm[, uniqueN(municipality_id)]


# Ids of the parish and municipalities
u_parish_ids       <- ibm[, unique(parish_id)]
u_municipality_ids <- ibm[, unique(municipality_id)]

# The population by parish and municipality in the model - .N is data.table special character
population <- ibm[, .N, keyby = .(parish_id, municipality_id)]

# The population by parish and municipality from alternative sources
pop_parish <- parish[.(parish_id = u_parish_ids), `Indbyggertal i sogn`, on = "parish_id"]
tmp <- parish[, sum(`Indbyggertal i sogn`), by = .(municipality_id)]
pop_municipality <- tmp[.(municipality_id = u_municipality_ids), V1, on = "municipality_id"]
pop_denmark <- NROW(ibm)
pop_age <- ibm[, .N, by = .(age_groups)]

dt_pop_municipality <- parish[, sum(`Indbyggertal i sogn`), by = .(municipality_id)]
names(dt_pop_municipality)[2] <- "pop"

all_pop_combi <- data.table(municipality_id = rep(u_municipality_ids, each = 9), age_groups = 1:9)



## Activity and restrictions ########################

sce_fac_current_beta <- 1 # Should be 1.05 if 5% increase, 0.95 if 5% decrease
sce_fac_current_beta_vec <- ifelse(exists("input_fac_beta"), input_fac_beta, 1) # Update if given as input

# Load the activity scenario (changes in restrictions over time)
activity_scenario <- lockdown_sce_beta_list$Fyn$S5.3

# Days of changing restriction (relative to start date)
day_restriction_change  <- as.numeric(c(activity_scenario$list_beta_dates) - as.Date("2020-01-01")) - start_denmark

# List of activity matrices - age stratified
list_beta <- activity_scenario$list_beta

# Days of changing incidence limits for imposing local lockdown (relative to start date)
day_limit_change <- c("2021-03-01", "2021-04-30", "2021-05-28", "2021-07-16", "2021-09-10", "2021-11-15")
day_limit_change <- as.numeric(as.Date(day_limit_change) - as.Date("2020-01-01")) - start_denmark

# Functions for lockdown (corresponding to Danish policy):
lockdown_parish_fun <- list(
  approxfun(x = c(300, 400),   y = c(0, 0.5), yleft = 0, yright = 1),
  approxfun(x = c(375, 500),   y = c(0, 0.5), yleft = 0, yright = 1),
  approxfun(x = c(450, 600),   y = c(0, 0.5), yleft = 0, yright = 1),
  approxfun(x = c(750, 1000),  y = c(0, 0.5), yleft = 0, yright = 1),
  approxfun(x = c(1000, 4000), y = c(0, 0.2), yleft = 0, yright = 0.5),
  approxfun(x = c(800, 3200),  y = c(0, 0.2), yleft = 0, yright = 0.5)
)

lockdown_municipality_fun <- list(
  approxfun(x = c(150, 200),  y = c(0, 0.5), yleft = 0, yright = 1),
  approxfun(x = c(188, 250),  y = c(0, 0.5), yleft = 0, yright = 1),
  approxfun(x = c(225, 300),  y = c(0, 0.5), yleft = 0, yright = 1),
  approxfun(x = c(375, 500),  y = c(0, 0.5), yleft = 0, yright = 1),
  approxfun(x = c(500, 2000), y = c(0, 0.2), yleft = 0, yright = 0.5),
  approxfun(x = c(400, 1600), y = c(0, 0.2), yleft = 0, yright = 0.5)
)



## Parameters controlling Delta #####################
day_delta_intro_sce <- as.numeric(as.Date("2021-06-01") - as.Date("2020-01-01")) - start_denmark
prob_delta_intro <- 0.02 # Converting X % of infected to delta variant on this day
variant_id_delta <- 3 # Variant id for delta



## Parameters related to testing ####################

# Determine the last day where the number of tests is known
# after this day, we fix p_test to the last known value
day_fix_p_test <- as.numeric(ntal[, as.Date(max(pr_date))] - as.Date("2020-01-01")) - start_denmark

sce_test_red <- 1 # Factor for probability of taking a test
test_red_fac <- 1 # Internal copy of sce_test_red when paste date



## Configure the simulation runs ####################

# Set seed for generating parameter combinations
set.seed(ifelse(exists("input_seed"), input_seed, 1))

# This simulation was implemented at a time with uncertainties on delta variant parameters
# Therefore scenarios of different parameter values for delta are included
sce_combi <- data.frame(
  par_id = 1:n_samples,
  sce_fac_current_beta = sce_fac_current_beta_vec,
  delta_rec_red = 1 - runif(n_samples, min = 0.6, max = 0.8), # VE of infection
  red_transmission_vac = 1 - runif(n_samples, min = 0.5, max = 0.8), # Transmission
  rel_alpha_delta = runif(n_samples, min = 1.65, max = 1.95)
)

first_run <- ifelse(exists("input_start"), input_start, 1)
n_runs    <- ifelse(exists("input_n_runs"), input_n_runs, n_samples) # Run all if not specified



## Data collection arrays ###########
# Data collection arrays without vaccination status
sim_test_positive <- array(0L, dim = c(length(times), n_age_groups, n_variants))
sim_hospital      <- array(0L, dim = c(length(times), n_age_groups, n_variants))

# Data collection arrays that include vaccination status
sim_test_positive_vac <- array(0L, dim = c(length(times), n_age_groups, n_variants, n_vac_groups_out))
sim_hospital_vac      <- array(0L, dim = c(length(times), n_age_groups, n_variants, n_vac_groups_out))

# Data collection arrays that include geographical information
sim_parish       <- array(0L, dim = c(length(times), n_parish))
sim_municipality <- array(0L, dim = c(length(times), n_municipality))


tic <- Sys.time()

# Branch out to parallel processes
sim_list <- foreach(run_this = (first_run - 1 + 1:n_runs), .packages = "data.table", .verbose = TRUE) %dopar% {

  scenario <- unlist(sce_combi[run_this, ])
  for (scenario_id in seq_along(scenario)) {
    assign(x = names(scenario)[scenario_id], value = scenario[scenario_id])
  }
  cat("\t run: ", run_this, " ")

  # Compute the delta params for the scenario
  delta_red_hospital_risk <- (1 - 0.9) * red_hospital_risk / delta_rec_red * 2
  delta_vac_effect <- delta_rec_red * c(1, 1)

  # Make sure parameters are loaded fresh onto all cores
  rm("ibm")
  load("./popibm_init.Rdata", verbose = load_info)

  # Start and end times are in init file
  end_times <- as.numeric(new_end_date) - as.numeric(as.Date("2020-01-01"))
  times <- seq(start_denmark, end_times, 1)
  xdates <- as.Date(times, origin = "2020-01-01")

  # Initialise spatial heterogeneity in parishes.
  # During initialisation, rel_risk_parish is set to the observed cumulative incidence from begining of epidemic until 26 April 2021.
  # However, distribution of risks  ....
  #
  # For reference, see
  # Græsbøll, K., Eriksen, R. S., Kirkeby, C., & Christiansen, L. E., & (2023).
  #   Mass testing and local lockdowns effectively controls COVID-19. Manuscript
  #   accepted for publication in Communications Medicine
  ibm[, rel_risk_parish := rel_risk_parish^(1 / 3)]
  ibm[, rel_risk_parish := rel_risk_parish * .N / sum(rel_risk_parish)]

  # Set initial parameters, that will change over time
  ibm[, lockdown_fac := 1.]
  ibm[, non_isolated := 1L]
  ibm[, p_test := 2e5 / pop_denmark]

  # Make sure individual already vaccinated are correctly labelled
  ibm[, vac_fac_trans := 1.0]
  ibm[vac_fac < 1, vac_fac_trans := red_transmission_vac]

  ibm[, vac_eff_dose := 0L]
  ibm[vac_time > breaks_vac[[1]], vac_eff_dose := 1L]
  ibm[vac_time > breaks_vac[[2]], vac_eff_dose := 2L]

  # Setting seed per rep
  set.seed(123456 + run_this - 1)

  # Set parameters related to disease progression
  v_rel_beta <- c(1, 1.55, 1.55 * rel_alpha_delta)
  v_scale_i <- rep(5.3, 9) / v_shape_i
  ibm[disease == 2L, tt := tt + round(rexp(.N, 1 / 2))]

  # Data collection arrays for incidence in parish/municipality
  inc_his_parish       <- array(0, dim = c(length(times), n_parish))
  inc_his_municipality <- array(0, dim = c(length(times), n_municipality))

  # Index individuals for faster runtime
  setkey(ibm, municipality_id, parish_id, age_groups, vac_target_group)

  # Reference transmission risk scaling - fitted prior
  r_ref <- 0.7

  # Profiler for testing bottlenecks in code - only for test runs
  # profvis({

  # Main loop
  for (day in seq_along(times)) {

    # Set "beta" based on restriction levels and seasonal change
    beta_season <- (1 - season_fac * (1 - seasonal_rel_beta(as.Date(start_denmark, origin = "2020-01-01"), day)))

    if (day > day_restriction_change[1]) { # Activity different from initial activity (restriction change)
      i_beta <- max(which(day_restriction_change <= day))

      current_beta <- beta_season * r_ref * 0.35 * list_beta[[i_beta]]

      lockdown_factor <- sqrt(eigen(list_beta[[1]])$values[1] / eigen(list_beta[[i_beta]])$values[1])

    } else { # Initial activity level
      current_beta <- beta_season * r_ref * 0.35 * list_beta[[1]]
    }


    # Delta variant introduced by changing some infections to the delta variant
    if (day == day_delta_intro_sce && prob_delta_intro > 0) {
      prob <- c(1 - prob_delta_intro, prob_delta_intro)
      ibm[variant == 2, variant := sample(c(2, variant_id_delta), size = .N, replace = TRUE, prob = prob)]
    }

    # Set probability of test in accordance with 7 day test-incidence
    # n_test_dk* files loaded from init file
    n_test         <- n_test_dk(as.Date(start_denmark, origin = "2020-01-01"), day)
    n_test_age     <- n_test_dk_age(as.Date(start_denmark, origin = "2020-01-01"), day)
    n_test_age_vac <- n_test_dk_age_vac(as.Date(start_denmark, origin = "2020-01-01"), day)

    # Adjust number of test according to scenario
    n_test                <- n_test * frac_n_tests
    n_test_age$w_test     <- n_test_age$w_test * frac_n_tests
    n_test_age_vac$w_test <- n_test_age_vac$w_test * frac_n_tests

    # When 7-day incidences are available, adjust test behavior according to incidence.
    # until then, use unadjusted test behavior.
    if (day <= 7) {

      ibm[, p_test := (n_test[1, 1] + 0.5 * n_test[2, 1]) / pop_denmark * test_red_fac]

    } else {

      # Compute latest 7-day test-incidence
      inc <- colSums(sim_municipality[(day - 7):(day - 1), ], na.rm = TRUE) / pop_municipality * 1e5

      # The compute the probability of test when adjusting for the number of tests
      p_test_corr <- p_test_inc(inc)

      if (day <= day_fix_p_test) { # The number of tests is known

        # Group population by age group and vaccinated / un-vaccinated
        tmp <- ibm[, .N, keyby = .(age_groups, !(vac_time < breaks_vac[[1]] | is.na(vac_time)))]

        # Then merge population counts with testing behavior in age and vaccine groups (n_test_age_groups_int_vac).
        # This gives the ???
        # (n_test_age_groups_int_vac is loaded as part of init)
        t_pop_age_vac <- tmp[n_test_age_groups_int_vac, , on = c("age_groups", "vac_time")]
        t_pop_age_vac[is.na(N), N := 0]

        # Count population by municipality, age group and vaccinated / un-vaccinated
        tmp <- ibm[, .(pop = .N), keyby = .(municipality_id, age_groups, !(vac_time < breaks_vac[[1]] | is.na(vac_time)))]

        tmp <- tmp[t_pop_age_vac, , on = c("age_groups", "vac_time")]
        names(tmp)[c(3, 5)] <- c("vac_status", "t_pop")
        names(n_test_age_vac)[1] <- "age_groups"
        n_test_age_vac$age_groups <- as.integer(n_test_age_vac$age_groups)

        tmp_test <- tmp[n_test_age_vac, , on = c("age_groups", "vac_status")]

        tmp_test[, p_test_corr :=  w_test / t_pop]

      }




      tmp2 <- data.table(municipality_id = u_municipality_ids,
                         p_test_fac = p_test_corr)

      tmp <- tmp_test[tmp2, , on = "municipality_id"]

      tmp[, p_test_corr :=  p_test_corr * p_test_fac]
      tmp[, p_test_corr := p_test_corr * sum(n_test_age_vac$w_test) / sum(p_test_corr * pop)]

      tmp[, vac_eff_dose := as.integer(vac_status)]

      tmp2 <- copy(tmp[vac_eff_dose == 1L, ])
      tmp2[, vac_eff_dose := 2L]

      tmp <- rbindlist(list(tmp, tmp2))

      ibm[tmp, on = c("municipality_id", "age_groups", "vac_eff_dose"), p_test := p_test_corr]

    }


    # Determine who is detected by tests.
    # Assuming non-isolated, non-vaccinated, infected or newly-recovered (within 5 days) are eligible for
    # testing positive
    id_test_positive <- ibm[
      non_isolated == 1L & (disease %in% 1:2 | (disease == 3L & tt >= -5)) &
        (is.na(vac_type) | vac_time < breaks_vac[[1]]),
      .(id, p_test)
    ][runif(.N) < p_test, id]

    # Those who test positive gets their time-to symptoms set to 0
    ibm[id %in% id_test_positive, tt_symp := 0L]

    # Test positives isolate themselves
    ibm[tt_symp == 0L, non_isolated := 0L]



    # Collect data on the number of test positives each day - by variant, age and vaccination status
    for (variant_id in 1:n_variants) {

      sim_test_positive[day, , variant_id] <- ibm[
        tt_symp == 0L & variant == variant_id, .N, by = .(age_groups)
      ][.(age_groups = 1:9), on = "age_groups"]$N

      sim_test_positive_vac[day, , variant_id, 1] <- ibm[
        tt_symp == 0L & variant == variant_id & (vac_time < breaks_vac[1] | is.na(vac_time)),
        .N,
        by = .(age_groups)
      ][.(age_groups = 1:9), on = "age_groups"]$N

      for (vac_id in 2:n_vac_groups_out) {
        sim_test_positive_vac[day, , variant_id, vac_id] <- ibm[
          tt_symp == 0L & variant == variant_id & vac_time >= breaks_vac[vac_id - 1] & vac_time < breaks_vac[vac_id],
          .N,
          by = .(age_groups)
        ][.(age_groups = 1:9), on = "age_groups"]$N
      }
    }

    # Collect number of test positives by parish
    sim_parish[day, ] <- ibm[
      tt_symp == 0L, .N, by = .(parish_id, age_groups)
    ][, sum(N), by = parish_id
    ][.(parish_id = u_parish_ids), on = "parish_id"]$V1

    # Collect number of test positives by municipality
    sim_municipality[day, ] <- ibm[
      tt_symp == 0L, .N, by = .(municipality_id, age_groups)
    ][, sum(N), by = municipality_id
    ][.(municipality_id = u_municipality_ids), on = "municipality_id"]$V1


    # Collect probability of hospitalisation each day - by variant, age and vaccination status
    for (variant_id in 1:n_variants) {
      sim_hospital[day, , variant_id] <- ibm[
        disease == 2L & tt == 0 & variant == variant_id,
        sum(prob_hospital),
        by = .(age_groups)
      ][.(age_groups = 1:9), on = "age_groups"]$V1

      sim_hospital_vac[day, , variant_id, 1] <- ibm[
        disease == 2L & tt == 0 & variant == variant_id & (vac_time < breaks_vac[1] | is.na(vac_time)),
        sum(prob_hospital),
        by = .(age_groups)
      ][.(age_groups = 1:9), on = "age_groups"]$V1

      for (vac_id in 2:n_vac_groups_out) {
        sim_hospital_vac[day, , variant_id, vac_id] <- ibm[
          disease == 2L & tt == 0 & variant == variant_id &
            vac_time >= breaks_vac[vac_id - 1] & vac_time < breaks_vac[vac_id],
          sum(prob_hospital),
          by = .(age_groups)
        ][.(age_groups = 1:9), on = "age_groups"]$V1
      }
    }




    # Recover from disease I -> R
    ibm[disease == 2L & tt == 0, disease := 3L]

     # All age groups have same disease progression E-> I (disease states 1L -> 2L).
    # Upon entering the I state, the time to recovery (tt) and time to symptoms (tt_symp) are drawn.
    # Around 50% of infected people don't develop symptoms to the degree that they take a test.
    # Therefore, we set tt_symp to NA for 50% of infected.
    ibm[
      disease == 1L & tt == 0,
      `:=`(
        disease = 2L,
        tt = pmax(1L, round(rgamma(.N, v_shape_i[1], scale = v_scale_i[1]))),
        tt_symp = pmax(1L, round(rgamma(.N, v_shape_tt_symp[1], scale = v_scale_tt_symp[1]))) *
          sample(c(1L, NA_integer_), .N, replace = TRUE, prob = c(0.5, 0.5))
      )
    ]

    # Persons already isolated (non_isolated == 0) are assumed to not test themselves again once they develop symptoms.
    # We therefore set tt_symp to -1L to prevent such tests.
    ibm[disease == 2L & non_isolated == 0L & tt_symp > 0, tt_symp := -1L]


    # Implement the effects of local lockdown
    if (activate_lockdown && day > day_limit_change[1]) {

      # Lockdown
      i_lock <- sum(day > day_limit_change)

       # Compute and store the incidences for the local lockdown rules.
      # 1) Compute 7-day incidences per 100k leading up to current day
      # 2) Store the incidence in the history tracker
      # 3) Compute maximum 7-day incidence over the last 7 days
      # 4) Apply the current lockdown thresholds given this max incidence

      ## Parish
      n_7d_cases_parish <- colSums(sim_parish[(day - 7):(day - 1), ], na.rm = TRUE)

      # NOTE: for parishes, local lockdown required at least 20 positive cases in the parish.
      # We therefore record incidences as 0 in these cases.
      inc_his_parish[day, ] <- (n_7d_cases_parish >= 20) * n_7d_cases_parish / pop_parish * 1e5

      max_7d_inc <- apply(inc_his_parish[(day - 6):day, ], 2, max, na.rm = TRUE)

      lockdown_parish_fac <- lockdown_parish_fun[[i_lock]](max_7d_inc)


      ## Municipality
      inc_his_municipality[day, ] <- sim_municipality[(day - 7):(day - 1), ] |>
        colSums(na.rm = TRUE) / pop_municipality * 1e5

      max_7d_inc <- apply(inc_his_municipality[(day - 6):day, ], 2, max, na.rm = TRUE)

      lockdown_municipality_fac <- lockdown_municipality_fun[[i_lock]](max_7d_inc)



      population[
        data.table(parish_id = u_parish_ids, lockdown_parish_fac),
        on = "parish_id",
        parish_fac := lockdown_parish_fac
      ]

      population[
        data.table(municipality_id = u_municipality_ids, lockdown_municipality_fac),
        on = "municipality_id",
        kom_fac := lockdown_municipality_fac
      ]

      population[, lockdown_max := pmax(parish_fac, kom_fac)]

      # Weighted sum as lockdown factor
      population[, lockdown_fac := 1 * (1 - lockdown_max) + lockdown_factor * lockdown_max]

      # Merging on ibm:
      ibm[population, on = c("parish_id", "municipality_id"), lockdown_fac := lockdown_fac]

    }

    # Infected individuals with different strains
    for (variant_id in 1:n_variants) {

      # Calculate the infection pressure
      inf_persons_municipality <- ibm[
        disease == 2L & variant == variant_id,
        .(inf_persons = sum(lockdown_fac * non_isolated * vac_fac_trans)),
        by = .(municipality_id, age_groups)
      ][all_pop_combi, on = c("municipality_id", "age_groups")]

      inf_persons_municipality[is.na(inf_persons), inf_persons := 0]
      inf_persons_municipality[, inf_persons := inf_persons * v_rel_beta[variant_id]]

      inf_pressure <- inf_persons_municipality[
        , current_beta %*% inf_persons, by = .(municipality_id)
      ][, age_groups := rep(1:9, n_municipality)]

      names(inf_pressure)[2] <- "r_inf_municipality"

      inf_pressure <- dt_pop_municipality[inf_pressure, , on = "municipality_id"]
      inf_pressure[, r_inf_municipality := r_inf_municipality / pop]

      inf_persons <- inf_persons_municipality[, .(inf_persons = sum(inf_persons)), by = .(age_groups)]

      tmp <- inf_persons$inf_persons
      inf_pressure_dk <- inf_persons[
        ,
        .(r_inf_denmark = sum(current_beta[age_groups, ] * tmp / pop_denmark)),
        by = .(age_groups)
      ]

      inf_pressure_total <- inf_pressure_dk[inf_pressure, , on = "age_groups"]
      inf_pressure_total[
        ,
        prob_inf := (1 - exp(-(1 - w_municipality) * r_inf_denmark - w_municipality * r_inf_municipality))
      ]

      tmp2 <- inf_pressure_total[, c(1, 3, 6)]
      names(tmp2)[3] <- "prob_inf_new"

      # Evaluate probability of infection
      ibm[tmp2, on = c("municipality_id", "age_groups"), prob_inf := prob_inf_new]

      # NB for future use - should maybe include a check on reasonable values [0;1]
      ibm[, prob_inf := prob_inf * vac_fac * rel_risk_parish * lockdown_fac]

      # Randomly infect some individuals based on probability
      ibm[disease == 0L & runif(.N) < prob_inf,
          `:=`(disease = 1L,
               tt = pmax(1L, round(rgamma(n = .N, shape = v_shape_e[age_groups],
                                          scale = v_scale_e[age_groups]))),
               variant = variant_id)]
    }

    # Count down to change in disease state or symptoms
    ibm[, tt := tt - 1L]
    ibm[, tt_symp := tt_symp - 1L]

    # Count up to vaccination time
    ibm[, vac_time := vac_time + 1L]


    # Implement the effect of vaccination
    for (vac_id in 1:n_vac){

      ibm[vac_type == vac_id & vac_time == v_vac_tt_effect[vac_id],
          `:=`(vacF_ec = delta_vac_effect[vac_id],
               prob_hospital = delta_red_hospital_risk * prob_hospital,
               vac_fac_trans = red_transmission_vac)]

    }

    # Vaccination doses takes effect - TODO move parameters to input file
    ibm[vac_time == breaks_vac[[1]] + 1, vac_eff_dose := 1L]
    ibm[vac_time == breaks_vac[[2]] + 1, vac_eff_dose := 2L]

  }

  #})
  return(list(test_positive = sim_test_positive[, , ], test_positive_vac = sim_test_positive_vac[, , , ],
              hospital = sim_hospital[, , ], hos_vac = sim_hospital_vac[, , , ],
              parish = sim_parish[, ], kom = sim_municipality[, ], par_id = par_id))
}
gc()
toc <- Sys.time()

toc - tic

stopImplicitCluster()

rm(ibm)
par_string <- paste0("popIBM", substr(Sys.time(), 1, 10), "rep", n_samples, "perT", frac_n_tests * 100)

# Save output together with parameters
save.image(file = paste0("./", par_string, ".RData"))
