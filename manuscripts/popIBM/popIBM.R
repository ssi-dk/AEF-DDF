# popIBM model - cleaned for publication in Communications in medicine titled:
# "Digital twin simulation modelling shows that
# mass testing and local lockdowns effectively controlled COVID-19 in Denmark"
# further commented for submission to ECDC

# This is a detailed, individual-based simulation model of SARS-CoV-2 transmission
# to evaluate mass testing and local lockdowns during the Alpha wave in Denmark
# in counterfactual scenarios.

# Denmark tested ten times more than median country in EU during alpha wave
# local lockdown was based on observed 7 day incidence in parish/municipality

# please note that the model is coded using data.table
# this has a specific syntax, please see: r-datatable.com for an introduction

library(doParallel)
library(data.table)

###########################################################
#### Parameters that can be sensibly changed by user ####
##########################################################

# number of repetitions - 100 in the publication
n_samples <- 1

# are automatic lockdowns activated in the model
activate_lockdown <- TRUE

# fraction of tests available compared to observed, 1=mass test, 0.1=limited test
frac_n_tests <- 0.1

# choose number of cores to be used by do par
use_cores <- 1

# choose number of threads to be used by data.table, likely do not work well with doParallel
setDTthreads(1)

# Print which elements are loaded
load_info <- FALSE

# the model cannot run without the input of data. - not submitted
# many parameters enter through this file, and also the basic structure of the individuals
# load("./popibm_init.Rdata")

# dates
new_end_times <- as.Date("2021-06-30")
end_times <- as.numeric(new_end_times) - as.numeric(as.Date("2020-01-01"))
times <- seq(start_denmark, end_times, 1)
xdates <- as.Date(times, origin = "2020-01-01")

# Proportion of transmission within municipality
w_municipality <- 0.9

# Seasonal factor (relative to estimate from Sweden)
season_fac <- 0.8

##############################################################
#### parameters beyond this point should NOT be changed #######
##############################################################

# vaccination groups to output
n_vac_gr_out <- 3
# days after vaccination groups are divided into
br_vac_out <- c(14, 14 + 28, Inf)

# data collection arrays
sim_tp2      <- array(0L, dim = c(length(times), n_age_groups, n_variants))
sim_hospital <- array(0L, dim = c(length(times), n_age_groups, n_variants))

# data collection arrays that include vaccination status
sim_tp2_vac      <- array(0L, dim = c(length(times), n_age_groups, n_variants, n_vac_gr_out))
sim_hospital_vac <- array(0L, dim = c(length(times), n_age_groups, n_variants, n_vac_gr_out))

# individuals are stored in the data.table called "ibm"
# a data.table is equivalent to a data.frame but has additional functionality
# each line in the data.table i equivalent to a person

# number of parishes in the input
n_parish <- ibm[, uniqueN(parish_id)]
# number of municipalities in the input
n_municipality <- ibm[, uniqueN(municipality_id)]

# data collection arrays
sim_parish       <- array(0L, dim = c(length(times), n_parish))
sim_municipality <- array(0L, dim = c(length(times), n_municipality))

# ids of the parish and municipalities
u_parish_ids       <- ibm[, unique(parish_id)]
u_municipality_ids <- ibm[, unique(municipality_id)]

# the population by parish and municipality - .N is data.table special character
population <- ibm[, .N, keyby = .(parish_id, municipality_id)]

# the population by parish and municipality from alternative sources
pop_parish <- parish[.(parish_id = u_parish_ids), `Indbyggertal i sogn`, on = "parish_id"]
tmp <- parish[, sum(`Indbyggertal i sogn`), by = .(municipality_id)]
pop_municipality <- tmp[.(municipality_id = u_municipality_ids), V1, on = "municipality_id"]
pop_dk <- NROW(ibm)
pop_age <- ibm[, .N, by = .(age_groups)]

dt_pop_municipality <- parish[, sum(`Indbyggertal i sogn`), by = .(municipality_id)]
names(dt_pop_municipality)[2] <- "pop"

mfka <- data.table(municipality_id = rep(u_municipality_ids, each = 9), age_groups = 1:9)

# dates of changing restriction
day_cha   <- as.numeric(c(lockdown_sce_beta_list$Fyn$S5.3$list_beta_dates) - as.Date("2020-01-01")) - start_denmark

# list of activity matrices - age stratified
list_beta <- lockdown_sce_beta_list$Fyn$S5.3$list_beta

# dates of changing incidence limits for imposing local lockdown
day_lock_vec <- as.numeric(as.Date(c("2021-03-01", "2021-04-30", "2021-05-28", "2021-07-16", "2021-09-10", "2021-11-15")) - as.Date("2020-01-01")) - start_denmark

# functions for lockdown:
lockdown_parish_fun <- list(
  approxfun(x = c(300, 400), y = c(0, 0.5), yleft = 0, yright = 1),
  approxfun(x = c(375, 500), y = c(0, 0.5), yleft = 0, yright = 1),
  approxfun(x = c(450, 600), y = c(0, 0.5), yleft = 0, yright = 1),
  approxfun(x = c(750, 1000), y = c(0, 0.5), yleft = 0, yright = 1),
  approxfun(x = c(1000, 4000), y = c(0, 0.2), yleft = 0, yright = 0.5),
  approxfun(x = c(800, 3200), y = c(0, 0.2), yleft = 0, yright = 0.5)
)

lockdown_municipality_fun <- list(
  approxfun(x = c(150, 200), y = c(0, 0.5), yleft = 0, yright = 1),
  approxfun(x = c(188, 250), y = c(0, 0.5), yleft = 0, yright = 1),
  approxfun(x = c(225, 300), y = c(0, 0.5), yleft = 0, yright = 1),
  approxfun(x = c(375, 500), y = c(0, 0.5), yleft = 0, yright = 1),
  approxfun(x = c(500, 2000), y = c(0, 0.2), yleft = 0, yright = 0.5),
  approxfun(x = c(400, 1600), y = c(0, 0.2), yleft = 0, yright = 0.5)
)

# Automatic for last date i data (ntal)
day_fix_p_test <- as.numeric(ntal[, as.Date(max(pr_date))] - as.Date("2020-01-01")) - start_denmark  # Update

#
red_vac_fac_trans <- ifelse(exists("input_red_vac_fac_trans"), input_red_vac_fac_trans, 0.1) # reduction factor on transmission when effectively vaccinated # .1 / .2 / .3
red_prob_hospital <- 0.25 # reduction factor on risk going to hospital when effectively vaccinated

# introducing delta variant in simulation
day_delta_intro_sce <- as.numeric(as.Date("2021-06-01") - as.Date("2020-01-01")) - start_denmark
prob_delta_intro <- 0.02 # Converting X% of infected to delta variant on this day
variant_id_delta <- 3 # Variant id for delta

sce_fac_cur_beta <- 1 # should be 1.05 if 5% increase, 0.95 if 5% decrease
sce_test_red <- 1 # Factor for probability of taking a test
test_red_fac <- 1 # Internal copy of sce_test_red when paste date

registerDoParallel(cores = use_cores)

sce_fac_cur_beta_vec <- ifelse(exists("input_fac_beta"), input_fac_beta, 1) # Update if given as input

# maximal number of vaccination doses in the simulation - depend on end time
n_max_doses <- 3

# Set seed for generating parameter combinations
set.seed(ifelse(exists("input_seed"), input_seed, 1))

# this simulation was implemented at a time with uncertainties on delta variant parameters
# therefore scenarios of different parameter values for delta are included
sce_combi <- data.frame(
  par_id = 1:n_samples,
  sce_fac_cur_beta = sce_fac_cur_beta_vec,
  delta_rec_red = 1 - runif(n_samples, min = 0.6, max = 0.8), # VE of infection
  red_vac_fac_trans = 1 - runif(n_samples, min = 0.5, max = 0.8), # Transmission
  rel_alpha_delta = runif(n_samples, min = 1.65, max = 1.95)
)

first_run <- ifelse(exists("input_start"), input_start, 1)
n_runs <- ifelse(exists("input_n_runs"), input_n_runs, n_samples) # Run all if not specified


tic <- Sys.time()

# branch out to parallel processes
sim_list <- foreach(run_this = (first_run - 1 + 1:n_runs), .packages = "data.table", .verbose = TRUE) %dopar% {
  tmp <- unlist(sce_combi[run_this, ])
  for (i in seq_along(tmp)) {
    assign(x = names(tmp)[i], value = tmp[i])
  }
  cat("\t run: ", run_this, " ")
  # start and end times are in init file
  delta_red_prob_hospital <- (1 - 0.9) * red_prob_hospital / delta_rec_red * 2
  delta_vac_effect <- delta_rec_red * c(1, 1)

  rm("ibm")
  # make sure parameters are loaded fresh onto all cores
  load("./popibm_init.Rdata")

  end_times <- as.numeric(new_end_times) - as.numeric(as.Date("2020-01-01"))
  times <- seq(start_denmark, end_times, 1)
  xdates <- as.Date(times, origin = "2020-01-01")

  # initialise spatial heterogeneity in parishes
  ibm[, lockdown_fac := 1.]
  ibm[, rel_risk_parish := rel_risk_parish^(1 / 3)]
  ibm[, rel_risk_parish := rel_risk_parish * .N / sum(rel_risk_parish)]

  # set initial parameters, that will change over time
  ibm[, non_iso := 1L]
  ibm[, p_test := 2e5 / pop_dk]

  # make sure individual already vaccinated are correctly labelled
  ibm[, vac_fac_trans := 1.0]
  ibm[vac_fac < 1, vac_fac_trans := red_vac_fac_trans]

  ibm[, vac_eff_dose := 0L]
  ibm[vac_time > 14, vac_eff_dose := 1L]
  ibm[vac_time > (14 + 28), vac_eff_dose := 2L]

  # Setting seed per rep
  set.seed(123456 + run_this - 1)

  # change some params
  v_rel_beta <- c(1, 1.55, 1.55 * rel_alpha_delta)
  v_scale_i <- rep(5.3, 9) / v_shape_i
  ibm[disease == 2L, tt := tt + round(rexp(.N, 1 / 2))]

  # get history of incidence in parish/municipality
  inc_his_parish <- array(0, dim = c(length(times), n_parish))
  inc_his_municipality <- array(0, dim = c(length(times), n_municipality))

  # index individuals for faster runtime
  setkey(ibm, municipality_id, parish_id, age_groups, vac_maal_gr)

  # reference transmission risk scaling - fitted prior
  r_ref <- 0.7

  # profiler for testing bottlenecks in code - only for test runs
  # profvis({

  # time loop
  for (day in seq_along(times)) {

    # set "beta" based on restriction levels and seasonal change
    if (day > day_cha[1]) {
      i_beta <- max(which(day_cha <= day))
      cur_beta <- (1 - season_fac * (1 - seasonal_rel_beta(as.Date(start_denmark, origin = "2020-01-01"), day))) *
        r_ref * 0.35 * list_beta[[i_beta]]
      lockdown_factor <- sqrt(eigen(list_beta[[1]])$values[1] / eigen(list_beta[[i_beta]])$values[1])
    } else {
      cur_beta <-  (1 - season_fac * (1 - seasonal_rel_beta(as.Date(start_denmark, origin = "2020-01-01"), day))) *
        r_ref * 0.35 * list_beta[[1]]

    }


    ## Change some to delta variant
    if (day == day_delta_intro_sce && prob_delta_intro > 0) {
      prob <- c(1 - prob_delta_intro, prob_delta_intro)
      ibm[variant == 2, variant := sample(c(2, variant_id_delta), size = .N, replace = TRUE, prob = prob)]
    }

    # make p_test ~ 7 day incidence
    n_test <- n_test_dk(as.Date(start_denmark, origin = "2020-01-01"), day)
    n_test_age <- n_test_dk_age(as.Date(start_denmark, origin = "2020-01-01"), day)
    n_test_age_vac <- n_test_dk_age_vac(as.Date(start_denmark, origin = "2020-01-01"), day)

    # adjust number of test according to scenario
    n_test <- n_test * frac_n_tests
    n_test_age$w_test <- n_test_age$w_test * frac_n_tests
    n_test_age_vac$w_test <- n_test_age_vac$w_test * frac_n_tests

    # when incidences are available, adjust test behaviour according to incidence
    if (day > 7) {
      # LAEC2: not including today
      inc <- colSums(sim_municipality[(day - 7):(day - 1), ], na.rm = TRUE) / pop_municipality * 1e5
      p_test_corr <- p_test_inc(inc)

      if (day <= day_fix_p_test) {
        # should maybe be done per municipality
        tmp <- ibm[, .N, keyby = .(age_groups, !(vac_time < 14 | is.na(vac_time)))]
        t_pop_age_vac <- tmp[n_test_age_groups_int_vac, , on = c("age_groups", "vac_time")]
        t_pop_age_vac[is.na(N), N := 0]

        tmp <- ibm[, .(pop = .N), keyby = .(municipality_id, age_groups, !(vac_time < 14 | is.na(vac_time)))]
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

    } else {
      ibm[, p_test := (n_test[1, 1] + 0.5 * n_test[2, 1]) / pop_dk * test_red_fac]
    }

    # determine who is detected by tests
    id_tp <- ibm[non_iso == 1L & (disease %in% 1:2 | (disease == 3L & tt >= -5)) &
                   (is.na(vac_type)  | vac_time < 14),
                 .(id, p_test)][runif(.N) < p_test, id]
    ibm[id %in% id_tp, tt_symp := 0L] # a little ugly but faster

    # collect data on the number of test positives each day - by variant, age and vaccination status
    for (k in 1:n_variants) {
      sim_tp2[day, , k] <- ibm[
        tt_symp == 0L & variant == k, .N, by = .(age_groups)
      ][.(age_groups = 1:9), on = "age_groups"]$N

      sim_tp2_vac[day, , k, 1] <- ibm[tt_symp == 0L & variant == k &
                                        (vac_time < br_vac_out[1] | is.na(vac_time)), .N,
                                      by = .(age_groups)][.(age_groups = 1:9), on = "age_groups"]$N
      for (kk in 2:n_vac_groups_out) { # LAEC: 1 stik for sig
        sim_tp2_vac[day, , k, kk] <- ibm[
          tt_symp == 0L & variant == k & vac_time >= br_vac_out[kk - 1] & vac_time < br_vac_out[kk], .N,
          by = .(age_groups)
        ][.(age_groups = 1:9), on = "age_groups"]$N
      }
    }

    # collect number of test positives by parish
    sim_parish[day, ] <- ibm[
      tt_symp == 0L, .N, by = .(parish_id, age_groups)
    ][, sum(N), by = parish_id
    ][.(parish_id = u_parish_ids), on = "parish_id"]$V1

    # collect number of test positives by municipality
    sim_municipality[day, ] <- ibm[
      tt_symp == 0L, .N, by = .(municipality_id, age_groups)
    ][, sum(N), by = municipality_id
    ][.(municipality_id = u_municipality_ids), on = "municipality_id"]$V1

    # test positives isolate themselves
    ibm[tt_symp == 0L, non_iso := 0L]

    # collect probability of hospitalisation each day - by variant, age and vaccination status
    for (k in 1:n_variants) {
      sim_hospital[day, , k] <- ibm[disease == 2L & tt == 0 & variant == k, sum(prob_hospital),
                                    by = .(age_groups)][.(age_groups = 1:9), on = "age_groups"]$V1

      sim_hospital_vac[day, , k, 1] <- ibm[
        disease == 2L & tt == 0 & variant == k & (vac_time < br_vac_out[1] | is.na(vac_time)),
        sum(prob_hospital),
        by = .(age_groups)
      ][.(age_groups = 1:9), on = "age_groups"]$V1

      for (kk in 2:n_vac_groups_out) {
        sim_hospital_vac[day, , k, kk] <- ibm[
          disease == 2L & tt == 0 & variant == k & vac_time >= br_vac_out[kk - 1] & vac_time < br_vac_out[kk],
          sum(prob_hospital),
          by = .(age_groups)
        ][.(age_groups = 1:9), on = "age_groups"]$V1
      }


    }

    # recover from disease I -> R
    ibm[disease == 2L & tt == 0, disease := 3L]

    # when all age groups have same disease progression E-> I
    # also draw time to being symptomatic
    ibm[
      disease == 1L & tt == 0,
      `:=`(
        disease = 2L,
        tt = pmax(1L, round(rgamma(.N, v_shape_i[1], scale = v_scale_i[1]))),
        tt_symp = pmax(1L, round(rgamma(.N, v_shape_tt_symp[1], scale = v_scale_tt_symp[1]))) *
          sample(c(1L, NA_integer_), .N, replace = TRUE, prob = c(0.5, 0.5))
      )
    ]

    # do not double count people found in E states
    ibm[disease == 2L & non_iso == 0L & tt_symp > 0, tt_symp := -1L]


    # implement the effects of local lockdown
    if (activate_lockdown && day > day_lock_vec[1]) {

      # lockdown
      i_lock <- sum(day > day_lock_vec)

      # parish
      n_cases <- colSums(sim_parish[(day - 7):(day - 1), ], na.rm = TRUE)

      inc_his_parish[day, ] <- (n_cases >= 20) * n_cases / pop_parish * 1e5

      max_7d_inc <- apply(inc_his_parish[(day - 6):day, ], 2, max, na.rm = TRUE)
      lockdown_parish_fac <- lockdown_parish_fun[[i_lock]](max_7d_inc)

      # municipality
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
    for (k in 1:n_variants) {
      # Calculate the infection pressure
      inf_pers_municipality <- ibm[disease == 2L & variant == k,
                                   .(inf_pers = sum(lockdown_fac * non_iso * vac_fac_trans)),
                                   by = .(municipality_id, age_groups)][mfka, on = c("municipality_id", "age_groups")]
      inf_pers_municipality[is.na(inf_pers), inf_pers := 0]
      inf_pers_municipality[, inf_pers := inf_pers * v_rel_beta[k]]

      inf_pres <- inf_pers_municipality[
        , cur_beta %*% inf_pers, by = .(municipality_id)
      ][, age_groups := rep(1:9, n_municipality)]

      names(inf_pres)[2] <- "r_inf_municipality"

      inf_pres <- dt_pop_municipality[inf_pres, , on = "municipality_id"]
      inf_pres[, r_inf_municipality := r_inf_municipality / pop]

      inf_pers <- inf_pers_municipality[, .(inf_pers = sum(inf_pers)), by = .(age_groups)]

      tmp <- inf_pers$inf_pers
      inf_pres_dk <- inf_pers[, .(r_inf_dK = sum(cur_beta[age_groups, ] * tmp / pop_dk)), by = .(age_groups)]

      inf_prestot <- inf_pres_dk[inf_pres, , on = "age_groups"]
      inf_prestot[, prob_inf := (1 - exp(-(1 - w_municipality) * r_inf_dK - w_municipality * r_inf_municipality))]

      tmp2 <- inf_prestot[, c(1, 3, 6)]
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
               variant = k)]
    }

    # count down to change in disease state or symptoms
    ibm[, tt := tt - 1L]
    ibm[, tt_symp := tt_symp - 1L]

    # count up to vaccination time
    ibm[, vac_time := vac_time + 1L]


    # implement the effect of vaccination
    for (k in 1:n__vac){

      ibm[vac_type == k & vac_time == v_vac_tt_effect[k],
          `:=`(vacF_ec = delta_vac_effect[k],
               prob_hospital = delta_red_prob_hospital * prob_hospital,
               vac_fac_trans = red_vac_fac_trans)]

    }

    # vaccination doses takes effect - TODO move parameters to input file
    ibm[vac_time == 15, vac_eff_dose := 1L]
    ibm[vac_time == (14 + 28 + 1), vac_eff_dose := 2L]

  }

  #})
  return(list(tp2 = sim_tp2[, , ], tp2_vac = sim_tp2_vac[, , , ], hospital = sim_hospital[, , ],
              hos_vac = sim_hospital_vac[, , , ], parish = sim_parish[, ], kom = sim_municipality[, ], par_id = par_id))
}
gc()
toc <- Sys.time()

toc - tic

stopImplicitCluster()

rm(ibm)
par_string <- paste0("popIBM", substr(Sys.time(), 1, 10), "rep", n_samples, "perT", frac_n_tests * 100)

# save output together with parameters
save.image(file = paste0("./", par_string, ".RData"))
