# We first create a simple model without age_groups

# Define model stratifications
age_cuts_lower <- 0 # Splits in the age groups

serotype_groups <- list( # Using German-splits for testing
  "G1" = c("4", "6B", "9V", "14", "18C", "19F", "23F"),
  "G2" = c("1", "5", "6A", "6C", "7F", "19A"),
  "G3" = "3",
  "G4" = c("22F", "33F"),
  "G5" = c("8", "10A", "11A", "12F", "15B"),
  "G6" = c("2", "9N", "17F", "20"),
  "G7" = "*"
)

carriage_states <- c(
  # Single carriage
  names(serotype_groups),

  # Double carriage
  tidyr::expand_grid(names(serotype_groups), names(serotype_groups)) |>
    purrr::pmap_chr(~ paste(sort(union(..1, ..2)), collapse = "", sep = "")) |>
    unique() |>
    sort()
) |>
  unique()

# SIR(S)
compartment_structure <- c("I" = 1L, "R" = 1L)


# Define waning immunity scenario
immunity <- diseasy::DiseasyImmunity$new()

# TODO : How to match havelåge function
immunity$set_sigmoidal_waning()

# TODO: A non protective scenario (i.e SIS model)




####

# Define some helpers
n_age_groups <- length(age_cuts_lower)
n_carriage_states   <- length(carriage_states)
n_IR_states <- sum(compartment_structure)
n_states     <- n_age_groups * (n_IR_states * n_carriage_states + 1)



# Get risks and accompanying rates from DiseasyImmunity
immunity_approx <- immunity$approximate_compartmental(M = compartment_structure[["R"]])

immunity_risks <- 1 - immunity_approx$gamma$infection
immunity_rates <- immunity_approx$delta



# Configure the passive inflow/outflow to/from the compartments
# That is, the flows that are arise from the disease's natural progression within an individual
# This vector can then be multiplied by the state vector to give the flow out of each compartment
# and also shifted to give the flow into each compartment
# The state vector is assumed to be ordered as follows:
# [ [I, R]_age_group_1_variant_1, [I, R]_age_group_2_variant_1, ..., S ]

# First we need to convert disease_progression_rates so it matches the given compartment structure
# and uses the approximated immunity rates

# Scale the given rates for each compartment set (I) so overall rate is conserved
# then add the R rates from the immunity scenario
progression_flow_rates <- c(
  rep(
    compartment_structure["I"] * ri,
    compartment_structure["I"]
  ),
  immunity_rates,
  0
)

# Above, we have the progression rate for each "track" in the model
# We now repeat for each track the model to construct the full vector
# and add rates for the S states at the end.
progression_flow_rates <- c(
  rep(
    progression_flow_rates,
    private %.% n_age_groups * private %.% n_carriage_states
  ),
  rep(0, private %.% n_age_groups) # Add a zero for the S compartments
)





# Write the RHS function to solve
rhs <- function(t, state_vector, parms = NULL) {

  # Compute the flow from infections
  # Each serotype group attempts to infect the population

  ## Step 1, determine the number of infected by age group and serotype group

  # If the number of infected is the tensor I_{v,a,k}, then we need the matrix I_{a,v} = sum_k I_{a,v,k}
  infected <- vapply(
    i_state_indices,
    \(idx) sum(state_vector[idx]),
    FUN.VALUE = numeric(1),
    USE.NAMES = FALSE
  )

  # Reshape the infected vector to a matrix for later computation
  infected <- matrix(infected, nrow = n_age_groups)

  ## Step 2, determine their contacts with other age groups (beta * I)
  infected_contact_rate <- contact_matrix(t) %*% infected


  ## Step 3, apply the effect of season, overall infection risk, and variant-specific relative infection risk
  # rr * beta * beta_v * I * s(t)                                                                                   # nolint: commented_code_linter
  infection_rate <- infected_contact_rate *
    season$model_t(t + unclass(training_period$end - season$reference_date)) *
    overall_infection_risk


  ## Step 4, determine the infective interactions
  # We use the pre computed immunity_matrix to account for waning and cross-immunity
  infection_matrix <- immunity_matrix * state_vector[rs_state_indices] *
    infection_rate[rs_age_group, , drop = FALSE]  # R challenge: "respect data-types". Level: Impossible

  # Then we can compute the loss from each compartment
  loss_due_to_infections <- rowSums(infection_matrix)

  # Now we need to compute the flow into the exposed compartments
  # For this, we use the pre-computed infection_matrix_to_rs_indices map
  new_infections <- vapply(
    infection_matrix_to_rs_indices,
    \(idx) sum(infection_matrix[idx]),
    FUN.VALUE = numeric(1),
    USE.NAMES = FALSE
  )


  ## Step 5, compute the disease progression flow in the model
  progression_flow <- progression_flow_rates * state_vector


  ## Combine into final RHS computation
  # Disease progression flow between compartments
  dy_dt <- c(0, progression_flow[-n_states]) - progression_flow

  # Combined loss to infections (across all variants)
  dy_dt[rs_state_indices] <- dy_dt[rs_state_indices] - loss_due_to_infections

  # Add the inflow from infections
  dy_dt[e1_state_indices] <- dy_dt[e1_state_indices] + new_infections

  return(list(dy_dt))
}


# Define starting conditions

# y0 = ...



# Solve ODE system
t <- seq(
  from = as.Date("2025-06-25"),
  to = as.Date("2040-06-25"),
  by = "60 days"
)

sol <- deSolve::ode(y0, t, func = rhs)


# Plot solution
# plot(t, ...)