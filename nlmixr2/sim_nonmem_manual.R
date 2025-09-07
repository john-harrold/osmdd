library(babelmixr2)
set.seed(5446)
rxode2::rxSetSeed(5446)

resFileName <- system.file("mods/cpt/runODE032.res", package="nonmem2rx")
mod  <- nonmem2rx(resFileName, validate = TRUE, save=FALSE)

# All of the original subjects in the nonmem analysis:
sub_orig =  mod$ini |>
  dplyr::filter(name %in% mod$props$pop)             |>
  dplyr::select("name", "est")                       |>
  tidyr::pivot_wider(
    names_from="name",
    values_from="est")                               |>
  cbind(mod$etaData)                                 |> 
  dplyr::relocate(ID)                                |>
  dplyr::rename(id = ID)

# This contains parameters, but can also include non-time-varying covairates as
# well. Here we're creating a fake weight column so I can do some weight-based
# dosing below.
sub_orig[["BW"]] = 70*exp(rnorm(n=nrow(sub_orig), mean=0, sd=.1))

# Resampling to create parameters for subjects to perform a new simulation
nSub <- 200
sub_sim = sub_orig[sample(nrow(sub_orig), nSub, replace=TRUE), ] |>
  dplyr::mutate(id = 1:nSub)

ev <- et()
for(sidx in 1:nSub){
  tmp_dose <- 1700*sub_sim[sidx, "BW"]
  tmp_id   <- sub_sim[sidx, "id"]
  ev <- ev |>
        et(id  = tmp_id,      
          amt  = tmp_dose,    # 10 doses 
          time = 0,           # First dose
          addl = 9,           # 9 additional doses
          ii   = 12)      |>  # Every 12 hours
        et(id  = tmp_id, 
          amt  = tmp_dose,    # Followed by 6 doses every 24 hours 
          time = 120,         # First dose at 120 hours
          addl = 5,           # Five additional doses
          ii   = 24)          # Every 24 hours
}

# Adding hourly sampling to make the plots smooth:
ev <- ev |> add.sampling(time=0:272)

# The sim_sub contains subject level parameters as well as any non-time-varying covariates 
# and the events contains the subject level dosing information 
sim_res = rxSolve(mod, params=sub_sim, events=ev)

# This shows how to use a single table for simulations. This is how it's
# normally done in NONMEM and mrgsolve and it's what you need to do if you have
# time-varying covariates.

# This combines the events with the subject level. It shoild include parameters
# (post hoc estimates) and covaraites if you have anything. 
evall = as.data.frame(ev) |>
  dplyr::left_join(sub_sim, by=c("id"="id"))

sim_res = rxSolve(mod, events=evall)

confint(sim_res, "ipred", level=0.95) |>
  plot()
