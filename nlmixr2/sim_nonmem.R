library(babelmixr2)
set.seed(5446)
rxode2::rxSetSeed(5446)

# Based on course here:
# https://github.com/nlmixr2/courses/blob/main/ACoP2024/rxode2_simulation.R

# Questions for Matt
#  - does it look at all the output genereted by $TABLE for the PRED, IPRED IWRES?
#  - rxSolve sample from subjects vs generating subjects from IIV
#
# res or xml
# nonmem analysis dataset
# patab
# sdtab
resFileName <- system.file("mods/cpt/runODE032.res", package="nonmem2rx")
mod  <- nonmem2rx(resFileName, validate = TRUE, save=FALSE)

xmlFileName <- system.file("mods/cpt/runODE032.xml", package="nonmem2rx")
# Show the rxode2 model
rxode_mod_str = paste0(deparse(as.function(mod)), collapse="\n")
cat(rxode_mod_str)

# Validation
plot(mod)
plot(mod, page=1, log="y") 

# Flat dosing simulation event table
ev <- et(amt=120000, time=0, addl=9, ii=12)   |>  # 10 doses (first +9 addl every 12 hours)
      et(amt=120000, time=120, addl=5, ii=24) |>  # Follwed by 6 doses every 24 hours
      et(0, 200, length.out=100)

ev

# Individual simulation using the typical values (do not specify nSub)
# https://nlmixr2.github.io/rxode2/articles/rxode2-single-subject.html
sim_ind <- rxSolve(mod, ev)
plot(sim_ind, ipred)

# Accessing the dataframe of the simulation
sim_ind_res <- as.data.frame(sim)

# Population simulation from iiv (specify nSub)
# https://nlmixr2.github.io/rxode2/articles/rxode2-sim-var.html
nsub = 100
sim_pop <- rxSolve(mod, ev, nSub=nsub)
confint(sim_pop, "ipred", level=0.95) |>
  plot()

# Accessing the dataframe of the simulations
sim_pop_res <- as.data.frame(sim_pop)
