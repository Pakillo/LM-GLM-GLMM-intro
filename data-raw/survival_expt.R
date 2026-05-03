set.seed(8)

dt <- DHARMa::createData(family = binomial(), binomialTrials = 10, numGroups = 1,
                         sampleSize = 50, overdispersion = 2, fixedEffects = 1)

dt <- dt |>
  dplyr::select(Survived = observedResponse1,
                Died = observedResponse0,
                Stress = Environment1)

readr::write_csv(dt, "data/survival_expt.csv")
