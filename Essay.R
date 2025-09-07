library(tidyverse)
library(stargazer)
lijphart <- read.csv("lijphart_data.csv")
giuliani <- read.csv("lijphart-data-revisited.csv")
lijphart$new <- NA
lijphart$new <- seq.int(nrow(lijphart))
giuliani$new <- NA
giuliani$new <- seq.int(nrow(giuliani))
combined <- full_join(lijphart, giuliani, by="new")

# WGI
# Reproduce Lijphart's test
model_dm1 <- lm(data = lijphart, govt_effectiveness_1996_2009 ~ exec_parties_1981_2010 + hdi_2010 + log(pop_in_thousands_2009))
model_dm2 <- lm(data = lijphart, regulatory_quality_1996_2009 ~ exec_parties_1981_2010 + hdi_2010 + log(pop_in_thousands_2009))
model_dm3 <- lm(data = lijphart, rule_of_law_1996_2009 ~ exec_parties_1981_2010 + hdi_2010 + log(pop_in_thousands_2009))
model_dm4 <- lm(data = lijphart, control_of_corruption_1996_2009 ~ exec_parties_1981_2010 + hdi_2010 + log(pop_in_thousands_2009))
model_dm5 <- lm(data = lijphart, corruption_perception_index_2010 ~ exec_parties_1981_2010 + hdi_2010 + log(pop_in_thousands_2009))
models_dm <- list(model_dm1, model_dm2, model_dm3, model_dm4,model_dm5)
stargazer(models_dm, 
          title="Multivariate regression analyses of the effect of consensus democracy (executives-parties dimension) on government performance variables, with controls for the effects of the level of economic development and logged population size, and with extreme outliers removed",
          notes.label = "Significance levels",
          dep.var.caption = "DV: WGI measures",
          covariate.labels = c("Consensus (1981-2010)", "Human Development Index (2010)", "Population Logged (2009)"),
          keep.stat = c("n","rsq","adj.rsq"),
          out='wgi_original.html')

# Evaluate Each Component of the Consensus Score
# Consensus 4 Replication
model_dmc1 <- lm(data = giuliani, Gvt_effectiveness_96_09 ~ Cons4_81_10 + Corp_81_10 + Hdi_10 + logpop)
model_dmc2 <- lm(data = giuliani, Reg_quality_96_09 ~ Cons4_81_10 + Corp_81_10 + Hdi_10 + logpop)
model_dmc3 <- lm(data = giuliani, Rule_law_96_09 ~ Cons4_81_10 + Corp_81_10 + Hdi_10 + logpop)
model_dmc4 <- lm(data = giuliani, Ctrl_Corrup_96_09 ~ Cons4_81_10 + Corp_81_10 + Hdi_10 + logpop)
model_dmc5 <- lm(data = giuliani, Corrup_percep_2010 ~ Cons4_81_10 + Corp_81_10 + Hdi_10 + logpop)
models_dmc <- list(model_dmc1, model_dmc2, model_dmc3, model_dmc4, model_dmc5)
stargazer(models_dmc, 
          title="Consensualism and worldwide governance indicators: Replication with Consensus 4 and Corporatism",
          notes.label = "Significance levels",
          dep.var.caption = "DV: WGI measures",
          covariate.labels = c("Consensus 4 (1981-2010)", "Corporatism (1981-2010)", "Human Development Index (2010)", "Population Logged (2009)"),
          keep.stat = c("n","rsq","adj.rsq"),
          out='wgi_consensus4.html')

# Macroeconomic Performance
# Reproduce Lijphart's test
# Exclusion1
ex1 <- combined %>% filter(ccodealp != 'ISR')
ex2 <- combined %>% filter(ccodealp != 'NOR')
model_ep1 <- lm(data = combined, Growth_81_09 ~ exec_parties_1981_2010 + hdi_2010 + log(pop_in_thousands_2009))
model_ep1b <- lm(data = combined, Growth_91_09 ~ exec_parties_1981_2010 + hdi_2010 + log(pop_in_thousands_2009))
model_ep2 <- lm(data = ex1, Inflation1_81_09 ~ exec_parties_1981_2010 + hdi_2010 + log(pop_in_thousands_2009))
model_ep2b <- lm(data = ex1, Inflation2_81_09 ~ exec_parties_1981_2010 + hdi_2010 + log(pop_in_thousands_2009))
model_ep3 <- lm(data = ex1, Inflation1_91_09 ~ exec_parties_1981_2010 + hdi_2010 + log(pop_in_thousands_2009))
model_ep3b <- lm(data = ex1, Inflation2_91_09 ~ exec_parties_1981_2010 + hdi_2010 + log(pop_in_thousands_2009))
model_ep4 <- lm(data = combined, Unempl_81_09 ~ exec_parties_1981_2010 + hdi_2010 + log(pop_in_thousands_2009))
model_ep4b <- lm(data = combined, Unempl_91_09 ~ exec_parties_1981_2010 + hdi_2010 + log(pop_in_thousands_2009))
model_ep5 <- lm(data = ex2, Budget_00_08 ~ exec_parties_1981_2010 + hdi_2010 + log(pop_in_thousands_2009))
model_ep5b <- lm(data = ex2, Budget_03_07 ~ exec_parties_1981_2010 + hdi_2010 + log(pop_in_thousands_2009))
models_ep <- list(model_ep1, model_ep1b, model_ep2, model_ep2b, model_ep3, model_ep3b, model_ep4, model_ep4b, model_ep5, model_ep5b)
stargazer(models_ep, 
          title="Multivariate regression analyses of the effect of consensus democracy (executives-parties dimension) on macroeconomic indicators",
          notes.label = "Significance levels",
          dep.var.caption = "DV: Macroeconomic Indicators",
          covariate.labels = c("Consensus (1981-2010)", "Human Development Index (2010)", "Population Logged (2009)"),
          keep.stat = c("n","rsq","adj.rsq"),
          out='ep_original.html')

# Evaluate Each Component of the Consensus Score
# Consensus 4 Replication
model_epc1 <- lm(data = combined, Growth_81_09 ~ Cons4_81_10 + Corp_81_10 + hdi_2010 + log(pop_in_thousands_2009))
model_epc1b <- lm(data = combined, Growth_91_09 ~ Cons4_81_10 + Corp_81_10 + hdi_2010 + log(pop_in_thousands_2009))
model_epc2 <- lm(data = ex1, Inflation1_81_09 ~ Cons4_81_10 + Corp_81_10 + hdi_2010 + log(pop_in_thousands_2009))
model_epc2b <- lm(data = ex1, Inflation2_81_09 ~ Cons4_81_10 + Corp_81_10 + hdi_2010 + log(pop_in_thousands_2009))
model_epc3 <- lm(data = ex1, Inflation1_91_09 ~ Cons4_81_10 + Corp_81_10 + hdi_2010 + log(pop_in_thousands_2009))
model_epc3b <- lm(data = ex1, Inflation2_91_09 ~ Cons4_81_10 + Corp_81_10 + hdi_2010 + log(pop_in_thousands_2009))
model_epc4 <- lm(data = combined, Unempl_81_09 ~ Cons4_81_10 + Corp_81_10 + hdi_2010 + log(pop_in_thousands_2009))
model_epc4b <- lm(data = combined, Unempl_91_09 ~ Cons4_81_10 + Corp_81_10 + hdi_2010 + log(pop_in_thousands_2009))
model_epc5 <- lm(data = ex2, Budget_00_08 ~ Cons4_81_10 + Corp_81_10 + hdi_2010 + log(pop_in_thousands_2009))
model_epc5b <- lm(data = ex2, Budget_03_07 ~ Cons4_81_10 + Corp_81_10 + hdi_2010 + log(pop_in_thousands_2009))
models_epc <- list(model_epc1, model_epc1b, model_epc2, model_epc2b, model_epc3, model_epc3b, model_epc4, model_epc4b, model_epc5, model_epc5b)
stargazer(models_epc, 
          title="Consensualism and macroeconomic performance: Replication with Consensus 4 and Corporatism",
          notes.label = "Significance levels",
          dep.var.caption = "DV: Macroeconomic Indicators",
          covariate.labels = c("Consensus 4 (1981-2010)", "Corporatism (1981-2010)", "Human Development Index (2010)", "Population Logged (2009)"),
          keep.stat = c("n","rsq","adj.rsq"),
          out='ep_consensus4.html')


# Control of Violence
# Replication of Lijphart's Analysis
# Remove India and Israel
sin_is_in <- combined %>% filter(ccodealp != "IND" & ccodealp != "ISR")
# Remove UK
sin_is_in_uk <- sin_is_in %>% filter(ccodealp != 'GBR')
# Consensus 5 Replication
model_v1 <- lm(data = sin_is_in, pol_stab_and_absence_of_violence_1996_2009 ~ exec_parties_1981_2010 + hdi_2010 + log(pop_in_thousands_2009) + plural_society_code)
model_v2 <- lm(data = sin_is_in, internal_conflict_risk_1990_2004 ~ exec_parties_1981_2010 + hdi_2010 + log(pop_in_thousands_2009) + plural_society_code)
model_v3 <- lm(data = sin_is_in_uk, weighted_domestic_conflict_1981_2009 ~ exec_parties_1981_2010 + hdi_2010 + log(pop_in_thousands_2009) + plural_society_code)
model_v4 <- lm(data = sin_is_in_uk, weighted_domestic_conflict_1990_2009 ~ exec_parties_1981_2010 + hdi_2010 + log(pop_in_thousands_2009) + plural_society_code)
model_v5 <- lm(data = sin_is_in, deaths_from_domestic_terrorism_1985_2010 ~ exec_parties_1981_2010 + hdi_2010 + log(pop_in_thousands_2009) + plural_society_code)
models_v <- list(model_v1, model_v2, model_v3, model_v4, model_v5)
stargazer(models_v, 
          title="Multivariate regression analyses of the effect of consensus democracy (executives-parties dimension) on control of violence",
          notes.label = "Significance levels",
          dep.var.caption = "DV: Control of Violence Indicators",
          covariate.labels = c("Consensus (1981-2010)", "Human Development Index (2010)", "Population Logged (2009)"),
          keep.stat = c("n","rsq","adj.rsq"),
          out='violence_original.html')
# Consensus 4 Replication
model_vc1 <- lm(data = sin_is_in, pol_stab_and_absence_of_violence_1996_2009 ~ Cons4_81_10 + Corp_81_10 + hdi_2010 + log(pop_in_thousands_2009) + plural_society_code)
model_vc2 <- lm(data = sin_is_in, internal_conflict_risk_1990_2004 ~ Cons4_81_10 + Corp_81_10 + hdi_2010 + log(pop_in_thousands_2009) + plural_society_code)
model_vc3 <- lm(data = sin_is_in_uk, weighted_domestic_conflict_1981_2009 ~ Cons4_81_10 + Corp_81_10 + hdi_2010 + log(pop_in_thousands_2009) + plural_society_code)
model_vc4 <- lm(data = sin_is_in_uk, weighted_domestic_conflict_1990_2009 ~ Cons4_81_10 + Corp_81_10 + hdi_2010 + log(pop_in_thousands_2009) + plural_society_code)
model_vc5 <- lm(data = sin_is_in, deaths_from_domestic_terrorism_1985_2010 ~ Cons4_81_10 + Corp_81_10 + hdi_2010 + log(pop_in_thousands_2009) + plural_society_code)
models_vc <- list(model_vc1, model_vc2, model_vc3, model_vc4, model_vc5)
stargazer(models_vc,
          title="Consensualism and control of violence: Replication with Consensus 4 and Corporatism",
          notes.label = "Significance levels",
          dep.var.caption = "DV: Control of Violence Indicators",
          covariate.labels = c("Consensus 4 (1981-2010)", "Corporatism (1981-2010)", "Human Development Index (2010)", "Population Logged (2009)"),
          keep.stat = c("n","rsq","adj.rsq"),
          out='violence_consensus4.html')
