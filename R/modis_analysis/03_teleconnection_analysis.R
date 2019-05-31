# Copyright 2019 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

#### 5. CALC BY BC == LM AND COR (TEL/MSM) ####
n_iterations = 10000

# ANNUAL ANALYSIS
mod_lm_BC_year_mean  = lm_iter(df_BC_year_mean, groups = c("measurement","tel"), Y = "days", X = "index", textName = "sup01_mod_lm_BC_year_mean")
mod_cor_BC_year_mean = cor_iter(df_BC_year_mean, groups = c("measurement","tel"), Y = "days", X = "index", textName = "sup02_mod_cor_BC_year_mean")

# SEASONAL ANALYSIS

mod_lm_BC_seas_mean  = lm_iter(df_BC_seas_mean, groups = c("measurement","tel","season"), Y = "days", X = "index", textName = "sup03_mod_lm_BC_seas_mean")
mod_cor_BC_seas_mean = cor_iter(df_BC_seas_mean, groups = c("measurement","tel","season"), Y = "days", X = "index", textName = "sup04_mod_cor_BC_seas_mean")

#### 6: CALC BY zone == LM AND COR (TEL/MSM) ####

# ANNUAL ANALYSIS

mod_lm_zone_year_mean = lm_iter(df_zone_year_mean, groups = c("measurement","tel",zone_name), Y = "days", X = "index", textName = "sup05_mod_lm_zone_year_mean")
mod_lm_zone_year_mean = merge(mod_lm_zone_year_mean, filter(df_zone_year_mean, year == 2002), by = c("measurement","tel",zone_name))

mod_cor_zone_year_mean = cor_iter(df_zone_year_mean, groups = c("measurement","tel",zone_name), Y = "days", X = "index", textName = "sup06_mod_cor_zone_year_mean")
mod_cor_zone_year_mean = merge(mod_cor_zone_year_mean, filter(df_zone_year_mean, year == 2002), by = c("measurement","tel",zone_name))

# SEASONAL ANALYSIS

mod_lm_zone_seas_mean  = lm_iter(df_zone_seas_mean, groups = c("measurement","tel","season",zone_name), Y = "days", X = "index", textName = "sup07_mod_lm_zone_seas_mean")
mod_lm_zone_seas_mean = merge(mod_lm_zone_seas_mean, filter(df_zone_seas_mean, year == 2002), by = c(zone_name, "measurement","tel","season"))

mod_cor_zone_seas_mean = cor_iter(df_zone_seas_mean, groups = c("measurement","tel","season",zone_name), Y = "days", X = "index", textName = "sup08_mod_cor_zone_seas_mean")
mod_cor_zone_seas_mean = merge(mod_cor_zone_seas_mean, filter(df_zone_seas_mean, year == 2002), by = c(zone_name, "measurement","tel","season"))

#### ZONE AND grZ

# ANNUAL ANALYSIS

mod_lm_zoneZ_year_mean = lm_iter(df_zoneZ_year_mean, groups = c("measurement","tel",zone_name, "grZ"), Y = "days", X = "index", textName = "sup09_mod_lm_zoneZ_year_mean")
mod_lm_zoneZ_year_mean = merge(mod_lm_zoneZ_year_mean, filter(df_zoneZ_year_mean, year == 2002), by = c("measurement","tel",zone_name, "grZ"))

mod_cor_zoneZ_year_mean = cor_iter(df_zoneZ_year_mean, groups = c("measurement","tel",zone_name, "grZ"), Y = "days", X = "index", textName = "sup10_mod_cor_zoneZ_year_mean")
mod_cor_zoneZ_year_mean = merge(mod_cor_zoneZ_year_mean, filter(df_zoneZ_year_mean, year == 2002), by = c("measurement","tel",zone_name, "grZ"))

# SEASONAL ANALYSIS

mod_lm_zoneZ_seas_mean  = lm_iter(df_zoneZ_seas_mean, groups = c("measurement","tel","season",zone_name, "grZ"), Y = "days", X = "index", textName = "sup11_mod_lm_zoneZ_seas_mean")
mod_lm_zoneZ_seas_mean = merge(mod_lm_zoneZ_seas_mean, filter(df_zoneZ_seas_mean, year == 2002), by = c(zone_name, "grZ", "measurement","tel","season"))

mod_cor_zoneZ_seas_mean = cor_iter(df_zoneZ_seas_mean, groups = c("measurement","tel","season",zone_name, "grZ"), Y = "days", X = "index", textName = "sup12_mod_cor_zoneZ_seas_mean")
mod_cor_zoneZ_seas_mean = merge(mod_cor_zoneZ_seas_mean, filter(df_zoneZ_seas_mean, year == 2002), by = c(zone_name, "grZ", "measurement","tel","season"))

