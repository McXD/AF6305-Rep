# This script:
# 1. Run Fama-Macbeth regression on Size, BM, MOM, IVOL, RET

source("r/lib.R")

crsp_monthly <- tbl(db, "crsp_monthly") |>
  select(permno, month, ret) |>
  collect()

factors <- tbl(db, "factors_size_bm_mom") |>
  select(permno, month, size, bm, mom) |>
  collect()

betas <- tbl(db, "betas_ff3") |>
  select(permno, month, ivol = res_std) |>
  collect()

data <- crsp_monthly |>
  inner_join(factors, by = c("permno", "month")) |>
  inner_join(betas, by = c("permno", "month")) |>
  inner_join(crsp_monthly |> select(permno, month, ret_lead = ret) |> mutate(month = month %m-% months(1)), by = c("permno", "month")) |>
  drop_na() |>
  group_by(month) |>
  mutate(
    size = Winsorize(size, probs = c(0.005, 0.995)),
    bm = Winsorize(bm, probs = c(0.005, 0.995)),
    mom = Winsorize(mom, probs = c(0.005, 0.995)),
    ivol = Winsorize(ivol, probs = c(0.005, 0.995))
  ) |>
  ungroup()

fm(data, month, ret_lead ~ bm + size + mom + ivol + ret)

# Generate Report Artifact ------------------------------------------------

# Ref BM Table 6.3
spec1 <- fm(data, month, ret_lead ~ bm)
spec2 <- fm(data, month, ret_lead ~ size)
spec3 <- fm(data, month, ret_lead ~ mom)
spec4 <- fm(data, month, ret_lead ~ ivol)
spec5 <- fm(data, month, ret_lead ~ ret)
spec6 <- fm(data, month, ret_lead ~ bm + size + mom + ivol + ret)

result_fm <- bind_rows(
  spec1 |> mutate(spec = "1"),
  spec2 |> mutate(spec = "2"),
  spec3 |> mutate(spec = "3"),
  spec4 |> mutate(spec = "4"),
  spec5 |> mutate(spec = "5"),
  spec6 |> mutate(spec = "6")
) |> select(spec, everything()) |>
  pivot_longer(cols = c(estimate, nw_t_stat), names_to = "statistic", values_to = "value")

dbWriteTable(db, "artifact_11_fm", result_fm, overwrite = TRUE)
