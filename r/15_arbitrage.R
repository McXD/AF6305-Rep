# This script
# 1. Run FM on arbitrage cost measures

source("r/lib.R")

# Construct the panel

crsp_monthly <- tbl(db, "crsp_monthly") |>
  collect()

amihud_illiq <- tbl(db, "amihud_illiq") |>
  collect()

crsp_io <- tbl(db, "crsp_io") |>
  collect()

crsp_disp <- tbl(db, "crsp_disp") |>
  collect()

size_bm_mom <- tbl(db, "factors_size_bm_mom") |>
  collect() |>
  mutate(size = log(size))

t <- crsp_monthly |>
  select(permno, month, ret) |>
  inner_join(amihud_illiq, by = c("permno", "month")) |>
  inner_join(crsp_io, by = c("permno", "month")) |>
  inner_join(crsp_disp, by = c("permno", "month")) |>
  inner_join(size_bm_mom, by = c("permno", "month"))

ret_lead <- t |>
  mutate(month = month %m-% months(1)) |>
  select(permno, month, ret_lead=ret)

panel <- t |>
  left_join(ret_lead, by = c("permno", "month")) |>
  filter_all(all_vars(!is.na(.) & !is.nan(.) & !is.infinite(.)))

fm(panel, month, ret_lead ~ amihud_illiq + inst_own + disp + size + bm + mom + bm * amihud_illiq)

# Generate Report Artifact ------------------------------------------------

spec1 <- fm(panel, month, ret_lead ~ size + bm + mom + amihud_illiq + amihud_illiq*bm + amihud_illiq*size + amihud_illiq*mom)
spec2 <- fm(panel, month, ret_lead ~ size + bm + mom + inst_own + inst_own*bm + inst_own*size + inst_own*mom)
spec3 <- fm(panel, month, ret_lead ~ size + bm + mom + disp + disp*bm + disp*size + disp*mom)
spec4 <- fm(panel, month, ret_lead ~ size + bm + mom + amihud_illiq + inst_own + disp)

result_fm_arbitrage <- bind_rows(
  spec1 |> mutate(spec = "1"),
  spec2 |> mutate(spec = "2"),
  spec3 |> mutate(spec = "3"),
  spec4 |> mutate(spec = "4")
) |> select(spec, everything()) |>
  pivot_longer(cols = c(estimate, nw_t_stat), names_to = "statistic", values_to = "value")

dbWriteTable(db, "artifact_15_fm_arbitrage", result_fm_arbitrage, overwrite = TRUE)
