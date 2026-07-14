## multivariate-forecast-evaluation-figures.R
##
## Teaching figures for the "marginal is not joint" scoring motivation
## in the multivariate forecast evaluation slides.
##
## Produces three PNGs in sessions/slides/figures/:
##   1. ili-forecast-ribbon.png       one region, quantile fan (marginal)
##   2. ili-forecast-trajectories.png same region, sample paths (joint)
##   3. ili-trajectories-regions.png  sample paths across three regions
##
## The same fourth-root ARIMA(2,1,0) fit and bootstrap sample paths from
## hub-playground.qmd underlie all three figures. The ribbon and the
## single-region trajectory figures share axis limits so they can sit
## side by side on a slide: the ribbon shows only the spread at each
## week, while the trajectories keep the temporal dependence the ribbon
## discards.

library("nfidd.forecasting")
library("dplyr")
library("tsibble")
library("tidyr")
library("fable")
library("ggplot2")

theme_set(theme_bw(base_size = 16))

set.seed(406) # for Ted Williams

fig_dir <- here::here("sessions", "slides", "figures")

## --- Data and forecast settings -----------------------------------------

data(flu_data_hhs)

origin_date <- as.Date("2019-12-14")
history_start <- as.Date("2019-09-01")
horizon <- 8L
n_paths <- 75L

## fourth-root transform, as used throughout the course
fourth_root <- function(x) x^0.25
inv_fourth_root <- function(x) x^4
my_fourth_root <- new_transformation(fourth_root, inv_fourth_root)

regions <- c("HHS Region 1", "HHS Region 2", "HHS Region 3")

train <- flu_data_hhs |>
  filter(location %in% regions, origin_date <= !!origin_date)

history <- flu_data_hhs |>
  filter(
    location %in% regions,
    origin_date >= history_start,
    origin_date <= !!origin_date
  ) |>
  as_tibble()

## --- Fit and generate sample paths --------------------------------------

fit <- train |>
  model(arima210 = ARIMA(my_fourth_root(wili) ~ pdq(2, 1, 0)))

paths <- fit |>
  generate(h = horizon, times = n_paths, bootstrap = TRUE) |>
  as_tibble() |>
  group_by(location, .rep) |>
  mutate(horizon = row_number()) |>
  ungroup() |>
  rename(target_end_date = origin_date, value = .sim)

## anchor each path to the last observed value so lines flow from history
anchor <- history |>
  filter(origin_date == !!origin_date) |>
  transmute(location, target_end_date = origin_date, value = wili)

paths_region1 <- paths |>
  filter(location == "HHS Region 1")

paths_anchored <- paths |>
  bind_rows(
    tidyr::expand_grid(
      anchor,
      .rep = as.character(seq_len(n_paths))
    )
  )

## --- Quantile summary for the ribbon ------------------------------------

ribbon <- paths_region1 |>
  group_by(target_end_date) |>
  summarise(
    lower90 = quantile(value, 0.05),
    lower50 = quantile(value, 0.25),
    median = quantile(value, 0.5),
    upper50 = quantile(value, 0.75),
    upper90 = quantile(value, 0.95),
    .groups = "drop"
  )

## anchor the ribbon at the last observed value (zero-width interval)
ribbon <- bind_rows(
  anchor |>
    filter(location == "HHS Region 1") |>
    transmute(
      target_end_date,
      lower90 = value, lower50 = value, median = value,
      upper50 = value, upper90 = value
    ),
  ribbon
)

history_region1 <- history |> filter(location == "HHS Region 1")

## shared y-limits so ribbon and trajectory figures are comparable
y_max <- max(
  ribbon$upper90,
  paths_region1$value,
  history_region1$wili
)
y_lim <- c(0, y_max * 1.02)
x_lim <- c(history_start, max(paths$target_end_date))

fill_90 <- "#9ecae1"
fill_50 <- "#4292c6"
line_col <- "#08306b"
hist_col <- "grey20"

## --- Figure 1: ribbon (marginal) ----------------------------------------

p_ribbon <- ggplot() +
  geom_ribbon(
    data = ribbon,
    aes(x = target_end_date, ymin = lower90, ymax = upper90),
    fill = fill_90, alpha = 0.7
  ) +
  geom_ribbon(
    data = ribbon,
    aes(x = target_end_date, ymin = lower50, ymax = upper50),
    fill = fill_50, alpha = 0.7
  ) +
  geom_line(
    data = ribbon,
    aes(x = target_end_date, y = median),
    colour = line_col, linewidth = 1
  ) +
  geom_line(
    data = history_region1,
    aes(x = origin_date, y = wili),
    colour = hist_col, linewidth = 0.9
  ) +
  geom_vline(
    xintercept = origin_date, linetype = "dashed", colour = "grey50"
  ) +
  coord_cartesian(ylim = y_lim, xlim = x_lim) +
  labs(
    title = "Marginal forecast: uncertainty at each week",
    subtitle = "HHS Region 1 | 50% and 90% quantile intervals",
    x = NULL, y = "% of visits due to ILI"
  )

ggsave(
  file.path(fig_dir, "ili-forecast-ribbon.png"),
  p_ribbon, width = 1600 / 150, height = 900 / 150, dpi = 150
)

## --- Figure 2: trajectories (joint), same region ------------------------

paths_region1_anchored <- paths_anchored |>
  filter(location == "HHS Region 1")

p_traj <- ggplot() +
  geom_line(
    data = paths_region1_anchored,
    aes(x = target_end_date, y = value, group = .rep),
    colour = line_col, alpha = 0.2, linewidth = 0.4
  ) +
  geom_line(
    data = history_region1,
    aes(x = origin_date, y = wili),
    colour = hist_col, linewidth = 0.9
  ) +
  geom_vline(
    xintercept = origin_date, linetype = "dashed", colour = "grey50"
  ) +
  coord_cartesian(ylim = y_lim, xlim = x_lim) +
  labs(
    title = "Joint forecast: whole paths over time",
    subtitle = paste0("HHS Region 1 | ", n_paths, " sample trajectories"),
    x = NULL, y = "% of visits due to ILI"
  )

ggsave(
  file.path(fig_dir, "ili-forecast-trajectories.png"),
  p_traj, width = 1600 / 150, height = 900 / 150, dpi = 150
)

## --- Figure 3: trajectories across three regions ------------------------

region_cols <- c(
  "HHS Region 1" = "#1b9e77",
  "HHS Region 2" = "#d95f02",
  "HHS Region 3" = "#7570b3"
)

p_regions <- ggplot() +
  geom_line(
    data = paths_anchored,
    aes(x = target_end_date, y = value, group = .rep, colour = location),
    alpha = 0.2, linewidth = 0.4
  ) +
  geom_line(
    data = history,
    aes(x = origin_date, y = wili, colour = location),
    linewidth = 0.9
  ) +
  geom_vline(
    xintercept = origin_date, linetype = "dashed", colour = "grey50"
  ) +
  facet_wrap(~location, nrow = 1) +
  scale_colour_manual(values = region_cols, guide = "none") +
  labs(
    title = "Trajectories across locations",
    subtitle = "Fitted separately per region, so paths are not yet coupled across locations",
    x = NULL, y = "% of visits due to ILI"
  )

ggsave(
  file.path(fig_dir, "ili-trajectories-regions.png"),
  p_regions, width = 1600 / 150, height = 900 / 150, dpi = 150
)

message("Wrote three figures to ", fig_dir)
