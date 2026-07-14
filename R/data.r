#' Data on forecasted locations from the US COVID-19 Forecast Hub
#'
#' A dataset containing 53 rows with metadata about each US state.
#' The dataset was downloaded from
#' https://raw.githubusercontent.com/CDCgov/covid19-forecast-hub/refs/heads/main/auxiliary-data/locations.csv
#' @format A [tibble::tibble()] with a 4 columns and 53 rows
#' \describe{
#'   \item{abbreviation}{two-letter abbreviation for a jurisdiction}
#'   \item{location}{a two-letter FIPS code for identifying the jurisdiction}
#'   \item{location_name}{name of the jurisdiction}
#'   \item{population}{population of the jurisdiction as of some unknown time}
#' }
"covid_locations"

#' Time-series data on COVID-19 in the US
#'
#' A dataset containing versions of time-series data on COVID-19 in the US,
#' including data on weekly counts of hospital admissions due to COVID-19.
#' The first week of data for which observations are available ended on
#' 2022-10-01 and the last week ended on 2025-06-28. This dataset was downloaded
#' on 2025-07-09, with versions as early as 2024-11-20 (when the hub started
#' recording this dataset).
#'
#' @format A [tibble::tibble()] with a 6 columns and 52,778 rows.
#' \describe{
#'   \item{date}{date of the Saturday ending an epidemic week for which the
#'   count was made}
#'   \item{state}{two-letter abbreviation for a jurisdiction}
#'   \item{observation}{the count of the observation, e.g. number of hospital
#'   admissions in that week and location}
#'   \item{location}{a two-letter FIPS code for identifying the jurisdiction}
#'   \item{as_of}{date corresponding to the date (typically a Wednesday) the
#'   data were archived}
#'   \item{target}{the name of the prediction target: "wk inc covid hosp"}
#'   \item{abbreviation}{two letter abbreviation for the location}
#'   \item{location_name}{full name of the location}
#'   \item{population}{population size of the location}
#' }
"covid_time_series"

#' Forecast data for COVID-19 hospitalizations in the US
#'
#' A dataset containing forecasts of hospital admission counts in the US from
#' November 2024 through early July 2025.
#'
#' @format A [tibble::tibble()] with a 9 columns and 2,002,633 rows.
#' \describe{
#'   \item{reference_date}{the date of the Saturday following the Wednesday
#'   forecast submission date each week}
#'   \item{location}{a two-letter FIPS code for identifying the jurisdiction}
#'   \item{horizon}{an integer between 0 and 3, corresponding to the
#'   prediction horizon}
#'   \item{target_end_date}{date corresponding to the Saturday ending the
#'   epiweek being forecasted}
#'   \item{target}{"wk inc covid hosp", the name of the forecast target,
#'   corresponding to weekly incident covid hospitalizations}
#'   \item{output_type}{string denoting the forecast format, here, all "quantile"}
#'   \item{output_type_id}{number between 0 and 1 indicating the quantile level
#'   corresponding to the prediction}
#'   \item{value}{the numeric value of the prediction at the specified quantile
#'   level}
#'   \item{model_id}{the unique character string identifying the model that
#'   made the prediction}
#'   \item{abbreviation}{two letter abbreviation for the location}
#'   \item{location_name}{full name of the location}
#'   \item{population}{population size of the location}
#' }
"covid_forecasts"

#' Flusight ILI data for the US starting with the 2003/2004 season through the
#' of the 2017/2018 season.
#'
#' A dataset containing Flusight wILI (weighted Influenza-like Illness) data
#' starting with the 2003/2004 season through the 2017/2018 season. Contains the
#' wILI signal that is published by the CDC  which measures the percentage of
#' all outpatient doctor's office visits due to ILI in a given epidemiological
#' week.
#'
#' @format A [tibble::tibble()] with a 3 columns and 780 rows
#' \describe{
#'   \item{region}{the region for which `wili` is represented, all "nat" for
#'   national here.}
#'   \item{epiweek}{The beginning of the (MMWR) epiweek, which starts on a
#'   Sunday and ends on a Saturday}
#'   \item{wili}{the weighted ILI (Influenza-like Illness) variable}
#' }
"flu_data"

#' Flusight ILI data for the US starting with the 2003/2004 season through the
#' end of the 2019/2020 season.
#'
#' A dataset containing Flusight wILI (weighted Influenza-like Illness) data
#' starting with the 2003/2004 season through the 2019/2020 season. Contains the
#' wILI signal that is published by the CDC  which measures the percentage of
#' all outpatient doctor's office visits due to ILI in a given epidemiological
#' week.
#'
#' @format A [tibble::tibble()] with a 3 columns and 780 rows
#' \describe{
#'   \item{location}{the region for which `wili` is represented: "US National" for
#'   national, and "HHS Region X" for 1 through 10 for HHS regions of the US.}
#'   \item{origin_date}{The date of the Saturday that is the end of the epiweek
#'   to which the wILI measurement corresponds.}
#'   \item{wili}{the weighted ILI (Influenza-like Illness) variable}
#' }
"flu_data_hhs"

#' Simulated reported cases for the epidemiologically-motivated session
#'
#' The full 170-day series of simulated reported cases used in the
#' epidemiologically-motivated forecasting session. The outbreak was
#' simulated with `EpiNow2::simulate_infections()` using a constant
#' reproduction number of 1.25, a fixed population of 100,000 with
#' susceptible depletion applied across the whole period, the EpiNow2 example
#' generation time and delays fixed to point values, and negative binomial
#' observations. The series starts on 2023-01-01 and rises to a peak in late
#' March before declining. Generated by `data-raw/epi_forecasts.R`.
#'
#' @format A [tibble::tibble()] with 2 columns and 170 rows.
#' \describe{
#'   \item{date}{date of the simulated day, starting on 2023-01-01}
#'   \item{confirm}{the number of reported cases on that day}
#' }
"epi_reported"

#' Backtested forecasts for the epidemiologically-motivated session
#'
#' Sample-level predictions from the backtest in the
#' epidemiologically-motivated forecasting session. Five models are forecast
#' from 12 weekly forecast dates spanning the growth, peak, and decline of the
#' simulated outbreak in [epi_reported]. The models are the ARIMA baseline
#' (`"arima"`) and four EpiNow2 renewal models (`"default"`,
#' `"constant_rt_fixed"`, `"rw_fixed"`, `"rw_estimated"`) differing in how much
#' epidemiological mechanism and flexibility they include. Each forecast unit
#' (model, forecast date, target date, horizon) has been equalised to 200
#' samples so that forecast scoring does not warn about uneven sample counts.
#' Generated by `data-raw/epi_forecasts.R`.
#'
#' @format A [tibble::tibble()] with 6 columns.
#' \describe{
#'   \item{model}{the model that made the prediction: "arima", "default",
#'   "constant_rt_fixed", "rw_fixed", or "rw_estimated"}
#'   \item{forecast_date}{the date the forecast was made, using data up to and
#'   including this date}
#'   \item{date}{the target date being forecast}
#'   \item{horizon}{integer forecast horizon in days ahead of the forecast
#'   date (1 to 14)}
#'   \item{sample}{integer sample index from 1 to 200 within each forecast
#'   unit}
#'   \item{predicted}{the predicted number of reported cases for that sample}
#' }
"epi_forecasts"

#' Versioned (vintage) Flusight ILI data for the US and HHS regions
#'
#' Each reported version of weekly weighted ILI (wILI) for the US National
#' level and the 10 HHS regions, for the observation weeks of the five
#' forecastable seasons in the sandbox hub (2015/16-2019/20). Because ILINet is
#' revised as more reports arrive ("backfill"), the same observation week has
#' several values, one per data release, so you can see how an estimate for a
#' given week evolved and what a forecaster would have seen in real time.
#'
#' For weeks in the forecastable seasons, versions run from a week's first
#' release up to that season's finalized value, defined as the latest release on
#' or before 1 July of the season's end year (weeks in Aug-Dec belong to the
#' season ending the following year). Later cross-season re-baselines of ILINet
#' are excluded, so the last `as_of` for each such week is that season's final
#' value. Weeks before the forecastable era (prior to the 2015/16 season) were
#' already settled when forecasting begins and carry a single baseline version
#' (their finalized value, `as_of` 2015-10-01), so the full 2003-onward series is
#' available -- e.g. for reconstructing the training data available at any date.
#'
#' @format A [tibble::tibble()] with 4 columns.
#' \describe{
#'   \item{location}{"US National", or "HHS Region 1" through "HHS Region 10"}
#'   \item{origin_date}{the Saturday ending the observed epiweek}
#'   \item{as_of}{the date this version of the observation was published by the
#'   CDC (the release date); the season-final value is the one with the latest
#'   `as_of`}
#'   \item{wili}{the weighted ILI value as reported in that release}
#' }
"flu_data_hhs_versions"

#' Vintage time-series cross-validation datasets for the sandbox hub seasons
#'
#' Five expanding-window time-series cross-validation datasets, one per
#' forecastable season (2015/16 through 2019/20), for the US National level and
#' the 10 HHS regions. Each `.split` corresponds to a forecast origin, and its
#' wILI values reflect the data **as it was available in real time** at that
#' origin: for a split whose last observed week ends on `origin_date` D, each
#' week takes its latest FluView release on or before the historical FluSight
#' submission deadline (Monday of week XX+2 = D + 9 days) -- so the origin week
#' appears at its first-reported value and earlier weeks at whatever revision was
#' published by then. Deep pre-vintage weeks (no release by D + 9) fall back to
#' the finalized [flu_data_hhs]. Consequently the same week's value differs
#' across splits, reproducing reporting backfill. This is the identical
#' reconstruction used for the hub's dashboard time-series, so the training data
#' matches the observed series shown for each forecast.
#'
#' These mirror the shape of a [tsibble::stretch_tsibble()] result and can be
#' passed directly to [fabletools::model()].
#'
#' @format A [tsibble::tsibble()] keyed by `location` and `.split`, indexed by
#'   `origin_date`.
#' \describe{
#'   \item{location}{"US National", or "HHS Region 1" through "HHS Region 10"}
#'   \item{origin_date}{the Saturday ending an epiweek}
#'   \item{wili}{the vintage weighted ILI value available at that split's origin}
#'   \item{.split}{integer id of the cross-validation split (forecast origin)}
#' }
#' @name flu_data_hhs_tscv
"flu_data_hhs_tscv_season1"

#' @rdname flu_data_hhs_tscv
#' @format NULL
"flu_data_hhs_tscv_season2"

#' @rdname flu_data_hhs_tscv
#' @format NULL
"flu_data_hhs_tscv_season3"

#' @rdname flu_data_hhs_tscv
#' @format NULL
"flu_data_hhs_tscv_season4"

#' @rdname flu_data_hhs_tscv
#' @format NULL
"flu_data_hhs_tscv_season5"

#' Historical ILI trajectory library for the copycat analog forecast model
#'
#' A library of past US influenza seasons used by the [COPYCAT()] "method of
#' analogues" model. It covers the US National level, the 10 HHS regions, and the
#' states + DC, for every season before the 2015/2016 season (1997/98 through
#' 2014/15), drawn from ILINet via the Delphi Epidata API. Each (location,
#' season) trajectory is represented in the scale-free form the method matches
#' on: a GAM-smoothed log weekly growth rate by week-of-season, with its standard
#' error. Magnitudes are not stored or standardised -- forecasts are re-anchored
#' to the series being predicted. The code that produced it is in
#' `data-raw/copycat_db.R`. Method after Fox / epiENGAGE
#' (\url{https://github.com/sjfox/copycat}).
#'
#' @format A [tibble::tibble()] with 5 columns.
#' \describe{
#'   \item{location}{ILINet location code ("nat", "hhs1"-"hhs10", or a
#'   lower-case state/DC abbreviation)}
#'   \item{resp_season}{the respiratory season, labelled by its starting year
#'   (e.g. 2014 = the 2014/2015 season)}
#'   \item{resp_season_week}{week index within the season (1 = first observed
#'   week, seasons begin around MMWR week 40)}
#'   \item{pred}{GAM-smoothed log weekly growth rate,
#'   `log((value[t+1] + 1) / (value[t] + 1))`}
#'   \item{pred_se}{standard error of `pred` from the smoother}
#' }
"copycat_db"
