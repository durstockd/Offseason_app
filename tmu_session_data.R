# =============================================================================
# tmu_session_data.R  —  data layer for the TMU offseason app
# -----------------------------------------------------------------------------
# Bullpens and live ABs / scrimmages. Raw TrackMan CSV/XLSX exports sit in two
# folders next to app.R; build_tmu_session_data.R reads them once and saves
# tmu_session_data.rds, which is what the app loads at startup.
#
#   source("build_tmu_session_data.R")        # after dropping in new CSVs
#   -> tmu_session_data.rds                   # deploy this with the app
#
# The RDS remembers exactly which files it was built from (name + md5). With
# DATA_SOURCE = "auto" the app compares that list against the folders at
# startup: same files -> load the RDS; a file added, removed or re-downloaded
# -> re-read the CSVs for that mode. So a new upload always shows the new
# CSVs even if the build was not re-run, and the RDS is only ever a
# speed-up, never a stale copy.
#
#   source("tmu_session_data.R")
#   store <- load_session_store()
#   idx   <- build_session_index(store$bullpen)
#
# NO DEPENDENCIES ON THE OTHER APPS. This file deliberately does not source
# pitch_tagging_engine.R, opponent_tagging_engine.R, pitch_physics_core.R or
# fl_stuff_plus.R:
#   - pitch types come from the hand-entered TaggedPitchType; a pitch left untagged
#     falls back to TrackMan's AutoPitchType (see UNTAGGED_FALLBACK_TO_AUTO),
#     so no tagging engine is needed (and pitch_tagging_engine.R could not be sourced
#     anyway — it runs list.files() + stopifnot() for *.xlsx at source time and
#     stops R in a folder that has none);
#   - there is no Stuff+ anywhere in this app.
# =============================================================================

library(dplyr)
library(readr)
library(stringr)
library(purrr)

# =============================== KNOBS =======================================

# Folder layout. Drop a session's export in the matching folder and refresh.
# Both folders sit next to app.R.
#   bullpens/Bullpen-Aug_14_2026.csv
#   live_abs/Scrimmage-Sep_06_2026.csv
SESSION_DIRS <- c(bullpen = "bullpens", liveab = "live_abs")

SESSION_LABELS <- c(bullpen = "Bullpen", liveab = "Live ABs")

TMU_TEAM_CODE <- "THO_MOR"

# ---- Yakkertech ----
# Road games at Yakkertech parks (NKU) export in TrackMan's column layout
# with a few differences, handled per file at read time — see
# .normalize_source(). A file is treated as Yakkertech when it has a
# PitchUUID column, any yt_* column, or YAK: team ids.
#
# Team ids arrive as "YAK:THOMAS:9e782613-0d72:0936209". Each pattern below
# maps to a short code; any other YAK: id falls back to its middle part
# ("YAK:XAVIER:..." -> "XAVIER"). TrackMan codes pass through untouched.
TEAM_ALIASES <- c(
  "^YAK:THOMAS:" = "THO_MOR",
  "^YAK:NKUHOM:" = "NKU"
)
# How a team code reads in the session picker and report header.
TEAM_NAMES <- c(THO_MOR = "Thomas More", NKU = "NKU")

# Yakkertech doesn't measure extension — the column is a 6.0 placeholder on
# every pitch (and 59.33 on one). TRUE blanks it so it can't pass for data.
YAK_BLANK_EXTENSION <- TRUE

# Same player, different spelling between systems. Applied to Pitcher,
# Batter and Catcher in every file. Name as it arrives = name to use.
PLAYER_ALIASES <- c(
  "Bosio, Theo" = "Bosio, Theodore"      # Yakkertech 9/15 @ NKU
)

# Label for pitches with no tag from either source.
UNTAGGED_LABEL <- "Untagged"

# Untagged fallback. TRUE: a pitch with no hand tag (blank / Undefined
# TaggedPitchType) takes TrackMan's AutoPitchType instead. The hand tag always
# wins when there is one, and a manual override beats both. Every row carries
# TagSource = "Hand" / "TrackMan" / "Override" / "None" so a TrackMan-filled
# pitch is never mistaken for a hand-tagged one. FALSE restores hand-tags-only.
# CAUTION: AutoPitchType is unreliable on this data — on one live-AB session it
# called 30+ of the 80-88 mph fastballs "Changeup". Spot-check anything it fills in.
UNTAGGED_FALLBACK_TO_AUTO <- TRUE

# Where each tag came from.
TAG_SOURCE_HAND     <- "Hand"
TAG_SOURCE_TRACKMAN <- "TrackMan"
TAG_SOURCE_OVERRIDE <- "Override"
TAG_SOURCE_NONE     <- "None"

# Rule-book zone. Same fixed box as every other TMU/Y'alls surface, no
# batter-height adjustment.
SZ_XMIN <- -0.83; SZ_XMAX <- 0.83
SZ_YMIN <-  1.50; SZ_YMAX <- 3.50

# Untracked rows. TrackMan writes a row for a pitch the unit never picked up:
# no Date, no Time, no RelSpeed, no location, no movement, and
# TaggedPitchType = "Undefined" because the tagger had nothing to tag. It is not
# a pitch the app can say anything about, and leaving it in is what makes a card
# read "19 Pitches" in the header while the stats table (which needs RelSpeed)
# totals 18, and what puts a phantom bar in Pitch Usage. TRUE drops them at
# load. Set FALSE to keep them and see them as Untagged.
DROP_UNTRACKED <- TRUE

# Persistent tag overrides. Every correction made in the sidebar is written
# here, keyed on PitchUID, and read back at startup. PitchUID is assigned by
# TrackMan and is stable across re-downloads of the same session, so a
# correction survives replacing the export in bullpens/ with a fresh copy.
#
# The file sits next to app.R and is part of the deploy bundle. Locally it is
# written the moment Apply is pressed. On shinyapps.io the container filesystem is
# wiped on every restart, so a correction made in the DEPLOYED app lasts only
# until that instance recycles — fix tags locally, then deploy, and the file
# ships with the app and loads at startup for everyone.
TAG_OVERRIDE_FILE <- "tag_overrides.csv"

# Prebuilt data. build_tmu_session_data.R writes it; the app reads it.
SESSION_RDS <- "tmu_session_data.rds"

# Where the app gets its data at startup.
#   "auto" — the RDS, unless the CSVs in a folder no longer match what it was
#            built from (then that mode is re-read from the CSVs). A folder
#            that is missing or empty — e.g. the RDS deployed without the
#            CSVs — trusts the RDS.
#   "rds"  — the RDS only, whatever the folders say.
#   "csv"  — ignore the RDS and read the folders every time (the old way).
DATA_SOURCE <- "auto"

# ---- live-AB results ----
# Exit velo that counts as hard-hit, and below which contact counts as soft.
HARD_HIT_MPH     <- 95
SOFT_CONTACT_MPH <- 90

# KorBB is hand-tagged. TRUE: a PA whose last pitch is a called/swinging
# strike with 2 strikes already on it is a K, and a ball with 3 balls on it
# is a BB, even when KorBB was left Undefined. The hand-entered KorBB always wins
# when it is there.
DERIVE_K_BB_FROM_COUNT <- TRUE

# Mid-PA pitcher flicker. A PA has one pitcher: when the Pitcher tag changes
# partway through a live-AB PA, the minority pitches are re-tagged to the
# PA's most frequent pitcher (ties go to whoever threw the last pitch — the
# same modal rule as the Y'alls zeros board). It does happen: one live-AB PA
# had its fifth pitch tagged to the wrong arm, with release and movement that
# plainly belonged to the pitcher who threw the other four. Every re-tag is logged
# at load and listed by build_tmu_session_data.R. FALSE leaves tags alone
# (a real mid-PA change would then be charged to the pitcher who finished).
FIX_MIDPA_PITCHER <- TRUE

# =============================================================================

.num <- function(x) suppressWarnings(as.numeric(x))

pitch_colors <- c(
  "Four-Seam"   = "#FF0000",
  "Two-Seam"    = "#E67E22",
  "Sinker"      = "#E67E22",
  "Cutter"      = "#F1C40F",
  "Slider"      = "#3498DB",
  "Sweeper"     = "#14A3C7",
  "Curveball"   = "#9B59B6",
  "Changeup"    = "#2ECC71",
  "Splitter"    = "#1ABC9C",
  "Knuckleball" = "#73B761",
  "Untagged"    = "#7F8C8D"
)

pitch_abbr <- c(
  "Four-Seam" = "FF", "Two-Seam" = "FT", "Sinker" = "SI", "Cutter" = "FC",
  "Slider" = "SL", "Sweeper" = "ST", "Curveball" = "CU", "Changeup" = "CH",
  "Splitter" = "FS", "Knuckleball" = "KN", "Untagged" = "--"
)

# Display order — fastest family first, so tables and legends read the same way
# every time regardless of what a given arm threw.
PITCH_ORDER <- c("Four-Seam","Two-Seam","Sinker","Cutter","Slider","Sweeper",
                 "Curveball","Changeup","Splitter","Knuckleball","Untagged")

# -----------------------------------------------------------------------------
# std_pitch_name()
#   SPELLING ONLY. Folds TrackMan's variant spellings onto one label
#   ("ChangeUp"/"Changeup" -> "Changeup", "FourSeamFastBall" -> "Four-Seam").
#   It never reclassifies a pitch — whatever label it is handed is what comes
#   out. Blank or Undefined becomes UNTAGGED_LABEL. Used on both
#   TaggedPitchType and AutoPitchType.
# -----------------------------------------------------------------------------
std_pitch_name <- function(x) {
  x <- trimws(as.character(x))
  out <- dplyr::case_when(
    x %in% c("FourSeamFastBall","FourSeam","Four-Seam","FourSeamFastball",
             "Fastball","FA","FF","4-Seam","4Seam")          ~ "Four-Seam",
    x %in% c("TwoSeamFastBall","TwoSeam","Two-Seam","FT",
             "2-Seam","2Seam")                                ~ "Two-Seam",
    x %in% c("Sinker","SI","Sink")                            ~ "Sinker",
    x %in% c("Cutter","Cut","CT","FC")                        ~ "Cutter",
    x %in% c("Slider","SL","Slide")                           ~ "Slider",
    x %in% c("Sweeper","Sweep","SW","ST")                     ~ "Sweeper",
    x %in% c("Curveball","CurveBall","Curve","CU","CB",
             "KnuckleCurve","Knuckle-Curve")                  ~ "Curveball",
    x %in% c("ChangeUp","Changeup","Change","Change-Up",
             "CH","CHS","Change-up")                          ~ "Changeup",
    x %in% c("Splitter","Split","Split-Finger",
             "SplitFinger","FS")                              ~ "Splitter",
    x %in% c("Knuckleball","KnuckleBall","Knuckle")           ~ "Knuckleball",
    is.na(x) | x %in% c("","Undefined","Unknown","Other",
                        "NA","None")                          ~ UNTAGGED_LABEL,
    TRUE                                                      ~ stringr::str_to_title(x)
  )
  out[is.na(out)] <- UNTAGGED_LABEL
  out
}

# Factor in display order, keeping only the levels actually present.
order_pitch_factor <- function(x) {
  x  <- as.character(x)
  lv <- PITCH_ORDER[PITCH_ORDER %in% unique(x)]
  extra <- setdiff(unique(x), lv)          # anything unrecognised, kept visible
  factor(x, levels = c(lv, sort(extra)))
}

# -----------------------------------------------------------------------------
# normalize_session_date()
#   Date arrives as ISO, US, factor, Date or an Excel serial depending on the
#   export. Returns character YYYY-MM-DD, NA where unreadable. Never errors:
#   as.Date.default() stops on classes it doesn't recognise, so class is
#   checked before dispatch.
# -----------------------------------------------------------------------------
normalize_session_date <- function(x) {
  if (length(x) == 0) return(character(0))
  if (inherits(x, "Date"))   return(format(x, "%Y-%m-%d"))
  if (inherits(x, "POSIXt")) return(format(as.Date(x), "%Y-%m-%d"))
  if (is.factor(x)) x <- as.character(x)
  if (is.numeric(x)) return(format(as.Date(x, origin = "1899-12-30"), "%Y-%m-%d"))
  
  xc  <- as.character(x)
  out <- rep(NA_character_, length(xc))
  for (f in c("%Y-%m-%d", "%m/%d/%Y", "%m/%d/%y", "%Y/%m/%d")) {
    todo <- is.na(out) & !is.na(xc)
    if (!any(todo)) break
    d <- suppressWarnings(as.Date(xc[todo], format = f))
    out[todo][!is.na(d)] <- format(d[!is.na(d)], "%Y-%m-%d")
  }
  out
}

pretty_session_date <- function(x) {
  d <- suppressWarnings(as.Date(normalize_session_date(x)))
  ifelse(is.na(d), "Unknown date", format(d, "%b %d, %Y"))
}

# "08:06:05.04" -> "8:06 AM"
pretty_time <- function(x) {
  x  <- as.character(x)
  hh <- suppressWarnings(as.integer(substr(x, 1, 2)))
  mm <- substr(x, 4, 5)
  out <- rep(NA_character_, length(x))
  ok  <- !is.na(hh)
  if (any(ok)) {
    ap  <- ifelse(hh[ok] >= 12, "PM", "AM")
    h12 <- hh[ok] %% 12; h12[h12 == 0] <- 12
    out[ok] <- sprintf("%d:%s %s", h12, mm[ok], ap)
  }
  out
}

# -----------------------------------------------------------------------------
# clean_tilt()
#   TrackMan writes Tilt as "1:15". Any reader that type-guesses will treat that
#   as a time and hand back "01:15:00" (readr) or a fractional day (Excel), so
#   this normalises whatever arrives back to "H:MM". Applied even though the CSV
#   reader now forces character, because .xlsx sessions go through readxl.
# -----------------------------------------------------------------------------
clean_tilt <- function(x) {
  if (is.numeric(x)) {                       # Excel fraction-of-a-day
    mn <- round(x * 24 * 60) %% 720
    hh <- floor(mn / 60); mm <- mn - hh * 60
    hh[!is.na(hh) & hh == 0] <- 12
    return(ifelse(is.na(x), NA_character_, sprintf("%d:%02d", hh, mm)))
  }
  s <- trimws(as.character(x))
  s[s == "" | s == "NA"] <- NA_character_
  # "01:15:00" / "01:15" -> "1:15"
  hit <- !is.na(s) & grepl("^[0-9]{1,2}:[0-9]{2}(:[0-9]{2})?$", s)
  if (any(hit)) {
    hh <- as.integer(sub("^([0-9]{1,2}):.*$", "\\1", s[hit])) %% 12
    hh[hh == 0] <- 12
    mm <- sub("^[0-9]{1,2}:([0-9]{2}).*$", "\\1", s[hit])
    s[hit] <- paste0(hh, ":", mm)
  }
  s
}

# -----------------------------------------------------------------------------
# tilt_from_axis()
#   SpinAxis (degrees) -> clock tilt. Verified against TrackMan's own Tilt
#   column on the 8/14 TMU pen: matches all 45 pitches once rounded to the
#   nearest quarter hour, which is what TrackMan itself reports.
# -----------------------------------------------------------------------------
tilt_from_axis <- function(axis, round_min = 15) {
  a  <- .num(axis)
  mn <- (((a + 180) %% 360) / 30) * 60
  if (!is.null(round_min) && round_min > 0) mn <- round(mn / round_min) * round_min
  mn <- mn %% 720
  hh <- floor(mn / 60); mm <- round(mn - hh * 60)
  hh[!is.na(hh) & hh == 0] <- 12
  ifelse(is.na(a), NA_character_, sprintf("%d:%02d", hh, mm))
}

# circular mean of an angle vector, in degrees
mean_axis <- function(a) {
  a <- .num(a); a <- a[!is.na(a)]
  if (!length(a)) return(NA_real_)
  r <- a * pi / 180
  (atan2(mean(sin(r)), mean(cos(r))) * 180 / pi) %% 360
}

# -----------------------------------------------------------------------------
# .read_one_session_file()
#   Port of read_one_game() from build_pitcher_data.R, minus the game-specific
#   parts: same junk-column, empty-column and footer-row handling, same
#   character coercion on ID columns (they overflow a double and lose digits).
# -----------------------------------------------------------------------------
.ID_CHAR_COLS <- c("AwayTeamForeignID","HomeTeamForeignID","GameForeignID",
                   "PitchUID","GameUID","PlayID","PitcherId","BatterId","CatcherId")

# -----------------------------------------------------------------------------
# Team / player name standardisation (both sources)
# -----------------------------------------------------------------------------
std_team <- function(x) {
  x <- trimws(as.character(x))
  for (i in seq_along(TEAM_ALIASES)) {
    hit <- !is.na(x) & grepl(names(TEAM_ALIASES)[i], x)
    x[hit] <- TEAM_ALIASES[[i]]
  }
  yak <- !is.na(x) & grepl("^YAK:[^:]+:", x)
  x[yak] <- sub("^YAK:([^:]+):.*$", "\\1", x[yak])
  x
}

team_display <- function(code) {
  out <- unname(TEAM_NAMES[code])
  ifelse(is.na(out), code, out)
}

std_player <- function(x) {
  x <- trimws(as.character(x))
  hit <- !is.na(x) & x %in% names(PLAYER_ALIASES)
  x[hit] <- unname(PLAYER_ALIASES[x[hit]])
  x
}

.is_yakkertech <- function(df) {
  "PitchUUID" %in% names(df) || any(startsWith(names(df), "yt_")) ||
    ("PitcherTeam" %in% names(df) &&
       any(grepl("^YAK:", df$PitcherTeam), na.rm = TRUE))
}

# -----------------------------------------------------------------------------
# .normalize_source()
#   Tags every row with Source and folds a Yakkertech export onto the
#   TrackMan names the app reads. Checked against the 9/15 @ NKU file:
#     PitchUUID -> PitchUID        HitType -> TaggedHitType
#     Note      -> Notes           PitchCall "Foul" -> "FoulBall"
#     Extension -> blank (placeholder, see YAK_BLANK_EXTENSION)
#   Not changed here, handled downstream:
#     - KorBB is never written. K / BB / HBP live in PlayResult
#       ("StrikeoutSwinging", "StrikeoutLooking", "Walk" — HBPs are ALSO
#       written as "Walk", with PitchCall HitByPitch) -> .pa_result()
#     - Batter / PAofInning / PitchofPA / count lag or repeat at PA
#       boundaries -> add_pa_outcomes() splits Yakkertech PAs on batter
#       change and results only, and never infers a K/BB from the count
#     - team ids -> std_team() in load_sessions()
#   The main pitch columns (RelSpeed, InducedVertBreak, HorzBreak,
#   PlateLoc*, RelSide, Bearing) are Yakkertech's TrackMan-equivalent values
#   with TrackMan's sign conventions (checked: RHP release side and fastball
#   run positive, LHP negative, pull-side spray negative for RHH). The yt_*
#   duplicates are its native readings and are not used.
# -----------------------------------------------------------------------------
.normalize_source <- function(df) {
  if (!.is_yakkertech(df)) {
    df$Source <- "TrackMan"
    return(df)
  }
  df$Source <- "Yakkertech"
  if (!"PitchUID" %in% names(df) && "PitchUUID" %in% names(df))
    df$PitchUID <- df$PitchUUID
  if (!"TaggedHitType" %in% names(df) && "HitType" %in% names(df))
    df$TaggedHitType <- df$HitType
  if (!"Notes" %in% names(df) && "Note" %in% names(df))
    df$Notes <- df$Note
  if ("PitchCall" %in% names(df))
    df$PitchCall[!is.na(df$PitchCall) & df$PitchCall == "Foul"] <- "FoulBall"
  if (isTRUE(YAK_BLANK_EXTENSION) && "Extension" %in% names(df))
    df$Extension <- NA_character_
  df
}

.read_one_session_file <- function(path) {
  ext <- tolower(tools::file_ext(path))
  
  df <- tryCatch({
    if (ext %in% c("xlsx","xls")) {
      if (!requireNamespace("readxl", quietly = TRUE))
        stop("readxl is needed to read ", basename(path))
      readxl::read_excel(path)
    } else if (ext == "csv") {
      # Some exports carry a banner line above the real header.
      first_line <- tryCatch(readLines(path, n = 1, warn = FALSE),
                             error = function(e) "")
      skip_n <- if (length(first_line) == 0 ||
                    !grepl("PitchNo|PitcherTeam", first_line)) 1L else 0L
      # EVERYTHING as character, coerced explicitly further down. Letting readr
      # guess is what turned Tilt into an hms — "1:15" looks like a time, so it
      # parsed as one and printed as "01:15:00" in every table.
      readr::read_csv(path, show_col_types = FALSE,
                      col_types = readr::cols(.default = readr::col_character()),
                      skip = skip_n, progress = FALSE)
    } else return(NULL)
  }, error = function(e) {
    warning("Could not read ", basename(path), ": ", conditionMessage(e))
    NULL
  })
  if (is.null(df) || nrow(df) == 0) return(NULL)
  
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  
  # TrackMan junk columns ("...118", "High...160")
  junk <- grepl("^\\.{3}[0-9]+$|^High\\.{3}[0-9]+$", names(df))
  if (any(junk)) df <- df[, !junk, drop = FALSE]
  
  # Entirely-NA columns. A bullpen export is mostly these (no batted balls, no
  # fielder positions); dropping them here keeps the bind cheap.
  empty <- vapply(df, function(x) all(is.na(x)), logical(1))
  if (any(empty)) df <- df[, !empty, drop = FALSE]
  
  # Summary-footer rows — and, in a Yakkertech game file, every pitch the
  # system tracked that the operator never assigned to a pitcher (warm-ups
  # between innings: 147 of 502 rows at NKU). Counted so the load status can
  # say so.
  n_unassigned <- 0L
  if ("Pitcher" %in% names(df)) {
    no_p <- is.na(df$Pitcher) | trimws(df$Pitcher) == ""
    if (.is_yakkertech(df)) n_unassigned <- sum(no_p)
    df <- df[!no_p, , drop = FALSE]
  }
  if (nrow(df) == 0) return(NULL)
  
  # EVERY column to character, not just the IDs. CSVs are already read that
  # way; an .xlsx comes back from readxl with numeric columns, and binding the
  # two in one folder fails with "Can't combine <character> and <double>" —
  # which load_sessions() used to swallow, leaving the tab empty. Tilt is kept
  # as-is so clean_tilt() still sees an Excel fraction-of-a-day.
  for (cc in setdiff(names(df), "Tilt")) {
    if (inherits(df[[cc]], c("Date", "POSIXt"))) {
      df[[cc]] <- format(df[[cc]], "%Y-%m-%d")
    } else {
      df[[cc]] <- as.character(df[[cc]])
    }
  }
  
  df <- .normalize_source(df)
  df$SourceFile <- basename(path)
  attr(df, "n_unassigned") <- n_unassigned
  df
}

# Columns the app reads. Anything absent is created empty so a partial export
# degrades to blanks instead of erroring inside a ggplot.
.SESSION_NUM_COLS <- c(
  "PitchNo","Inning","PAofInning","PitchofPA","Balls","Strikes","Outs",
  "RelSpeed","SpinRate","SpinAxis","RelHeight","RelSide","Extension",
  "InducedVertBreak","VertBreak","HorzBreak","PlateLocHeight","PlateLocSide",
  "VertApprAngle","HorzApprAngle","ZoneSpeed","EffectiveVelo",
  "ExitSpeed","Angle","Direction","Distance","Bearing","HangTime",
  "OutsOnPlay","RunsScored"
)

.SESSION_CHR_COLS <- c(
  "Pitcher","PitcherId","PitcherThrows","PitcherTeam","PitcherSet",
  "Batter","BatterId","BatterSide","BatterTeam","Catcher","CatcherTeam",
  "TaggedPitchType","AutoPitchType","PitchCall","KorBB","TaggedHitType","AutoHitType",
  "PlayResult","Notes","Tilt","Date","Time",
  "HomeTeam","AwayTeam","Stadium","Level","League","GameID","PitchUID",
  "TopBottom","Source"
)

# -----------------------------------------------------------------------------
# Load status. load_sessions() records what it actually saw for each mode, so
# an empty tab can say WHY it is empty (folder missing on the server, no files,
# a file that would not read, ...) instead of a generic "no sessions".
# -----------------------------------------------------------------------------
.LOAD_STATUS <- new.env(parent = emptyenv())

.set_load_status <- function(mode, msg) {
  assign(mode, msg, envir = .LOAD_STATUS)
  message("[", mode, "] ", msg)
  invisible(msg)
}

session_load_status <- function(mode) {
  if (exists(mode, envir = .LOAD_STATUS, inherits = FALSE))
    get(mode, envir = .LOAD_STATUS) else "load_sessions() was never run."
}

# -----------------------------------------------------------------------------
# load_sessions()
#   Reads every file in one mode's folder and returns one tidy frame.
#
#   Adds: SessionMode, SessionKey, SessionLabel, PitchGroup (the hand tag),
#         SeqNo (per pitcher per session), PTNum / PTTot.
#
#   PitchNo is NOT renumbered — on a two-arm pen it runs 1-19 for the first arm
#   and 20-45 for the second, i.e. it is session-global, not per-pitcher. SeqNo
#   is the per-pitcher sequence and is what every display should use.
# -----------------------------------------------------------------------------
load_sessions <- function(mode = c("bullpen","liveab"), dir = NULL) {
  mode <- match.arg(mode)
  if (is.null(dir)) dir <- SESSION_DIRS[[mode]]
  
  if (!dir.exists(dir)) {
    # The server's file system is case-sensitive; a Mac's is not. A folder
    # named "Live_ABs" works locally and does not exist on shinyapps.io.
    near <- list.dirs(".", full.names = FALSE, recursive = FALSE)
    near <- near[tolower(near) == tolower(dir)]
    .set_load_status(mode, paste0(
      "Folder '", dir, "/' not found next to app.R",
      if (length(near)) paste0(" (found '", near[1], "/' \u2014 the server is ",
                               "case-sensitive, rename it to '", dir, "')")
      else " \u2014 it was not included in the deploy",
      ". Folders present: ",
      paste(list.dirs(".", full.names = FALSE, recursive = FALSE),
            collapse = ", "), "."))
    return(NULL)
  }
  
  files <- list.files(dir, pattern = "\\.(csv|xlsx|xls)$",
                      full.names = TRUE, ignore.case = TRUE)
  if (length(files) == 0) {
    everything <- list.files(dir, recursive = TRUE)
    .set_load_status(mode, paste0(
      "'", dir, "/' exists but has no .csv/.xlsx directly inside it",
      if (length(everything)) paste0(" (it holds: ",
                                     paste(head(everything, 5), collapse = ", "),
                                     " \u2014 files in sub-folders are not read)")
      else " \u2014 the export was not included in the deploy",
      "."))
    return(NULL)
  }
  
  parts <- purrr::compact(lapply(files, .read_one_session_file))
  if (length(parts) == 0) {
    .set_load_status(mode, paste0("None of the files in '", dir,
                                  "/' could be read: ",
                                  paste(basename(files), collapse = ", "), "."))
    return(NULL)
  }
  
  n_unassigned <- sum(vapply(parts, function(x) {
    v <- attr(x, "n_unassigned"); if (is.null(v)) 0L else as.integer(v) },
    integer(1)))
  
  df <- tryCatch(dplyr::bind_rows(parts), error = function(e) {
    .set_load_status(mode, paste0("Files in '", dir, "/' would not combine: ",
                                  conditionMessage(e)))
    NULL
  })
  if (is.null(df)) return(NULL)
  
  # TrackMan's "Top/Bottom" is not a syntactic name; renamed once here so no
  # downstream code has to backtick it (same convention as the Y'alls build).
  if ("Top/Bottom" %in% names(df)) {
    if (!"TopBottom" %in% names(df)) df$TopBottom <- df[["Top/Bottom"]]
    df[["Top/Bottom"]] <- NULL
  }
  
  for (cc in .SESSION_CHR_COLS) if (!cc %in% names(df)) df[[cc]] <- NA_character_
  for (cc in .SESSION_NUM_COLS) if (!cc %in% names(df)) df[[cc]] <- NA_real_
  
  # Guard: is this actually a pitch export? TrackMan drops a fielder-positioning
  # companion CSV next to the real one which has PitchNo, Date and PitcherTeam
  # but none of the pitch metrics. Without this it sails through and renders as
  # pages of empty plots.
  if (all(is.na(.num(df$RelSpeed))) && all(is.na(.num(df$PlateLocHeight)))) {
    .set_load_status(mode, paste0(
      "Files in '", dir, "/' carry no pitch metrics (RelSpeed and ",
      "PlateLocHeight are entirely empty) \u2014 that is the fielder-",
      "positioning companion export, not the pitch export."))
    return(NULL)
  }
  
  for (cc in .SESSION_NUM_COLS) df[[cc]] <- .num(df[[cc]])
  df$Tilt <- clean_tilt(df$Tilt)          # must run BEFORE the blanket as.character
  for (cc in .SESSION_CHR_COLS) df[[cc]] <- as.character(df[[cc]])
  
  df$Date        <- normalize_session_date(df$Date)
  df$SessionMode <- mode
  df$Source[is.na(df$Source)] <- "TrackMan"
  
  for (cc in intersect(c("PitcherTeam","BatterTeam","HomeTeam","AwayTeam",
                         "CatcherTeam"), names(df))) {
    df[[cc]] <- std_team(df[[cc]])
  }
  for (cc in intersect(c("Pitcher","Batter","Catcher"), names(df))) {
    df[[cc]] <- std_player(df[[cc]])
  }
  
  # Drop the rows TrackMan never tracked. A row survives if it carries ANY
  # measurement at all, so a partially-tracked pitch (velo but no location, say)
  # is kept and only a completely empty one goes. Runs after the numeric
  # coercion above, so these are real NAs and not the strings "NA".
  #
  # LIVE ABs keep an untracked row that still carries a charted result
  # (PitchCall). TrackMan missing the ball does not mean the pitch didn't
  # happen — dropping it would delete the strikeout or the hit on it. Those
  # rows stay, flagged Tracked = FALSE, and the plots skip them.
  tracked <- !is.na(df$RelSpeed)          | !is.na(df$PlateLocHeight) |
    !is.na(df$PlateLocSide)      | !is.na(df$SpinRate)       |
    !is.na(df$InducedVertBreak)  | !is.na(df$HorzBreak)
  df$Tracked <- tracked
  if (isTRUE(DROP_UNTRACKED)) {
    has_event <- mode == "liveab" & !is.na(df$PitchCall) &
      !(trimws(df$PitchCall) %in% c("", "Undefined"))
    keep   <- tracked | has_event
    n_drop <- sum(!keep)
    n_kept <- sum(!tracked & keep)
    if (n_drop > 0) {
      message("Dropped ", n_drop, " untracked row(s) from ", dir,
              " — no velo, no location, no movement. TrackMan logged the pitch ",
              "but never measured it.")
      df <- df[keep, , drop = FALSE]
    }
    if (n_kept > 0) {
      message("Kept ", n_kept, " untracked row(s) in ", dir,
              " because they carry a charted result (PitchCall).")
    }
    if (nrow(df) == 0) {
      .set_load_status(mode, paste0("Every row in '", dir,
                                    "/' was untracked (no velo, location or movement)."))
      return(NULL)
    }
  }
  
  # Session key: GameID when the export has one (it does —
  # "20260814-ThomasMoreStadium-Private-2"), otherwise the filename, so two
  # sessions on the same date never collapse into one.
  df$SessionKey <- ifelse(is.na(df$GameID) | df$GameID == "",
                          df$SourceFile, df$GameID)
  
  # An untracked row has no Date. Fill it from the rest of its session so a
  # kept live-AB result still lands on the right day in the hitter trend.
  sess_date <- tapply(df$Date, df$SessionKey, function(v) {
    v <- v[!is.na(v)]; if (length(v)) v[[1]] else NA_character_ })
  miss <- is.na(df$Date)
  if (any(miss)) df$Date[miss] <- unname(sess_date[df$SessionKey[miss]])
  
  # Opponent: whichever Home/Away team isn't TMU (blank for an intra-squad
  # scrimmage, where both are THO_MOR).
  sess_opp <- tapply(seq_len(nrow(df)), df$SessionKey, function(i) {
    t <- unique(c(df$HomeTeam[i], df$AwayTeam[i]))
    t <- t[!is.na(t) & t != "" & t != TMU_TEAM_CODE]
    if (length(t)) t[[1]] else NA_character_ })
  df$Opponent <- unname(sess_opp[df$SessionKey])
  
  df$Pitcher <- trimws(df$Pitcher)
  df <- df[!is.na(df$Pitcher) & df$Pitcher != "", , drop = FALSE]
  if (nrow(df) == 0) {
    .set_load_status(mode, paste0("No row in '", dir, "/' has a Pitcher name."))
    return(NULL)
  }
  
  if (any(is.na(df$PitchUID) | df$PitchUID == "")) {
    gen <- paste(df$SessionKey, df$Date, df$Pitcher, df$PitchNo, sep = "__")
    df$PitchUID <- ifelse(is.na(df$PitchUID) | df$PitchUID == "", gen, df$PitchUID)
  }
  
  if (mode == "liveab" && isTRUE(FIX_MIDPA_PITCHER)) df <- fix_midpa_pitcher(df)
  
  # HAND TAG first. Spelling standardised, nothing reclassified. Where a pitch
  # was left untagged, TrackMan's AutoPitchType fills it (if the knob is on) and
  # TagSource records which one it was.
  hand_tag <- std_pitch_name(df$TaggedPitchType)
  auto_tag <- std_pitch_name(df$AutoPitchType)
  use_auto <- isTRUE(UNTAGGED_FALLBACK_TO_AUTO) &
    hand_tag == UNTAGGED_LABEL & auto_tag != UNTAGGED_LABEL
  df$PitchGroup <- ifelse(use_auto, auto_tag, hand_tag)
  df$TagSource  <- dplyr::case_when(
    use_auto                   ~ TAG_SOURCE_TRACKMAN,
    hand_tag != UNTAGGED_LABEL ~ TAG_SOURCE_HAND,
    TRUE                       ~ TAG_SOURCE_NONE)
  
  df <- df %>%
    dplyr::arrange(SessionKey, Pitcher, is.na(PitchNo), PitchNo) %>%
    dplyr::group_by(SessionKey, Pitcher) %>%
    dplyr::mutate(SeqNo = dplyr::row_number()) %>%
    dplyr::group_by(SessionKey, Pitcher, PitchGroup) %>%
    dplyr::mutate(PTNum = dplyr::row_number(), PTTot = dplyr::n()) %>%
    dplyr::ungroup()
  
  df$SessionLabel <- paste0(pretty_session_date(df$Date), " \u2014 ",
                            ifelse(is.na(df$Opponent), SESSION_LABELS[[mode]],
                                   paste0("vs ", team_display(df$Opponent))))
  
  n_untagged <- sum(df$PitchGroup == UNTAGGED_LABEL, na.rm = TRUE)
  n_auto     <- sum(df$TagSource == TAG_SOURCE_TRACKMAN, na.rm = TRUE)
  .set_load_status(mode, paste0(
    "Loaded ", nrow(df), " pitches / ",
    dplyr::n_distinct(df$SessionKey), " session(s) / ",
    dplyr::n_distinct(df$Pitcher), " arm(s) from ", dir, "/ (",
    paste(unique(df$SourceFile), collapse = ", "), ")",
    if (n_auto > 0)     paste0("  [", n_auto, " filled from the auto tag]") else "",
    if (n_untagged > 0) paste0("  [", n_untagged, " untagged]") else "",
    if (n_unassigned > 0) paste0("  [", n_unassigned, " Yakkertech pitches ",
                                 "with no pitcher dropped]") else ""))
  df
}

# -----------------------------------------------------------------------------
# build_session_index() — one row per session, newest first. Drives the picker.
# -----------------------------------------------------------------------------
build_session_index <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(data.frame(SessionKey = character(), Date = character(),
                      Pitches = integer(), Arms = integer(),
                      Label = character(), stringsAsFactors = FALSE))
  }
  # Grouped on SessionKey ALONE, with the date taken as the first non-missing
  # value in the session. Grouping on (SessionKey, Date) split one session into
  # two picker entries whenever a row carried no Date — an untracked pitch has
  # no Date — and both entries pointed at the same key, so one of them was a
  # bogus "Unknown date · 1 P · 1 arm" line.
  if (!"Opponent" %in% names(df)) df$Opponent <- NA_character_
  df %>%
    dplyr::group_by(SessionKey) %>%
    dplyr::summarise(Date    = {v <- Date[!is.na(Date)]
    if (length(v)) v[[1]] else NA_character_},
    Opponent = Opponent[1],
    Pitches = dplyr::n(),
    Arms    = dplyr::n_distinct(Pitcher),
    .groups = "drop") %>%
    dplyr::mutate(Label = paste0(pretty_session_date(Date),
                                 ifelse(is.na(Opponent), "",
                                        paste0(" vs ", team_display(Opponent))),
                                 "  \u00B7  ",
                                 Pitches, " P  \u00B7  ", Arms,
                                 ifelse(Arms == 1, " arm", " arms"))) %>%
    dplyr::arrange(dplyr::desc(Date), SessionKey) %>%
    as.data.frame(stringsAsFactors = FALSE)
}

session_choices <- function(idx) {
  if (is.null(idx) || nrow(idx) == 0) return(character(0))
  stats::setNames(idx$SessionKey, idx$Label)
}

# -----------------------------------------------------------------------------
# Shared derivations — written once so the bullpen and live-AB tabs cannot
# drift apart on what counts as a strike, a swing or a chase.
# -----------------------------------------------------------------------------
# FoulTip: a swing with contact that counts as a strike (a K with two
# strikes). Yakkertech writes it; not a whiff.
STRIKE_CALLS <- c("StrikeCalled","StrikeSwinging","InPlay","FoulTip",
                  "FoulBall","FoulBallNotFieldable","FoulBallFieldable")
SWING_CALLS  <- c("StrikeSwinging","FoulBall","FoulBallNotFieldable",
                  "FoulBallFieldable","FoulTip","InPlay")

# add_session_flags()
#   strike_from_zone = TRUE derives IsStrike from GEOMETRY (in the rulebook box
#   = strike, outside = ball) and ignores PitchCall entirely. That is the right
#   call for a bullpen: there are no swings, so the only strikes possible are
#   called ones, and a called strike in a pen is one person's judgment with no
#   umpire behind it. On the 8/14 pen the tag and the box disagreed on 2 of 45
#   pitches — both called strikes that crossed ~8 inches off the outside edge.
#
#   Note this makes IsStrike identical to InZone by construction, so anything
#   reporting both Strike% and Zone% off a zone-derived frame is printing the
#   same number twice. Untracked pitches are neither: IsStrike is NA, not FALSE,
#   so a pitch TrackMan missed can't be scored a ball for having no location.
add_session_flags <- function(df, strike_from_zone = FALSE) {
  df %>%
    dplyr::mutate(
      HasLoc = !is.na(PlateLocSide) & !is.na(PlateLocHeight),
      InZone = HasLoc &
        PlateLocSide   >= SZ_XMIN & PlateLocSide   <= SZ_XMAX &
        PlateLocHeight >= SZ_YMIN & PlateLocHeight <= SZ_YMAX,
      IsStrike = if (isTRUE(strike_from_zone)) ifelse(HasLoc, InZone, NA)
      else PitchCall %in% STRIKE_CALLS,
      IsSwing  = PitchCall %in% SWING_CALLS,
      IsWhiff  = PitchCall == "StrikeSwinging",
      # An untracked pitch is UNKNOWN, not out of the zone — letting NA count
      # as a chase is the bug that was found in the Y'alls leaderboard, so the
      # HasLoc term is mandatory here.
      IsChase  = HasLoc & !InZone & IsSwing,
      IsBIP    = PitchCall == "InPlay"
    )
}

# -----------------------------------------------------------------------------
# has_live_abs()
#   TRUE only when a frame carries real swing or contact events. A pen is
#   charted with stand-in batters AND populated PAofInning / PitchofPA / Balls /
#   Strikes columns, so column presence proves nothing — the 8/14 pen even has
#   KorBB values on it. This tests for events a pen cannot have, and is what
#   lets a panel hide itself instead of printing a column of zeros.
# -----------------------------------------------------------------------------
has_live_abs <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(FALSE)
  any(df$PitchCall %in% SWING_CALLS, na.rm = TRUE)
}

# =============================================================================
# PERSISTENT TAG OVERRIDES
# -----------------------------------------------------------------------------
# A two-column CSV — PitchUID, ManualPitchGroup — sitting next to app.R. It is
# the only place a hand correction is stored; the TrackMan exports in bullpens/
# and live_abs/ are never rewritten, so a corrected tag and the raw export can
# always be told apart.
# =============================================================================

.empty_overrides <- function() {
  data.frame(PitchUID = character(), ManualPitchGroup = character(),
             stringsAsFactors = FALSE)
}

# -----------------------------------------------------------------------------
# read_tag_overrides()
#   Missing, empty or malformed file all return an empty frame rather than
#   erroring — a bad overrides file must never stop the app from starting.
# -----------------------------------------------------------------------------
read_tag_overrides <- function(path = TAG_OVERRIDE_FILE) {
  if (!file.exists(path)) return(.empty_overrides())
  
  ov <- tryCatch(
    readr::read_csv(path, show_col_types = FALSE,
                    col_types = readr::cols(.default = readr::col_character()),
                    progress = FALSE),
    error = function(e) {
      warning("Could not read ", path, ": ", conditionMessage(e)); NULL
    })
  if (is.null(ov) || nrow(ov) == 0) return(.empty_overrides())
  
  ov <- as.data.frame(ov, stringsAsFactors = FALSE)
  if (!all(c("PitchUID", "ManualPitchGroup") %in% names(ov))) {
    warning(path, " has no PitchUID / ManualPitchGroup columns — ignoring it.")
    return(.empty_overrides())
  }
  
  ov <- ov[, c("PitchUID", "ManualPitchGroup"), drop = FALSE]
  ov$PitchUID <- trimws(as.character(ov$PitchUID))
  ov <- ov[!is.na(ov$PitchUID) & nzchar(ov$PitchUID) &
             !is.na(ov$ManualPitchGroup), , drop = FALSE]
  if (nrow(ov) == 0) return(.empty_overrides())
  
  # Same spelling fold as the exports, so an override typed as "ChangeUp"
  # matches a tag read as "Changeup".
  ov$ManualPitchGroup <- std_pitch_name(ov$ManualPitchGroup)
  
  # Last write wins on a duplicated UID.
  ov <- ov[!duplicated(ov$PitchUID, fromLast = TRUE), , drop = FALSE]
  rownames(ov) <- NULL
  ov
}

# -----------------------------------------------------------------------------
# write_tag_overrides()
#   Returns TRUE on success, FALSE on failure — the caller surfaces the failure
#   in the sidebar rather than letting a correction look saved when it is not.
#   An empty frame writes a header-only file instead of deleting it, so "no
#   overrides" is recorded rather than looking like a missing file.
# -----------------------------------------------------------------------------
write_tag_overrides <- function(ov, path = TAG_OVERRIDE_FILE) {
  if (is.null(ov)) ov <- .empty_overrides()
  ov <- as.data.frame(ov, stringsAsFactors = FALSE)
  ov <- ov[!is.na(ov$PitchUID) & nzchar(ov$PitchUID), , drop = FALSE]
  if (nrow(ov) > 0) ov <- ov[order(ov$PitchUID), , drop = FALSE]
  
  tryCatch({
    readr::write_csv(ov[, c("PitchUID", "ManualPitchGroup"), drop = FALSE], path)
    TRUE
  }, error = function(e) {
    warning("Could not write ", path, ": ", conditionMessage(e))
    FALSE
  })
}

# -----------------------------------------------------------------------------
# merge_tag_overrides()
#   The bullpen tab and the live-AB tab each hold their own copy of the
#   overrides in memory. Writing one straight to disk would erase the other's,
#   so a save replaces only the rows whose PitchUID belongs to the saving tab's
#   data (scope_uids) and leaves everything else on disk untouched.
# -----------------------------------------------------------------------------
merge_tag_overrides <- function(disk, new_ov, scope_uids) {
  if (is.null(disk))   disk   <- .empty_overrides()
  if (is.null(new_ov)) new_ov <- .empty_overrides()
  keep <- disk[!(disk$PitchUID %in% scope_uids), , drop = FALSE]
  out  <- rbind(keep[, c("PitchUID","ManualPitchGroup"), drop = FALSE],
                new_ov[, c("PitchUID","ManualPitchGroup"), drop = FALSE])
  out[!duplicated(out$PitchUID, fromLast = TRUE), , drop = FALSE]
}

# -----------------------------------------------------------------------------
# apply_tag_overrides()
#   Adds IsOverride and rewrites PitchGroup where an override exists. Kept here
#   rather than in the server so the download handler and the card cannot drift
#   apart on what a corrected frame looks like.
# -----------------------------------------------------------------------------
apply_tag_overrides <- function(df, ov) {
  if (is.null(df) || nrow(df) == 0) return(df)
  if (!"TagSource" %in% names(df)) {
    df$TagSource <- ifelse(as.character(df$PitchGroup) == UNTAGGED_LABEL,
                           TAG_SOURCE_NONE, TAG_SOURCE_HAND)
  }
  if (is.null(ov) || nrow(ov) == 0) {
    df$IsOverride <- FALSE
    return(df)
  }
  hit <- match(df$PitchUID, ov$PitchUID)
  new <- ov$ManualPitchGroup[hit]
  df$IsOverride <- !is.na(new)
  df$TagSource  <- ifelse(df$IsOverride, TAG_SOURCE_OVERRIDE, df$TagSource)
  # Character, not a factor — load_sessions() leaves PitchGroup as character and
  # every card orders it with order_pitch_factor() at draw time. Handing back a
  # factor here would make an overridden frame a different type from a clean
  # one, which is exactly the kind of difference that shows up as one broken
  # plot months later.
  df$PitchGroup <- ifelse(is.na(new), as.character(df$PitchGroup), new)
  df
}

# -----------------------------------------------------------------------------
# suggest_pitch_tag()
#   NOT an auto tagger. Nothing here ever writes a tag — this only answers
#   "which of THIS pitcher's already-tagged pitches does the selected one look
#   most like", so the override dropdown can open on the obvious answer and be
#   confirmed with a click.
#
#   Deliberately narrow:
#     - the pool is that pitcher's own HAND-tagged (or overridden) pitches in
#       the loaded data — never a league model, and never a pitch whose tag
#       came from AutoPitchType, so TrackMan cannot vouch for itself;
#     - distance is z-scored per feature off the pool's own spread, so a 2-mph
#       velo gap and a 2-inch break gap are not treated as equal;
#     - features missing on either side are skipped and the distance is
#       averaged over the ones actually compared, so a pitch with no spin
#       reading still gets a sensible neighbour;
#     - returns NULL when the pitcher has nothing tagged to compare against.
#
#   `dist` is in pooled standard deviations. Under ~1 is a tight match; a large
#   value means the pitch does not resemble anything he has thrown, which is
#   exactly when the suggestion should NOT be accepted.
# -----------------------------------------------------------------------------
SUGGEST_FEATURES <- c("RelSpeed", "InducedVertBreak", "HorzBreak", "SpinRate")

suggest_pitch_tag <- function(df, uid) {
  if (is.null(df) || nrow(df) == 0 || is.null(uid) || is.na(uid)) return(NULL)
  
  i <- which(df$PitchUID == uid)
  if (length(i) != 1) return(NULL)
  tgt <- df[i, , drop = FALSE]
  
  feats <- SUGGEST_FEATURES[SUGGEST_FEATURES %in% names(df)]
  if (!length(feats)) return(NULL)
  
  x <- suppressWarnings(as.numeric(unlist(tgt[1, feats])))
  if (all(is.na(x))) return(NULL)          # untracked pitch — nothing to match
  
  pool <- df[!is.na(df$Pitcher) & df$Pitcher == tgt$Pitcher[1] &
               !is.na(df$PitchGroup) &
               as.character(df$PitchGroup) != UNTAGGED_LABEL &
               (if ("TagSource" %in% names(df))
                 df$TagSource %in% c(TAG_SOURCE_HAND, TAG_SOURCE_OVERRIDE)
                else TRUE) &
               df$PitchUID != uid, , drop = FALSE]
  if (nrow(pool) == 0) return(NULL)
  
  M <- as.matrix(pool[, feats, drop = FALSE])
  storage.mode(M) <- "double"
  
  s <- apply(M, 2, stats::sd, na.rm = TRUE)
  s[!is.finite(s) | s < 1e-6] <- 1
  
  d2 <- rep(0, nrow(M)); used <- rep(0L, nrow(M))
  for (j in seq_along(feats)) {
    ok <- !is.na(M[, j]) & !is.na(x[j])
    d2[ok]   <- d2[ok] + ((M[ok, j] - x[j]) / s[j])^2
    used[ok] <- used[ok] + 1L
  }
  if (!any(used > 0)) return(NULL)
  
  dist <- sqrt(d2 / pmax(used, 1L))
  dist[used == 0] <- Inf
  b <- which.min(dist)
  if (!is.finite(dist[b])) return(NULL)
  
  list(
    tag      = as.character(pool$PitchGroup[b]),
    dist     = unname(dist[b]),
    n_pool   = nrow(pool),
    n_feats  = unname(used[b]),
    neighbor = as.list(pool[b, feats, drop = FALSE])
  )
}
# =============================================================================
# PREBUILT RDS
# -----------------------------------------------------------------------------
# build_session_rds() is called by build_tmu_session_data.R.
# load_session_store() is called by app.R at startup.
#
# The RDS is a list:
#   $bullpen, $liveab  — exactly what load_sessions() returns for each mode
#   $status            — the load-status line for each mode
#   $manifest          — per mode, the files it was built from (name + md5)
#   $built_at, $version
#
# Overrides are NOT baked in. tag_overrides.csv is applied at runtime, so a
# correction made in the app never needs a rebuild.
# =============================================================================
# Bump whenever load_sessions() changes what it produces — an RDS built by an
# older version is treated as stale and the CSVs are re-read.
SESSION_RDS_VERSION <- 3L

.session_manifest <- function(mode) {
  dir <- SESSION_DIRS[[mode]]
  if (!dir.exists(dir)) return(NULL)
  f <- list.files(dir, pattern = "\\.(csv|xlsx|xls)$",
                  full.names = TRUE, ignore.case = TRUE)
  if (!length(f)) return(NULL)
  f <- f[order(basename(f))]
  # md5, not modified time — a deploy rewrites every file's mtime, so a
  # timestamp check would call every file "changed" on the server.
  data.frame(file = basename(f), md5 = unname(tools::md5sum(f)),
             stringsAsFactors = FALSE)
}

.same_manifest <- function(a, b) {
  if (is.null(a) || is.null(b)) return(FALSE)
  identical(as.character(a$file), as.character(b$file)) &&
    identical(as.character(a$md5), as.character(b$md5))
}

build_session_rds <- function(path = SESSION_RDS) {
  out <- list(version = SESSION_RDS_VERSION, built_at = Sys.time(),
              bullpen = NULL, liveab = NULL,
              status = list(), manifest = list())
  for (m in names(SESSION_DIRS)) {
    # out[m] <- list(x), NOT out[[m]] <- x: assigning NULL with [[ ]] deletes
    # the element, and an empty mode must still be recorded as empty.
    out[m] <- list(load_sessions(m))
    out$status[m]   <- list(session_load_status(m))
    out$manifest[m] <- list(.session_manifest(m))
  }
  saveRDS(out, path)
  invisible(out)
}

load_session_store <- function(source = DATA_SOURCE, path = SESSION_RDS) {
  source <- match.arg(source, c("auto", "rds", "csv"))
  
  store <- NULL
  if (source != "csv" && file.exists(path)) {
    store <- tryCatch(readRDS(path), error = function(e) {
      message("Could not read ", path, ": ", conditionMessage(e)); NULL })
    if (!is.list(store) || is.null(store$version)) {
      if (!is.null(store)) message(path, " is not a session store — ignoring it.")
      store <- NULL
    } else if (!identical(as.integer(store$version), SESSION_RDS_VERSION)) {
      message(path, " was built by an older tmu_session_data.R (v",
              store$version, ", now v", SESSION_RDS_VERSION,
              ") — reading CSVs. Re-run build_tmu_session_data.R.")
      stale_version <- TRUE
      store <- NULL
    }
  } else if (source == "rds") {
    message("DATA_SOURCE = \"rds\" but ", path, " does not exist — reading CSVs.")
  }
  
  if (!exists("stale_version", inherits = FALSE)) stale_version <- FALSE
  
  res <- list(bullpen = NULL, liveab = NULL,
              origin = c(bullpen = NA_character_, liveab = NA_character_))
  rewrite <- FALSE
  
  for (m in names(SESSION_DIRS)) {
    use_rds <- FALSE
    if (!is.null(store)) {
      if (source == "rds") {
        use_rds <- TRUE
      } else {
        cur <- .session_manifest(m)
        # No folder / no files here: the RDS is all there is, trust it.
        use_rds <- is.null(cur) || .same_manifest(cur, store$manifest[[m]])
      }
    }
    
    if (use_rds) {
      res[m] <- list(store[[m]])
      res$origin[[m]] <- "rds"
      st <- store$status[[m]]
      if (is.null(st)) st <- "No load status stored."
      .set_load_status(m, paste0(st, "  [", path, ", built ",
                                 format(store$built_at, "%b %d %H:%M"), "]"))
    } else {
      res[m] <- list(load_sessions(m))
      res$origin[[m]] <- "csv"
      if (!is.null(store)) {
        message("[", m, "] files in ", SESSION_DIRS[[m]],
                "/ differ from ", path, " — read from the CSVs.")
        store[m]          <- list(res[[m]])
        store$status[m]   <- list(session_load_status(m))
        store$manifest[m] <- list(.session_manifest(m))
        rewrite <- TRUE
      }
    }
  }
  
  # Keep the RDS current so the next start is fast again. Best effort only —
  # a read-only filesystem just means the CSVs get read next time too.
  if (stale_version && source == "auto") {
    tryCatch({ build_session_rds(path); message("Rebuilt ", path) },
             error = function(e) message("Could not rebuild ", path, ": ",
                                         conditionMessage(e)))
  } else if (rewrite && source == "auto") {
    store$built_at <- Sys.time()
    tryCatch({ saveRDS(store, path); message("Refreshed ", path) },
             error = function(e) message("Could not refresh ", path, ": ",
                                         conditionMessage(e)))
  }
  res
}

# =============================================================================
# LIVE-AB RESULTS — plate appearances, outcomes, slash lines
# -----------------------------------------------------------------------------
# Written once here so the app's Game Stats tab, the Hitters tab and
# TMUPitcherReport.Rmd cannot disagree about what a hit or a PA is.
# =============================================================================
BALL_CALLS <- c("BallCalled", "BallinDirt", "BallIntentional")

HIT_RESULTS <- c("1B", "2B", "3B", "HR")
# Results that are official at-bats. BIP = put in play with no PlayResult
# charted — counted as an AB with no hit rather than dropped, and surfaced
# as its own column so an untagged result is visible.
AB_RESULTS  <- c(HIT_RESULTS, "Out", "FC", "E", "BIP", "K")
OUT_RESULTS <- c("K", "Out", "FC", "SF", "SH")

# -----------------------------------------------------------------------------
# add_pa_outcomes()
#   Run on a WHOLE session frame (every pitcher), before any pitcher filter —
#   a PA is defined by the pitch order of the session, not of one arm.
#
#   A new PA starts on any of:
#     - the first pitch of a session
#     - PitchofPA == 1
#     - a different Batter
#     - a change in Inning / TopBottom / PAofInning
#     - the pitch after a PA-ending event (K, BB, HBP, ball in play) — this is
#       what separates two ABs in a row by the same hitter in live BP, where
#       PitchofPA is not always reset
#   A pitching change in the middle of a PA does NOT start a new one; the PA
#   is charged to whoever threw its last pitch.
#
#   Adds per pitch: PAKey, PitchInPA, PAEnd (last pitch of its PA) and
#   PAResult (the PA's result, repeated on every pitch of it):
#     K, BB, HBP, 1B, 2B, 3B, HR, Out, FC, E, SF, SH, BIP, Incomplete
#   "Incomplete" = the PA stopped without a result (a live AB cut off at
#   1-1). It is never counted as a PA.
# -----------------------------------------------------------------------------
.pa_result <- function(pc, kb, pr, balls, strikes, hit_type, derive_ok = TRUE) {
  pc <- ifelse(is.na(pc), "", pc); kb <- ifelse(is.na(kb), "", kb)
  pr <- ifelse(is.na(pr), "", pr); ht <- ifelse(is.na(hit_type), "", hit_type)
  derive <- isTRUE(DERIVE_K_BB_FROM_COUNT) & derive_ok
  dplyr::case_when(
    kb == "Strikeout" |
      pr %in% c("Strikeout", "StrikeoutSwinging", "StrikeoutLooking") ~ "K",
    # HBP before BB: Yakkertech writes an HBP as PlayResult "Walk"
    pc == "HitByPitch" | pr == "HitByPitch"                  ~ "HBP",
    kb == "Walk" | pr %in% c("Walk", "IntentionalWalk")      ~ "BB",
    pc == "InPlay" & pr == "Single"                          ~ "1B",
    pc == "InPlay" & pr == "Double"                          ~ "2B",
    pc == "InPlay" & pr == "Triple"                          ~ "3B",
    pc == "InPlay" & pr == "HomeRun"                         ~ "HR",
    pc == "InPlay" & pr == "Out"                             ~ "Out",
    pc == "InPlay" & pr == "FieldersChoice"                  ~ "FC",
    pc == "InPlay" & pr %in% c("Error", "Errir")             ~ "E",
    pc == "InPlay" & pr %in% c("SacrificeBunt", "SacrificeHit") ~ "SH",
    pc == "InPlay" & pr == "SacrificeFly"                    ~ "SF",
    pc == "InPlay" & pr %in% c("Sacrifice", "Sacrfice") &
      ht == "Bunt"                                           ~ "SH",
    pc == "InPlay" & pr %in% c("Sacrifice", "Sacrfice")      ~ "SF",
    pc == "InPlay"                                           ~ "BIP",
    derive & pc %in% c("StrikeCalled", "StrikeSwinging", "FoulTip") &
      !is.na(strikes) & strikes >= 2                         ~ "K",
    derive & pc %in% BALL_CALLS & !is.na(balls) & balls >= 3 ~ "BB",
    TRUE                                                     ~ "Incomplete"
  )
}

add_pa_outcomes <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(df)
  
  ord <- order(df$SessionKey, is.na(df$PitchNo), df$PitchNo, seq_len(nrow(df)))
  d   <- df[ord, , drop = FALSE]
  n   <- nrow(d)
  
  lag1 <- function(x) c(x[1], x[-n])
  sess <- d$SessionKey
  bat  <- ifelse(is.na(d$Batter), "", trimws(d$Batter))
  key3 <- paste(d$Inning, d$TopBottom, d$PAofInning, sep = "|")
  
  half <- paste(d$Inning, d$TopBottom, sep = "|")
  yak  <- if ("Source" %in% names(d)) d$Source %in% "Yakkertech" else rep(FALSE, n)
  
  # does THIS pitch end a PA? (used to split the next pitch off)
  # Yakkertech counts are unreliable at PA boundaries, so a K/BB is never
  # inferred from the count there — its PlayResult always carries them.
  res_here <- .pa_result(d$PitchCall, d$KorBB, d$PlayResult,
                         d$Balls, d$Strikes, d$TaggedHitType,
                         derive_ok = !yak)
  ends     <- res_here != "Incomplete"
  
  # Yakkertech: PitchofPA / PAofInning repeat, skip and restart mid-PA (on
  # the NKU file PitchofPA == 1 lands on a non-first pitch 7 times), so only
  # the batter, the half-inning and the previous result split a PA there.
  new_pa <- rep(FALSE, n)
  new_pa[1] <- TRUE
  if (n > 1) {
    i <- 2:n
    new_pa[i] <- sess[i] != sess[i - 1] |
      bat[i] != bat[i - 1] |
      half[i] != half[i - 1] |
      ends[i - 1] |
      (!yak[i] & ((!is.na(d$PitchofPA[i]) & d$PitchofPA[i] == 1) |
                    key3[i] != key3[i - 1]))
  }
  
  d$PANum <- stats::ave(as.integer(new_pa), sess, FUN = cumsum)
  d$PAKey <- paste0(sess, "#", sprintf("%03d", d$PANum))
  
  d <- d %>%
    dplyr::group_by(PAKey) %>%
    dplyr::mutate(PitchInPA = dplyr::row_number(),
                  PAEnd     = PitchInPA == dplyr::n()) %>%
    dplyr::ungroup()
  
  res_last <- ifelse(d$PAEnd, res_here, NA_character_)
  pa_res   <- stats::setNames(res_last[d$PAEnd], d$PAKey[d$PAEnd])
  d$PAResult <- unname(pa_res[d$PAKey])
  
  # back to the caller's row order
  d[order(ord), , drop = FALSE]
}

# -----------------------------------------------------------------------------
# fix_midpa_pitcher() — see FIX_MIDPA_PITCHER. Rewrites Pitcher, PitcherId,
#   PitcherThrows and PitcherTeam on the minority pitches of a PA, copying
#   them from one of the modal pitcher's own pitches. Records what it changed
#   in PitcherOriginal (NA where untouched).
# -----------------------------------------------------------------------------
fix_midpa_pitcher <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(df)
  df$PitcherOriginal <- NA_character_
  k <- add_pa_outcomes(df)$PAKey          # same row order as df
  
  pos   <- seq_len(nrow(df))
  ord   <- order(df$SessionKey, is.na(df$PitchNo), df$PitchNo, pos)
  n_fix <- 0L
  for (key in unique(k[ord])) {
    i <- ord[k[ord] == key]                 # this PA's rows, in pitch order
    arms <- df$Pitcher[i]
    if (dplyr::n_distinct(arms) < 2) next
    tab  <- table(arms)
    top  <- names(tab)[tab == max(tab)]
    modal <- if (length(top) == 1) top else utils::tail(arms, 1)
    bad  <- i[arms != modal]
    src  <- i[arms == modal][1]
    for (cc in c("PitcherId", "PitcherThrows", "PitcherTeam")) {
      if (cc %in% names(df)) df[[cc]][bad] <- df[[cc]][src]
    }
    df$PitcherOriginal[bad] <- df$Pitcher[bad]
    message("Mid-PA pitcher tag fixed: ", key, " PitchNo ",
            paste(df$PitchNo[bad], collapse = ","), " '",
            paste(unique(df$Pitcher[bad]), collapse = "', '"), "' -> '",
            modal, "'")
    df$Pitcher[bad] <- modal
    n_fix <- n_fix + length(bad)
  }
  if (n_fix == 0) df$PitcherOriginal <- NULL
  df
}

# -----------------------------------------------------------------------------
# live_ab_checks() — charting problems that quietly bend the numbers. Used by
#   build_tmu_session_data.R; returns a list of small data frames (empty ones
#   dropped).
#     retagged      — pitches fix_midpa_pitcher() moved to another pitcher
#     odd_start     — a PA whose first pitch isn't 0-0 (count carried over)
#     count_breaks  — the recorded count doesn't follow the previous call
#     contact_calls — a ball / called or swinging strike with batted-ball
#                     data on it (usually a foul charted as something else)
#     yak_uncharted — Yakkertech pitches with no PitchCall (not charted);
#                     their PAs can't get a result unless one was entered
#   Count problems matter for the count filter, FPS%, Race-to-2K, Putaway%
#   and the K/BB-from-count fallback. PA results themselves come from
#   KorBB / PlayResult and are not affected.
# -----------------------------------------------------------------------------
live_ab_checks <- function(la) {
  out <- list()
  if (is.null(la) || nrow(la) == 0) return(out)
  if (!"PAKey" %in% names(la)) la <- add_pa_outcomes(la)
  la <- la[order(la$SessionKey, is.na(la$PitchNo), la$PitchNo), , drop = FALSE]
  cols <- function(d, extra = NULL) as.data.frame(
    d[, intersect(c("Date", "PitchNo", "Pitcher", "Batter", extra), names(d)),
      drop = FALSE])
  
  if ("PitcherOriginal" %in% names(la)) {
    r <- la[!is.na(la$PitcherOriginal), , drop = FALSE]
    if (nrow(r)) out$retagged <- cols(r, c("PitcherOriginal", "RelSpeed"))
  }
  
  # Count checks are TrackMan-only: Yakkertech's counts lag at every PA
  # boundary by design of its export, so flagging them is just noise.
  is_tm <- !("Source" %in% names(la)) | la$Source %in% "TrackMan"
  
  if ("Source" %in% names(la)) {
    y <- la[la$Source %in% "Yakkertech", , drop = FALSE]
    un <- y[is.na(y$PitchCall) | y$PitchCall == "", , drop = FALSE]
    if (nrow(un)) {
      out$yak_uncharted <- as.data.frame(
        un %>% dplyr::count(Date, Pitcher, PitcherTeam, name = "Pitches"))
    }
  }
  
  # Yakkertech PAs that ended with no result. Its batter label sometimes
  # shifts by one pitch at a PA boundary, so the pitch that ended a PA is
  # attributed to the next hitter and the real PA is left with nothing on
  # it. Seen twice in one game file. The next pitch is shown so the fix in
  # the CSV is obvious. Nothing here is auto-repaired.
  if ("Source" %in% names(la)) {
    yk  <- la$Source %in% "Yakkertech"
    end <- which(yk & la$PAEnd & la$PAResult %in% "Incomplete")
    if (length(end)) {
      nx <- pmin(end + 1L, nrow(la))
      same_s <- la$SessionKey[nx] == la$SessionKey[end] & nx != end
      pa_n <- table(la$PAKey)
      out$yak_no_result <- data.frame(
        Date       = la$Date[end],
        PitchNo    = la$PitchNo[end],
        Pitcher    = la$Pitcher[end],
        Batter     = la$Batter[end],
        Pitches    = as.integer(pa_n[la$PAKey[end]]),
        LastCall   = ifelse(is.na(la$PitchCall[end]), "(none)", la$PitchCall[end]),
        LastCount  = paste0(la$Balls[end], "-", la$Strikes[end]),
        NextBatter = ifelse(same_s, la$Batter[nx], NA),
        NextResult = ifelse(same_s & !is.na(la$PlayResult[nx]),
                            la$PlayResult[nx], ""),
        stringsAsFactors = FALSE)
    }
  }
  
  first <- la[is_tm & la$PitchInPA == 1 & !is.na(la$Balls) & !is.na(la$Strikes) &
                (la$Balls != 0 | la$Strikes != 0), , drop = FALSE]
  if (nrow(first)) {
    first$Count <- paste0(first$Balls, "-", first$Strikes)
    out$odd_start <- cols(first, "Count")
  }
  
  same_pa <- c(FALSE, la$PAKey[-1] == la$PAKey[-nrow(la)])
  prev <- function(x) c(NA, x[-length(x)])
  pc <- prev(la$PitchCall); pb <- prev(la$Balls); ps <- prev(la$Strikes)
  exp_b <- ifelse(pc %in% BALL_CALLS, pb + 1, pb)
  exp_s <- dplyr::case_when(
    pc %in% c("StrikeCalled", "StrikeSwinging") ~ ps + 1,
    grepl("^Foul", pc) & !is.na(ps) & ps < 2    ~ ps + 1,
    TRUE                                        ~ ps)
  brk <- is_tm & same_pa & !is.na(exp_b) & !is.na(la$Balls) & !is.na(la$Strikes) &
    (la$Balls != exp_b | la$Strikes != exp_s)
  if (any(brk)) {
    b <- la[brk, , drop = FALSE]
    b$Recorded <- paste0(b$Balls, "-", b$Strikes)
    b$Expected <- paste0(exp_b[brk], "-", exp_s[brk])
    b$AfterCall <- pc[brk]
    out$count_breaks <- cols(b, c("AfterCall", "Expected", "Recorded"))
  }
  
  cc <- la[is_tm & la$PitchCall %in% c(BALL_CALLS, "StrikeCalled", "StrikeSwinging") &
             !is.na(la$ExitSpeed), , drop = FALSE]
  if (nrow(cc)) out$contact_calls <- cols(cc, c("PitchCall", "ExitSpeed", "Angle"))
  
  out
}

# -----------------------------------------------------------------------------
# pa_table() — one row per COMPLETED plate appearance, from the frame it is
#   given. Pitcher and pitch info come from the PA's last pitch, so filtering
#   a frame to one pitcher first gives exactly the PAs he finished.
# -----------------------------------------------------------------------------
batted_ball_type <- function(angle) {
  dplyr::case_when(
    is.na(angle)  ~ NA_character_,
    angle <= 9    ~ "GB",
    angle <= 25   ~ "LD",
    TRUE          ~ "FB")
}

pa_table <- function(df) {
  if (is.null(df) || nrow(df) == 0 || !"PAEnd" %in% names(df)) return(NULL)
  n_pitch <- table(df$PAKey)
  pa <- df[!is.na(df$PAEnd) & df$PAEnd &
             !is.na(df$PAResult) & df$PAResult != "Incomplete", , drop = FALSE]
  if (nrow(pa) == 0) return(NULL)
  pa <- pa %>%
    dplyr::mutate(
      Pitches = as.integer(n_pitch[PAKey]),
      FinalCount = ifelse(is.na(Balls) | is.na(Strikes), NA_character_,
                          paste0(Balls, "-", Strikes)),
      IsAB   = PAResult %in% AB_RESULTS,
      IsHit  = PAResult %in% HIT_RESULTS,
      TB     = dplyr::case_when(PAResult == "1B" ~ 1L, PAResult == "2B" ~ 2L,
                                PAResult == "3B" ~ 3L, PAResult == "HR" ~ 4L,
                                TRUE ~ 0L),
      IsOut  = PAResult %in% OUT_RESULTS,
      BIP    = PitchCall == "InPlay",
      BBType = ifelse(BIP, batted_ball_type(Angle), NA_character_)
    )
  pa[order(pa$Date, pa$SessionKey, pa$PANum), , drop = FALSE]
}

.rate <- function(num, den) ifelse(!is.na(den) & den > 0, num / den, NA_real_)

# -----------------------------------------------------------------------------
# summarise_pa() — the slash line for any set of PAs. Numeric; format with
#   fmt_avg() / fmt_pct() at display time.
# -----------------------------------------------------------------------------
summarise_pa <- function(pa) {
  if (is.null(pa) || nrow(pa) == 0) {
    pa <- data.frame(PAResult = character(), IsAB = logical(), IsHit = logical(),
                     TB = integer(), ExitSpeed = numeric(), BIP = logical())
  }
  r  <- pa$PAResult
  PA <- length(r); AB <- sum(pa$IsAB); H <- sum(pa$IsHit)
  BB <- sum(r == "BB"); HBP <- sum(r == "HBP"); SF <- sum(r == "SF")
  K  <- sum(r == "K");  TB <- sum(pa$TB)
  ev <- pa$ExitSpeed[pa$BIP & !is.na(pa$ExitSpeed)]
  dplyr::tibble(
    PA = PA, AB = AB, H = H,
    `1B` = sum(r == "1B"), `2B` = sum(r == "2B"), `3B` = sum(r == "3B"),
    HR = sum(r == "HR"), BB = BB, HBP = HBP, K = K, SF = SF,
    SH = sum(r == "SH"), ROE = sum(r == "E"), BIPx = sum(r == "BIP"),
    AVG = .rate(H, AB),
    OBP = .rate(H + BB + HBP, AB + BB + HBP + SF),
    SLG = .rate(TB, AB),
    OPS = .rate(H + BB + HBP, AB + BB + HBP + SF) + .rate(TB, AB),
    `K%`  = .rate(K, PA),
    `BB%` = .rate(BB, PA),
    `Avg EV` = if (length(ev)) mean(ev) else NA_real_,
    `Max EV` = if (length(ev)) max(ev)  else NA_real_,
    `HH%`    = .rate(sum(ev >= HARD_HIT_MPH), length(ev))
  )
}

# -----------------------------------------------------------------------------
# summarise_pitches() — plate-discipline numbers for any set of pitches.
#   Chase% and Z-Swing% are over LOCATED pitches only (an untracked pitch is
#   unknown, not out of the zone). FPS% needs add_pa_outcomes() to have run.
# -----------------------------------------------------------------------------
summarise_pitches <- function(d) {
  f <- add_session_flags(d)
  oz  <- f$HasLoc & !f$InZone
  iz  <- f$HasLoc &  f$InZone
  sw  <- f$IsSwing
  wh  <- f$IsWhiff
  ev  <- f$ExitSpeed[f$IsBIP & !is.na(f$ExitSpeed)]
  bbt <- batted_ball_type(f$Angle[f$IsBIP])
  fp  <- if ("PitchInPA" %in% names(f)) f$PitchInPA == 1 else rep(FALSE, nrow(f))
  dplyr::tibble(
    Pitches      = nrow(f),
    `Strike%`    = .rate(sum(f$IsStrike, na.rm = TRUE), nrow(f)),
    `Swing%`     = .rate(sum(sw), nrow(f)),
    `Whiff%`     = .rate(sum(wh), sum(sw)),
    `CSW%`       = .rate(sum(f$PitchCall == "StrikeCalled" | wh, na.rm = TRUE), nrow(f)),
    `Chase%`     = .rate(sum(oz & sw), sum(oz)),
    `Z-Swing%`   = .rate(sum(iz & sw), sum(iz)),
    `Z-Contact%` = .rate(sum(iz & sw & !wh), sum(iz & sw)),
    `FPS%`       = .rate(sum(fp & f$IsStrike, na.rm = TRUE), sum(fp)),
    BIP          = sum(f$IsBIP),
    `Avg EV`     = if (length(ev)) mean(ev) else NA_real_,
    `Max EV`     = if (length(ev)) max(ev)  else NA_real_,
    `HH%`        = .rate(sum(ev >= HARD_HIT_MPH), length(ev)),
    `Soft%`      = .rate(sum(ev <  SOFT_CONTACT_MPH), length(ev)),
    `GB%`        = .rate(sum(bbt == "GB", na.rm = TRUE), sum(!is.na(bbt))),
    `LD%`        = .rate(sum(bbt == "LD", na.rm = TRUE), sum(!is.na(bbt))),
    `FB%`        = .rate(sum(bbt == "FB", na.rm = TRUE), sum(!is.na(bbt)))
  )
}

# -----------------------------------------------------------------------------
# hitter_session_log() — one row per hitter per session, oldest first, with
#   running (to-date) AVG / OBP / SLG / OPS across the offseason.
# -----------------------------------------------------------------------------
hitter_session_log <- function(pa) {
  if (is.null(pa) || nrow(pa) == 0) return(NULL)
  pa %>%
    dplyr::group_by(Batter, SessionKey) %>%
    dplyr::group_modify(~ dplyr::bind_cols(
      dplyr::tibble(Date = .x$Date[1]), summarise_pa(.x),
      dplyr::tibble(.tb = sum(.x$TB)))) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(Batter, Date, SessionKey) %>%
    dplyr::group_by(Batter) %>%
    dplyr::mutate(
      cAB = cumsum(AB), cH = cumsum(H), cTB = cumsum(.tb),
      cOBn = cumsum(H + BB + HBP), cOBd = cumsum(AB + BB + HBP + SF),
      `AVG to date` = .rate(cH, cAB),
      `OBP to date` = .rate(cOBn, cOBd),
      `SLG to date` = .rate(cTB, cAB),
      `OPS to date` = `OBP to date` + `SLG to date`,
      `PA to date`  = cumsum(PA)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-cAB, -cH, -cTB, -cOBn, -cOBd, -.tb)
}

# ---- shared display formats -------------------------------------------------
fmt_avg <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  out <- sprintf("%.3f", x)
  out <- sub("^0\\.", ".", out)
  ifelse(is.na(x), "\u2014", out)
}
fmt_pct <- function(x, digits = 0) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.na(x), "\u2014", paste0(formatC(100 * x, format = "f", digits = digits), "%"))
}
fmt_num <- function(x, digits = 1) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.na(x), "\u2014", formatC(x, format = "f", digits = digits))
}

# Formats a frame from summarise_pa()/summarise_pitches() for display: rate
# columns to .333 / 33%, EV to one decimal, counts left alone.
format_stat_cols <- function(x) {
  x <- as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)
  for (cc in names(x)) {
    if (cc %in% c("AVG", "OBP", "SLG", "OPS") || grepl("to date$", cc) &&
        !grepl("^PA", cc)) {
      x[[cc]] <- fmt_avg(x[[cc]])
    } else if (grepl("%$", cc)) {
      x[[cc]] <- fmt_pct(x[[cc]])
    } else if (cc %in% c("Avg EV", "Max EV", "Velo", "Avg Velo")) {
      x[[cc]] <- fmt_num(x[[cc]], 1)
    }
  }
  x
}