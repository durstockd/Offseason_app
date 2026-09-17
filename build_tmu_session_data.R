# =============================================================================
# build_tmu_session_data.R  —  CSVs -> tmu_session_data.rds
# -----------------------------------------------------------------------------
# Run it after dropping new exports into bullpens/ or live_abs/, then deploy.
# It finds the Offseason_app folder on its own, whatever R's working
# directory is, so any of these work:
#   - open this file in RStudio and hit Source (or Run All / Cmd+Enter)
#   - source("/path/to/Offseason_app/build_tmu_session_data.R")
# If it still can't tell where it lives, paste the folder path into APP_DIR
# just below. R's working directory is put back when the build finishes.
#
# The app loads the RDS at startup. If the CSVs in a folder no longer match
# what the RDS was built from (DATA_SOURCE = "auto" in tmu_session_data.R),
# the app re-reads that folder instead, so a forgotten rebuild never shows
# stale data — it just starts a little slower.
#
# Tag overrides are NOT baked in; tag_overrides.csv is applied live by the app
# and by both reports, so fixing a tag never needs a rebuild.
# =============================================================================

# Leave "" to auto-detect. Otherwise the full path to the Offseason_app folder.
APP_DIR <- ""

.find_app_dir <- function(manual = "") {
  ok <- function(d) length(d) == 1 && !is.na(d) && nzchar(d) &&
    file.exists(file.path(d, "tmu_session_data.R"))
  
  cands <- character(0)
  if (nzchar(manual)) cands <- c(cands, path.expand(manual))
  
  # 1. source("…/build_tmu_session_data.R") — source() keeps the path in `ofile`
  for (fr in rev(sys.frames())) {
    f <- tryCatch(get("ofile", envir = fr, inherits = FALSE), error = function(e) NULL)
    if (is.character(f) && length(f) == 1) cands <- c(cands, dirname(f))
  }
  
  # 2. RStudio: this script open in the editor (Source button, Run All, Cmd+Enter)
  if (requireNamespace("rstudioapi", quietly = TRUE) &&
      isTRUE(tryCatch(rstudioapi::isAvailable(), error = function(e) FALSE))) {
    ed <- tryCatch(rstudioapi::getSourceEditorContext()$path,
                   error = function(e) "")
    if (length(ed) == 1 && nzchar(ed)) cands <- c(cands, dirname(ed))
  }
  
  # 3. Rscript build_tmu_session_data.R
  fa <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(fa)) cands <- c(cands, dirname(sub("^--file=", "", fa[1])))
  
  # 4. the working directory itself
  cands <- c(cands, getwd())
  
  hit <- Filter(ok, unique(cands))
  if (length(hit)) return(normalizePath(hit[[1]]))
  
  if (nzchar(manual) && !ok(path.expand(manual))) {
    stop("APP_DIR is set to '", manual, "' but tmu_session_data.R is not in it.")
  }
  stop("Couldn't find the Offseason_app folder (looked in: ",
       paste(unique(cands), collapse = ", "), "). Open ",
       "build_tmu_session_data.R in RStudio and click Source, or set APP_DIR ",
       "at the top of the script to the Offseason_app folder's full path.")
}

.app_dir <- .find_app_dir(APP_DIR)
.old_wd  <- setwd(.app_dir)
message("Building from ", .app_dir)

# Everything below runs inside the app folder; the working directory is put
# back afterwards even if the build errors out.
tryCatch({
  
  source("tmu_session_data.R")
  
  # The build script and tmu_session_data.R are updated together. An older
  # tmu_session_data.R is missing functions used below — stop BEFORE writing
  # anything rather than dying halfway with "could not find function".
  .needed  <- c("load_session_store", "build_session_rds", "add_pa_outcomes",
                "fix_midpa_pitcher", "live_ab_checks", "std_team")
  .missing <- .needed[!vapply(.needed, exists, logical(1))]
  if (length(.missing) || !exists("SESSION_RDS_VERSION") || SESSION_RDS_VERSION < 3L) {
    stop("tmu_session_data.R in ", getwd(), " is an older version than this ",
         "build script (missing: ",
         paste(c(.missing, if (!exists("SESSION_RDS_VERSION") ||
                               SESSION_RDS_VERSION < 3L) "SESSION_RDS_VERSION 3"),
               collapse = ", "),
         "). Replace it with the latest tmu_session_data.R and run again. ",
         "Nothing was written.", call. = FALSE)
  }
  
  t0    <- Sys.time()
  store <- build_session_rds(SESSION_RDS)
  
  cat("\n================ TMU session build ================\n")
  cat("Wrote ", normalizePath(SESSION_RDS), "  (",
      format(structure(file.size(SESSION_RDS), class = "object_size"), units = "auto"),
      ", ", round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1), "s)\n\n",
      sep = "")
  
  for (m in names(SESSION_DIRS)) {
    cat("[", SESSION_LABELS[[m]], "] ", store$status[[m]], "\n", sep = "")
    mf <- store$manifest[[m]]
    if (!is.null(mf)) cat("   files: ", paste(mf$file, collapse = ", "), "\n", sep = "")
  }
  
  # ---- live-AB charting check -------------------------------------------------
  # Results come from PitchCall / KorBB / PlayResult. If those are thin, the
  # Game Stats and Hitters tabs will be thin too — this says which session to
  # go back and finish charting.
  la <- store$liveab
  if (!is.null(la) && nrow(la) > 0) {
    la <- add_pa_outcomes(la)
    pa_all <- la[la$PAEnd, , drop = FALSE]
    chk <- pa_all %>%
      dplyr::group_by(Date, SessionKey) %>%
      dplyr::summarise(
        Pitches     = sum(la$SessionKey == SessionKey[1]),
        PAs         = dplyr::n(),
        Complete    = sum(PAResult != "Incomplete"),
        Incomplete  = sum(PAResult == "Incomplete"),
        `In play, no result` = sum(PAResult == "BIP"),
        K           = sum(PAResult == "K"),
        BB          = sum(PAResult == "BB"),
        H           = sum(PAResult %in% HIT_RESULTS),
        .groups = "drop") %>%
      dplyr::arrange(Date)
    
    # inferred = a K/BB on a PA whose end pitch has no K/BB written anywhere
    # (TrackMan KorBB or Yakkertech PlayResult)
    tagged_kb <- pa_all$KorBB %in% c("Strikeout", "Walk") |
      pa_all$PlayResult %in% c("Strikeout", "StrikeoutSwinging",
                               "StrikeoutLooking", "Walk", "IntentionalWalk")
    kb_der <- sum(pa_all$PAResult %in% c("K", "BB") & !tagged_kb)
    
    cat("\n---- Live-AB charting check ----\n")
    print(as.data.frame(chk), row.names = FALSE)
    if (kb_der > 0) {
      cat("\n", kb_der, " K/BB inferred from the count because KorBB was left ",
          "Undefined (DERIVE_K_BB_FROM_COUNT = TRUE; TrackMan files only).\n", sep = "")
    }
    if (sum(chk$`In play, no result`) > 0) {
      cat("Balls in play with no PlayResult count as an AB with no hit ",
          "(shown as BIP) — tag them to get the hits right.\n")
    }
    if (sum(chk$Incomplete) > 0) {
      cat("Incomplete PAs (cut off with no result) are not counted as PAs.\n")
    }
    
    checks <- live_ab_checks(la)
    labels <- c(
      retagged      = "Pitches re-tagged to the PA's pitcher (FIX_MIDPA_PITCHER)",
      odd_start     = "PAs that don't start 0-0 (count carried over?)",
      count_breaks  = "Count doesn't follow the previous pitch's call",
      contact_calls = "Ball / called / swinging strike WITH exit velo (a foul?)",
      yak_uncharted = "Yakkertech pitches with no PitchCall (not charted there)",
      yak_no_result = paste0("Yakkertech PAs with no result — usually a missing ",
                             "Walk/K, or the result landed on a pitch labelled ",
                             "with the wrong batter (see Next*)"))
    if (length(checks)) {
      cat("\n---- Charting flags (worth a look in TrackMan / Yakkertech) ----\n")
      for (nm in names(checks)) {
        cat("\n", labels[[nm]], ":\n", sep = "")
        print(checks[[nm]], row.names = FALSE)
      }
      cat("\nCount flags affect the count filter, FPS%, R2K% and Putaway%;",
          "PA results come from KorBB / PlayResult and are not affected.\n")
    } else {
      cat("\nNo charting flags.\n")
    }
  }
  cat("===================================================\n")
  
}, finally = setwd(.old_wd))