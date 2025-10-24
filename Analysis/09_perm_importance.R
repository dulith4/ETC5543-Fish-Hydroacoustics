# analysis/09_perm_importance.R
suppressPackageStartupMessages({
  library(here); library(readr); library(dplyr); library(h2o)
})

# Start/reuse H2O quietly
h2o.no_progress()
if (is.null(tryCatch(h2o.getConnection(), error = function(e) NULL))) {
  h2o.init(nthreads = -1, max_mem_size = "6G")
}

# Find latest MOJO from your tsfeatures run (adjust the pattern if needed)
mojo_paths <- list.files(here("outputs","models"),
                         pattern = "model\\.mojo\\.zip$", recursive = TRUE, full.names = TRUE)
mojo_paths <- mojo_paths[grepl("tsf_q_all|tsf_quint_all", mojo_paths)]
stopifnot(length(mojo_paths) > 0)
model <- h2o.import_mojo(mojo_paths[order(file.info(mojo_paths)$mtime, decreasing = TRUE)][1])

# Load QUINTILES_ALLFREQ data
qa <- readRDS(here("outputs","tables","fish_quintiles_allfreq_tsfeat.rds"))

# Recreate the grouped split (seed = 73)
split_by_fish_strat <- function(df, p_train=.6, p_valid=.2, seed=73){
  set.seed(seed)
  ids <- dplyr::distinct(df, fishNum, species)
  split_one <- function(di){
    n <- nrow(di); idx <- sample.int(n)
    n_tr <- max(1, floor(p_train*n)); n_va <- max(1, floor(p_valid*n)); n_te <- max(1, n-n_tr-n_va)
    while (n_tr + n_va + n_te > n) {
      if (n_tr > 1) n_tr <- n_tr-1 else if (n_va > 1) n_va <- n_va-1 else n_te <- n_te-1
    }
    tibble(fishNum = di$fishNum[idx],
           split   = c(rep("train", n_tr), rep("valid", n_va), rep("test", n - n_tr - n_va)))
  }
  dplyr::bind_rows(lapply(dplyr::group_split(ids, species), split_one)) |>
    dplyr::right_join(df, by = "fishNum")
}

sp <- split_by_fish_strat(qa, seed = 73)
x_cols <- setdiff(names(qa), intersect(names(qa), c("species","fishNum","quantile")))
test_df <- dplyr::select(dplyr::filter(sp, split == "test"), dplyr::all_of(c("species", x_cols)))

hex_te <- as.h2o(test_df)
hex_te[,"species"] <- h2o.asfactor(hex_te[,"species"])

# Permutation importance over frequency columns only
freq_cols <- grep("^F\\d", x_cols, value = TRUE)

metric_auc <- function(mod, frame) {
  perf <- h2o.performance(mod, frame)
  as.numeric(h2o.auc(perf))
}
auc_base <- metric_auc(model, hex_te)

shuffle_col <- function(fr, col) {
  v <- as.data.frame(fr[, col])[[1]]
  h <- as.h2o(sample(v)); colnames(h) <- col; h
}

perm_drop <- function(col) {
  others <- setdiff(colnames(hex_te), col)
  tmp <- h2o.cbind(hex_te[, others], shuffle_col(hex_te, col))
  auc_base - metric_auc(model, tmp)
}

imp <- tibble(
  variable = freq_cols,
  auc_drop = vapply(freq_cols, perm_drop, numeric(1))
) |> arrange(desc(auc_drop))

dir.create(here("outputs","tables"), recursive = TRUE, showWarnings = FALSE)
write_csv(imp, here("outputs","tables","varimp_quintiles_allfreq_permutation.csv"))
