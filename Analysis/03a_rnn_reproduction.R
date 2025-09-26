# ==============================================================================
# 03a_rnn_reproduction.R
# Reproduce original RNN on size-standardised (450 mm) backscatter features.
# - Builds 5-ping sequences by Region from backscatter splits
# - Fits LSTM -> dense stack (same as workshop code)
# - Saves model, history, metrics, ROC PNG, confusion matrix
# ==============================================================================

rm(list = ls())
invisible(gc())

# ---- 0. Folders ---------------------------------------------------------------
dir.create("outputs", showWarnings = FALSE)
dir.create("outputs/tables", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs/models", showWarnings = FALSE, recursive = TRUE)
dir.create("figures", showWarnings = FALSE)

# ---- 1. Helpers: packages & here() -------------------------------------------
ensure_packages <- function(pkgs) {
  missing <- pkgs[!pkgs %in% rownames(installed.packages())]
  if (length(missing)) install.packages(missing, quiet = TRUE)
  invisible(lapply(pkgs, require, character.only = TRUE))
}

ensure_packages(c(
  "tidyverse","here","tidymodels","purrr","ROCR","jsonlite","lubridate"
))

# keras/tensorflow can’t be auto-installed reliably in a script; check & guide
if (!requireNamespace("keras", quietly = TRUE) ||
    !requireNamespace("tensorflow", quietly = TRUE)) {
  stop(
    "\nKeras/TensorFlow not available in this R environment.\n",
    "Install once inside the project (renv):\n",
    "  install.packages(c('keras','tensorflow'))\n",
    "  library(tensorflow); tensorflow::install()\n"
  )
}
library(keras)

library(here)
set.seed(15)
tensorflow::set_random_seed(15)

# ---- 2. Ensure backscatter splits exist --------------------------------------
builder <- here("Analysis","02a_check_transformations.R")
train_rds    <- here("outputs","tables","train_backscatter_450.rds")
validate_rds <- here("outputs","tables","validate_backscatter_450.rds")
test_rds     <- here("outputs","tables","test_backscatter_450.rds")

if (!file.exists(train_rds) || !file.exists(validate_rds) || !file.exists(test_rds)) {
  message("Backscatter files missing — running builder: ", builder)
  source(builder, local = TRUE)
  # Re-check
  if (!file.exists(train_rds) || !file.exists(validate_rds) || !file.exists(test_rds)) {
    stop("Expected backscatter files not created by ", basename(builder))
  }
}

# ---- 3. Load data & detect frequency columns ---------------------------------
train_bs    <- readRDS(train_rds)
validate_bs <- readRDS(validate_rds)
test_bs     <- readRDS(test_rds)

freq_cols <- names(train_bs)[stringr::str_detect(names(train_bs), "^F\\d+(?:\\.\\d+)?$")]
stopifnot(length(freq_cols) > 0)
stopifnot(all(c("Region","species") %in% names(train_bs)))

# ---- 4. Sequence builder (5 pings per segment) -------------------------------
# Split within each Region into consecutive blocks of 5; keep exact-5 blocks.
mk_blocks5 <- function(df) {
  df |>
    group_by(Region) |>
    mutate(.grp = rep(seq_len(ceiling(n()/5)), each = 5, length.out = n())) |>
    ungroup() |>
    group_split(Region, .grp) |>
    keep(~ nrow(.x) == 5)
}

# Convert a list of 5xP tibbles to array [N, 5, P]
list_to_array <- function(lst, p_cols) {
  mats <- lapply(lst, function(x) as.matrix(dplyr::select(x, all_of(p_cols))))
  if (!length(mats)) stop("No 5-ping sequences could be formed. Check data.")
  P <- ncol(mats[[1]]); Tt <- nrow(mats[[1]])
  N <- length(mats)
  arr <- array(NA_real_, dim = c(N, Tt, P))
  for (i in seq_len(N)) arr[i,,] <- mats[[i]]
  arr
}

# Build sequences + labels
build_xy <- function(df) {
  blocks <- mk_blocks5(df)
  x_arr  <- list_to_array(blocks, freq_cols)
  
  # one label per block = species of the first row in block
  y_lab <- purrr::map_chr(blocks, ~ as.character(.x$species[1]))
  list(x = x_arr, y = y_lab)
}

xy_train    <- build_xy(train_bs)
xy_validate <- build_xy(validate_bs)
xy_test     <- build_xy(test_bs)

# ---- 5. Encode labels (LT=0, SMB=1) ------------------------------------------
y_to_int <- function(y) { ifelse(y == "SMB", 1L, 0L) }  # keep workshop mapping
y_train_i    <- y_to_int(xy_train$y)
y_validate_i <- y_to_int(xy_validate$y)
y_test_i     <- y_to_int(xy_test$y)

dummy_y_train    <- keras::to_categorical(y_train_i, num_classes = 2L)
dummy_y_validate <- keras::to_categorical(y_validate_i, num_classes = 2L)
dummy_y_test     <- keras::to_categorical(y_test_i, num_classes = 2L)

# ---- 6. Model: mirror the workshop architecture ------------------------------
input_shape <- c(dim(xy_train$x)[2], dim(xy_train$x)[3])  # (5, 249)

rnn <- keras_model_sequential() |>
  layer_lstm(input_shape = input_shape, units = input_shape[2]) |>
  layer_activation_leaky_relu() |>
  layer_batch_normalization() |>
  layer_dense(units = 150, activity_regularizer = regularizer_l2(1e-4)) |>
  layer_activation_leaky_relu() |>
  layer_dense(units = 75,  activity_regularizer = regularizer_l2(1e-4)) |>
  layer_activation_leaky_relu() |>
  layer_dense(units = 38,  activity_regularizer = regularizer_l2(1e-4)) |>
  layer_activation_leaky_relu() |>
  layer_dense(units = 19,  activity_regularizer = regularizer_l2(1e-4)) |>
  layer_activation_leaky_relu() |>
  layer_dense(units = 2, activation = "sigmoid")  # keep workshop choice

rnn |> compile(
  loss = loss_binary_crossentropy,
  optimizer = optimizer_adam(3e-4),
  metrics = c("accuracy")
)

# ---- 7. Train ----------------------------------------------------------------
history <- rnn |> fit(
  x = xy_train$x, y = dummy_y_train,
  batch_size = 500,
  epochs = 38,
  validation_data = list(xy_validate$x, dummy_y_validate),
  class_weight = list("0" = 1, "1" = 2)   # match original
)

# ---- 8. Evaluate on test ------------------------------------------------------
eval_test <- rnn |> evaluate(xy_test$x, dummy_y_test, verbose = 0)
preds     <- rnn |> predict(xy_test$x)
pred_cls  <- apply(preds, 1, which.max)
pred_fac  <- factor(ifelse(pred_cls == 1, "LT", "SMB"), levels = c("LT","SMB"))
true_fac  <- factor(ifelse(y_test_i == 0, "LT", "SMB"), levels = c("LT","SMB"))

# Confusion matrix
cm <- yardstick::conf_mat(
  tibble(truth = true_fac, estimate = pred_fac), truth, estimate
)

# ROC (use prob for class SMB = column 2)
pred_obj <- ROCR::prediction(preds[,2], y_test_i)
perf_roc <- ROCR::performance(pred_obj, "tpr", "fpr")

roc_df <- tibble(
  fpr = perf_roc@x.values[[1]],
  tpr = perf_roc@y.values[[1]],
  thr = perf_roc@alpha.values[[1]]
)

ts_tag <- format(lubridate::with_tz(Sys.time(), "Australia/Melbourne"), "%Y%m%d_%H%M%S")
roc_path <- here("figures", paste0("roc_rnn_", ts_tag, ".png"))

roc_plot <- ggplot(roc_df, aes(fpr, tpr)) +
  geom_abline(slope = 1, intercept = 0, colour = "grey60") +
  geom_path() +
  coord_equal() +
  labs(title = "RNN ROC (SMB positive)",
       x = "False positive rate", y = "True positive rate") +
  theme_classic()

ggsave(roc_path, roc_plot, width = 6.5, height = 5, dpi = 160)

# ---- 9. Save artifacts --------------------------------------------------------
model_path   <- here("outputs","models", paste0("rnn_model_", ts_tag))
history_path <- here("outputs","tables", paste0("rnn_history_", ts_tag, ".rds"))
metrics_path <- here("outputs","tables", paste0("rnn_metrics_", ts_tag, ".json"))
cm_path      <- here("outputs","tables", paste0("rnn_confusion_", ts_tag, ".csv"))

# ---- 9. Save artifacts (robust on Windows/OneDrive) --------------------------
models_dir  <- here("outputs", "models")
dir.create(models_dir, showWarnings = FALSE, recursive = TRUE)

# Use a *short* subfolder/file name to avoid MAX_PATH & OneDrive issues
short_tag   <- format(lubridate::with_tz(Sys.time(), "Australia/Melbourne"), "%y%m%d_%H%M%S")
model_dir_tf <- file.path(models_dir, paste0("rnn_", short_tag))   # for SavedModel (folder)
model_h5     <- file.path(models_dir, paste0("rnn_", short_tag, ".h5"))  # single-file fallback
model_keras  <- file.path(models_dir, paste0("rnn_", short_tag, ".keras")) # single-file (Keras v3)

history_path <- here("outputs","tables", paste0("rnn_history_", short_tag, ".rds"))
metrics_path <- here("outputs","tables", paste0("rnn_metrics_", short_tag, ".json"))
cm_path      <- here("outputs","tables", paste0("rnn_confusion_", short_tag, ".csv"))

# Try SavedModel first; if it fails, fall back to single-file formats
saved_as <- NULL
ok <- TRUE
tryCatch({
  keras::save_model_tf(rnn, model_dir_tf)
  saved_as <- paste0("SavedModel folder: ", model_dir_tf)
}, error = function(e) {
  ok <<- FALSE
})

if (!ok) {
  ok <- TRUE
  tryCatch({
    keras::save_model(rnn, model_keras)   # Keras v3 single-file format
    saved_as <- paste0("Keras .keras: ", model_keras)
  }, error = function(e) {
    ok <<- FALSE
  })
}

if (!ok) {
  # Last resort: HDF5
  keras::save_model_hdf5(rnn, model_h5)
  saved_as <- paste0("HDF5 .h5: ", model_h5)
}

saveRDS(history, history_path)

metrics_list <- list(
  test_loss = unname(eval_test["loss"]),
  test_accuracy = unname(eval_test["accuracy"]),
  n_train_seq = dim(xy_train$x)[1],
  n_valid_seq = dim(xy_validate$x)[1],
  n_test_seq  = dim(xy_test$x)[1],
  input_shape = input_shape,
  roc_png     = roc_path,
  saved_as    = saved_as
)
jsonlite::write_json(metrics_list, metrics_path, pretty = TRUE, auto_unbox = TRUE)
cm_df <- cm$table %>% as.matrix() %>% as.data.frame()
cm_df <- tibble::rownames_to_column(cm_df, var = "truth")
names(cm_df)[-1] <- paste0("pred_", levels(true_fac))
readr::write_csv(cm_df, cm_path)


message("\n==== RNN reproduction complete ====")
message("Saved:   ", saved_as)
message("History: ", history_path)
message("Metrics: ", metrics_path)
message("CM CSV:  ", cm_path)
message("ROC:     ", roc_path, "\n")

