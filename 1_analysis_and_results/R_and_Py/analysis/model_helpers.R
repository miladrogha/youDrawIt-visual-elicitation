# if model results exist, only re-run if there's been a 
# change in the data or specification
brms.file_refit = 'on_change'

model_results_dir = function(model_id) {
  model_dir = sprintf('%s/%s', MOD_PATH, model_id)
  
  # create directory if it doesn't exist
  if (!dir.exists(model_dir)) {
    dir.create(model_dir)
  }
  return(model_dir)
}

# path to fitted model
model_fit_path = function(model_id) {
  model_dir = model_results_dir(model_id)
  return(sprintf('%s/%s_fit.rds', model_dir, model_id))
}