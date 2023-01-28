# References:
# https://www.kaggle.com/code/titericz/better-xgb-baseline
# https://www.kaggle.com/code/xxbxyae/auto-arima-model-baseline-with-r/notebook

library(tidyverse)
library(data.table)
library(xgboost)

# I train in RStudio and then enter nrounds for submission
# in last  xgb.train model.
submit <- TRUE
dotrain <- FALSE



clip_activity <- function(activity, lower_bound, upper_bound) {
  lower_bound + (activity - lower_bound > 0) * (activity - lower_bound) - (activity - upper_bound > 0) * (activity - upper_bound)
}


census <- read_csv("./data/census_starter.csv")
train <- read_csv("./data/processed/enriched_train.csv")
test <- read_csv("./data/test.csv")

counties <- distinct(train, cfips, county, state, census_region)

train <- train %>% 
  mutate(
    date = lubridate::ymd(first_day_of_month),
    istest = 0
  ) %>%
  select(row_id, cfips, county, state, date, activity, active)

test <- test %>%
  left_join(counties, by = 'cfips') %>%
  transmute(row_id, cfips, county, state, date = lubridate::ymd(first_day_of_month)) %>%
  mutate(istest = 1)

df <- train %>%
  as_tibble() %>%
  transmute(row_id = paste(cfips, date, sep = "_"), cfips, county, state, date, activity, active, istest = 0) %>%
  bind_rows(test) %>%
  arrange(cfips, date) %>%
  mutate(
    time = ave(cfips == cfips, cfips, FUN = cumsum),
    month = lubridate::month(date),
    state_dummy = as.numeric(factor(state))
  )

df <- df %>%
  # Create new columns for the lag of activity and the difference between activity and the lag
  mutate(
    # create mbd_lag_1 column with the previous value of activity
    mbd_lag_1 = lag(activity),
    # create dif column with the difference calculation
    dif = case_when(
      #if activity and the previous value are both 0, the difference is 0
      activity == 0 & mbd_lag_1 == 0 ~ 0,
      #if activity is greater than 0 and the previous value is 0, the difference is 1
      activity > 0 & mbd_lag_1 == 0 ~ 1,
      #otherwise, the difference is the absolute value of the change in activity
      TRUE ~ abs(activity / mbd_lag_1 - 1)
    )
  )

train_size <- 39

df <-
  df %>%
  group_by(cfips) %>%
  mutate(
    var = activity,
    #' the threshold value derivation makes no sense
    thr = 0.15 * mean(var[1:(train_size - 1)], na.rm = TRUE),
    abs_diff = abs(var - lag(var)),
    #' neither does this absolute difference
    var = case_when(
      abs_diff >= thr & lag(var) != 0 ~ var * (var / lag(var)),
      TRUE ~ var
    )
  ) %>%
  ungroup() %>%
  mutate(activity = var, var = var[2] * 0.99, var = lead(var)) %>%
  select(-var)
#' what is the reason for this "blacklist"?
blacklist <-
  c(
    'North Dakota',
    'Iowa',
    'Kansas',
    'Nebraska',
    'South Dakota',
    'New Mexico',
    'Alaska',
    'Vermont'
  )
blacklistcfips <- c(
  1019,1027,1029,1035,1039,1045,1049,1057,1067,1071,1077,1085,1091,1099,1101,1123,1131,1133,
  4001,4012,4013,4021,4023,5001,5003,5005,5017,5019,5027,5031,5035,5047,5063,5065,5071,5081,
  5083,5087,5091,5093,5107,5109,5115,5121,5137,5139,5141,5147,6003,6015,6027,6033,6053,6055,
  6057,6071,6093,6097,6103,6105,6115,8003,8007,8009,8019,8021,8023,8047,8051,8053,8055,8057,
  8059,8061,8065,8067,8069,8071,8073,8075,8085,8091,8093,8097,8099,8103,8105,8107,8109,8111,
  8115,8117,8121,9007,9009,9015,12009,12017,12019,12029,12047,12055,12065,12075,12093,12107,
  12127,13005,13007,13015,13017,13019,13027,13035,13047,13065,13081,13083,13099,13107,13109,
  13117,13119,13121,13123,13125,13127,13135,13143,13147,13161,13165,13171,13175,13181,13193,
  13201,13221,13225,13229,13231,13233,13245,13247,13249,13257,13279,13281,13287,13289,13293,
  13301,13319,15001,15005,15007,16001,16003,16005,16007,16013,16015,16017,16023,16025,16029,
  16031,16033,16035,16037,16043,16045,16049,16061,16063,16067,17001,17003,17007,17009,17013,
  17015,17023,17025,17031,17035,17045,17051,17059,17061,17063,17065,17067,17069,17075,17077,
  17081,17085,17087,17103,17105,17107,17109,17115,17117,17123,17127,17133,17137,17141,17143,
  17147,17153,17167,17169,17171,17177,17179,17181,17185,17187,17193,18001,18007,18009,18013,
  18015,18019,18021,18025,18035,18037,18039,18041,18053,18061,18075,18079,18083,18087,18099,
  18103,18111,18113,18115,18137,18139,18145,18153,18171,18179,21001,21003,21013,21017,21023,
  21029,21035,21037,21039,21045,21047,21055,21059,21065,21075,21077,21085,21091,21093,21097,
  21099,21101,21103,21115,21125,21137,21139,21141,21149,21155,21157,21161,21165,21179,21183,
  21191,21197,21199,21215,21217,21223,21227,21237,21239,22019,22021,22031,22039,22041,22047,
  22069,22085,22089,22101,22103,22109,22111,22115,22119,22121,23003,23009,23021,23027,23029,
  24011,24027,24029,24031,24035,24037,24039,24041,25011,25015,26003,26007,26011,26019,26021,
  26025,26027,26033,26037,26041,26043,26051,26053,26057,26059,26061,26065,26071,26077,26079,
  26083,26089,26097,26101,26103,26109,26111,26115,26117,26119,26127,26129,26131,26135,26141,
  26143,26155,26161,26165,27005,27011,27013,27015,27017,27021,27023,27025,27029,27047,27051,
  27055,27057,27065,27069,27073,27075,27077,27079,27087,27091,27095,27101,27103,27105,27107,
  27109,27113,27117,27119,27123,27125,27129,27131,27133,27135,27141,27147,27149,27155,27159,
  27167,27169,28017,28019,28023,28025,28035,28045,28049,28061,28063,28093,28097,28099,28125,
  28137,28139,28147,28159,29001,29015,29019,29031,29033,29041,29049,29051,29055,29057,29063,
  29065,29069,29075,29085,29089,29101,29103,29111,29121,29123,29125,29135,29137,29139,29143,
  29157,29159,29161,29167,29171,29173,29175,29177,29183,29195,29197,29199,29203,29205,29207,
  29209,29213,29215,29217,29223,29227,29229,30005,30009,30025,30027,30033,30035,30037,30039,
  30045,30049,30051,30053,30055,30057,30059,30069,30071,30073,30077,30079,30083,30085,30089,
  30091,30093,30101,30103,30105,30107,30109,32005,32009,32017,32023,32027,32029,32510,33005,
  33007,34021,34027,34033,34035,36011,36017,36023,36033,36043,36047,36049,36051,36057,36061,
  36067,36083,36091,36097,36103,36107,36113,36115,36121,36123,37005,37009,37011,37017,37023,
  37029,37031,37049,37061,37075,37095,37117,37123,37131,37137,37151,37187,37189,37197,39005,
  39009,39015,39017,39019,39023,39037,39039,39043,39049,39053,39057,39063,39067,39071,39077,
  39085,39087,39091,39097,39105,39107,39113,39117,39119,39125,39127,39129,39135,39137,39151,
  39153,39157,40003,40013,40015,40023,40025,40027,40035,40039,40043,40045,40053,40055,40057,
  40059,40065,40067,40073,40077,40079,40099,40105,40107,40111,40115,40123,40127,40129,40133,
  40141,40147,40151,40153,41001,41007,41013,41015,41017,41021,41025,41031,41033,41037,41051,
  41055,41063,41067,41069,42005,42007,42011,42013,42015,42019,42027,42029,42031,42035,42053,
  42057,42067,42071,42083,42085,42093,42097,42105,42111,42113,42115,42123,42125,42127,42129,
  44005,44007,44009,45001,45009,45021,45025,45031,45059,45067,45071,45073,45089,47001,47005,
  47013,47015,47019,47021,47023,47027,47035,47039,47041,47047,47055,47057,47059,47061,47069,
  47073,47075,47077,47083,47087,47099,47105,47121,47127,47131,47133,47135,47137,47147,47151,
  47153,47159,47161,47163,47169,47177,47183,47185,48001,48011,48017,48019,48045,48057,48059,
  48063,48065,48073,48077,48079,48081,48083,48087,48095,48101,48103,48107,48109,48115,48117,
  48119,48123,48125,48129,48149,48151,48153,48155,48159,48161,48165,48175,48189,48191,48195,
  48197,48211,48221,48229,48233,48235,48237,48239,48241,48243,48245,48255,48261,48263,48265,
  48267,48269,48275,48277,48283,48293,48299,48305,48311,48313,48319,48321,48323,48327,48333,
  48345,48347,48355,48369,48377,48379,48383,48387,48389,48401,48403,48413,48417,48431,48433,
  48437,48443,48447,48453,48455,48457,48461,48463,48465,48469,48471,48481,48483,48485,48487,
  48495,48499,49001,49009,49013,49019,49027,49031,49045,51005,51017,51025,51029,51031,51036,
  51037,51043,51057,51059,51065,51071,51073,51077,51079,51083,51091,51095,51097,51101,51111,
  51115,51119,51121,51127,51135,51147,51155,51159,51165,51167,51171,51173,51181,51183,51191,
  51197,51530,51590,51610,51620,51670,51678,51720,51735,51750,51770,51810,51820,53013,53019,
  53023,53031,53033,53037,53039,53041,53047,53065,53069,53071,53075,54013,54019,54025,54031,
  54033,54041,54049,54055,54057,54063,54067,54071,54077,54079,54085,54089,54103,55001,55003,
  55005,55007,55011,55017,55021,55025,55029,55037,55043,55047,55049,55051,55061,55065,55067,
  55075,55077,55091,55097,55101,55103,55109,55117,55123,55125,55127,56007,56009,56011,56015,
  56017,56019,56021,56027,56031,56037,56043,56045,12061,  6095, 49025, 18073, 29029, 29097,
  48419, 51830, 30067, 26095, 18159, 32001, 54065, 54027, 13043, 48177, 55069, 48137, 30087, 
  29007, 13055, 48295, 28157, 29037, 45061, 22053, 13199, 47171, 53001, 55041, 51195, 18127, 
  29151, 48307, 51009, 16047, 29133,  5145, 17175, 21027, 48357, 29179, 13023, 16077, 48371,
  21057, 16039, 21143, 48435, 48317, 48475,  5129, 36041, 48075, 29017, 47175, 39167, 47109,
  17189, 17173, 28009, 39027, 48133, 18129, 48217, 40081, 36021,  6005, 42099, 18051, 36055,
  53051, 6109, 21073, 27019,  6051, 48055,  8083, 48503, 17021, 10003, 41061, 22001, 22011,
  21205, 48223, 51103, 51047, 16069, 17033, 41011,  6035, 47145, 27083, 18165, 36055, 12001,
  26159,  8125, 34017,28141, 55119, 48405, 40029, 18125, 21135, 29073, 55115, 37149,55039,
  26029, 12099, 13251, 48421, 39007, 41043, 22015, 37115,54099, 51137, 22049, 55131, 17159,
  56001, 40005, 18017, 28091,47101, 27037, 29005, 13239, 21019, 55085, 48253, 51139, 40101,
  13283, 18049, 39163, 45049, 51113)

df <- df %>%
  group_by(cfips) %>%
  mutate(
    target = (lead(activity) / activity) - 1
  ) %>%
  mutate(target = ifelse(cfips %in% c(28055, 48269), 0, target)) %>%
  ungroup()


df <- df %>% group_by(cfips) %>%
  mutate(lastactive = active[39]) %>%
  ungroup()

df <- df %>% group_by(cfips) %>%
  mutate(lasttarget = activity[39]) %>%
  ungroup()

df <- df %>% group_by(cfips) %>%
  mutate(
    mbd_lag_1 = lag(target),
    mbd_lag_2 = lag(target, n = 2L),
    mbd_lag_3 = lag(target, n = 3L),
    act_lag_1 = active - lag(active),
    act_lag_2 = active - lag(active, n = 2L),
    act_lag_3 = active - lag(active, n = 3L),
    mbd_rollmea2 = data.table::frollsum(mbd_lag_1, 2),
    mbd_rollmea4 = data.table::frollsum(mbd_lag_1, 4),
    mbd_rollmea12 = data.table::frollsum(mbd_lag_1, 12),
    mbd_rollmea6 = data.table::frollsum(mbd_lag_1, 6)
  )

features <-
  c(
    "state_dummy",
    "mbd_lag_1",
    "mbd_lag_2",
    "mbd_lag_3",
    "act_lag_1",
    "act_lag_2",
    "act_lag_3" ,
    "mbd_rollmea12",
    "mbd_rollmea2",
    "mbd_rollmea4",
    "mbd_rollmea6"
  )  

#' initialize some values for the training phase
active_threshold = 1.8
absolute_threshold = 1.00
time_series_length <- 37
all_scores <- numeric()
all_rounds <- numeric()
sxgb <- numeric()
slast <- numeric()
i <- 0

df <- df %>%
  mutate(ypred_last = NA,
         ypred = NA,
         k = 1.0)

# Define the modeling pipeline
xgb_control <- trainControl(method = "repeatedcv", number = 5, repeats = 5, savePredictions = TRUE)

best_parameters <- list()

# Iterate through each time series index
for (t in 31:37) {
  # Filter dataframe for train and validation indices
  train_indices <- df %>%
    filter((istest == 0) &
             (time < t) &
             (time >= 2) &
             (lastactive > active_threshold)  &
             (lasttarget > absolute_threshold)
    )
  valid_indices <- df %>% filter((istest == 0) & (time == t))
  
  # Clipping the target variable
  df$target[train_indices] <- clip_activity(df$target[train_indices], -0.0043, 0.0045)
  
  # Prepare the train and test data for xgb model
  dtrain <- xgb.DMatrix(data = as.matrix(df[train_indices, features]), label = as.matrix(df[train_indices, "target"]))
  dtest <- xgb.DMatrix(data = as.matrix(df[valid_indices, features]), label = as.matrix(df[valid_indices, "target"]))
  watchlist <- list(train = dtrain, test = dtest)
  
  # Initialize a variable to store the best parameters for the current time index
  best_params <- NULL
  best_rmse <- Inf
  
  # Iterate through the hyperparameter grid
  for (i in 1:nrow(hyper_grid)) {
    # Extract the current set of hyperparameters
    params <- hyper_grid[i,]
    
    # Tune the model using the current set of hyperparameters
    xgb_tune <- xgb.train(
      data = dtrain,
      eval_metric = 'mae',
      tree_method = "hist",
      nrounds = 4999,
      params,
      watchlist = watchlist,
      early_stopping_rounds = 70,
      objective = "reg:pseudohubererror",
      nthread = 2,
      verbose = 1000
    )
    
    # Extract the RMSE of the model
    rmse <- xgb_tune$evaluation_log$test_rmse_mean
    
    # Update the best parameters if the current RMSE is lower than the best RMSE
    if (rmse < best_rmse) {
      best_params_t <- params
      best_rmse <- rmse
    }
  }
  # store the best parameters for the current time index
  best_parameters[[t]] <- best_params_t
}


#' refitting to full data
active_threshold <- 1.8   
absolute_threshold <- 1.00
time_series_length <- 39

train_indices <-
  which((df$istest == 0) &
          (df$time  < time_series_length) &
          (df$time >= 2) &
          (df$lastactive > active_threshold)  &
          (df$lasttarget > absolute_threshold)
  )

valid_indices <- which((df$time == time_series_length))

dtrain <- xgb.DMatrix(data = as.matrix(df[train_indices, features]), label = as.matrix(df[train_indices, "target"]))

hyper_grid <- expand.grid(
  eta = 0.01,
  max_depth = 3,
  min_child_weight = 3,
  subsample = 0.5,
  colsample_bytree = 0.5,
  gamma = c(0, 0.5, 1),
  lambda = c(0, 1e-2, 0.1, 1),
  alpha = c(0, 1e-2, 0.1, 1)
)


# Perform the grid search
for (i in seq(nrow(hyper_grid))) {
  set.seed(123)
  print(paste("Running iteration", i, "of", nrow(hyper_grid)))
  m <- xgb.cv(
    data = dtrain,
    nrounds = 4000,
    objective = "reg:pseudohubererror",
    early_stopping_rounds = 50,
    nfold = 10,
    verbose = 0,
    params = list(
      eta = hyper_grid$eta[i],
      max_depth = hyper_grid$max_depth[i],
      min_child_weight = hyper_grid$min_child_weight[i],
      subsample = hyper_grid$subsample[i],
      colsample_bytree = hyper_grid$colsample_bytree[i],
      gamma = hyper_grid$gamma[i],
      lambda = hyper_grid$lambda[i],
      alpha = hyper_grid$alpha[i]
    )
  )
  hyper_grid$rmse[i] <- min(m$evaluation_log$test_rmse_mean)
  hyper_grid$trees[i] <- m$best_iteration
  print(paste("Iteration", i, "completed"))
}

# Print the results
hyper_grid %>%
  filter(rmse > 0) %>%
  arrange(rmse) %>%
  glimpse()

# optimal parameter list
params <- list(
  eta = 0.01,
  max_depth = 3,
  gamma = 0.5,
  lambda = 0.01,
  alpha = 0.1,
  min_child_weight = 3,
  subsample = 0.5,
  max_leaves = 17,
  colsample_bytree = 0.5
)

# train final model
xgb.fit.final <- xgboost(
  params = params,
  data = dtrain,
  objective = "reg:pseudohubererror",
  nrounds = 5000,
  early_stopping_rounds = 75,
  verbose = 0
)

model <- xgb.train(
  data = dtrain,
  eval_metric = 'mae',
  tree_method = "hist",
  nrounds = 1111,
  max_depth = 3,
  learning_rate = 0.0075,
  nthread = 2,
  objective = "reg:pseudohubererror",
  max_leaves = 17,
  subsample = 0.50,
  colsample_bytree = 0.50,
  max_bin = 4096,
  n_jobs = 2,
  verbose = 100
)

ypred <- predict(model, as.matrix(df[valid_indices, features]))
ypred <- predict(xgb.fit.final, as.matrix(df[valid_indices, features]))

df$k[valid_indices] <- ypred + 1
df$k[valid_indices] <- df$k[valid_indices] * df$activity[valid_indices]

df <- df %>% group_by(cfips) %>%
  mutate(lastval = activity[TS]) %>%
  ungroup()

dt <- df %>% group_by(cfips) %>%
  select(cfips, time, k) %>%
  filter(time == (time_series_length)) %>%
  ungroup()

yhat <- df %>% group_by(cfips) %>%
  select(cfips,
         state,
         time,
         lastactive,
         lastval) %>%
  filter(time == (TS + 1)) %>%
  ungroup()

yhat <- yhat %>%
  mutate(pred = if_else(lastactive <= active_threshold, lastval, dt$k),
         pred = if_else(lastval <= absolute_threshold, lastval, pred),
         pred = if_else(state %in% blacklist, lastval, pred),
         pred = if_else(cfips %in% blacklistcfips, lastval, pred))

df <- df %>%
  mutate(activity = case_when(cfips == 28055 ~ 0,
                                           cfips == 48269 ~ 1.762115,
                                           TRUE ~ as.numeric(activity)))

fct_values <- df %>% group_by(cfips) %>%
  filter(istest == 1) %>%
  select(row_id, cfips, time) %>%
  ungroup()

xgboost_fcts <- left_join(fct_values, yhat, by = c("cfips", "time")) %>%
  mutate(microbusiness_density = pred) %>%
  select(row_id, microbusiness_density)

a <- 1.0
xgboost_fcts <- xgboost_fcts %>%
  mutate(microbusiness_density = if_else(
    is.na(microbusiness_density),
    a,
    as.numeric(microbusiness_density)
  ))

save_data(xgboost_fcts, path = "output")
