# Arbol elemental con libreria  rpart
# Debe tener instaladas las librerias  data.table  ,  rpart  y  rpart.plot

# cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")
require("parallel")

# Aqui se debe poner la carpeta de la computadora local
setwd("C:\\Users\\blank\\Projects") # Establezco el Working Directory

semilla <- 42
set.seed(semilla)

# cargo los datos
dataset <- fread(".\\dmeyf2023-main\\competencia_01.csv")
dataset_test <- fread(".\\dmeyf2023-main\\competencia_01_crudo.csv")

dtrain <- dataset[foto_mes == 202103] # defino donde voy a entrenar
dapply <- dataset_test[foto_mes == 202105] # defino donde voy a aplicar el modelo

dtrain <- dtrain %>% mutate(clase_ternaria = case_when(clase_ternaria == "BAJA+1" ~ "CONTINUA",
                                                       clase_ternaria == "BAJA+2" ~ "BAJA+2",
                                                       clase_ternaria == "CONTINUA" ~ "CONTINUA"))


pesos <- copy( dtrain[, ifelse( clase_ternaria=="CONTINUA",   1.0, 100.0  ) ] )

param_grid <- expand.grid(
  cp = seq(0, 0.0001, by = 0.001),
  minsplit = seq(750, 850, by = 100),
  minbucket = seq(400, 450, by = 50),
  maxdepth = seq(15, 19, by = 1),
  level = 0
)

#param_grid <- param_grid[param_grid$minsplit /  param_grid$minbucket == 2, ]

# Define the number of folds (e.g., 5-fold cross-validation)
num_folds <- 5

best_row <- 0 #no model
best_ganancia <- 0 #if no treatment

for (i in 1:nrow(param_grid)) {
  
  # Get hyperparameters set
  row_data <- param_grid[i, ]  
  
  # Create a cross-validation object using createFolds
  cv_folds <- createFolds(dtrain$clase_ternaria, k = num_folds, list = TRUE, returnTrain = FALSE)
  
  # Perform cross-validation using your modeling algorithm
  
  avg_ganancia <- rep(0, 200)
  for (fold in 1:num_folds) {
    
    train_indices <- unlist(cv_folds[-fold])
    test_indices <- unlist(cv_folds[fold])
    
    # Split the data into training and testing sets
    train_data <- dtrain[train_indices, ]
    test_data <- dtrain[test_indices, ]
    
    # Replace this with your modeling algorithm (e.g., random forest)
    model <- rpart(
      formula = "clase_ternaria ~ . -numero_de_cliente",
      weights = pesos[train_indices],
      data = train_data, # los datos donde voy a entrenar
      xval = 0,
      cp = row_data$cp, # esto significa no limitar la complejidad de los splits
      minsplit = row_data$minsplit, # minima cantidad de registros para que se haga el split
      minbucket = row_data$minbucket, # tamaño minimo de una hoja
      maxdepth = row_data$maxdepth
    )  
    
    # Make predictions on the test set
    predictions <- predict(model, newdata = test_data)
    
    #calculate ganancia for all prob thresholds
    for (cutoff in 1:200)
      {
      ganancia <- test_data[, sum(ifelse(predictions[, "BAJA+2"] > cutoff/200,
                                         ifelse(clase_ternaria == "BAJA+2", 273000, -7000),
                                         0))]
      avg_ganancia[cutoff] <- avg_ganancia[cutoff] + ganancia #expected if scaled to original dataset size
      }
      
  }
  max_index <- which.max(avg_ganancia)
  param_grid[i,"level"] <- max_index/200
  # Найдите максимальное значение
  max_value <- avg_ganancia[max_index]
  
  
  cat("with params cp", param_grid[i,"cp"],
      "minsplit", param_grid[i,"minsplit"],
      "minbucket", param_grid[i,"minbucket"],
      "maxdepth", param_grid[i,"maxdepth"],
      "level", param_grid[i,"level"],
      "avg ganancia is ", sprintf("%.2f", max_value), "\n")
  if (max_value > best_ganancia) {
    best_ganancia <- max_value
  } else {
    best_row <- i
  }
}

cat ("params cp", param_grid[best_row,"cp"],
       "minsplit", param_grid[best_row,"minsplit"],
       "minbucket", param_grid[best_row,"minbucket"],
       "maxdepth", param_grid[best_row,"maxdepth"], 
       "level", param_grid[best_row,"level"],
       'is the best parameter set')

model <- rpart(
  formula = "clase_ternaria ~ . -numero_de_cliente",
  weights = pesos,
  data = dtrain, # los datos donde voy a entrenar
  xval = 0,
  cp = 0, # esto significa no limitar la complejidad de los splits
  minsplit = 750, # minima cantidad de registros para que se haga el split
  minbucket = 450, # tamaño minimo de una hoja
  maxdepth = 15
) 


prediccion <- predict(
  object = model,
  newdata = dapply,
  type = "prob"
)

# agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dapply[, prob_baja2 := prediccion[, "BAJA+2"]]

# solo le envio estimulo a los registros
#  con probabilidad de BAJA+2 mayor  a  1/40
dapply[, Predicted := as.numeric(prob_baja2 > 0.80)]

# genero el archivo para Kaggle
# primero creo la carpeta donde va el experimento
#dir.create("./dmeyf2023-main/exp/")
to_write <- dapply[, list(numero_de_cliente, Predicted)]
# solo los campos para Kaggle
fwrite(to_write,
       file = "./dmeyf2023-main/exp/sub_20.csv",
       sep = ","
)

