## Funcion para agregar los datos de todas las hojas del archivo de excel

cirugias_data_func <- function(year){
  df_sheet <- read_excel("data/CirugiasEfectuadas.xlsx", sheet = as.character(year) )
  df_sheet <- df_sheet %>% rename("TiempoEdad" = ...2)
  
  if(length(df_sheet)==21){
    df_sheet <- df_sheet %>%
      mutate(C.I.E.9MC...22 = NA,
             DESCRIPCION...23 = NA,
             C.I.E.9MC...24 = NA,
             DESCRIPCION...25 = NA)
  } else if (length(df_sheet) == 23){
    df_sheet <- df_sheet %>%
      mutate(C.I.E.9MC...24 = NA,
             DESCRIPCION...25 = NA)
  }
  df_sheet <- df_sheet %>% mutate(year=as.character(year))
}


## Funcion para cambiar los nombres de la tabla 

CIE9_names_func <- function(n){
  
  n1 <- 4+(n*2)
  n2 <- 5+(n*2)
  
  CIE9_col <- paste0("C.I.E.9MC...", n1)
  desc_col <- paste0("DESCRIPCION...", n2)
  
  CIE9 <- cirugias %>% filter(!is.na(!!sym(CIE9_col))) %>%
    group_by(!!sym(CIE9_col), !!sym(desc_col)) %>%
    summarise(n = n(),.groups = "drop") %>% rename("CIE9" := !!sym(CIE9_col), "DESCRIPCION" = !!sym(desc_col))
  
  CIE9
}

## Funcion para obtener el RMSE

RMSE <- function(true_data, predicted_data){
  sqrt(mean((true_data - predicted_data)^2))
}

## Funcion para obtener el modelo de efectos regularizados

mrEffects <-  function(l, m, colnme, train_dat, test_dat, rmse = TRUE, cm = FALSE, results = FALSE){
  
  b_i <- train_dat %>% 
    group_by(EDAD) %>%
    summarize(b_i = sum(!!sym(colnme) - m)/(n()+l),.groups = "drop")
  
  b_s <- train_dat %>% 
    left_join(b_i, by="EDAD") %>%
    group_by(SEXO) %>%
    summarize(b_s = sum(!!sym(colnme) - b_i - m)/(n()+l),.groups = "drop")
  
  b_c <- train_dat %>% 
    left_join(b_i, by="EDAD") %>%
    left_join(b_s, by="SEXO") %>%
    group_by(CIE10) %>%
    summarize(b_c = sum(!!sym(colnme) - b_i - b_s - m)/(n()+l),.groups = "drop")
  
  predicted <- test_dat %>% 
    left_join(b_i, by = "EDAD") %>%
    left_join(b_s, by="SEXO") %>%
    left_join(b_c, by = "CIE10") %>%
    mutate(pred = mu_n + b_i + b_s + b_c) %>%
    pull(pred)
  
  if(isTRUE(rmse) & isFALSE(results)){
    return(RMSE(test_dat[[colnme]], round(predicted)))
  } else if(isTRUE(cm) & isFALSE(results)){
    return(confusionMatrix(factor(test_dat[[colnme]]), factor(round(predicted)))$overall['Accuracy'])
    
  } else if(isTRUE(rmse) & isTRUE(results)){
    
    results <- list(
      rmse = RMSE(test_dat[[colnme]], round(predicted)),
      mu = m,
      bi = b_i,
      bs = b_s,
      bc = b_c,
      pred = round(predicted)
    )
    
    return(results)
      
  } else if(isTRUE(cm) & isTRUE(results)){
      
      results <- list(
        accuracy = confusionMatrix(factor(test_dat[[colnme]]), factor(round(predicted)))$overall['Accuracy'],
        mu = m,
        bi = b_i,
        bs = b_s,
        bc = b_c,
        pred = round(predicted)
      )
    
    return(results)
    
  }
  
  
}


## Funciones para la aplicación

procedimientos <- function(dat){
  result <- 
    dat %>% 
    left_join(procedimientos_mdl$bi, by = "EDAD") %>%
    left_join(procedimientos_mdl$bs, by="SEXO") %>%
    left_join(procedimientos_mdl$bc, by = "CIE10") %>%
    mutate(pred = mu + b_i + b_s + b_c) %>%
    pull(pred)
  result
}

n_procedimientos <- function(dat){
  result <- 
    dat %>% 
    left_join(ncie9_mdl$bi, by = "EDAD") %>%
    left_join(ncie9_mdl$bs, by="SEXO") %>%
    left_join(ncie9_mdl$bc, by = "CIE10") %>%
    mutate(pred = mu + b_i + b_s + b_c) %>%
    pull(pred)
  result
}


dfCreator <- function(edad, sexo, diagnostico) {

  sex <- ifelse(sexo == "Femenino", "FEM", "MAS")
  cie10 <- CIE10_5a %>% filter(DESCRIPCION == diagnostico) %>% pull(CIE10)
  if(!is.null(edad) & !is.null(sex) & !is.null(diagnostico)){
    datos <- data.frame(
      EDAD = edad,
      SEXO = sex,
      CIE10 = cie10
    )
  }
  datos
}

dataFilter <- function(no, dat) {
  
  nombre <- paste0("CIE9_",no)
  recomendation <- cirugias_dat %>% filter(#EDAD %in% c(dat$EDAD-5:dat$EDAD-5) &
    CIE10 == dat$CIE10 &
      !is.na(!!sym(nombre)))
  
  n <- length(recomendation[[nombre]])
  
  recomendation <- recomendation %>% 
    group_by(!!sym(nombre)) %>% summarise(Probabilidad = round(n()/n, 2),.groups = "drop") %>%
    rename("CIE9" = !!sym(nombre))

  descripcion <- CIE9_names %>% filter(CIE9 %in% recomendation$CIE9) %>% select(-n)
  
  recomendation <- merge(recomendation, descripcion)

  if(length(recomendation[,1]) == 0){
    recomendation <- NULL
  }
  
  recomendation
  
}



proced_tables <- function(n, dat, output, np){
  
    nombre1 <- paste0("CIE9_", n, "Text")
    nombre2 <- paste0("CIE9_", n, "Table")
  
    if(!is.null(dat[[n]])){
      output[[nombre1]] <- renderText({
        paste("Sugerencia procedimiento", n)
      })
    
      output[[nombre2]] <- DT::renderDataTable({
        dat <- dat[[n]]
        if(!is.null(dat)){
          dat <- dat %>% arrange(desc(Probabilidad))
          DT::datatable(dat)
        }
      })
    
    } else {
      output[[nombre1]] <- renderText({})
      output[[nombre2]] <- DT::renderDataTable({})
    }
  
}






