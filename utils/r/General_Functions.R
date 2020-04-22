
#Get packages for the functions
library("dplyr")
library("tidyr")
#Structuring-------------------------------

dir_skeleton <- function() {
  #Create folder structure
  folders <-
    c(
      "data",
      "data/raw",
      "data/temp",
      "output",
      "src",
      "src/r",
      "src/sql",
      "src/python",
      "models",
      "reports",
      "notebooks"
      )
  for (i in folders) {
    dir.create(path = i,
               recursive = F,
               showWarnings = T)
  }
}
code_skeleton <- function() {
  # Create code files
  files <- c("1_get_data.R",
             "2_clean_up.R",
             "3_feature creation.R")
  
  for (i in files) {
    file.create(paste("src/r/", files, sep = ""))
  }
  
  file.create(paste("src/r/functions_1.R", sep = ""))
}

#Filtering and Selection function-----------------------------

which_na <- function(x) {
  # get NAs
  return (which(is.na(x)))
}
remove_NA <- function(x) {
  # remove NA
  nas <- which_na(x)
  if (length(nas) > 0) {
    return(x[-nas])
  } else
    return(x)
}
Which_UUID <- function(x) {
  #UUID Check
  grep(
    "^[a-fA-F0-9]{8}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{12}$",
    x
  )
}
len_unique <- function(x) {
  #Length of unique elements in a vector
  return(length(unique(x)))
}
len_na <- function(x) {
  #Length of NAs
  return (length(which_na(x)))
}

len_which <- function(x) {
  #Length of conditionally satisfied vector
  return (length(which(x)))
}

NAs <-
  function(dataframe) {
    #List of na elements in all coluns of a data frame
    return(sapply(dataframe, function(x)
      which_na(x)))
  }

Freq_tb <- function(x,
                    #Frequency Table
                    y = NA,
                    Greater_Than_y = F) {
  t <- data.frame(table(x))
  
  if (!is.na(y)) {
    if (Greater_Than_y) {
      return(t[which(t$Freq > y),])
    } else
    {
      return(t[which(t$Freq == y),])
    }
  } else
  {
    return(t)
  }
}

u_col <-
  function(dataframe) {
    #List of unique elements in all coluns of a data frame
    return(sapply(dataframe, function(x)
      unique(x)))
  }
#Data Wrangling---------------------------------------------------

unique_cols <- function(df) {
  ##Get unique columns and return un-duplicated
  names = colnames(df)
  dup_cols = Freq_tb(names, y = 1, Greater_Than_y = T)
  if (nrow(dup_cols) != 0) {
    for (i in nrow(dup_cols)) {
      cols = which(names == dup_cols$x[i])
      df <- df[-cols[-1]]
    }
  }
  return(df)
}
standardize <-
  function(data, cols, type) {
    #Standardizing column name and types
    require(dplyr)
    existing <- names(data) [which(names(data) %in% cols)]
    new <- cols [which(!cols %in% names(data))]
    
    data <- add_columns(data = data, cols = new)
    data <- as.data.frame(data)
    for (i in 1:length(type)) {
      col = which(names(data) == cols[i])
      if (type[i] == "char") {
        data[, col]  <- as.character(data[, col])
      }
      if (type[i] == "numeric") {
        data[, col] <- as.numeric(data[, col])
      }
      #logical cannot handle characterized numeric values properly
      # if (type[i] == "logical") {
      #   if(class(data[, col]))
      #   data[, col]  <- as.logical(data[, col])
      # }
    }
    return(data %>% select(cols))
  }
add_columns <- function(data, cols) {
  #add columns
  data[, cols] <- NA
  return(data)
}

#Date-time-------------------
difftime_days <- function(time1, time2) {
  t = (as.numeric(difftime(
    time1 = time1 ,
    time2 = time2,
    units = "days"
  )))
  
  t = ifelse(test = t <  0 & t > -1, yes = 0, t)
  return(floor(t))
}
#Convert specifed cols to lower stings
DFtolower <- function(dataframe,
                      col.index = NULL,
                      col.string = NULL,
                      col.class = NULL) {
  cols <- NULL
  
  #get cols whose index is specified
  if (!length(col.index) == 0) {
    cols <- append(cols, col.index)
  }
  #get cols whose name is specified
  
  if (!length(col.string) == 0) {
    matches <-
      grep(pattern = paste(col.string, collapse = "|"), names(dataframe))
    cols <- append(cols, matches)
  }
  #get cols whose class is specified
  
  if (!length(col.class) == 0) {
    matches <- which(sapply(dataframe, class) == col.class)
    cols <- append(cols, matches)
  }
  
  cols <- unique(cols)
  for (i in cols) {
    dataframe[, i] <- tolower(dataframe[, i])
  }
  print(paste("changes made to ", paste(names(dataframe[cols]), collapse = (", "))))
  return(dataframe)
}


#Data cleaning---------------------------------------------------
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) |
           x > quantile(x, 0.75) + 1.5 * IQR(x))
}

Create_outlier_column <- function(Data, Col, Group) {
  #Data= Data File
  #Col: Columns in which outlier need to be found
  #Group: Columns in which you have variable acc. to which groups sub-sets needs to be formed
  
  out <-
    do.call(data.frame, aggregate(
      x = Data[, Col],
      by = list(Data[, Group]),
      FUN = is_outlier
    ))
  
  Outlier <- data.frame(NULL)
  for (i in 1:length(unique(Data[, Group]))) {
    df <-
      data.frame(rep(x = as.character(out[i, 1]), length(out[1,-1])), as.character(out[i,-1]))
    
    names(df) <- c(Group, paste("Outlier-", Col))
    Outlier <- rbind(Outlier, df)
  }
  Data <-
    cbind(Data, Outlier[, 2])
  names(Data)[length(names(Data))] <- paste("Outlier-", Col)
  return(Data)
}

UUID_assign <- function(Change_vector) {
  require("uuid")
  source_table_key_Var <-
    data.frame(Variable = unique(Change_vector),
               UUID =
                 tolower(replicate(
                   length(unique(Change_vector)),
                   uuid::UUIDgenerate(use.time = TRUE)
                 )))
  changes <-
    vlookup(Change_vector = Change_vector, source_table_key_Var = source_table_key_Var)
  return(changes)
}

#Equivalent of vlook in Excel
vlookup <-
  function(Change_vector,
           source_table_key_Var,
           na_treatment = 0) {
    # Change_vector:    Column in DF which needs to be changed
    # source_table_Var_key: DF with @ columns- var and key where Var has same calues as change vector
    # na_treatment: 0- Leave NAs   1- ORiginal value   2- replace with UUID
    source_table_key_Var <-
      source_table_key_Var[which(!is.na(source_table_key_Var[1])),]
    
    
    #  if(len_which(is.na(Change_vector)>0)) stop("Change_vector contains NAs")
    
    temp <-
      data.frame(Variable = Change_vector, Key = seq(1:length(Change_vector)))
    temp <-
      merge(
        x = temp,
        y = source_table_key_Var,
        by.x = "Variable",
        by.y = names(source_table_key_Var)[1],
        all.x = T
      )
    
    #Handle NAs
    
    na_rows <- which(is.na(temp[3]))
    if (length(na_rows) != 0) {
      if (na_treatment == 1) {
        temp[na_rows, 3] <- as.character(temp[na_rows, 1])
      } else if (na_treatment == 2) {
        temp[na_rows, 3] <- UUID_assign(Change_vector = temp[na_rows, 1])
      }
    }
    
    #Getting value and indexes
    temp$Variable = temp[, 3]
    temp <- temp[order(temp$Key), ]
    na_rows <-
      which(is.na(temp[3])) #Getting NA indexes for re-orderd rows
    
    new_values = as.character(temp$Variable)
    if (length(na_rows) != 0) {
      values_changed = temp$Key[-na_rows]
    } else{
      values_changed = temp$Key
      na_rows = "No NA Rows"
    }
    return(list(
      new_values = new_values,
      values_changed = values_changed,
      na_rows = na_rows
    ))
  }

#Features finding-------------
stat_summary <- function(df, group_by_cols, metric_cols) {
  final <- df %>% select(group_by_cols, metric_cols) %>%
    group_by_(.dots = group_by_cols) %>%
    summarise_if(
      is.numeric,
      list(
        N = length,
        mean = mean,
        sd = sd,
        se   = function(x)
          sd(x) / sqrt(length(x)),
        ciMult = function(x)
          qt(.975 / 2 + .5, length(x) - 1),
        ci = function(x)
          (sd(x) / sqrt(length(x))) * (qt(.975 / 2 + .5, length(x) - 1))
      )
    )
  return(final)
}

#Classification functions-------------------------

#Create sub-groups for classification
sub_groups <- function(x,
                       
                       value_based = F,
                       range_based = F,
                       diff_based = F,
                       delta = NA,
                       use_absolute = T) {
  k = 1
  session = c(k)
  if (length(x) == 1) {
    return(session)
  } else {
    for (i in 2:length(x)) {
      if (value_based) {
        change = x[i] != x[i - 1]
      } else if (range_based) {
        change = x[i] >= delta
      } else {
        compare = ifelse(test = use_absolute,
                         yes = abs(x[i] - x[i - 1]) ,
                         no = x[i] - x[i - 1])
        
        change = compare >= delta
      }
      if (change) {
        k = k + 1
      }
      session = append(session, k)
    }
  }
  return(session)
}

#assumes breaks are a seq of numbers
gen_labels <- function(breaks, string_to_add) {
  labels <- NULL
  for (i in 1:length(breaks)) {
    if (i < length(breaks)) {
      t <-
        paste(breaks[i],
              string_to_add,
              " - ",
              breaks[i + 1],
              string_to_add,
              sep = "")
      labels <- append(labels, t)
    }
  }
  return(labels)
}


#Read functions-------------


