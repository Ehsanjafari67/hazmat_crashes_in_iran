#--------------------------------Initial Variables------------------------------
#| cache: TRUE

# Create a new data frame 'crash' with transformations
crash <- hazmat  |> 
  arrange(desc(Accident_ID))  |> 
  transmute(
    Injury_Level = case_when(
      Fatal_Num > 0 ~ "Fatal",
      Injury_Num > 0 ~ "Injury",
      TRUE ~ "PDO"
    ),
    Season,
    Hour_Category,
    Weather_Con,
    Accident_Type,
    Overturn,
    Veh_Veh,
    Veh_Fixed,
    Veh_Multiveh,
    Collision_Type,
    Front_Rear,
    Road_Exit,
    Front_Front,
    Front_Side,
    Road_Overturn,
    Accident_Cause,
    Not_Front_Attension,
    Veh_Control_Inability,
    Speed_Violation,
    Left_Deviation,
    Awareness_Lack,
    Veh_Tech_Defect,
    Other_Cause,
    Road_Section,
    Involved_Veh_Num,
    Car_Involved,
    Truck_Involved,
    Lighting_Con,
    Daylight,
    Dark,
    Dusk_Dawn,
    Weekday,
    Weekend,
    Not_Weekend,
    Holiday,
    Pavement_Con,
    Acceptable_Pave,
    Low_Failure_Pave,
    Moderate_Failure_Pave,
    ADT,
    Heavy_Veh_Percentage,
    Average_Speed,
    Speed_Violation_Percentage,
    Design_Average_Speed,
    Road_Fun,
    Freeway,
    Highway,
    Main_Road,
    Other_Road,
    Lane_Num,
    Total_Hazard_Score_Category,
    Group_Hazard_Score_Category,
    Travel_Time_Category,
    Age,     
    Education,
    Mariage,
    Household,
    Experience,
    Veh_Age
  )

str(crash)
sumtable(crash)
#--------------------------------Variables evaluation---------------------------
#| cache: TRUE

df <- crash
df[df == ""] <- NA

for (col in names(df)) {
  uniq_val <- unique(df[[col]])
  n_uniq <- length(uniq_val)
  n_miss <- sum(is.na(df[[col]]))
  if (n_uniq < 100) {
    print(paste("Column:", col, "- Number of unique values:", n_uniq))
    print(paste("Column:", col, "- Number of missing values:", n_miss))
    tbl <- table(df[[col]])
    print(paste("Column:", col, "- Ordered Frequency Table:"))
    print(tbl[order(tbl, decreasing = TRUE)])
  } 
}

crash <- df
sumtable(crash)
#-----------------------Classification & create new variables-------------------
#| cache: TRUE

# Creating a new "Driver Age" column based on Age group categories
crash <- mutate(crash, Driver_Age = 
                  case_when(
                    Age > 30 & Age < 55 ~ 2,  # middle-aged driver
                    Age >= 55 ~ 3,            # old driver
                    TRUE ~ 1                  # young driver
                  ))

# Creating a new "experience" column based on the Experience column
crash <- mutate(crash, Driver_Exp = 
                  case_when(
                    Experience < 10 ~ 1,  # Inexperience driver
                    TRUE ~ 2              # All other cases
                  ))

# Creating a new "Vehicle Age" column based on Veh_Age group categories
crash <- mutate(crash, Vehicle_Age = 
                  case_when(
                    Veh_Age >= 15 ~ 3,  # Old Vehicle
                    Veh_Age <= 10 ~ 1,  # new Vehicle
                    TRUE ~ 2            # middle-aged Vehicle
                  ))

# Creating a new "multi/single Vehicle" column based on Involved_Veh_Num group categories
crash <- mutate(crash, Multi_Single_Veh = 
                  case_when(
                    Involved_Veh_Num == 1 ~ 1,  #single crash
                    Involved_Veh_Num > 1 ~ 2    #multi vehicle crash
                  ))

# Creating a new "road traffic volume" column
crash <- mutate(crash, Traffic_Volume = 
                  case_when(
                    ADT < 10000 ~ 1,  #road with low traffic volume
                    ADT > 20000 ~ 3,  #road with high traffic volume
                    TRUE ~ 2       #Other
                  ))

# Creating a new "heavy vehicle presence" column
crash <- mutate(crash, Heavy_Presence = 
                  case_when(
                    Heavy_Veh_Percentage < 10 ~ 1,  #low presence of heavy vehicles
                    Heavy_Veh_Percentage > 30 ~ 3,  #large presence of heavy vehicles
                    TRUE ~ 2                        #Other
                  ))

# Creating a new "road average speed" column
crash <- mutate(crash, Road_Average_Speed = 
                  case_when(
                    Average_Speed < 50 ~ 1,  #low speed road
                    Average_Speed > 80 ~ 3,  #high speed road
                    TRUE ~ 2                 #Other
                  ))

# Creating a new "road speed violation" column
crash <- mutate(crash, Road_Speed_Violation = 
                  case_when(
                    Speed_Violation_Percentage < 5 ~ 1,  #low speed violation
                    Speed_Violation_Percentage > 25 ~ 3, #high speed violation
                    TRUE ~ 2                 #Other
                  ))
#-----------------------------select final variables----------------------------
#| cache: TRUE

names(crash)

crash <- subset(crash, select = -c(Age,Experience,Veh_Age,Involved_Veh_Num,
                                   ADT,Heavy_Veh_Percentage,Average_Speed,
                                   Speed_Violation_Percentage,Design_Average_Speed
))

str(crash)
sumtable(crash)
#------------------------create frequency & Percentage tables-------------------
#| cache: TRUE

variables_to_analyze <- c(
  "Season",
  "Hour_Category",
  "Weather_Con",
  "Accident_Type",
  "Collision_Type",
  "Accident_Cause",
  "Road_Section",
  "Lighting_Con",
  "Weekday",
  "Weekend",
  "Holiday",
  "Pavement_Con",
  "Traffic_Volume",
  "Heavy_Presence",
  "Road_Average_Speed",
  "Road_Speed_Violation",
  "Road_Fun",
  "Lane_Num",
  "Total_Hazard_Score_Category",
  "Group_Hazard_Score_Category",
  "Travel_Time_Category",
  "Driver_Age",
  "Education",
  "Mariage",
  "Household",
  "Driver_Exp",
  "Vehicle_Age",
  "Multi_Single_Veh"
)

# Create frequency and percentage tables for each variable
result_tables <- list()
for (variable in variables_to_analyze) {
  # Check if the variable is a data frame or not
  if (is.data.frame(crash[[variable]])) {
    # If it's a data frame, you may want to extract a specific column or handle it differently
    # For example, if 'variable' is a data frame, and you want to analyze one of its columns named 'ColumnName':
    if ("ColumnName" %in% colnames(crash[[variable]])) {
      freq_table <- table(crash[[variable]]$ColumnName, crash$Injury_Level)
    } else {
      # Handle the case where the specified column does not exist in the data frame
      freq_table <- NULL
    }
  } else {
    # If it's an atomic vector, you can proceed as before
    freq_table <- table(crash[[variable]], crash$Injury_Level)
  }
  
  if (!is.null(freq_table)) {
    perc_table <- prop.table(freq_table, margin = 2) * 100
    result_tables[[variable]] <- list(Frequency = as.data.frame(freq_table), Percentage = as.data.frame(perc_table))
  }
}

# Iterate through the result_tables list and view each table
for (variable_name in names(result_tables)) {
  freq_table <- result_tables[[variable_name]]$Frequency
  perc_table <- result_tables[[variable_name]]$Percentage
  
  # Print the variable name
  cat("Variable:", variable_name, "\n\n")
  
  # Print the frequency table
  cat("Frequency Table:\n")
  print(freq_table)
  
  # Print the percentage table
  cat("\nPercentage Table:\n")
  print(perc_table)
  
  cat("\n--------------------------------\n")
}
#-------------------------------------------------------------------------------
cat("\014")
#
for (col in names(crash)) {
  uniq_val <- unique(crash[[col]]) 
  n_uniq <- length(uniq_val)
  n_miss <- sum(is.na(crash[[col]]))
  if (n_uniq < 100) {
    print(paste("Column:", col, "- Number of unique values:", n_uniq))
    print(paste("Column:", col, "- Number of missing values:", n_miss))
    tbl <- table(crash[[col]])
    print(paste("Column:", col, "- Ordered Frequency Table:"))
    print(tbl[order(tbl, decreasing = TRUE)])
  } 
}

#
InitialDataForAnalysis <- crash
save(InitialDataForAnalysis,
     file = "InitialDataForAnalysis.RData")

write.xlsx(InitialDataForAnalysis, "InitialDataForAnalysis.xlsx")