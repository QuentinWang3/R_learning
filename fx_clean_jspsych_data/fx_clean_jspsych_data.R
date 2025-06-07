# ---
# name: clean_jspsych_data.R
# description: 一个用于清洗由jsPsych实验程序采集的实验数据的函数/ A function designed for reorganizing data structure obtained from jsPsych experiment.
# author: WANG Zhiyan
# date: 2025-06-06
# tags: [R functions, data cleaning]
# ---


#' @details In this function, a jspsych data file need to be selected and the target trials will be extracted from the data file for further analysis. In order to make the analysis easier, this function also changes variables' types, such as changing reaction time variable to numeric variable.
#' 
#' @title clean_jspsych_data
#' @description A data cleaning function for jsPsych experiment result file.
#' @param input_path The path to the file, default is NULL, and a selection window will open automatically for users to select the folder that has all the raw data.
#' @param phases_to_keep Because the raw data in jsPsych is composed of many rows that are redundant, only the rows containing these characters will be kept.
#' @param file_prefix_length For files named as "01MyName", the first two digits will be recorded in the dataframe as "pID" which means participants' ID.
#' @param ifws_pid_threshold Since my own jsPsych experiment has two versions for same group of participants, participants 1~15 are experiment type 1, and 16~30 are type 2, the number 15 is used to record this difference.
#' @param resp_col The name of column that records keyboard response.
#' @param answ_col The name of column that records correct answer of each trial.
#' @return a cleaned dataframe
#' @examples an example of how to use the function
#' clean_data <- clean_jspsych_data(input_path = NULL, 
#'                                  phases_to_keep = c("word", "sentence"), 
#'                                  file_prefix_length = 2, 
#'                                  ifws_pid_threshold = 15, 
#'                                  resp_col = "response", 
#'                                  answ_col = "correct_ans")
#'
#'


# ---- 0. load library (or ensure they're loaded)  ----
# These packages need to be loaded before using：
# library(rstudioapi)
# library(readxl) # if the data file is Excel file
# library(tidyverse)
# library(lmerTest)
# library(car)
# library(performance)
# library(systemfonts)

clean_jspsych_data <- function(input_path = NULL,
                               phases_to_keep = c("word", "sentence"),
                               file_prefix_length = 2,
                               ifws_pid_threshold = 15,
                               resp_col = "response",
                               answ_col = "correct_ans"
                               ){
  
# ---- 1. import data ----
if (is.null(input_path)) {
  if (interactive() && requireNamespace("rstudioapi", quietly = T) && 
      rstudioapi::isAvailable()) { 
    # check if R is running under interactive mode
    message("No folder path provided, please select folder path containing csv files.")
    
    # use utils::choose.dir() to let users choose a folder
    input_path <- rstudioapi::selectDirectory(caption = "choose the folder containing your raw data")
    
    if (is.null(input_path)) {
      stop("Folder selection is cancelled, please provide a path to the folder or run the function again.", call. = F)
    } # implicit else
    message("This folder is selected: ", input_path)
    
  } else {
    # if R is not running under interactive mode, require a path directly
    stop("'input_path' is not provided, and this session is not run in RStudio or the package 'rstudioapi' is not loaded, cannot directly choose folder path from this window", call. = F)
  }
}

# ---- 1.1 Folder path input check ----
if (!dir.exists(input_path)) {
  stop("Selected folder '", input_path, "' does not exist.", call. =F)
}

# ---- 2. list and import all the files ----
file_list <- list.files(path = input_path,
                        full.names = T)

if (length(file_list) == 0) {
  stop("In this directory, no file is available", call. = F)
}

message("R is now importing and merging the files below.")
print(basename(file_list))

all_data_raw <- purrr::map_dfr(file_list, function(file) {
  
  # 1. read all the csv and Excel files files to "file_list" 
  file_ext <- tolower(tools::file_ext(file)) # read the extension of files
  
  if (file_ext == "csv") {
    df <- readr::read_csv(file, 
                          col_types = readr::cols(.default = "c"),
                          show_col_types = F)
  } else if (file_ext %in% c("xlsx", "xls")) {
    
    if (!requireNamespace("readxl", quietly = T)) {
      stop("In order to import Excel files, the package 'readxl' need to be installed and loaded.")
    } 
    
    df <- readxl::read_excel(file, 
                             col_types = "text")
  } else {
    
    warning("This type of file '", basename(file) , "' is not supported, please make sure its csv or Excel file.")
    return(NULL)
  }
  
  # 2. mark the pid(participant ID) for the replacement in next step
  pid_pattern <- paste0("^(.{", file_prefix_length,"}).*")
  
  # 3. extract csv file name to dataframes in "file_list"
  df$pID <- sub(pid_pattern, "\\1", basename(file))
  
  return(df)
}
)

message("Raw data import and merge finished")

# ---- 3. Filtering ----
if (!"phase" %in% names(all_data_raw)) {
  stop("There is no colomn named 'phase' in data, please modify the parameter.", call. = F)
}

filtered_df <- all_data_raw %>% 
  dplyr::filter(phase %in% phases_to_keep, )

if (nrow(filtered_df) == 0) {
  warning("After filtering, the dataframe has no rows left. Please check the phase name and the corresponding coloumn name in raw data.")
}
message(paste("The dataframes have been filtered according to the phases(", paste(phases_to_keep, collapse = ", ") ,") in the experiment."))

# ---- 4. Delete the unnecessary columns ----
delete_col <- c("stimulus", "success", "timeout", "failed_images", 
                "failed_audio", "failed_video", "deviceInfo", 
                "internal_node_id", "trial_type", "width", "height",
                "webaudio", "browser", "browser_version", "os", "mobile", 
                "fullscreen", "vsync_rate", "webcam", "microphone")

actual_delete_col <- intersect(delete_col, names(filtered_df))
if (length(actual_delete_col) >0) {
  filtered_df <- filtered_df %>% dplyr::select(-dplyr::all_of(actual_delete_col))
  message(paste("These columns have been deleted: ", paste(actual_delete_col, collapse = ", ", sep = "")))
} else {
  message("There is no such column in dataframe.")
}

# ---- 5. Add column ----

if(!"pID" %in% names(filtered_df)) {
  stop("Column 'pID' is not found, 'ifws' column cannot be added", call. = F)
}

filtered_df$pID <- as.numeric(filtered_df$pID)

if (any(is.na(filtered_df$pID))) {
  warning("The extracted pID has NA value and you may should check the raw data file name and 'file_prefix_length' parameter.")
}

filtered_df$ifws <- dplyr::if_else(filtered_df$pID <= ifws_pid_threshold, 1, 0)
message("'ifws' column has been added.")

#### 
if(!resp_col %in% names(filtered_df) || !answ_col %in% names(filtered_df)) {
  stop(paste("The column '", resp_col, "' or the '", answ_col, "' is not  found in the dataframe, thus the column 'ifcorr' cannot be created", sep = ""), call. = F)
}

filtered_df$ifcorr <- dplyr::if_else(filtered_df[[resp_col]] == filtered_df[[answ_col]], 1, 0)
message("The column of correctness 'ifcorr' has been added.")

# ---- 6. Rename columns ----
if ("stimulus_type" %in% names(filtered_df)) {
  filtered_df <- filtered_df %>% dplyr::rename(group = stimulus_type)
  message("The column 'stimulus_type' has been renamed to 'group'")
} else {
  warning("Cannot find column 'stimulus_type' in dataframe, the rename cannot be done.")
}

# ---- 7. Add column "trial_type" according to column "group" ----
if (!"group" %in% names(filtered_df)) {
  stop("The column 'group' does not exist in dataframe, cannot create 'trial_type'", call. = F)
}
filtered_df$trial_type <- dplyr::case_when(
  filtered_df$group == "EE" ~ "NS-NS", 
  filtered_df$group %in% c("JJ", "MM", "KK") ~ "NNS(A)-NNS(A)",
  filtered_df$group %in% c("EJ", "EM", "EK") ~ "NS-NNS",
  TRUE ~ "NNS(A)-NNS(B)"
)
message("Column 'trial_type' has been added.")

# ---- 8. Data type transform ----
message("Transform the column 'rt' to numeric, and the 'phase' and 'trial_type' to factor.")

if ("rt" %in% names(filtered_df)) {
  filtered_df <- filtered_df %>% 
    dplyr::mutate(rt = as.numeric(rt))
} else {
  warning("Cannot find column 'rt', and the type transform cannot be done.")
}

cols_to_factor <- intersect(c("phase", "trial_type", "group"), names(filtered_df))
if (length(cols_to_factor) > 0 ) {
  filtered_df <- filtered_df %>% 
    dplyr::mutate(dplyr::across(dplyr::all_of(cols_to_factor), as.factor))
}
message("Transformation finished.")


# ---- 9. Return the cleaned data ----
return(filtered_df)

}







