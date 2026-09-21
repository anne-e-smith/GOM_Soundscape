#' ---------------------------------------------
#' 
#' 
#' Helper functions for AMP analysis
#' 
#' 
#' ---------------------------------------------



Compile_ves_detections <- function(site_id = character()){
  dir_detect <- choose.dir(caption = paste0("Select folder with validated detections ", site_id))
  det_tables <- dir_detect |>
    # list all files with .csv extension
    list.files(pattern = ".csv") |>
    map_chr(~paste0(dir_detect,"\\",  .)) |>
    # use map to iterate read_csv() function over each file in the directory
    map(~read_csv(.) |>
          select(Detection, ISOStartTime, ISOEndTime, StartTime, EndTime,
                 start, end, `New Labels`) |>
          # mutate to add deployment ID from filename 
          # use .* operator to subset after "_MA-RI_" and before "_5s_48Hz" in filename
          mutate(start_date_ISO = as_date(ISOStartTime),
                 Dep_ID = sub(".*_GOM_","", .x),
                 Dep_ID = sub("_5s_48Hz.*","",Dep_ID))) 
  
  
  # Now we have selection tables as a list, but we want them all together in one dataframe
  all_dets <- do.call("rbind", det_tables) |>
    # add new column for site
    mutate(SITE = site_id)
  
 
  return = all_dets
  
}

# Test
# test <- Compile_ves_detections(site_id = "NS01")


dets_to_hp <- function(det_table, 
                       site_id = character()){
  # ves_og <- read_csv(choose.files(caption = "Choose Ship Detection Notes for given dep")) |>
  ves_og <- det_table #|>  
    #rename('New Labels' = 'New_Labels')
  
  #change start_date column from a type of <character> to a <date>
  #ymd("1996-12-13")
  #ves_og$start_date <- ymd(ves_og$start_date)
  
  # Filter out ambient detections so it's all ships
  ves_data <- ves_og |>
    filter('New Labels' != "ambient")
  
  
  #### Create Hourly Presence Table ####
  
  # get hours from date-times with functions from lubridate pkg
  
  all_ves_hr <- ves_data |>
    mutate(Begin_Hour = hour(ISOStartTime),
           End_Hour = hour(ISOEndTime))
  
  # Count instances of each behavior per date-hour
  hr_tally <- all_ves_hr |>
    group_by(start_date_ISO, Begin_Hour, SITE) |>
    count() |>
    rename("Ves_counts" = "n")
  
  # create a new df with all hours for the whole dataset
  date_range_dep <- seq.Date(from = start_dep_date, to = end_dep_date, by = "day") |>
    crossing(seq(0,23,1)) 
  
  # rename columns
  names(date_range_dep) <- c("start_date_ISO","Begin_Hour")
  
  # join 2 data frames together to add behavior tally
  hourly_pres <- date_range_dep |>
    left_join(hr_tally, by = c("start_date_ISO","Begin_Hour")) |>
    replace_na(list(Ves_counts = 0,
                    SITE = site_id))
  
  return = hourly_pres
}
