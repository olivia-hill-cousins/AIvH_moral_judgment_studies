###############################################################################
# READ IN DFs
################################################################################
read_in_csv <- function(csv){
  df <- read.csv(csv)
  df
}

################################################################################
# COMBINE KEYS
################################################################################
# keys were counterbalanced (aka F=Yes or J=Yes) so we combine the two datasets here
combine_keys_to_one_df <- function(data_fkey, data_jkey){
  # find common columns (stops any issues when binding)
  common_cols <- intersect(names(data_fkey), names(data_jkey))
  df <- bind_rows(
    data_fkey |> dplyr::select(all_of(common_cols)),
    data_jkey |> dplyr::select(all_of(common_cols))
  )
  df
}
################################################################################
# CLEAN RAW DATA
################################################################################
# clean demographic data
clean_demo_data <- function(demo, prolific_demo){
  ## create list of IDs that are "awaiting review" in prolific
  ids_to_keep <- prolific_demo |> 
    dplyr::pull(ID)
  # keep only data rows with moral judgements, remove irrelevant rows (e.g., instructions, etc)
  demo <- demo %>% 
    filter(Response.Type == "response") |> 
    dplyr::filter(Participant.Private.ID %in% ids_to_keep) |> 
    rename(ID = Participant.Private.ID)
  
  ethnicity <- demo |>
    filter(`Object.Name` %in% c("Multiple Choice", "pref_no")) |>
    group_by(ID) |>
    summarise(ethnicity = paste(c(Key[Response == 1 & Key != "__other"], Response[Key == "other"]), collapse = " "), .groups = "drop")
  
  gender <- demo |> 
    filter(Question == "**How do you currently describe your gender?**") |> 
    group_by(ID) |> 
    summarise(
      gender = if (any(Key == "value" & Response == "__other")) {Response[Key == "other"]} else {
        Response[Key == "value"]},.groups = "drop")
  
  age <- demo |>
    filter(Object.Name %in% c("age")) |>
    group_by(ID) |>
    summarise(age = paste(c(Response[Key == "value"]), collapse = " "), .groups = "drop")
  
  gender_age <- left_join(gender, age, by = "ID")
  gorilla_demo <- left_join(gender_age, ethnicity, by = "ID")
  demo <- gorilla_demo |> 
    full_join(prolific_demo |> dplyr::select(ID, Sex, Age, Ethnicity.simplified, Nationality, Country.of.birth, Country.of.residence), by = "ID") |>
    mutate(gender = coalesce(gender, Sex),
           age = coalesce(age, as.character(Age)),
           ethnicity = coalesce(ethnicity, Ethnicity.simplified)) |>
    dplyr::select(ID, gender, age, ethnicity, Nationality, Country.of.birth, Country.of.residence) |> 
    mutate(gender = case_when(
      gender == "Female" ~ "Woman",
      gender == "Male" ~ "Man",
      TRUE ~ gender
    ),
    gender = stringr::str_to_sentence(gender))
  # to check which Ps we are using data from prolific:
  # test_demo <- gorilla_demo |> 
  #   full_join(prolific_demo |> dplyr::select(ID, Sex, Age, Ethnicity.simplified, Nationality, Country.of.birth, Country.of.residence), by = "ID") |>
  #   mutate(
  #     gender_filled_from_prolific = is.na(gender) & !is.na(Sex),
  #     age_filled_from_prolific = is.na(age) & !is.na(Age),
  #     ethnicity_filled_from_prolific = is.na(ethnicity) & !is.na(Ethnicity.simplified),
  #     gender = coalesce(gender, Sex),
  #     age = coalesce(age, as.character(Age)),
  #     ethnicity = coalesce(ethnicity, Ethnicity.simplified)
  #   )
  
  demo <- demo |> 
    rename(
      Ethnicity = ethnicity,
      Gender = gender, 
      Age = age, 
      Country_of_Birth = Country.of.birth, 
      Country_of_Residence = Country.of.residence
      
    )
  
  demo
  
}




## correct factors in dfs first
correct_df_with_mg_key <- function(df, MG_key){
  df <- df  %>% 
    mutate(
      Dilemma_base = str_remove(Dilemma, "_(Trolley|Crane|Mine|Truck)$")
    ) 
  
  
  df <- df %>%
    rename_with(~paste0(.x, "_actual"), c(PBH, PDE, Intent, BA, MC, MG)) %>%
    left_join(
      MG_key,
      by = c("Dilemma_base" = "Dilemma")
    ) 
  df
}
#===============================================================================
# STUDY 1 & 2
#===============================================================================
# ### pre sharable data
# # prolific saves demographics with public IDs, for this reason, the private ID was added so that the demographic read in, in this workflow only has the private ID. The code below is what was used to do that:
# prolific_demographic$ID <- unblinded_short$"Participant Private ID"[match(prolific_demographic$`Participant.id`, unblinded_short$"Participant Public ID")]
# keep_these_priv_ids <- c("16462581", "16462596", "16462492", "16463010", "16464498")
# dont_keep_these_priv_ids <- c("16462121")
# ## next, we filter the df, keeping rows where either their status is marked as complete, or their priv ID is one of the exceptions noted above
# 
# # ## next, we filter the df, keeping rows where either their status is marked as complete, or their priv ID is one of the exceptions noted above
# priv_prolific_demographic <- prolific_demographic %>%
#   filter(Status == "AWAITING REVIEW" | ID %in% keep_these_priv_ids) |>
#   filter(Status == "complete" | ID %notin% dont_keep_these_priv_ids) |>
#   select(-"Participant.id", -"Submission.id")
# write.csv(priv_prolific_demographic, "data_raw/private_prolific_demographic.csv")
################################################################################
# CLEAN RAW DATA
################################################################################
# here we filter & select the data we need
s1_s2_clean_raw_data <- function(df){
  # keep only data rows with moral judgements, remove irrelevant rows (e.g., instructions, etc)
  df <- df %>% 
    filter(Response.Type == "response")
  # select data we need 
  df <- df %>% 
    dplyr::select(Participant.Private.ID, Task.Name, Trial.Number, Spreadsheet..Dilemma, Spreadsheet..Context, Spreadsheet..DilemmaByContext, Spreadsheet..Prohibited.act, Spreadsheet..PDE, Spreadsheet..PDE.noBA, Spreadsheet..PDE.GE.BE, Spreadsheet..PDE.SE_notME, Spreadsheet..MG.Permissible, Spreadsheet..Agent, Absolute.Reaction.Time, Response, randomiser.zmvq)
  
  # rename columns to make them easier to work with
  df <- df %>%
    rename(PBH = Spreadsheet..Prohibited.act,
           PDE = Spreadsheet..PDE,
           BA = Spreadsheet..PDE.noBA,
           MC = Spreadsheet..PDE.GE.BE,
           intent = Spreadsheet..PDE.SE_notME,
           MG = Spreadsheet..MG.Permissible,
           RT = Absolute.Reaction.Time,
           ID = Participant.Private.ID,
           MJ = Response,
           Agent = Spreadsheet..Agent,
           Dilemma = Spreadsheet..DilemmaByContext,
           DilemmaAll = Spreadsheet..Dilemma,
           context = Spreadsheet..Context,
           keys = randomiser.zmvq)
  
  df
  
}

################ removing some scenarios from the actual dataset
fix_dfs <- function(df, MG_key){
  df$oldDilemma <- df$Dilemma
  df <- df %>%
    mutate(
      Dilemma = case_when(
        # Fix typo: "Unprohibted" -> "Unprohibited"
        str_detect(Dilemma, "^Unprohibted_") ~ 
          str_replace(Dilemma, "^Unprohibted_", "Unprohibited_"),
        # Default: keep original
        TRUE ~ Dilemma
      )) %>%
    mutate(
      # remove numbers (e.g. from substituted consent 2)
      Dilemma = str_replace(Dilemma, "[0-9]", ""),
      # remove Hang from e.g. drop man hang
      Dilemma = str_replace(Dilemma, "Hang$", "")) %>% 
    mutate(
      Dilemma = Dilemma %>%
        # 1. Insert _ between camelCase parts (letter–letter)
        str_replace_all("([a-z])([A-Z])", "\\1_\\2") %>%
        # 2. Insert _ between letter and digit
        str_replace_all("([a-zA-Z])([0-9])", "\\1_\\2") %>%
        # 3. Insert _ between digit and letter
        str_replace_all("([0-9])([a-zA-Z])", "\\1_\\2") %>%
        # 4. Replace spaces with underscores
        str_replace_all(" ", "_") %>%
        # 5. Collapse multiple underscores
        str_replace_all("_+", "_") %>%
        # 6. Ensure underscore before context names at end if missing
        str_replace_all("([^_])(Trolley|Mine|Truck|Crane)$", "\\1_\\2")
    ) %>%
    mutate(
      Dilemma = case_when(
        # Consistency
        Dilemma == "Bystander_PHA&O" ~ "Bystander_PHA&O_Trolley",
        Dilemma == "drop_man_Crane" ~ "Drop_Man_Crane",
        Dilemma == "collapse_bridge_Crane" ~ "Collapse_Bridge_Crane",
        Dilemma == "loop_Truck" ~ "Loop_Truck",
        
        
        
        # Default: keep original
        TRUE ~ Dilemma
      )
    ) %>%
    mutate(
      Dilemma = str_split(Dilemma, "_") %>%
        lapply(function(x) {
          x <- ifelse(
            str_detect(x, "^[A-Z0-9&]+$"),  # keep abbreviations like PHO, PHA, O
            x,
            str_to_title(x)
          )
          paste(x, collapse = "_")
        }) %>%
        unlist()
    )  %>% 
    mutate(
      Dilemma = if_else(
        str_detect(Dilemma, "_(Trolley|Crane|Mine|Truck)$"),
        Dilemma,
        paste0(Dilemma, "_Trolley")
      )
    ) %>%
    mutate(
      Dilemma = str_replace(Dilemma, "^Loop_", "Loop_Track_"),
      Dilemma = str_replace(Dilemma, "PHA&O", "PHA_PHO")
    ) 
  df <- df %>%
    mutate(
      Dilemma = case_when(
        # Fix typo: "Unprohibted" -> "Unprohibited"
        str_detect(Dilemma, "^Unprohibted_") ~ 
          str_replace(Dilemma, "^Unprohibted_", "Unprohibited_"),
        # Default: keep original
        TRUE ~ Dilemma
      )) %>% 
    mutate(
      Dilemma = Dilemma %>%
        # 1. Insert _ between camelCase parts (letter–letter)
        str_replace_all("([a-z])([A-Z])", "\\1_\\2") %>%
        # 4. Replace spaces with underscores
        str_replace_all(" ", "_") %>%
        # 5. Collapse multiple underscores
        str_replace_all("_+", "_") %>%
        # 6. Ensure underscore before context names at end if missing
        str_replace_all("([^_])(Trolley|Mine|Truck|Crane)$", "\\1_\\2")
    )  %>%
    mutate(
      Dilemma = str_split(Dilemma, "_") %>%
        lapply(function(x) {
          x <- ifelse(
            str_detect(x, "^[A-Z0-9&]+$"),  # keep abbreviations like PHO, PHA, O
            x,
            str_to_title(x)
          )
          paste(x, collapse = "_")
        }) %>%
        unlist()
    )  %>% 
    mutate(
      Dilemma = if_else(
        str_detect(Dilemma, "_(Trolley|Crane|Mine|Truck)$"),
        Dilemma,
        paste0(Dilemma, "_Trolley")
      ),
      Dilemma = str_replace(Dilemma, "Man-In-Front", "Man_in_Front"),
      Dilemma = str_replace(Dilemma, "man-in-front", "Man_in_Front"),
      Dilemma = str_replace(Dilemma, "Man-In-Front$", "Man_in_Front"),
      Dilemma = str_replace(Dilemma, "Man_In_Front$", "Man_in_Front"),
      Dilemma = str_replace(Dilemma, "man-in-front$", "Man_in_Front")
    ) 
  
  df <- df  %>% 
    mutate(
      Dilemma_base = str_remove(Dilemma, "_(Trolley|Crane|Mine|Truck)$")
    ) 
  #}
  
  
  df <- df %>%
    rename_with(~paste0(.x, "_actual"), c(PBH, PDE, Intent, BA, MC, MG)) %>%
    left_join(
      MG_key,
      by = c("Dilemma_base" = "Dilemma")
    ) 
  
  df
}
################################################################################
# JOINING STUDY 1 & 2
################################################################################
# here we join the dfs for studies 1 & 2 to aid with making data summaries at later stages
join_s1_s2 <- function(df1, df2){
  df1 <- df1 %>% mutate(study = "1") %>%
    dplyr::select(-"X", -identified_variants, -variants_string)
  df2 <- df2 %>% mutate(study = "2") %>% 
    mutate(Nationality = "US") %>% 
    dplyr::select(-"X",-"TVideo_1", -"Country_of_residence", -"identified_prefs", -"prefs_string")
  df <- rbind(df1, df2)
  df
}
################################################################################
# JOINING STUDY 1 & 2
################################################################################
# here we join the dfs for studies 1 & 2 to aid with making data summaries at later stages
join_s1_s2_s3 <- function(df1, df2, df3){
  df1 <- df1 %>% mutate(study = "1") 
  df2 <- df2 %>% mutate(study = "2") %>% 
    mutate(Nationality = "US") 
  df3 <- df3 %>% mutate(study = "3") %>% 
    mutate(Nationality = "US") 
  df1_col <- colnames(df1)
  df2_col <- colnames(df2)
  df3_col <- colnames(df3)
  df1_v_df2 <- setdiff(df1_col, df2_col)
  df1_v_df3 <- setdiff(df1_col, df3_col)
  df2_v_df1 <- setdiff(df2_col, df1_col)
  df2_v_df3 <- setdiff(df2_col, df3_col)
  df3_v_df1 <- setdiff(df3_col, df1_col)
  df3_v_df2 <- setdiff(df3_col, df2_col)
  df1 <- df1 %>% dplyr::select(-all_of(df1_v_df2), -all_of(df1_v_df3))
  df2 <- df2 %>% dplyr::select(-all_of(df2_v_df1), -all_of(df2_v_df3))
  df3 <- df3 %>% dplyr::select(-all_of(df3_v_df1), -all_of(df3_v_df2))
  df <- rbind(df1, df2, df3)
  df
}
#===============================================================================
# STUDY 1
################ tidy vars in s1 for consistency
tidy_s1_vars <- function(df) {
  df <- df %>% rename(Dilemma = "dilemma", Intent = "intent", Context = "context") 
  df
}
################ fixing data frame for s1
# some categorising info didn't copy over for some rows. This doesn't affect the data at all, but we need to add them back in here so we can analyse. 
fix_s1_df <- function(df){
  
  # Create lookup table from F_YES keys
  # YES - create dictionary from F_YES first (clean & simple)
  pbh_dict <- df %>%
    filter(grepl("F_YES", keys)) %>%
    dplyr::select(Dilemma, PBH_correct = PBH) %>%
    mutate(PBH_correct = as.character(as.vector(PBH_correct))) %>%
    distinct(Dilemma, .keep_all = TRUE)  # one PBH per Dilemma
  
  # Create dictionary for MC from F_YES keys
  mc_dict <- df %>%
    filter(grepl("F_YES", keys)) %>%
    dplyr::select(Dilemma, MC_correct = MC) %>%
    mutate(MC_correct = as.character(as.vector(MC_correct))) %>%
    distinct(Dilemma, .keep_all = TRUE)
  
  # Create dictionary for MG from F_YES keys  
  mg_dict <- df %>%
    filter(grepl("F_YES", keys)) %>%
    dplyr::select(Dilemma, MG_correct = MG) %>%
    mutate(MG_correct = as.character(as.vector(MG_correct))) %>%
    distinct(Dilemma, .keep_all = TRUE)
  
  # Apply ALL dictionaries to fix J_YES rows
  df <- df %>%
    left_join(pbh_dict, by = "Dilemma") %>%
    left_join(mc_dict, by = "Dilemma") %>%
    left_join(mg_dict, by = "Dilemma") %>%
    mutate(
      PBH = case_when(
        grepl("J_YES", keys) ~ PBH_correct,
        TRUE ~ as.character(as.vector(PBH))
      ),
      MC = case_when(
        grepl("J_YES", keys) ~ MC_correct,
        TRUE ~ as.character(as.vector(MC))
      ),
      MG = case_when(
        grepl("J_YES", keys) ~ MG_correct,
        TRUE ~ as.character(as.vector(MG))
      )
    ) %>%
    dplyr::select(-ends_with("_correct"))  # drop all temp columns
  
  # change MG val. for omission 
  df$MG[df$Dilemma %in% c("OmissionBystanderPHOCrane","OmissionBystanderPHOTrolley",
                          "OmissionFootbridgePHOCrane","OmissionFootbridgePHOTrolley","BystanderPHO","BystanderPHOCrane")] <- "No"
  
  df$MG[df$Dilemma %in% c("SubstitutedConsent", "SubstitutedConsentCrane")] <- "Yes"
  df$MG[df$Dilemma %in% c("InefficientRisk", "InefficientRiskCrane")] <- "No"
  df$PDE[df$Dilemma %in% c("SubstitutedConsent", "SubstitutedConsentCrane")] <- "Yes"
  df$PDE[df$Dilemma %in% c("BystanderPHO","BystanderPHOCrane")] <- "No"
  df$PBH[df$Dilemma %in% c("OmissionBystanderPHOCrane","OmissionBystanderPHOTrolley",
                           "OmissionFootbridgePHOCrane","OmissionFootbridgePHOTrolley")] <- "Yes"
  
  df
}
## next we joined the experimental dfs with the demogrpahics dfs. This function is also included for demonstrative purposes, and is not actually used in this targets workflow directly. 
join_df_demo <- function(df, demo){
  df$ID <- as.factor(df$ID)
  demo$ID <- as.factor(demo$ID)
  df <- df %>% 
    left_join(demo, by = "ID")
  df
}

################################################################################
# CLEANING DEMOGRAPHIC DATA
################################################################################
# As noted at the top of this document, some code cannot be shared in its full form due to it containing information that can't be shared. Below we include an example of the data processing done in this step. 
assign_manually_entered_s1_data_to_categorical_vars <- function(demo){
  demo <- demo %>% 
    mutate(
      questionnaire.qemh.cultural.white = case_when(
        ID == "XXXXXXX" ~ "British or English or Scottish or Welsh or Northern Irish",
        ID == "XXXXXXX" ~ "Dutch",
        ID == "XXXXXX" ~ "Portuguese",
        TRUE ~ questionnaire.qemh.cultural.white
      ))
  
  demo <- demo %>% 
    rename(
      ID = "Participant.Private.ID",
      Gender = "questionnaire.qemh.Gender",
      Gender.quant = "questionnaire.qemh.Gender.quantised",
      Age = "questionnaire.qemh.Age",
      cultural.arab = "questionnaire.qemh.cultural.arab",
      cultural.asian = "questionnaire.qemh.cultural.asian",
      cultural.black = "questionnaire.qemh.cultural.black",
      cultural.multiple = "questionnaire.qemh.cultural.multiple",
      cultural.white = "questionnaire.qemh.cultural.white",
      cultural.prefNo = "questionnaire.qemh.cultural.prefNo",
      cultural.another = "questionnaire.qemh.cultural.another",
      Nationality = "Nationality"
    )
  
  
  # select only columns we renamed above
  demo <- demo %>% 
    dplyr::select(ID, Gender, Gender.quant, Age, Nationality, cultural.arab, cultural.asian, cultural.black,
                  cultural.multiple, cultural.white, cultural.prefNo, cultural.another)
  
  
  demo <- demo %>%
    mutate(cultural.another = if_else(cultural.another == "In another way (specify, if you wish)", 
                                      NA_character_, cultural.another)) %>% 
    mutate(cultural.multiple = if_else(cultural.multiple == "Any other mixed or multiple ethnic backgrounds (specify, if you wish).",
                                       NA_character_, cultural.multiple))
  
  
  
  cultural_cols <- c("cultural.arab", "cultural.asian", "cultural.black", 
                     "cultural.multiple", "cultural.white", "cultural.prefNo", 
                     "cultural.another")
  
  # Assign primary cultural category as first non-empty for each participant
  demo <- demo %>%
    mutate(
      primary_cultural = case_when(
        !is.na(cultural.white) & cultural.white != "" ~ "White",
        !is.na(cultural.black) & cultural.black != "" ~ "Black",
        !is.na(cultural.asian) & cultural.asian != "" ~ "Asian",
        !is.na(cultural.arab) & cultural.arab != "" ~ "Arab",
        !is.na(cultural.multiple) & cultural.multiple != "" ~ "Multiple",
        !is.na(cultural.prefNo) & cultural.prefNo != "" ~ "No preference",
        !is.na(cultural.another) & cultural.another != "" ~ "Other (specified)",
        TRUE ~ NA_character_
      ),
      multiple_cultural = rowSums(across(all_of(cultural_cols), ~ !is.na(.) & . != "")) > 1
    )
  
  demo <- demo %>%
    rowwise() %>%
    mutate(
      identified_variants = list(
        c_across(all_of(cultural_cols)) %>%
          keep(~ !is.na(.) && . != "")  # Keep only non-NA non-empty values
      ),
      variants_string = ifelse(length(identified_variants) == 0, 
                               NA_character_, 
                               paste(identified_variants, collapse = "; "))
    ) %>%
    ungroup()
  
}

#===============================================================================
# STUDY 3
#===============================================================================
# ### pre sharable data
# # prolific saves demographics with public IDs, for this reason, the private ID was added so that the demographic read in, in this workflow only has the private ID. The code below is what was used to do that:
# prolific_demographic$ID <- unblinded_short$"Participant Private ID"[match(prolific_demographic$`Participant.id`, unblinded_short$"Participant Public ID")]
# keep_these_priv_ids <- c("16462581", "16462596", "16462492", "16463010", "16464498")
# dont_keep_these_priv_ids <- c("16462121")
# ## next, we filter the df, keeping rows where either their status is marked as complete, or their priv ID is one of the exceptions noted above
# 
# # ## next, we filter the df, keeping rows where either their status is marked as complete, or their priv ID is one of the exceptions noted above
# priv_prolific_demographic <- prolific_demographic %>%
#   filter(Status == "AWAITING REVIEW" | ID %in% keep_these_priv_ids) |>
#   filter(Status == "complete" | ID %notin% dont_keep_these_priv_ids) |>
#   select(-"Participant.id", -"Submission.id")
# write.csv(priv_prolific_demographic, "data_raw/private_prolific_demographic.csv")
# add demo data to df
s3_add_demo_to_df <- function(df, demo){
  df <- df |> left_join(demo, by="ID") |> 
    dplyr::select(ID, Gender, Age, Ethnicity, Nationality, Country_of_Birth, Country_of_Residence, Task.Name, Trial.Number, Keys, Stimuli_List,PMC_Stimuli_List, Context, Dilemma, PBH, PDE, Agent, BA, MC, Intent, MG, MJ,RT)
  df
}
# here we filter & select the data we need
s3_clean_raw_data <- function(df, prolific_demo){
  ## create list of IDs that are "awaiting review" in prolific
  ids_to_keep <- prolific_demo |> 
    dplyr::pull(ID)
  # keep only data rows with moral judgements, remove irrelevant rows (e.g., instructions, etc)
  df <- df %>% 
    filter(Response.Type == "response") |> 
    dplyr::filter(Participant.Private.ID %in% ids_to_keep)
  
  # remove Ps who didn't complete study
  # ## some Ps completed but were marked as "live" or "rejected" by gorilla, so we first specify which Ps IDs that applies to, so we can ensure the df retains them
  # keep_these_priv_ids <- c("16462581", "16462596", "16462492", "16463010", "16464498")
  # dont_keep_these_priv_ids <- c("16462121")
  # ## next, we filter the df, keeping rows where either their status is marked as complete, or their priv ID is one of the exceptions noted above
  # df <- df %>%
  #   filter(Participant.Status == "complete" | Participant.Private.ID %in% keep_these_priv_ids) %>%
  #   filter(Participant.Status == "complete" | Participant.Private.ID %notin% dont_keep_these_priv_ids)
  
  msg1 <- dplyr::n_distinct(df$`Participant.Private.ID`)
  msg2 <- capture.output(df |> distinct(`Participant.Private.ID`, Participant.Status) |> count(Participant.Status)) |> paste(collapse = "\n")
  #msg3 <- capture.output(df |> distinct(`Participant.Private.ID`, Participant.Status) |> filter(`Participant.Private.ID` %in% keep_these_priv_ids)) |> paste(collapse = "\n")
  
  # select data we need 
  df <- df %>% 
    dplyr::select(Participant.Private.ID, Task.Name, Trial.Number, Spreadsheet..Dilemma, Spreadsheet..Context, 
                  Spreadsheet..PBH, Spreadsheet..PDE, Spreadsheet..BA, Spreadsheet..MC, Spreadsheet..Intent, Spreadsheet..MG, Spreadsheet..Agent, 
                  Absolute.Reaction.Time, Response, randomiser.zmvq, counterbalance.5cg9, counterbalance.5v2u)
  
  # rename columns to make them easier to work with
  df <- df %>%
    rename(PBH = Spreadsheet..PBH,
           PDE = Spreadsheet..PDE,
           BA = Spreadsheet..BA,
           MC = Spreadsheet..MC,
           Intent = Spreadsheet..Intent,
           MG = Spreadsheet..MG,
           RT = Absolute.Reaction.Time,
           ID = Participant.Private.ID,
           MJ = Response,
           Agent = Spreadsheet..Agent,
           Dilemma = Spreadsheet..Dilemma,
           Context = Spreadsheet..Context,
           Keys = randomiser.zmvq,
           Stimuli_List = counterbalance.5cg9,
           PMC_Stimuli_List = counterbalance.5v2u)
  
  msg <- paste0("Number of participants: ", paste(msg1),"\n", "Participant status counts:\n", paste(msg2))
  message(print(msg))
  
  return(df)
  
}
## remove columns we don't need
s3_tidy_df_cols <- function(df){
  df <- df |> 
    dplyr::select(ID, Gender, Age, Ethnicity, Nationality, Country_of_Birth, Country_of_Residence, Task.Name, Trial.Number, Keys, 
                  Stimuli_List,PMC_Stimuli_List, Context, Dilemma, PBH, PDE, Agent, BA, MC, Intent, MG, MJ,RT)
  df
}

#### remove timeout responses
s3_remove_timeout_responses <- function(df){
  df <- df |> dplyr::filter(MJ != "timeout") |> 
    droplevels()
  df
}

swap_keys_because_im_silly <- function(df){
  df <- df |>
    mutate(
      MJ = case_when(
        str_detect(Keys, "F_YES") & MJ == "Yes" ~ "No",
        str_detect(Keys, "F_YES") & MJ == "No" ~ "Yes",
        TRUE ~ MJ
      )
    )
  df
}

## split data so MJ and Endorsement responses are separate 
s3_separate_response_DVs <- function(df){
  mj <- df |> 
    filter(is.na(Probe_Text))
  
  endorsement <- df |> 
    filter(!is.na(Probe_Text))
  
  out <- list(mj, endorsement)
  out
  
}