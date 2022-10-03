## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ---- load-packages, include=FALSE--------------------------------------------
library(CTNote)
library(readxl)
library(kableExtra)
library(tidyverse)

## ---- import-table-one, include=FALSE-----------------------------------------
pathToTable1_char <- system.file(
  "suppl_docs", "definitions_20220405.xlsx", package = "CTNote", mustWork = TRUE
)
tab1_df <- readxl::read_xlsx(pathToTable1_char) 

## ---- tidy-table-one, include=FALSE-------------------------------------------
tabTidy1_df <- 
  tab1_df %>% 
  select(-`Frequency of UOS`, -`Coded column name`, -DOI) %>% 
  rename(
    Group = `Outcome Group`,
    Endpoint = `Primary Endpoint`,
    Class = `Numeric Class`,
    Definition = `Definition/Assessment of Outcome`,
    `Missing is` = `Missing UOS coded as`
  ) %>% 
  mutate(
    Group = case_when(
      str_detect(Group, "Abstinence") ~ "Abstinence",
      str_detect(Group, "Relapse") ~ "Relapse",
      str_detect(Group, "Reduction") ~ "Reduction",
    )
  ) %>% 
  filter(Group == "Abstinence") %>% 
  arrange(Reference)

defns_char <- tabTidy1_df$Definition
names(defns_char) <- tabTidy1_df$Reference

## ---- show-table-one-reduction, results='asis', echo=FALSE--------------------
tabTidy1_df  %>% 
  kable("html") %>%
  column_spec(1:4, width = "3cm") %>%
  column_spec(5, width = "5cm") %>%
  kable_styling("striped", font_size = 11) %>%
  kable_minimal() %>% 
  # All styling and spec calls have to come BEFORE this line.
  scroll_box(width = "1000px", height = "500px") 

## -----------------------------------------------------------------------------
###  Full Data  ###
udsOutcomes_df <- 
	CTNote::outcomesCTN0094 %>% 
  select(who, usePatternUDS)

# Make a copy
outcomesAbs_df <- udsOutcomes_df


###  Examples  ###
examplePeople_int <- c(1, 163, 210, 242, 4, 17, 13, 1103, 233, 2089)
outcomesAbs_df %>% 
  filter(who %in% examplePeople_int)

## -----------------------------------------------------------------------------
outcomesAbs_df <- 
	outcomesAbs_df %>%
  rowwise() %>% 
	mutate(
		udsPattern = recode_missing_visits(
			use_pattern = usePatternUDS,
		)
	) %>% 
  # mixed results != abstinence
	mutate(
		udsPattern = recode_missing_visits(
			use_pattern = udsPattern,
			missing_is = "*"
		)
	) %>% 
  # We did not code this definition with an "end", so participants with longer
  #   stays in treatment could have higher scores
	mutate(
		fiellin2006_abs = count_matches(
			use_pattern = udsPattern,
			match_is = "-"
		)
	) %>% 
	select(who, fiellin2006_abs) %>% 
	left_join(outcomesAbs_df, ., by = "who")

outcomesAbs_df %>% 
  filter(who %in% examplePeople_int) %>% 
  select(who, usePatternUDS, fiellin2006_abs)

## -----------------------------------------------------------------------------
outcomesAbs_df <- 
	outcomesAbs_df %>%
  rowwise() %>% 
  mutate(
		kosten1993_isAbs = detect_subpattern(
			usePatternUDS,
			subpattern = "---" 
		)
	) %>% 
	select(who, kosten1993_isAbs) %>% 
	left_join(outcomesAbs_df, ., by = "who")

outcomesAbs_df %>% 
  filter(who %in% examplePeople_int) %>% 
  select(who, usePatternUDS, kosten1993_isAbs)

## ---- include=FALSE-----------------------------------------------------------
whichKrupitsky_idx <- which(
  names(defns_char) == "Krupitsky et al., 2011"
)

## -----------------------------------------------------------------------------
outcomesAbs_df <- 
	outcomesAbs_df %>%
  rowwise() %>% 
	mutate(
		udsPattern = recode_missing_visits(
			use_pattern = usePatternUDS,
		)
	) %>% 
	mutate(
		udsPattern = recode_missing_visits(
			use_pattern = udsPattern,
			missing_is = "*"
		)
	) %>% 
	mutate(
		useProp = count_matches(
			use_pattern = udsPattern,
			match_is = "+",
			start = 5L,
			# Set this to the length of your protocol, or 24, whichever is shorter
			end = 15L,
			proportion = TRUE
		)
	) %>% 
	mutate(krupitsky2011A_isAbs = useProp == 0) %>% 
	select(who, krupitsky2011A_isAbs) %>% 
	left_join(outcomesAbs_df, ., by = "who")

outcomesAbs_df %>% 
  filter(who %in% examplePeople_int) %>% 
  select(who, usePatternUDS, krupitsky2011A_isAbs)

## -----------------------------------------------------------------------------
outcomesAbs_df <- 
	outcomesAbs_df %>%
  rowwise() %>% 
  mutate(
		udsPattern = recode_missing_visits(
			use_pattern = usePatternUDS,
		)
	) %>% 
	mutate(
		udsPattern = recode_missing_visits(
			use_pattern = udsPattern,
			missing_is = "*"
		)
	) %>% 
	mutate(
		krupitsky2011B_abs = count_matches(
			use_pattern = udsPattern,
			match_is = "-",
			start = 5L,
			# This trial protocol has a clear end date; we adjust it to our data
			end = 15L
		)
	) %>% 
	select(who, krupitsky2011B_abs) %>% 
	left_join(outcomesAbs_df, ., by = "who")

outcomesAbs_df %>% 
  filter(who %in% examplePeople_int) %>% 
  select(who, usePatternUDS, krupitsky2011B_abs)

## -----------------------------------------------------------------------------
outcomesAbs_df <- 
	outcomesAbs_df %>%
  rowwise() %>% 
  mutate(
		ling1998_isAbs = detect_subpattern(
			use_pattern = usePatternUDS,
			# 13 consecutive UDS at 3x per week is 4.3 weeks
			subpattern = "----"
		)
	) %>% 
	select(who, ling1998_isAbs) %>% 
	left_join(outcomesAbs_df, ., by = "who")
  

outcomesAbs_df %>% 
  filter(who %in% examplePeople_int) %>% 
  select(who, usePatternUDS, ling1998_isAbs)

## -----------------------------------------------------------------------------
###  Define 15-week Lattice  ###
lofwallLattice_char <- collapse_lattice(
	lattice_patterns = c("o", "_o"),
	# For the lattice as defined over 24 weeks, you need 12 weeks of weekly visits
	#   and 6 sets of alternating "no visit" and "visit" week pairs, or c(12, 6).
	#   For us, we want 7 weeks straight of weekly visits followed by 4 pairs of
	#   alternating visits (8 weeks) for a total of 15 weeks.
	times = c(7, 4)
)
lofwallLattice_char


###  Calculate Weighted Abstinence  ###
outcomesAbs_df <- 
	outcomesAbs_df %>%
  rowwise() %>% 
  # Change mixed and missing results to positive
	mutate(
		udsPattern = recode_missing_visits(
			use_pattern = usePatternUDS,
			missing_is = "*"
		)
	) %>% 
  mutate(
    udsPattern = recode_missing_visits(udsPattern)
  ) %>% 
  # "observe" only the UDS that would have been caught by the protocol
	mutate(
		udsLattice = view_by_lattice(
			use_pattern = udsPattern,
			lattice_pattern = str_sub(lofwallLattice_char, end = 15) # first 15 weeks
		)
	) %>% 
	# Impute the visits that were not "observed"
	mutate(
		udsLatticeLOCF = impute_missing_visits(
			use_pattern = udsLattice,
			method = "locf",
			missing_is = "_",
			quietly = TRUE
		)
	) %>% 
  # Count for Weeks 5-7; Week 8; and Weeks 9-15
	mutate(
		prop57 = count_matches(
			udsLatticeLOCF,
			match_is = "-",
			start = 5L,
			end = 7L,
			proportion = TRUE
		),
		clean8 = count_matches(
			udsLatticeLOCF,
			match_is = "-",
			start = 8L,
			end = 8L
		),
		prop915 = count_matches(
			udsLatticeLOCF,
			match_is = "-",
			start = 9L,
			end = 15L,
			proportion = TRUE
		),
	) %>% 
  # Check interval counts/proportions
	mutate(
		lofwall2018_isAbs = (prop57 >= 2/3) & (clean8 == 1) & (prop915 >= 5/6)
	) %>% 
	select(who, lofwall2018_isAbs) %>% 
	left_join(outcomesAbs_df, ., by = "who")

outcomesAbs_df %>% 
  filter(who %in% examplePeople_int) %>% 
  select(who, usePatternUDS, lofwall2018_isAbs)

## -----------------------------------------------------------------------------
outcomesAbs_df <- 
	outcomesAbs_df %>%
  rowwise() %>% 
  mutate(
		udsPattern = recode_missing_visits(
			use_pattern = usePatternUDS
		)
	) %>%
	mutate(
		udsPattern = recode_missing_visits(
			use_pattern = udsPattern,
			missing_is = "*"
		)
	) %>%
  # Find the number of weeks until the first "+"
	mutate(
		mokri2016_abs = detect_in_window(
			use_pattern = udsPattern,
			window_width = 1L,
			threshold = 1L
		)
	) %>%
	unnest(cols = "mokri2016_abs", names_sep = "_") %>%
	select(who, starts_with("mokri2016_abs")) %>%
	left_join(outcomesAbs_df, ., by = "who")
  
outcomesAbs_df %>% 
  filter(who %in% examplePeople_int) %>% 
  select(who, usePatternUDS, starts_with("mokri2016_abs"))

## -----------------------------------------------------------------------------
outcomesAbs_df %>% 
  filter(who %in% examplePeople_int) %>% 
  mutate(
    mokri2016_wksAbst = survival::Surv(
      time = mokri2016_abs_time,
      event = mokri2016_abs_event
    )
  ) %>% 
  # FOR PRINTING THE TABLE ONLY. DO NOT USE NEXT LINE IN PRACTICE!!!
  mutate(mokri2016_wksAbst = as.character(mokri2016_wksAbst)) %>% 
  select(who, usePatternUDS, mokri2016_wksAbst)

## -----------------------------------------------------------------------------
outcomesAbs_df <- 
	outcomesAbs_df %>%
  rowwise() %>% 
  # Ignore missing visits
  mutate(
		udsPattern = recode_missing_visits(
			use_pattern = usePatternUDS,
			missing_becomes = ""
		)
	) %>% 
  # Mixed are positive
	mutate(
		udsPattern = recode_missing_visits(
			use_pattern = udsPattern,
			missing_is = "*"
		)
	) %>% 
  # Measure the length of the longest period of continuous abstinence
	mutate(
		schottenfeld2005_abs = measure_abstinence_period(
			use_pattern_binary = udsPattern
		)
	) %>% 
	select(who, schottenfeld2005_abs) %>% 
	left_join(outcomesAbs_df, ., by = "who")
  

outcomesAbs_df %>% 
  filter(who %in% examplePeople_int) %>% 
  select(who, usePatternUDS, schottenfeld2005_abs)

## -----------------------------------------------------------------------------
outcomesAbs_df <- 
	outcomesAbs_df %>%
  rowwise() %>% 
  mutate(
		udsPattern = recode_missing_visits(
			use_pattern = usePatternUDS
		)
	) %>% 
	mutate(
		udsPattern = recode_missing_visits(
			use_pattern = udsPattern,
			missing_is = "*"
		)
	) %>% 
	mutate(
		schottenfeld2008A_abs = detect_in_window(
			use_pattern = udsPattern,
			window_width = 1L,
			threshold = 1L
		)
	) %>% 
	unnest(cols = "schottenfeld2008A_abs", names_sep = "_") %>% 
	select(who, starts_with("schottenfeld2008A_abs")) %>% 
	left_join(outcomesAbs_df, ., by = "who")
  
outcomesAbs_df %>% 
  filter(who %in% examplePeople_int) %>% 
  select(who, usePatternUDS, starts_with("schottenfeld2008A_abs"))

## -----------------------------------------------------------------------------
outcomesAbs_df <- 
	outcomesAbs_df %>%
  rowwise() %>% 
  mutate(
		udsPattern = recode_missing_visits(
			use_pattern = usePatternUDS
		)
	) %>% 
	mutate(
		udsPattern = recode_missing_visits(
			use_pattern = udsPattern,
			missing_is = "*"
		)
	) %>%
	mutate(
		schottenfeld2008B_abs = measure_abstinence_period(
			use_pattern_binary = udsPattern
		)
	) %>% 
	select(who, schottenfeld2008B_abs) %>% 
	left_join(outcomesAbs_df, ., by = "who")
  

outcomesAbs_df %>% 
  filter(who %in% examplePeople_int) %>% 
  select(who, usePatternUDS, schottenfeld2008B_abs)

## -----------------------------------------------------------------------------
outcomesAbs_df <- 
	outcomesAbs_df %>%
  rowwise() %>% 
  # Set "o" to "-"
	mutate(
		udsPattern = recode_missing_visits(
			use_pattern = usePatternUDS,
			missing_becomes = "-"
		)
	) %>% 
	# Set "*" to "+"
	mutate(
		udsPattern = recode_missing_visits(
			use_pattern = udsPattern,
			missing_is = "*"
		)
	) %>% 
	mutate(
		shufman1994_absN = detect_in_window(
			use_pattern = udsPattern,
			window_width = 1L,
			threshold = 1L
		)
	) %>% 
	unnest(cols = "shufman1994_absN", names_sep = "_") %>% 
	select(who, starts_with("shufman1994_absN")) %>% 
	left_join(outcomesAbs_df, ., by = "who")
  

outcomesAbs_df %>% 
  filter(who %in% examplePeople_int) %>% 
  select(who, usePatternUDS, starts_with("shufman1994_absN"))

## -----------------------------------------------------------------------------
outcomesAbs_df <- 
	outcomesAbs_df %>%
  rowwise() %>% 
  mutate(
		udsPattern = recode_missing_visits(
			use_pattern = usePatternUDS,
		)
	) %>% 
	mutate(
		udsPattern = recode_missing_visits(
			use_pattern = udsPattern,
			missing_is = "*"
		)
	) %>% 
	mutate(
		cleanLastWeek = detect_subpattern(
			use_pattern = udsPattern,
			subpattern = "-",
			start = -1,
			end = -1
		)
	) %>% 
	mutate(
		finalUseCount = count_matches(
			use_pattern = udsPattern,
			match_is = "+",
			# 3 weeks leading up to the last week
			start = -4L,
			end = -2L
		)
	) %>% 
	mutate(weissLingCTN0030_isAbs = cleanLastWeek & (finalUseCount <= 1)) %>% 
	select(who, weissLingCTN0030_isAbs) %>% 
	left_join(outcomesAbs_df, ., by = "who")
  

outcomesAbs_df %>% 
  filter(who %in% examplePeople_int) %>% 
  select(who, usePatternUDS, weissLingCTN0030_isAbs)

## -----------------------------------------------------------------------------
sessionInfo()

## ---- include=FALSE, eval=FALSE-----------------------------------------------
#  # write_csv(
#  #   outcomesAbs_df,
#  #   file = "../inst/extdata/outcomes_abstinence_20220818.csv"
#  # )

