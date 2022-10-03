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
  filter(Group == "Relapse") %>% 
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
outcomesRel_df <- udsOutcomes_df


###  Examples  ###
examplePeople_int <- c(1, 163, 210, 242, 4, 17, 13, 1103, 233, 2089)
outcomesRel_df %>% 
  filter(who %in% examplePeople_int)

## ---- include=FALSE-----------------------------------------------------------
which0094_idx <- which(
  names(defns_char) == "CTN-0094"
)

## -----------------------------------------------------------------------------
outcomesRel_df <- 
	outcomesRel_df %>%
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
		ctn0094_relapse = detect_in_window(
			use_pattern = udsPattern,
			window_width = 4L,
			threshold = 4L
		)
	) %>% 
	unnest(cols = "ctn0094_relapse", names_sep = "_") %>% 
	select(who, starts_with("ctn0094_relapse")) %>% 
	left_join(outcomesRel_df, ., by = "who")

outcomesRel_df %>% 
  filter(who %in% examplePeople_int) %>% 
  select(who, usePatternUDS, starts_with("ctn0094_relapse"))

## -----------------------------------------------------------------------------
outcomesRel_df <- 
	outcomesRel_df %>%
  rowwise() %>% 
  # do NOT recode any missing visits
  mutate(
		ctn0094_dropout = detect_in_window(
			use_pattern = usePatternUDS,
			window_width = 4L,
			threshold = 4L,
			match_is = "o"
		)
	) %>% 
	unnest(cols = "ctn0094_dropout", names_sep = "_") %>% 
	select(who, starts_with("ctn0094_dropout")) %>% 
	left_join(outcomesRel_df, ., by = "who")

outcomesRel_df %>% 
  filter(who %in% examplePeople_int) %>% 
  select(who, usePatternUDS, starts_with("ctn0094_dropout"))

## -----------------------------------------------------------------------------
outcomesRel_df <- 
	outcomesRel_df %>%
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
		johnson1992_hasRel = detect_subpattern(
			use_pattern = udsPattern,
			subpattern = "++",
			# Starting at 4 weeks of treatment
			start = 4L
		)
	) %>% 
	select(who, johnson1992_hasRel) %>% 
	left_join(outcomesRel_df, ., by = "who")

outcomesRel_df %>% 
  filter(who %in% examplePeople_int) %>% 
  select(who, usePatternUDS, johnson1992_hasRel)

## -----------------------------------------------------------------------------
outcomesRel_df <- 
	outcomesRel_df %>%
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
		krupitsky2004_hasRel = detect_subpattern(
			use_pattern = udsPattern,
			subpattern = "+++"
		)
	) %>% 
	select(who, krupitsky2004_hasRel) %>% 
	left_join(outcomesRel_df, ., by = "who")

outcomesRel_df %>% 
  filter(who %in% examplePeople_int) %>% 
  select(who, usePatternUDS, krupitsky2004_hasRel)

## -----------------------------------------------------------------------------
outcomesRel_df <- 
	outcomesRel_df %>%
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
		lee2016_rel = detect_in_window(
			use_pattern = udsPattern,
			window_width = 4L,
			threshold = 2L
		)
	) %>% 
	unnest(cols = "lee2016_rel", names_sep = "_") %>% 
	select(who, starts_with("lee2016_rel")) %>% 
	left_join(outcomesRel_df, ., by = "who")

outcomesRel_df %>% 
  filter(who %in% examplePeople_int) %>% 
  select(who, usePatternUDS, starts_with("lee2016_rel"))

## -----------------------------------------------------------------------------
outcomesRel_df <- 
	outcomesRel_df %>%
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
		udsPatternTrimmed = str_sub(udsPattern, start = 3L)
	) %>% 
	rowwise() %>% 
	mutate(
		lee2018_rel = detect_in_window(
			use_pattern = udsPatternTrimmed,
			window_width = 4L,
			threshold = 4L
		)
	) %>% 
	unnest(cols = "lee2018_rel", names_sep = "_") %>% 
	mutate(lee2018_rel_time = lee2018_rel_time + 2) %>% 
	select(who, starts_with("lee2018_rel")) %>% 
	left_join(outcomesRel_df, ., by = "who")

outcomesRel_df %>% 
  filter(who %in% examplePeople_int) %>% 
  select(who, usePatternUDS, starts_with("lee2018_rel"))

## -----------------------------------------------------------------------------
outcomesRel_df <- 
	outcomesRel_df %>%
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
		schottenfeld2008_rel = detect_in_window(
			use_pattern = udsPattern,
			window_width = 3L,
			threshold = 3L
		)
	) %>% 
	unnest(cols = "schottenfeld2008_rel", names_sep = "_") %>% 
	select(who, starts_with("schottenfeld2008_rel")) %>% 
	left_join(outcomesRel_df, ., by = "who")

outcomesRel_df %>% 
  filter(who %in% examplePeople_int) %>% 
  select(who, usePatternUDS, starts_with("schottenfeld2008_rel"))

## -----------------------------------------------------------------------------
sessionInfo()

## ---- include=FALSE, eval=FALSE-----------------------------------------------
#  # write_csv(
#  #   outcomesRel_df,
#  #   file = "../inst/extdata/outcomes_relapse_20220818.csv"
#  # )

