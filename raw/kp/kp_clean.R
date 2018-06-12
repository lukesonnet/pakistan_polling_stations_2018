# clean tabula files
library(tidyverse)
library(stringr)

# wd is proj dir (git root)

# Get list of tabula files
kpcsv <- list.files(
  "raw/kp/csv", 
  pattern = "csv$", 
  full.names = TRUE
)

# find header (e.g. "1", "2", "3") rows
get_header <- function(.x) {
  apply(.x[, c(1:3)], 1, function(x) all(x == c("1", "2", "3")))
}

# find rows/columns where all characters are in some set of 
# characters
get_dim_chars <- function(.x, dim, chars = "") {
  apply(.x, dim, function(x) all(x %in% chars))
}

# find first column with census block looking number
find_num <- function(.x) {
  apply(.x, 1, function(x) {
    numloc <- which(nchar(x) > 6 & !is.na(as.numeric(x)))
    if (length(numloc)) numloc[1] else NA
  })
}

# find 3 columns in a row where first two columns sum to third columns
find_trio <- function(.x) {
  apply(.x, 1, function(x) {
    xn <- as.numeric(x) 
    for (i in seq_along(x)) {
      sum2 <- sum(xn[i:(i+1)], na.rm = TRUE)
      if (sum2 == xn[i+2] & !is.na(xn[i+2]) & sum2 < 1e6)  {
        return(i)
      }
    }
    return(NA_integer_)
  })
}

# General strategy is to work with matrices of character vectors
# and aim to realign everything
dists <- map_chr(kpcsv, 
  ~ trimws(
    gsub("(tabula\\-|PK)", "", str_extract(.x, "tabula\\-(.*?)PK"))
  ))

# Load and first cleaning of data
kpmat <- setNames(
  kpcsv %>%
  map(read.csv, header = FALSE, colClasses = "character") %>%
  map(as.matrix) %>%
  # Drop rows or columns that are all empty
  map( ~ .x[!get_dim_chars(.x, 1, ""), !get_dim_chars(.x, 2, "")]) %>%
  # Drop any rows thare are just the 1,2,3,4,.... column headers
  map( ~ .x[!get_header(.x), ]),
  dists
)

# Fix DI Khan census blocks which have some peculiar errors
dimat <- kpmat[["D.I.Khan"]]
to_clean <- grepl("^0", dimat) & nchar(dimat) > 6
kpmat[["D.I.Khan"]][to_clean] <- gsub("(^0|\\s+)", "", dimat)[to_clean]

dat <- kpmat %>% 
  # find the column with the census block number
  map( ~ data.frame(numloc = find_num(.x),
                    dat = I(split(.x, 1:nrow(.x))))) %>%
  map( ~ mutate(
    .x,
    # find where to shift rows over to align census block nubmers
    pin = pmax(0, numloc - 2),
    minloc = min(numloc, na.rm = TRUE),
    maxloc = max(numloc, na.rm = TRUE),
    mov = maxloc - minloc) %>%
      filter(!is.na(numloc)) %>% # drop rows with out census block
      rowwise() %>%
      mutate(
        # Actually shift cols over
        dat = list(c(dat[0:pin], rep("", times = maxloc - numloc), dat[(pin+1):length(dat)])),
        ldat = length(dat)
      ) %>%
      ungroup() %>%
      mutate(maxldat = max(ldat)) %>%
      rowwise() %>%
      mutate(dat = list(c(dat, rep("", times = maxldat - ldat))))
  ) %>%
  map( ~ do.call(rbind, .x$dat)) %>%
  map( ~ .x[, !get_dim_chars(.x, 2, c("", "-", "---"))]) %>%
  map(data.frame, stringsAsFactors = FALSE) %>%
  map2(.y = dists, ~ mutate(.x, district = .y))

# Some manual cleaning to get the following
# col 1 ps number
# col 2 ps name
# col 3 area name
# col 4 census block number
names(dat)
dat[["Kohat"]][["X2"]] <- 
  ifelse(dat[["Kohat"]][["X2"]] != "",
         dat[["Kohat"]][["X2"]], 
         dat[["Kohat"]][["X3"]])
dat[["Kohat"]][["X3"]] <- NULL

dat[["Kohistan"]][["X2"]] <- 
  ifelse(dat[["Kohistan"]][["X2"]] != "", 
         dat[["Kohistan"]][["X2"]], 
         dat[["Kohistan"]][["X3"]])
dat[["Kohistan"]][["X3"]] <- NULL

dat[["Shangla"]][["X4"]] <- 
  ifelse(dat[["Shangla"]][["X4"]] != "", 
         dat[["Shangla"]][["X4"]], 
         dat[["Shangla"]][["X5"]])
dat[["Shangla"]][["X5"]] <- NULL

dat[["Tank"]][["X6"]] <-
  paste0(dat[["Tank"]][, "X3"], dat[["Tank"]][, "X6"])
dat[["Tank"]][["X3"]] <- NULL
dat[["Tank"]][["X4"]] <- NULL
dat[["Tank"]][["X5"]] <- NULL
dat[["Tank"]][["X7"]] <- NULL

# Drop peshawar for now
dat[["Peshawar"]] <- NULL

dat2 <- dat %>%
  map( ~ mutate(
    .x,
    # If second column is just a number, use it, if not, use first column
    ps_number = ifelse(!is.na(as.numeric(.[[2]])),
                       as.numeric(.[[2]]),
                       as.numeric(.[[1]])),
    ps_name = ifelse(nchar(.[[1]]) > 0 & 
                       is.na(as.numeric(.[[1]])) & 
                       nchar(.[[2]]) == 0, 
                     .[[1]], 
                     .[[2]]),
    elec_area = .[[3]],
    census_block = .[[4]]
  )) %>% map_dfr(
    ~ .x %>%
      mutate(
        trio_pos = find_trio(.),
        male_voters = as.numeric(.[cbind(seq_along(trio_pos), trio_pos)]),
        female_voters = as.numeric(.[cbind(seq_along(trio_pos), trio_pos+1)]),
        total_voters = as.numeric(.[cbind(seq_along(trio_pos), trio_pos+2)])
      ) %>%
      select(district, ps_number, ps_name, elec_area, census_block,
             male_voters, female_voters, total_voters, trio_pos)
  )

# Still have to error check + get peshawar working

# Fill down ps_number


table(dat2$ps_number, dat2$district)
