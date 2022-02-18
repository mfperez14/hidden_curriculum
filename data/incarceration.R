
# Set working directory
# setwd()


new_data <- read.table('incarceration.dat', sep=' ')
names(new_data) <- c('E8022201',
  'E8022202',
  'E8022203',
  'E8022204',
  'E8022205',
  'E8022206',
  'E8022207',
  'E8022208',
  'E8022209',
  'E8022210',
  'E8022211',
  'E8022212',
  'R0000100',
  'R0536300',
  'R0536401',
  'R0536402',
  'R1235800',
  'R1482600')


# Handle missing values

  new_data[new_data == -1] = NA  # Refused 
  new_data[new_data == -2] = NA  # Dont know 
  new_data[new_data == -3] = NA  # Invalid missing 
  new_data[new_data == -4] = NA  # Valid missing 
  new_data[new_data == -5] = NA  # Non-interview 


# If there are values not categorized they will be represented as NA

vallabels = function(data) {
  data$E8022201 <- factor(data$E8022201, 
    levels=c(0.0,1.0,99.0), 
    labels=c("0: R not incarcerated in this month and not incarcerated in a previous month",
      "1: R was incarcerated during all or some of this month",
      "99: R incarcerated previously but not in this month"))
  data$E8022202 <- factor(data$E8022202, 
    levels=c(0.0,1.0,99.0), 
    labels=c("0: R not incarcerated in this month and not incarcerated in a previous month",
      "1: R was incarcerated during all or some of this month",
      "99: R incarcerated previously but not in this month"))
  data$E8022203 <- factor(data$E8022203, 
    levels=c(0.0,1.0,99.0), 
    labels=c("0: R not incarcerated in this month and not incarcerated in a previous month",
      "1: R was incarcerated during all or some of this month",
      "99: R incarcerated previously but not in this month"))
  data$E8022204 <- factor(data$E8022204, 
    levels=c(0.0,1.0,99.0), 
    labels=c("0: R not incarcerated in this month and not incarcerated in a previous month",
      "1: R was incarcerated during all or some of this month",
      "99: R incarcerated previously but not in this month"))
  data$E8022205 <- factor(data$E8022205, 
    levels=c(0.0,1.0,99.0), 
    labels=c("0: R not incarcerated in this month and not incarcerated in a previous month",
      "1: R was incarcerated during all or some of this month",
      "99: R incarcerated previously but not in this month"))
  data$E8022206 <- factor(data$E8022206, 
    levels=c(0.0,1.0,99.0), 
    labels=c("0: R not incarcerated in this month and not incarcerated in a previous month",
      "1: R was incarcerated during all or some of this month",
      "99: R incarcerated previously but not in this month"))
  data$E8022207 <- factor(data$E8022207, 
    levels=c(0.0,1.0,99.0), 
    labels=c("0: R not incarcerated in this month and not incarcerated in a previous month",
      "1: R was incarcerated during all or some of this month",
      "99: R incarcerated previously but not in this month"))
  data$E8022208 <- factor(data$E8022208, 
    levels=c(0.0,1.0,99.0), 
    labels=c("0: R not incarcerated in this month and not incarcerated in a previous month",
      "1: R was incarcerated during all or some of this month",
      "99: R incarcerated previously but not in this month"))
  data$E8022209 <- factor(data$E8022209, 
    levels=c(0.0,1.0,99.0), 
    labels=c("0: R not incarcerated in this month and not incarcerated in a previous month",
      "1: R was incarcerated during all or some of this month",
      "99: R incarcerated previously but not in this month"))
  data$E8022210 <- factor(data$E8022210, 
    levels=c(0.0,1.0,99.0), 
    labels=c("0: R not incarcerated in this month and not incarcerated in a previous month",
      "1: R was incarcerated during all or some of this month",
      "99: R incarcerated previously but not in this month"))
  data$E8022211 <- factor(data$E8022211, 
    levels=c(0.0,1.0,99.0), 
    labels=c("0: R not incarcerated in this month and not incarcerated in a previous month",
      "1: R was incarcerated during all or some of this month",
      "99: R incarcerated previously but not in this month"))
  data$E8022212 <- factor(data$E8022212, 
    levels=c(0.0,1.0,99.0), 
    labels=c("0: R not incarcerated in this month and not incarcerated in a previous month",
      "1: R was incarcerated during all or some of this month",
      "99: R incarcerated previously but not in this month"))
  data$R0000100[1.0 <= data$R0000100 & data$R0000100 <= 999.0] <- 1.0
  data$R0000100[1000.0 <= data$R0000100 & data$R0000100 <= 1999.0] <- 1000.0
  data$R0000100[2000.0 <= data$R0000100 & data$R0000100 <= 2999.0] <- 2000.0
  data$R0000100[3000.0 <= data$R0000100 & data$R0000100 <= 3999.0] <- 3000.0
  data$R0000100[4000.0 <= data$R0000100 & data$R0000100 <= 4999.0] <- 4000.0
  data$R0000100[5000.0 <= data$R0000100 & data$R0000100 <= 5999.0] <- 5000.0
  data$R0000100[6000.0 <= data$R0000100 & data$R0000100 <= 6999.0] <- 6000.0
  data$R0000100[7000.0 <= data$R0000100 & data$R0000100 <= 7999.0] <- 7000.0
  data$R0000100[8000.0 <= data$R0000100 & data$R0000100 <= 8999.0] <- 8000.0
  data$R0000100[9000.0 <= data$R0000100 & data$R0000100 <= 9999.0] <- 9000.0
  data$R0000100 <- factor(data$R0000100, 
    levels=c(0.0,1.0,1000.0,2000.0,3000.0,4000.0,5000.0,6000.0,7000.0,8000.0,9000.0), 
    labels=c("0",
      "1 TO 999",
      "1000 TO 1999",
      "2000 TO 2999",
      "3000 TO 3999",
      "4000 TO 4999",
      "5000 TO 5999",
      "6000 TO 6999",
      "7000 TO 7999",
      "8000 TO 8999",
      "9000 TO 9999"))
  data$R0536300 <- factor(data$R0536300, 
    levels=c(0.0,1.0,2.0), 
    labels=c("No Information",
      "Male",
      "Female"))
  data$R0536401 <- factor(data$R0536401, 
    levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0), 
    labels=c("1: January",
      "2: February",
      "3: March",
      "4: April",
      "5: May",
      "6: June",
      "7: July",
      "8: August",
      "9: September",
      "10: October",
      "11: November",
      "12: December"))
  data$R1235800 <- factor(data$R1235800, 
    levels=c(0.0,1.0), 
    labels=c("Oversample",
      "Cross-sectional"))
  data$R1482600 <- factor(data$R1482600, 
    levels=c(1.0,2.0,3.0,4.0), 
    labels=c("Black",
      "Hispanic",
      "Mixed Race (Non-Hispanic)",
      "Non-Black / Non-Hispanic"))
  return(data)
}

varlabels <- c("2002 INCARCERATION STATUS L1",
  "2002 INCARCERATION STATUS L2",
  "2002 INCARCERATION STATUS L3",
  "2002 INCARCERATION STATUS L4",
  "2002 INCARCERATION STATUS L5",
  "2002 INCARCERATION STATUS L6",
  "2002 INCARCERATION STATUS L7",
  "2002 INCARCERATION STATUS L8",
  "2002 INCARCERATION STATUS L9",
  "2002 INCARCERATION STATUS L10",
  "2002 INCARCERATION STATUS L11",
  "2002 INCARCERATION STATUS L12",
  "PUBID - YTH ID CODE 1997",
  "KEY!SEX (SYMBOL) 1997",
  "KEY!BDATE M/Y (SYMBOL) 1997",
  "KEY!BDATE M/Y (SYMBOL) 1997",
  "CV_SAMPLE_TYPE 1997",
  "KEY!RACE_ETHNICITY (SYMBOL) 1997"
)


# Use qnames rather than rnums

qnames = function(data) {
  names(data) <- c("INCARC_STATUS_2002.01_XRND",
    "INCARC_STATUS_2002.02_XRND",
    "INCARC_STATUS_2002.03_XRND",
    "INCARC_STATUS_2002.04_XRND",
    "INCARC_STATUS_2002.05_XRND",
    "INCARC_STATUS_2002.06_XRND",
    "INCARC_STATUS_2002.07_XRND",
    "INCARC_STATUS_2002.08_XRND",
    "INCARC_STATUS_2002.09_XRND",
    "INCARC_STATUS_2002.10_XRND",
    "INCARC_STATUS_2002.11_XRND",
    "INCARC_STATUS_2002.12_XRND",
    "PUBID_1997",
    "KEY_SEX_1997",
    "KEY_BDATE_M_1997",
    "KEY_BDATE_Y_1997",
    "CV_SAMPLE_TYPE_1997",
    "KEY_RACE_ETHNICITY_1997")
  return(data)
}


#********************************************************************************************************

# Remove the '#' before the following line to create a data file called "categories" with value labels. 
#categories <- vallabels(new_data)

# Remove the '#' before the following lines to rename variables using Qnames instead of Reference Numbers
#new_data <- qnames(new_data)
#categories <- qnames(categories)

# Produce summaries for the raw (uncategorized) data file
summary(new_data)

# Remove the '#' before the following lines to produce summaries for the "categories" data file.
#categories <- vallabels(new_data)
#summary(categories)

#************************************************************************************************************

