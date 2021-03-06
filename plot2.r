
# Function for loading a smaller interval from a dataset in a CSV.
# The csv must contain columns name "Date" and "Time"
# in the following format: %d/%m/%Y and %H:%M:%S
loadIntervalCSV <- function (pathToCsv, firstDay, nDays = 1) {
  # Load first row
  firstRow <- read.csv(pathToCsv, sep=";", nrows=1)
  
  # Epoch of the first record
  epochFirstRow <- as.integer(
    as.POSIXct(paste(firstRow$Date, firstRow$Time), format="%d/%m/%Y %H:%M:%S")
  )
  
  # Epoch of the first minute of a given day
  epochStart <- as.integer(
    as.POSIXct(paste(firstDay, "00:00:00"), format="%d/%m/%Y %H:%M:%S")
  )
  
  # Number of minutes between of the first record to be read i.e. rows to skip
  skipRows = (epochStart - epochFirstRow) / 60
  
  # Number of minutes in n days
  minutes2days = 24 * 60 * nDays
  
  # Load data
  dataSet <- read.csv("household_power_consumption.txt", sep=";", 
                      skip=skipRows, nrows=minutes2days)
  
  # Set colnames in dataSet
  colnames(dataSet) <- colnames(firstRow)

  return(dataSet)
}

# Load 2007-02-01 and 2007-02-02
dataSet <- loadIntervalCSV("household_power_consumption.txt", "01/02/2007", nDays = 2)

# Set bg color transparent
par(bg = "transparent")

# Start device
png(file = "plot2.png")

# Adjust Locale
#Sys.setlocale("LC_TIME", "en_US.utf8")

# Draw plot 2
with(dataSet, 
  plot(Global_active_power ~ as.POSIXct(paste(Date, Time), format="%d/%m/%Y %H:%M:%S"),
       ylab="Global Active Power (kilowatts)",
       xlab="",
       type="l")
)

# Close the device
dev.off()
