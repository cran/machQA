Sys.setenv(TZ="UTC")

# function to connect to Machina
mach.Go <-
  function(    
    username = NULL,
    password = NULL,
    model = NULL)
    {
      startSession(username, password)
      m <- openModel(model)
}

# function to run the QA suite on the indicators
mach.QA <-
  function(
    ticker = NULL,
    day = NULL)
    {
        indicators = c("sma", "wavg", "xavg", "hma", "adma", "tsi", "rsi", "gauss", "momo", "t3", "macd")
        today <- gsub("-", "", Sys.Date())

        # Create new directory for today's output files
        output_filepath <- paste(getwd(), "/R_output_files/", sep = "")
        dir.create(output_filepath, showWarnings = FALSE)
        file_path <- paste(output_filepath, today, "/", sep = "")
        dir.create(file_path, showWarnings = FALSE)
        
        # create TS dataframe and csv for ticker and day
        base_filepath = file.path(path.package("machQA"), "")
        full_file <- paste(base_filepath, "/TS_files/", ticker, "1minuteTopStudies.csv", sep = "")
        full <- read.csv(paste(sep = "", full_file), header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE)
        Date = NULL
        temp <- data.frame(subset(full, Date==day))
        
        # read base differences file into data frame
        baseline_file <- paste(base_filepath, "/base_files/", ticker, "_base.csv", sep="")
        base_diff <- read.csv(baseline_file, header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE)
        temp_base <- data.frame(subset(base_diff, Date==day))
        
        nCount <- length(indicators)
        for(n in 1:nCount)
        {
          ind <- indicators[n]
          
          # creates the query and a dataframe named with the ticker_indicator, for example ibm_sma
          times <- gsub(":", "", gsub("-", "", gsub(" ", "_", Sys.time())))
          query <- paste(ticker, " | ", ind, "(12)")
          mach_q <- addRow(query, includeData=T, startDate = day, endDate = day, updateModel = F)
          tick_indi <- paste(ticker, "_", ind, "_", day, "_", times, sep = "")
            
          ## this creates CSV files, in case they are desired
          filename <- paste(tick_indi, ".csv", sep = "")
          write.csv(as.data.frame(mach_q$ts), file=paste(file_path, filename, sep = ""))
            
          #bring data from the CSV back to new dataframe for more manipulation
          mach_q <- as.data.frame(read.csv(file=paste(file_path, filename, sep = ""), header = FALSE, stringsAsFactors = FALSE, quote = "\"", na.strings="unknown", skip = 41, col.names = c("Date", indicators[n]), sep = ","))
            
          # split the Date column into Date and Time, delete Date (we only use one day so date not needed)
          mach_q$Time <- unlist(lapply(strsplit(as.character(mach_q$Date), " "), "[", 2))
          mach_q <- mach_q[,-1]
          mach_q$Time <- gsub(':', '', gsub('"', '', mach_q$Time))
            
          # reorder columns
          mach_q <- mach_q[ , c('Time', ind)]
            
          # add TS column to machina data frame
          mach_q$TS <- temp[,(n + 2)]
          
          # calculate difference between machina and TS indicator calculations
          suppressWarnings(mach_q$diff <- as.numeric(mach_q[,3]) - as.numeric(mach_q[,2]))
          
          # add baseline differences to data frame
          mach_q$base_diff <- temp_base[,(n + 2)]
          
          # calculate difference between current and base difference
          new_diff = NULL
          suppressWarnings(mach_q$new_diff <- as.numeric(mach_q[,5]) - as.numeric(mach_q[,4]))
          mach_q[,'new_diff'] <- round(mach_q[,'new_diff'],4)
          
          # Count number of times each result occurs
          mach_q_num <- count(mach_q, 'new_diff')
          
          #assign new dataframe to object named with ticker, day, timestamp
          assign(tick_indi, mach_q)
          
          # Write the dataframe to a csv file in case further investigation is needed
          filename <- paste(tick_indi, ".csv", sep = "")
          write.csv(as.data.frame(mach_q), file=paste(file_path, filename, sep = ""))
          
          # Print results for each query
          sub <- subset(mach_q_num, new_diff == 0)$freq
          if ( sub != 350 )
          {
            print("")
            print(paste("Results for ", toupper(ind), sep = ""))
            print(as.data.frame(mach_q_num))
            print(paste("(Full results in '", file_path, filename, "')", sep = ""))
          } else {
            print(paste("Results for ", toupper(ind), ": PASS!", sep = ""))
          }
        }
}