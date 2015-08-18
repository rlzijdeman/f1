library(curl)
library(XML)
library(data.table)
library(ggplot2)



### INFO ON F1 FILES
#u <- "http://www.fia.com/events/fia-formula-one-world-championship/season-2015/qualifying-classifications-2"
#test <- readHTMLTable(u)
#test[[1]]
#test[[2]] # sector
#test[[3]] # position by number
#test[[4]] # speed

# function to derive qualification table from F1
qualifun <- function(x) {
    url <- "http://www.fia.com/events/fia-formula-one-world-championship/season-2015/qualifying-classifications-"
    doc <- htmlParse(paste0(url,x))
    tableNodes <- getNodeSet(doc, "//table")
    df <- readHTMLTable(tableNodes[[1]], 
                        colClasses = c("numeric", "character", 
                                       rep(c("character", "numeric"), 3)),
                        stringsAsFactors = FALSE)
    df <- df[2:nrow(df), ]
    names(df) <- c("pos", "driver", "q1", "laps1", "q2", "laps1", "q3", "laps3")
    df$race <- x # add number of race
    
    # change q1-q3 times to seconds
    timing <- function(x) {
        m <- as.numeric(substr(x, 1, 1))
        s <- as.numeric(substr(x, 3, 8))
        time <- m*60+s
        time
    }
    
    df$q1 <- timing(df$q1)
    df$q2 <- timing(df$q2)
    df$q3 <- timing(df$q3)
    
    df
}

data <- do.call("rbind", lapply(1:6, qualifun))

# subset rookies
rookies <- c("Carlos Sainz Jr.", "Max Verstappen", "Felipe Nasr",
             "Roberto Merhi Muntan", "Will Stevens")

data.rookies <- data[data$driver %in% rookies, ]
# tapply(as.numeric(data.rookies$pos), INDEX = data.rookies$driver, mean)
# tapply(as.numeric(data.rookies$q1), INDEX = data.rookies$driver, mean)

p1 <- ggplot(data.rookies, aes(x = driver, y = pos))
p1 + geom_boxplot() + 
    geom_jitter(position = position_jitter(width = .1)) +
    labs(list(title = "Qualifying Position (before penalties)",
                           x = "Driver",
                           y = "Position")) +
    theme_bw()

ggsave("")






str(x)

x

