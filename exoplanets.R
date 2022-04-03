library(rvest)
library(dplyr)
library(jsonlite)


xpath <- "/html/body/div[3]/div[3]/div[5]/div[1]/table[2]"
url <- "https://en.wikipedia.org/wiki/List_of_potentially_habitable_exoplanets"

####### Data Retrieval #######
exoplanets <- url %>%
  read_html() %>%
  html_nodes(xpath = xpath) %>%
  html_table(header = TRUE) %>%
  bind_rows() %>%
  as_tibble()

####### Data Processing #######

####### Data Cleaning #######
exoplanets <- exoplanets %>%
  rename_with( ~ gsub( " \\(.*", "", .x)) %>% # Removing mathematical symbols from column name
  select(-c("Star", "Star type", "Mass", "Density", "Refs/Notes")) # Removing columns not needed for vis

# Registring rows that contain "-" or "" as NA and then omitting NA
exoplanets[exoplanets==""]<-NA
exoplanets[exoplanets=="—"]<-NA
exoplanets <- na.omit(exoplanets)

clean_convert <- function(x) {
  x <- gsub("[~><≥]", "", x) %>% as.numeric(x) #removing non number chars and then converting to numeric
  return(x)
}

exoplanets <- exoplanets %>% mutate(across("Radius":"Distance", clean_convert))

####### Scaling #######
# Log scale to "compact" values for visualization purposes
exoplanets$Scaled_Distance <- exoplanets$Distance %>%
  sapply(function(x) { log(x, exp(10)) })

exoplanets[exoplanets==slice(exoplanets, 1)$Scaled_Distance] <- 0 # log of 0 for earth

# Log Scaling Period
exoplanets$Scaled_Period <- exoplanets$Period %>%
  sapply(function(x) { log(x) })

# Scaling (normalizing) Flux so it can be used as HSL saturation parameter
exoplanets$Scaled_Flux <- exoplanets$Flux %>%
  sapply(function(x) { as.integer(((x - min(exoplanets$Flux)) / (max(exoplanets$Flux) - min(exoplanets$Flux)))*100) })

####### Modifying dataset for visualization #######
# Sorting by distance (that will be order of presentation and is needed for later calculation)
exoplanets <- exoplanets[order(exoplanets$Scaled_Distance),]

# Calculating distance to previous planet (regarding distance)
# Copying scaled distance column
exoplanets$Scaled_Distance_Previous <- exoplanets$Scaled_Distance

for (row in 2:nrow(exoplanets)) {
  exoplanets[row, "Scaled_Distance_Previous"] <- as.numeric(exoplanets[row, "Scaled_Distance"] - exoplanets[row-1, "Scaled_Distance"])
}

# Adding column containing categories of colors for water (or background color) assigned based on Flux
# calculating steps
steps <- seq(min(exoplanets$Teq), max(exoplanets$Teq), as.integer(max(exoplanets$Teq)-min(exoplanets$Teq))/6) # 6 colors

#assigning color categories
exoplanets$Water_Color <- NA
exoplanets$Water_Color[exoplanets$Teq >= steps[6]  & exoplanets$Teq <= steps[7]] <- "A"
exoplanets$Water_Color[exoplanets$Teq >= steps[5]  & exoplanets$Teq < steps[6]] <- "B" # Earth Water Color
exoplanets$Water_Color[exoplanets$Teq >= steps[4]  & exoplanets$Teq < steps[5]] <- "C"
exoplanets$Water_Color[exoplanets$Teq >= steps[3]  & exoplanets$Teq < steps[4]] <- "D"
exoplanets$Water_Color[exoplanets$Teq >= steps[2] & exoplanets$Teq < steps[3]] <- "E"
exoplanets$Water_Color[exoplanets$Teq >= steps[1] & exoplanets$Teq < steps[2]] <- "F"



####### Adding column for names, based on Wikipedia list of goddesses #######
url <- "https://en.wikipedia.org/wiki/List_of_goddesses"

goddesses <- url %>%
  read_html() %>%
  html_elements("li") %>%
  html_text2()

goddesses <- goddesses[152:2630]
#removing table of contents and stuff at end

#some quick noise removal
goddesses <- gsub("\\(.*", "", goddesses)
goddesses <- gsub("\\n.*", "", goddesses)
goddesses <- gsub(" - .*", "", goddesses)
goddesses <- trimws(goddesses)

names <- sample(goddesses, nrow(exoplanets)) #randomly selecting enough names

exoplanets$Name <- names #adding column with random names

exoplanets$Name[1]="Earth" #Earth doesnt need name

# Saving data
json <- toJSON(exoplanets, pretty=TRUE)
write(json, "exoplanets.json")






