readTCX

library(xml2)

# read xml file
doc <- read_xml(file)

# namespaces
ns <- xml2::xml_ns(doc)

children_names <- function(x, xpath, ns) {
  unique(xml2::xml_name(xml2::xml_children(xml2::xml_find_all(x, xpath, ns))))
}

## Core namespaces
activity_ns <- names(which(ns == "http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2")[1])
## https://www8.garmin.com/xmlschemas/ActivityExtensionv2.xsd
## https://www8.garmin.com/xmlschemas/ActivityExtensionv1.xsd
extensions_ns <- c("http://www.garmin.com/xmlschemas/ActivityExtension/v2",
                   "http://www.garmin.com/xmlschemas/ActivityExtension/v1")
extensions_ns <- na.omit(sapply(extensions_ns, function(e) names(which(ns == e)[1])))

## Guess sport from data
sport <- guess_sport(xml_attr(xml_find_first(doc, paste0("//", activity_ns, ":", "Activity")), "Sport"))
## If not successful, try filename
if (is.na(sport)) {
  sport <- guess_sport(basename(file))
}

## Trackpoints
tp_xpath <- paste0("//", activity_ns, ":", "Trackpoint")
ch_names <- children_names(doc, tp_xpath, ns)
if (length(ch_names) == 0) {
  stop("No usable data have been found in ", file)
}
tp_vars <- data.frame(name = ch_names,
                      ns = activity_ns)

## Position
position_xpath <- paste0("//", activity_ns, ":", "Position")
## Add any nested fields here
is_position <- tp_vars$name == "Position"
if (any(is_position)) {
  ## remove position
  tp_vars <- tp_vars[!is_position, ]
  ## Add longitude/latitude
  children <- data.frame(name = children_names(doc, position_xpath, ns[activity_ns]),
                         ns = activity_ns)
  tp_vars <- rbind(tp_vars, children)
}

## Extensions
is_extensions <- tp_vars$name == "Extensions"
if (any(is_extensions)) {
  ## remove position
  tp_vars <- tp_vars[!is_extensions, ]
  for (e in extensions_ns) {
    e_xpath <- paste0("//", e, ":", "TPX")
    ## Add any extensions
    ch_nam <- children_names(doc, e_xpath, ns[e])
    if (length(ch_nam)) {
      children <- data.frame(name = ch_nam, ns = e)
      tp_vars <- rbind(tp_vars, children)
    }
  }
}
is_time <- tp_vars$name == "Time"

tps <- xml_find_all(doc, tp_xpath, ns[activity_ns])
## Double loop to extract obs
observations <- apply(tp_vars, 1, function(var) {
  c_xpath <- paste0(".", "//", var["ns"], ":", var["name"])
  c_ns <- ns[var["ns"]]
  sapply(tps, function(x) {
    xml_text(xml_find_first(x, c_xpath, c_ns))
  })
})

observations <- as.data.frame(observations, stringsAsFactors = FALSE)

names(observations) <- tp_vars$name

## Convert to numeric
observations[!is_time] <- apply(observations[!is_time], 2, as.numeric)

## human names
allnames <- generate_variable_names()
namesOfInterest <- allnames$tcx2_names
namesToBeUsed <- allnames$human_names
inds <- match(namesOfInterest, names(observations), nomatch = 0)
observations <- observations[inds]
names(observations) <- namesToBeUsed[inds!=0]

## coerce time into POSIXct
observations$time <- gsub("[\t\n]", "", observations$time)
observations$time <- convertTCXTimes2POSIXct(observations$time, timezone = timezone)

## Add missing varibles
missingVars <- namesToBeUsed[match(namesToBeUsed, names(observations), nomatch = 0) == 0]
if (nrow(observations) > 0) {
  for (nn in missingVars) {
    observations[[nn]] <- NA
  }
}

## convert speed from speedunit to m/s
if (speedunit != "m_per_s") {
  speedConversion <- match.fun(paste(speedunit, "m_per_s", sep = "2"))
  observations$speed <- speedConversion(observations$speed)
}

## convert distance from distanceunit to m
if (distanceunit != "m") {
  distanceConversion <- match.fun(paste(distanceunit, "m", sep = "2"))
  observations$distance <- distanceConversion(observations$distance)
}

## use variable order for trackeRdata
if (any(names(observations) != allnames$human_names)) {
  observations <- observations[, allnames$human_names]
}

attr(observations, "sport") <- sport
attr(observations, "file") <- file

return(observations)
