#############################################
# Heetch data science task ##################
#############################################

#############################################
# Here I summarized the full code for the data science task at Heetch,
# the slide numbers link to my presentation I gave ("Heetch Data science task")
# author: Konstantin Kroeniger
#############################################

# load packages
library(h3r)
library(leaflet)
library(dplyr)
library(geosphere)
library(ggplot2)
library(ggvenn)
library(psych)
library(lubridate)
library(binom)

# working directory
path <- paste0(here::here(), "/code")
setwd(path)

###############################
# Data read in 
###############################

# preorder data
dat.preo <- read.csv(file = "../data/preorder.csv", na.strings = "")


# demand session
dat.demsess <- read.csv(file = "../data/demand_session.csv", na.strings = "")


# Price estimates
dat.price <- read.csv(file = "../data/price_estimates.csv", na.strings = "")


# booking_request
dat.booking <- read.csv(file = "../data/booking_requests.csv")


###############################
# Sanity checks on row logic, slide 5
###############################

# check if there is one line per demand_id in preorder data
table(duplicated(dat.preo$demand_id))
# FALSE 
# 139826
# --> demand_id is unique key


# check if there is one line per demand_ids times product_ids in demands
table(duplicated(dat.demsess[, c("demand_id", "product_id")]))
# FALSE 
# 345412
# --> demand_id x product_id is unique key


# check if there is one row per driver_id X order_id in bookings
table(duplicated(dat.booking[, c("driver_id", "order_id")]))
# FALSE 
# 131444
# --> driver_id X order_id is unique key


# check if there is one row per request_id X product_id in price estimates
table(duplicated(dat.price[, c("request_id", "product_id")]))
# FALSE   TRUE 
# 599113   5980
# --> 5980 duplicated entries --> investigate further


# extract request_ids with errors
IDs <- dat.price$request_id[duplicated(dat.price[, c("request_id", "product_id")])]

# extract sub data with errors
dat.price.error <- dat.price[dat.price$request_id %in% IDs,]

# order by request_id and product_id
dat.price.error <- dat.price.error[order(dat.price.error$request_id, 
                                         dat.price.error$product_id),]

# order by request_id and product_id
dat.price.error <- dat.price.error[order(dat.price.error$request_id, 
                                         dat.price.error$product_id),]

# extract first 6 columns for output
head(dat.price.error[, c("request_id", "product_id", 
                         "passenger_id", "demand_id", "created_at")])


# to continue --> cut digits for seconds
dat.price$created_at  <- substr(dat.price$created_at, start = 1, stop = 19)

# remove duplicates from data set
dat.price <- unique(dat.price)



###############################
# Sanity checks on links by demand_ids, order_ids , slide 6
###############################


# Venn diagram of demand IDs in the three connected data sets
IDs <- list(preorder = sort(unique(dat.preo$demand_id)),
            demand_session = sort(unique(dat.demsess$demand_id)),
            price = sort(unique(dat.price$demand_id)))

# output
ggvenn(IDs)


# Venn diagram of order IDs in the two connected data sets
IDs <- list(price = sort(unique(dat.price$order_id)),
            booking = sort(unique(dat.booking$order_id)))

# output
ggvenn(IDs)



###############################
# Extracting background information, Time period , slide 7
###############################


# change class of time information
for(col in c("preorder_started_at", "preorder_ended_at")){
  dat.preo[, col] <- as.POSIXct(x = dat.preo[, col])
}

# add preorder duration in minutes
dat.preo$preorder_duration_mins <- as.numeric(difftime(time1 = dat.preo$preorder_ended_at,
                                                       time2 = dat.preo$preorder_started_at,
                                                       units = "min"))

# transform ETAs to numerics
for(col in c("dropoff_screen_eta", "payments_screen_eta")){
  dat.preo[, col] <- as.numeric(dat.preo[, col])
}


# output
summary(dat.preo)



###############################
# Extracting background information, Passengers , slide 8
###############################

# amount of passengers in demand data set
length(unique(dat.demsess$passenger_id))
# 44395

# extract products per passenger
tmp <- unique(dat.demsess[, c("passenger_id", "product_id")])
table(tmp$product_id)
# product_A product_B 
# 44395     44395
# --> every passenger has both products

# demands per passenger
tmp <- as.data.frame(table(
  dat.demsess[dat.demsess$product_id %in% "product_A", "passenger_id"]))

# output
ggplot(data = tmp, aes(y = Freq)) +
  geom_boxplot() +
  xlim(c(-1,1)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  ylab("demand_ids per passenger")



###############################
# Extracting background information, Location , slide 9
###############################

# transform geoindex data using "Hierarchical Geospatial Indexing"
dat.geo <- unique(dat.demsess[, c("passenger_id", "pickup_geoindex")])
dat.geo <- cbind(dat.geo,
                 h3r::cellToLatLng(dat.geo$pickup_geoindex))

# output
head(dat.geo)

# output
leaflet(data = dat.geo) %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  setView(lng = mean(dat.geo$lng),
          lat = mean(dat.geo$lat), zoom = 1) %>%
  addCircleMarkers(
    lng = ~lng,
    lat = ~lat,
    radius = 3,          
    stroke = FALSE,     
    fillOpacity = 0.5
  )



##################
# Validation probability, slide 11
##################

# extract data for model
dat.fit <- dat.price[!is.na(dat.price$is_validated_order), 
                     c("is_validated_order","product_id", "multiplier", 
                       "passenger_price", "estimated_trip_duration", 
                       "estimated_trip_distance")]

# estimate validation probability in data set, output
binom.confint(x = sum(dat.fit$is_validated_order %in% "true"),
              n = nrow(dat.fit), methods = "exact")

# transform columns for model fit
dat.fit$is_validated_order <- ifelse(dat.fit$is_validated_order %in% "true", 1, 0)
dat.fit$product_id_A <- ifelse(dat.fit$product_id %in% "product_A", 1, 0)
dat.fit$product_id <- NULL

# model fit, output
summary(glm(is_validated_order~., data = dat.fit, family = binomial(link = "logit")))

# bar plot output
dat.plot <- dat.price[!is.na(dat.price$is_validated_order), 
                      c("is_validated_order", "product_id")]

ggplot(data = dat.plot, aes(x = is_validated_order, fill = product_id)) +
  geom_bar(stat = "count")



##################
# Termination requests probability, slide 12
##################


# extract data for model
dat.fit <- dat.price[dat.price$is_validated_order %in% "true", 
                     c("order_state", "product_id", "multiplier", 
                       "passenger_price", "estimated_trip_duration", "estimated_trip_distance")]

# estimate termination requests probability in data set, output
binom.confint(x = sum(dat.fit$terminated == 1), n = nrow(dat.fit), 
              methods = "exact")

# transform columns for model fit
dat.fit$terminated <- ifelse(dat.fit$order_state %in% "terminated", 1, 0)
dat.fit$order_state <- NULL
dat.fit$product_id_A <- ifelse(dat.fit$product_id %in% "product_A", 1, 0)
dat.fit$product_id <- NULL

# model fit, output
summary(glm(terminated~., data = dat.fit, family = binomial(link = "logit")))

# boxplot, output
dat.fit$terminated <- as.factor(dat.fit$terminated)

ggplot(data = dat.fit, aes(x = terminated, y = passenger_price)) +
  geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(limits = quantile(dat.fit$passenger_price, c(0.1,0.9)))



##################
# Price margin, slide 13
##################


# extract terminated prices
dat.fit <- dat.price[(dat.price$order_state %in% "terminated"),]

# merge with br_net_driver_price from dat.booking
dat.fit <- merge(dat.fit, dat.booking[, c("order_id", "br_net_driver_price")],
                 by = "order_id", all.x = FALSE, all.y = FALSE)

# determine price margin
dat.fit$price_margin <- dat.fit$passenger_price - dat.fit$br_net_driver_price

# extract relevant columns
dat.fit <- dat.fit[, c("product_id", "multiplier", "estimated_trip_duration", 
                       "estimated_trip_distance","price_margin")]

# median price margin and IQR, output
quantile(dat.fit$price_margin)

# transform columns for model fit
dat.fit$product_id_A <- ifelse(dat.fit$product_id %in% "product_A", 1, 0)
dat.fit$product_id <- NULL

# model fit, output
summary(lm(price_margin~., data = dat.fit))

# panels plot, output
pairs.panels(dat.fit[, c("estimated_trip_duration", "price_margin")])



##################
# Driver acceptance probability, slide 14
##################


# extract data for model
dat.fit <- dat.booking

# transform columns for model fit
dat.fit$br_accepted <- ifelse(dat.fit$br_state %in% "accepted", 1, 0)
dat.fit$hours_from_noon <- abs(12 - hour(dat.fit$br_created_at))
dat.fit$product_id_A <- ifelse(dat.fit$product_id %in% "product_A", 1, 0)
dat.fit$product_id <- NULL

# estimate driver acceptance probability in data set, output
binom.confint(x=sum(dat.fit$br_accepted), n = nrow(dat.fit), 
              methods = "exact")

# extract data for model fit
dat.fit <- dat.fit[, c("br_accepted", "br_estimated_approach_duration", 
                       "br_estimated_approach_distance", "br_net_driver_price",
                       "hours_from_noon","product_id_A")]

# model fit, output
summary(glm(br_accepted~., data = dat.fit, family = binomial(link = "logit")))

# violin plot, output
dat.fit$br_accepted <- as.factor(dat.fit$br_accepted)

ggplot(data = dat.fit, aes(x = br_accepted, y = br_estimated_approach_duration)) +
  geom_violin()



##################
# Example of additional analyses, slide 15
##################

# extract data set for terminations per day
tmp <- dat.price[(dat.price$is_validated_order %in% "true"), 
                 c("order_state", "order_created_at")]

# transform columns
tmp$terminated <- ifelse(tmp$order_state %in% "terminated", 1, 0)
tmp$order_state <- NULL

# extract day
tmp$order_created_at <- substr(tmp$order_created_at, start = 1, stop = 10)

# derive probability estimate per day
tmp <- aggregate(terminated~order_created_at, data = tmp, mean)
tmp$order_created_at <- as.POSIXct(tmp$order_created_at)

# dot plot, output
ggplot(data = tmp, aes(order_created_at, terminated)) +
  geom_point() +
  ylab("Termination request probability")




