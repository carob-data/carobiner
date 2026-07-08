
adm_pointRadius <- function(country, adm, cache_path=NULL, method="high_probability") {
	stopifnot(packageVersion("terra") >= "1.9.37")
	method <- match.arg(tolower(method), c("high_probability", "low_uncertainty"))
	
	if (inherits(country, "SpatVector")) {
		adm <- NA
		g <- country
	} else {
		adm <- round(adm[1])
		stopifnot(isTRUE(adm >= 0 & adm < 6))
		if (is.null(cache_path)) {
			g <- geodata::gadm(country, level=adm)
		} else {
			g <- geodata::gadm(country, level=adm, cache_path)	
		}
	}
	
	if (method == "high_probability") {
		xy <- terra::centroids(g, inside=FALSE, correct=TRUE)
	} else {
		h <- terra::hull(g, "circle", NA)
		hc <- terra::centroids(h)
		## correct: move hc to g if needed
		xy <- terra::snapeTo(hc, g)
	}
	unc <- round(terra::furdist(xy, g, pairwise=TRUE))
	xy <- round(terra::crds(xy), 4)
	colnames(xy) <- c("longitude", "latitude")
	if (isTRUE(adm >= 0)) {
		names <- paste0("NAME_", 1:adm)
		ad <- g[, names, drop=TRUE]
		names(ad) <- gsub("NAME_", "adm", names(ad))
		data.frame(country=country, ad, xy, geo_uncertainty=unc, geo_source=paste0("GADM 4.1, adm", adm))
	} else {
		data.frame(values(g), xy, geo_undertainty=unc, geo_source=NA)	
	}
}


geocode_duplicates <- function(x, vars=NULL) {
	if (is.null(vars)) {
		cn <- colnames(x)
		adm <- grep("adm", cn, value=TRUE)
		vars <- c("country", "location", adm)
	}
	uxy <- unique(x[, c(vars, "longitude", "latitude")])
	i <- is.na(uxy$longitude)
	xy <- uxy[i,]
	xy_notna  <- uxy[!i,]
	xy$longitude <- NULL
	xy$latitude <- NULL
	m <- unique(merge(xy, xy_notna, by=vars))
	m <- m[!duplicated(m[, vars]), ]
	cn <- colnames(m)
	cn <- gsub("longitude", "xlon", cn)
	cn <- gsub("latitude", "ylat", cn)
	colnames(m) <- cn
	x <- merge(x, m, by=vars, all.x=TRUE)
	i <- is.na(x$longitude)
	x$longitude[i] <- x$xlon[i]
	i <- is.na(x$latitude)
	x$latitude[i] <- x$ylat[i]
	x$xlon <- NULL 
	x$ylat <- NULL
	x
}
 

geocode_geonames <- function(place, username=NULL) { 
	# Author: Effie Ochieng' (efyrouwa)
	stopifnot(!is.null(username))
    u <- paste0("http://api.geonames.org/searchJSON?q=", place, "&username=", username) 
	out <- rep("", length(u))
	for (i in 1:length(u)) {
		print(place[i]); utils::flush.console()
		response <- try( httr::GET(u[i]) )
		if (!inherits(response, "try-error")) {
			out[i] <- httr::content(response, as="text")  
		} 
	}
	out
}
  
  
geocode_nominatim <- function(place, input) {
	url <- "https://nominatim.openstreetmap.org/search?q="
	u <- paste0(url, place, "&format=geojson&polygon_geojson=1")
	out <- rep("", length(u))
	for (i in 1:length(u)) {
		print(place[i]); utils::flush.console()
		response <- try( httr::GET(u[i]) )
		if (!inherits(response, "try-error")) {
			out[i] <- httr::content(response, as="text")  
		} 
	}
	x <- sapply(out, terra::svc)
	names(x) <- place
	
	y <- lapply(x, function(s) {
		if (length(s) == 0) {
			cbind(NA, NA)
		} else {
			v <- s[1]
			v <- v[which.min(v$place_rank)]
			terra::crds(terra::centroids(v, inside=TRUE))
		}
	})
	
	y <- do.call(rbind, y)
	y <- round(y, 4)
	colnames(y) <- c("lon", "lat")
	y <- data.frame(input, y)
	
	put <- parse(text = utils::capture.output(dput(y[!is.na(y$lon), ])))[[1]]
	list(df=y, svc=x, put=put)
} 


geocode <- function(country, location, adm1=NULL, adm2=NULL, adm3=NULL, adm4=NULL, adm5=NULL, service="nominatim", ...) {
	service <- tolower(service)
	service <- match.arg(service, c("nominatim", "geonames"))

	stopifnot(!any(is.na(country)))
	input <- cbind(country, adm1, adm2, adm3, adm4, adm4, location)
	addr <- trimws(input)
	addr[is.na(addr)] <- ""
	noc <- addr[,-1, drop=FALSE]
	noc <- apply(noc, 1, function(i) paste0(i, collapse=""))
	if (any(noc=="")) {
		stop("all records must include a country and a place or admin name")
	}
	addr <- apply(addr, 1, function(v) paste(v, collapse=","))
	addr <- gsub("(,)\\1+", "\\1", addr)
	addr <- gsub("( )\\1+", "\\1", addr)
	addr <- gsub(", ,", ",", addr)
	addr <- gsub(" ", "+", addr)
	addr <- utils::URLencode(addr)
	
	if (service == "nominatim") {
		geocode_nominatim(addr, input)
	} else {
		geocode_geonames(addr, ...)	
	}	
}

#g <- geocode("Kenya", c("Machakos", "Nairobi"))
  