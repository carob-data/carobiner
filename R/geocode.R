


adm_pointRadius <- function(country, adm, cache_path=NULL, method="high_probability", geo=NULL) {
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
	
	if (!is.null(geo)) {
		alev <- paste0("adm", adm)
		blev <- paste0("NAME_", adm)
		if (!(alev %in% names(geo))) {
			stop(paste(alev, "not in names(geo)"), call.=FALSE)
		}
		ugeo <- unique(geo[, alev])
		uadm <- unique(unlist(g[,blev,drop=TRUE]))
		i <- match(ugeo, uadm)
		if (any(is.na(i))) {
			error(paste(alev, " not in boundaries file: ", paste(ugeo[is.na(i)], collapse=",")), call.=FALSE)
		}
		i <- match(geo[, alev], unlist(g[,blev,drop=TRUE]))
		xy <- terra::vect(geo, c("longitude", "latitude"), crs="lonlat")
		g <- g[i, ]
		r <- terra::is.related(xy, g, "intersects")
		if (!all(r)) {
			warning(paste(sum(!r), " points not inside matching polygon"))
		}
		unc <- round(terra::furdist(xy, g, pairwise=TRUE))
		xy <- geo[, c("longitude", "latitude")]
	} else {	
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
	}
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


dms_to_dd <- function(d, m=0, s=0, hem=NULL, default_hem=NULL) {
	if (!is.null(default_hem)) {
		default_hem <- toupper(trimws(as.character(default_hem)))
		if (length(default_hem) != 1L || !(default_hem %in% c("N", "S", "E", "W"))) {
			stop("default_hem must be one of 'N', 'S', 'E', or 'W'", call.=FALSE)
		}
	}

	validate_hem <- function(h, default_hem) {
		valid <- c("N", "S", "E", "W")
		bad <- !is.na(h) & !(h %in% valid)
		if (any(bad)) {
			warning("invalid hemisphere value(s): ", paste(unique(h[bad]), collapse=", "), call.=FALSE)
			h[bad] <- NA_character_
		}
		present <- unique(h[!is.na(h)])
		if (any(present %in% c("N", "S")) && any(present %in% c("E", "W"))) {
			warning("mixed latitude (N/S) and longitude (E/W) hemispheres in one vector", call.=FALSE)
		}
		if (!is.null(default_hem)) {
			if ((default_hem %in% c("N", "S") && any(present %in% c("E", "W"))) ||
				(default_hem %in% c("E", "W") && any(present %in% c("N", "S")))) {
				stop("default_hem ('", default_hem, "') does not match hemisphere axis in data", call.=FALSE)
			}
		}
		h
	}

	# Single-argument DMS string mode
	if (missing(m) && missing(s) && is.null(hem)) {
		if (is.numeric(d)) {
			return(as.numeric(d))
		}
		x <- trimws(as.character(d))
		out <- rep(NA_real_, length(x))
		h_all <- rep(NA_character_, length(x))
		for (i in seq_along(x)) {
			z <- x[i]
			if (is.na(z) || !nzchar(z)) next
			z <- toupper(z)
			z <- gsub("[\u2032\u2019]", "'", z)
			z <- gsub("[\u2033\u201C\u201D]", "\"", z)
			hm <- regmatches(z, regexpr("[NSEW]", z))
			h <- if (length(hm) == 1L && nzchar(hm)) hm else NA_character_
			h_all[i] <- h
			neg <- grepl("^-", z)
			ms <- gregexpr("[0-9]+(?:\\.[0-9]+)?", z)
			nums <- as.numeric(regmatches(z, ms)[[1]])
			if (length(nums) == 0L || length(nums) > 3L || any(is.na(nums))) next
			deg <- nums[1]
			min <- if (length(nums) >= 2L) nums[2] else 0
			sec <- if (length(nums) >= 3L) nums[3] else 0
			val <- deg + min / 60 + sec / 3600
			if (is.na(h) && !is.null(default_hem)) {
				h <- default_hem
			}
			if (!is.na(h)) {
				if (h %in% c("S", "W")) val <- -val
			} else if (neg) {
				val <- -val
			}
			out[i] <- val
		}
		validate_hem(h_all, default_hem)
		return(out)
	}

	# Component vector mode (d, m, s, hem)
	clean_num <- function(v) {
		if (is.numeric(v)) return(as.numeric(v))
		v <- trimws(as.character(v))
		v[v %in% c("", ".", "NA", "na")] <- NA_character_
		as.numeric(v)
	}
	dn <- clean_num(d)
	mn <- clean_num(m)
	sn <- clean_num(s)
	n <- length(dn)
	if (length(mn) == 1L) mn <- rep(mn, n)
	if (length(sn) == 1L) sn <- rep(sn, n)

	dec <- abs(dn) + mn / 60 + sn / 3600
	if (!is.null(hem)) {
		h <- toupper(trimws(as.character(hem)))
		h[h %in% c("", ".", "NA")] <- NA_character_
		h <- validate_hem(h, default_hem)
		miss <- is.na(h) & !is.na(dec)
		if (any(miss)) {
			if (!is.null(default_hem)) {
				h[miss] <- default_hem
			} else {
				warning("missing hemisphere value(s) with no default_hem; returning NA", call.=FALSE)
				dec[miss] <- NA_real_
			}
		}
		dec[!is.na(h) & h %in% c("S", "W")] <- -dec[!is.na(h) & h %in% c("S", "W")]
		dec[is.na(h)] <- NA_real_
	} else if (!is.null(default_hem)) {
		if (default_hem %in% c("S", "W")) {
			dec <- -dec
		}
	} else {
		dec[!is.na(dn) & dn < 0] <- -dec[!is.na(dn) & dn < 0]
	}
	dec
}
