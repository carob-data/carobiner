
.carob_environment <- new.env(parent=emptyenv())


check_packages <- function(name, version) {
	if (utils::packageVersion(name) < version) {
		if (name == "vocal") {
			stop(paste0('please update package ', name, " with:\n   remotes::install.github('controvoc/", name, "')"))
		} else {
			stop(paste0('please update package ', name, " with:\n   remotes::install.github('carob-data/", name, "')"))		
		}
	}
}


check_consistency <- function(x, answ) {
	#e.g. if OM is used, then the type and amount should be specified 
	if (!is.null(x$crop_price)) {
		if (is.null(x$currency)) {
			answ[nrow(answ)+1, ] <- c("no currency", "crop_price variable used, but currency variable missing")
		} else {
			if (any(is.na(x$currency) & !is.na(x$crop_price))) {
				answ[nrow(answ)+1, ] <- c("currency missing", "crop_price values without currency values found")
			}
		}
	}
	answ
}


check_cropyield <- function(x, answ) {
	if (!all(c("crop", "yield") %in% names(x))) return(answ)
	if (all(is.na(x$yield))) return(answ)
	x <- x[, c("crop", "yield")]
	x <- stats::na.omit(x)
	if (nrow(x) == 0) {
		return(answ)
	}
	a <- suppressWarnings(
			stats::aggregate(x[,"yield", drop=FALSE], x[, "crop", drop=FALSE], max, na.rm=TRUE)
		)
	a <- a[which(a$yield < 100), ]
	if (nrow(a) > 0) {
		crops <- unique(a$crop)
		bad <- paste(crops, collapse=", ")
		answ[nrow(answ)+1, ] <- c("low yield (tons not kg?)", bad)
		return(answ)
	}
	trms <- vocal::accepted_values("crop")
	trms <- trms[match(unique(x$crop), trms$name), c("name", "max_yield")]
	trms <- stats::na.omit(trms)
	if (nrow(trms) == 0) return(answ)
	x <- stats::na.omit(merge(x, trms, by=1))
	i <- x$yield > x$max_yield
	if (any(i)) {
		crops <- unique(x$crop[i])
		if (length(crops) == 1) {
			bad <- paste0(crops, ": ", max(x$yield, na.rm=TRUE))
		} else {
			bad <- paste(crops, collapse=", ")
		}
		answ[nrow(answ)+1, ] <- c("high crop yield", bad)	
	}
	#check_outliers_iqr(x, "yield", TRUE)

	answ
}


check_pubs <- function(x, path, answ) {
	if (isTRUE(nchar(x$publication) > 0 )) {
		if (!grepl("http", x$publication)) {
			allpubs <- list.files(file.path(path, "references"))
			pubs <- unlist(strsplit(x$publication, ";|; "))
			pubs <- yuri::simpleURI(pubs)
			for (pub in pubs) {
				if (is.null(pub)) { next }
				where <- grep(pub, allpubs, fixed=TRUE)
				if (length(where) == 0) {
					answ[nrow(answ)+1, ] <- c("reference file missing", pub)
				}
			}
		}
	} 	
	answ
}





check_longrecs <- function(answ, longrecs, records) {
	rcid <- !is.null(longrecs$record_id)
	trid <- !is.null(longrecs$trial_id)
	if ((rcid + trid) != 1) {
	    answ[nrow(answ)+1, ] <- c("id", "longrecs must have either record_id or trial_id")
	} else if (rcid) {
		if (is.null(records$record_id)) {
			answ[nrow(answ)+1, ] <- c("id", "record_id in 'longrecs' but not in other records")
	    } else if (any(!(longrecs$record_id %in% records$record_id))) {
			answ[nrow(answ)+1, ] <- c("id", "record_id(s) do not match")
	    }
	} else {
	    if (is.null(records$trial_id)) {
			answ[nrow(answ)+1, ] <- c("id", "trial_id in 'longrecs' but not in other records")
	    } else if (any(!(longrecs$trial_id %in% records$trial_id))) {
			answ[nrow(answ)+1, ] <- c("id", "trial_id(s) do not match")
	    }
	}
	cns <- c(colnames(records), colnames(longrecs))

	expected <- c("date", "depth", "depth_top", "depth_bottom", "disease")
	if (!any(cns %in% expected)) {
		answ[nrow(answ)+1, ] <- c("time/depth", "no time/depth variables in long records?")	
	}

	cns <- cns[!(cns %in% c("dataset_id", "record_id", "trial_id", "date"))]  # date?
	cns <- table(cns)
	if (any(cns>1)) {
		dups <- paste(names(cns[cns>1]), collapse=", ")
		answ[nrow(answ)+1, ] <- c("duplicates", paste("duplicate variables in records and longrecs:", dups))
	}

	answ
}



## needs fixing. duplicates need to be considered together for recs and longrecs
find_duplicates <- function(answ, x, tmr=NULL) {
	if (is.null(tmr)) {
		if (nrow(x) != nrow(unique(x))) {
			answ[nrow(answ)+1, ] <- c("duplicates", "duplicate records detected")
		}
	}
	answ
}


check_treatments <- function(answ, treatment, exp_type, vars, records, type) {
	if (is.na(treatment)) {
		answ[nrow(answ)+1, ] <- c("metadata", paste(type, "cannot be NA"))
		return(answ)
	}
	
	treat <- trimws(unlist(strsplit(treatment, ";")))
	if (isTRUE(any(treat == "none"))) {
		if (type == "treatment") {
			if (grepl("experiment|trial", exp_type)) {
				answ[nrow(answ)+1, ] <- c("metadata", "treatment_vars cannot be 'none' for experiments")
				return(answ)
			} 
		}
		treat <- treat[treat  != "none"]
		if (length(treat) == 0) return(answ)
	}
	
	i <- !(treat %in% vars)
	if (any(i)) {
		#answ[nrow(answ)+1, ] <- c("metadata", 
		#	paste("not a variable in the data:",  paste(treat[i], collapse=", ")))
		stop(paste("treatment is not a variable in the data:",  treat[i], collapse=", "))
	}
	
	if (type == "treatment") {
		for (v in treat) {
			rv <- records[,v]
			if (any(is.na(rv))) {
				answ[nrow(answ)+1, ] <- c("metadata", 
					paste("missing values in treatment variable",  v, collapse=", "))
			}
			u <- na.omit(unique(rv))
			if (length(u) < 2) {
				answ[nrow(answ)+1, ] <- c("metadata", 
					paste("no variation in treatment variable",  v, collapse=", "))
			}
		}
	}
	answ
}


check_combined <- function(x, trms, answ, required=TRUE) {
	a1 <- vocal::check_variables(x, trms, required)
	a2 <- vocal::check_values(x, trms)
	answ <- rbind(answ, a1, a2) 
	dats <- grep("_date", names(x), value=TRUE)
	if (length(dats) > 0) {
		a3 <- do.call(rbind, lapply(dats, \(dat) vocal::check_date(x, dat, trms)))
		answ <- rbind(answ, a3) 
	}
	answ
}

check_weather <- function(x, answ) {
	trms <- vocal::accepted_variables(c("all", "location", "weather"))	
	answ <- check_combined(x, trms, answ)
	if (is.null(x$date)) {
		answ[nrow(answ)+1, ] <- c("weather", "variable 'date' is missing")			
	}
	answ
}

check_soil <- function(x, answ) {
	trms <- vocal::accepted_variables(c("all", "soil"))
	check_combined(x, trms, answ)
}



check_metadata <- function(x, answ) {
	trms <- vocal::accepted_variables("metadata")
	x[is.na(x)] <- as.character(NA)
	answ <- check_combined(x, trms, answ)
	if (grepl("http", x$uri)) {
		answ[nrow(answ)+1, ] <- c("uri", "http in uri")
	}
	answ
}


get_groupvars <- function(group) {		
	vars <- c("all", "location", "crop", "soil", "weather")
	if (grepl("maize", group)) vars <- c(vars, "maize")
	if (grepl("survey", group)) vars <- c(vars, "survey")
	if (grepl("soil", group)) vars <- vars[vars != "crop"]
	vars
}


check_records <- function(answ, x, group, check="all", required=TRUE, dupid=TRUE) {
	vars <- carobiner:::get_groupvars(group)
	trms <- vocal::accepted_variables(vars)
	answ <- check_combined(x, trms, answ, required=required)

	aw <- vocal::check_datespan(x, "planting_date", "harvest_date", smin=45, smax=366)
	answ <- rbind(answ, aw)
	answ <- check_consistency(x, answ)
	answ <- check_cropyield(x, answ)

	if (!("nogeo" %in% check)) {
		if (all(c("longitude", "latitude") %in% colnames(x))) {
			aw <- vocal::check_lonlat(x)	
			answ <- rbind(answ, aw)
		}
	}
	if (dupid) {
		if (!is.null(x$record_id)) {
			if (nrow(x) != length(unique(x$record_id))) {
				answ[nrow(answ)+1, ] <- c("duplicates", "duplicates in record_id")
			}		
		}
	}
	locvars <- c(paste0("adm", 1:5), "site", "location")
	locvars <- locvars[locvars %in% names(x)]
	answ <- rbind(answ, vocal::check_caps(x, locvars, minchar=5, frac=0.1))
	if (("site" %in% locvars) & (!("location" %in% locvars))) {
		answ[nrow(answ)+1, ] <- c("location/site", "variable 'site' is not allowed if variable 'location' is absent")
	}
	answ
}


carob_vocabulary <- function(x=NULL, save=FALSE, add=TRUE, reset=FALSE) {

	f <- file.path(rappdirs::user_data_dir(), ".carob/voc")
	def <- "github:controvoc/terminag"

	if (reset) {
		if (file.exists(f)) file.remove(f)
		.carob_environment$voc <- def
		vocal::set_vocabulary(def)
		return(def)
	}
	
	if (is.null(x)) {
		if (is.null(.carob_environment$voc)) {
			if (file.exists(f)) {
				readLines(f)
			} else {
				def
			}
		} else {
			.carob_environment$voc
		}
	} else {
		if (add) {
			x <- unique(c(def, x))
		}
		.carob_environment$voc <- x
		vocal::set_vocabulary(x)
		if (save) {
			dir.create(dirname(f), FALSE, FALSE)
			writeLines(x, f)
		}
		invisible(NULL)
	}
}


check_terms <- function(metadata=NULL, records=NULL, longrecs=NULL, wth=NULL, soil=NULL, group="", check="all") {

	check_packages("yuri", "0.2-6")
	check_packages("vocal", "0.3-6")
	
	voc <- carob_vocabulary()
	vocal::set_vocabulary(voc)
	vocal::check_vocabulary(delay=4, quiet=FALSE)
	
	answ <- data.frame(check="", msg="")[0,]
	if (check == "none") {
		return(answ)
	}
	if (!is.null(metadata)) {
		answ <- check_metadata(metadata, answ)
		recnms <- c(names(records), names(longrecs))
		if (!is.null(metadata$treatment_vars)) {
			answ <- check_treatments(answ, metadata$treatment_vars, metadata$data_type, recnms, records, "treatment")
		}
		if (!is.null(metadata$response_vars)) {
			answ <- check_treatments(answ, metadata$response_vars, metadata$data_type, recnms, records, "response")
		}
	}
	if (!is.null(records)) {
		answ <- check_records(answ, records, group=group, check=check)
		answ <- find_duplicates(answ, records, longrecs)
	}
	if (!is.null(longrecs)) {
		if (!is.null(records)) {
			answ <- check_longrecs(answ, longrecs, records)
		}
		if ("variable" %in% names(longrecs)) {
			vars <- get_groupvars(group)
			trms <- vocal::accepted_variables(vars)
			a <- vocal::check_variables(unique(longrecs$variable), trms, FALSE)
			answ <- rbind(answ, a) 
			## also need to check the values, by variable. 
			## should there be different value variables for different dataytypes?
		}
		answ <- check_records(answ, longrecs, group=group, check=check, required=FALSE, dupid=FALSE)
		answ <- find_duplicates(answ, records, longrecs)
	}
	
	if (!is.null(wth)) {
		answ <- check_weather(wth, answ)
	}
	if (!is.null(soil)) {
		answ <- check_soil(soil, answ)
	}

	answ
}

