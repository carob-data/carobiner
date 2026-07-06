
.carob_environment <- new.env(parent=emptyenv())


check_package_versions <- function(pkgs) {
	imp <- utils::packageDescription("carobiner")$Imports
	parts <- trimws(unlist(strsplit(imp, ",")))
	nms <- trimws(sub("\\(.*", "", parts))
	vers <- rep(NA_character_, length(parts))
	has_v <- grepl("\\(\\s*>=\\s*([^)]+)\\)", parts)
	vers[has_v] <- trimws(sub(".*\\(\\s*>=\\s*([^)]+)\\).*", "\\1", parts[has_v]))
	for (pkg in pkgs) {
		i <- which(nms == pkg)
		if (length(i) == 0) next
		v <- vers[i]
		if (is.na(v)) next
		if (utils::packageVersion(pkg) < v) {
			repo <- if (pkg == "vocal") "controvoc" else "carob-data"
			stop(paste0('please update package ', pkg, " with:\n   remotes::install.github('", repo, "/", pkg, "')"))
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
	if (!is.null(x$yield_moisture)) {
		if (all(is.na(x$yield_moisture))) {
			if (is.null(x$yield_isfresh)) {
				answ[nrow(answ)+1, ] <- c("yield_isfresh", "yield_isfresh must be set to TRUE or NA if yield_moisture is NA")
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
		xi <- x[i, ]
		mx <- stats::aggregate(xi[, "yield", drop=FALSE], xi[, "crop", drop=FALSE], max, na.rm=TRUE)
		bad <- paste(paste0(mx$crop, ": ", mx$yield), collapse=", ")
		answ[nrow(answ)+1, ] <- c("high crop yield", bad)	
	}
	#check_outliers_iqr(x, "yield", TRUE)

	answ
}


check_pubs <- function(x, path, answ) {
	if (isTRUE(nchar(x$publication) > 0 )) {
		allpubs <- tolower(basename(list.files(file.path(path, "references"))))
		publications <- tolower(trimws(unlist(strsplit(x$publication, ";|; "))))
		for (i in 1:seq_along(publications)) {
			pub <- publications[i]
			if (grepl("http", pub)) {
				if (grepl("handle\\.net|doi\\.org", pub)) {
					answ[nrow(answ)+1, ] <- c("use URI, not URL", pub)			
				}
			}
			# else {
			#	pub <- yuri::simpleURI(pub)
			#	if (is.null(pub)) { next }
			#	where <- grep(pub, allpubs, ignore.case=TRUE)
			#	if (length(where) == 0) {
			#		answ[nrow(answ)+1, ] <- c("reference file missing", pub)
			#	}
			#}
		} 	
	}
	answ
}





check_longrecs <- function(answ, longrecs, records) {
	rcid <- !is.null(longrecs$record_id)
	trid <- !is.null(longrecs$trial_id)
	hhid <- !is.null(longrecs$hhid)
	if ((hhid + rcid + trid) < 1) {
	    answ[nrow(answ)+1, ] <- c("id", "longrecs must have either record_id, trial_id or hhid")
	} else if (rcid) {
		if (is.null(records$record_id)) {
			answ[nrow(answ)+1, ] <- c("id", "record_id in long but not in wide records")
	    } else if (any(!(longrecs$record_id %in% records$record_id))) {
			answ[nrow(answ)+1, ] <- c("id", "record_id(s) do not match between long and wide records")
	    }
	} else if (hhid) {
		if (is.null(records$hhid)) {
			answ[nrow(answ)+1, ] <- c("id", "hhid in long but not in wide records")
	    } else if (any(!(unique(longrecs$hhid) %in% records$hhid))) {
			answ[nrow(answ)+1, ] <- c("id", "hhid(s) do not match between long and wide records")
	    }
	} else {
	    if (is.null(records$trial_id)) {
			answ[nrow(answ)+1, ] <- c("id", "trial_id in long but not in wide records")
	    } else if (any(!(longrecs$trial_id %in% records$trial_id))) {
			answ[nrow(answ)+1, ] <- c("id", "trial_id(s) do not match between long and wide records")
	    }
	}
	cns <- c(colnames(records), colnames(longrecs))

	expected <- c("date", "depth", "depth_top", "depth_bottom", "disease")
	if (!any(cns %in% expected)) {
		answ[nrow(answ)+1, ] <- c("time/depth", "no time/depth variables in long records?")	
	}

	cns <- cns[!(cns %in% c("dataset_id", "record_id", "trial_id", "date", "hhid"))]  # date?
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
		answ[nrow(answ)+1, ] <- c("metadata", 
			paste(type, "is not a variable in the data:",  paste(treat[i], collapse=", ")))
		#stop(paste(type, "is not a variable in the data:",  treat[i], collapse=", "))
	}
	
	if (type == "treatment") {
		treat <- treat[treat %in% names(records)]
		for (v in treat) {
			rv <- records[,v]
			if (any(is.na(rv))) {
				answ[nrow(answ)+1, ] <- c("metadata", 
					paste("missing values in treatment variable",  v, collapse=", "))
			}
			u <- stats::na.omit(unique(rv))
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
	trms <- vocal::accepted_variables(c("general", "location", "weather"))	
	answ <- check_combined(x, trms, answ)
	if (is.null(x$date)) {
		answ[nrow(answ)+1, ] <- c("weather", "variable 'date' is missing")			
	}
	answ
}

check_soil <- function(x, answ) {
	trms <- vocal::accepted_variables(c("general", "soil"))
	check_combined(x, trms, answ)
}



check_metadata <- function(x, answ) {
	trms <- vocal::accepted_variables(c("metadata", "carob-metadata"))
	answ <- check_combined(x, trms, answ)
	if ("uri" %in% names(x) && any(grepl("http", x$uri))) {
		answ[nrow(answ)+1, ] <- c("uri", "http in uri")
	}
	if (isTRUE(x$carob_date > "2026-07-01")) {
		if (is.null(x$carob_effort) || is.na(x$carob_effort)) {
			answ[nrow(answ)+1, ] <- c("metadata", "carob_effort not valid")			
		}
	}	
	if (is.null(x$carob_completion) || is.na(x$carob_completion)) {
		answ[nrow(answ)+1, ] <- c("metadata", "carob_completion not valid")			
	}
	answ
}


get_groupvars <- function(group) {		
	excl <- c("metadata", "carob-metadata")
	if (grepl("soil", group)) excl <- c("crop", "management", excl)
	vocal::accepted_variables(exclude=excl)
}


suppress_some_warnings <- function(x, group="") {
	x <- x[!((x[,1] == "all NA") & (x[,2] == "yield_isfresh")), , drop=FALSE]
	if (group == "survey")  {
		x <- x[!((x[,1] == "missing variables") & (x[,2] == "trial_id")), , drop=FALSE]
	}
	x
}


check_records <- function(answ, x, group, check="all", required=TRUE, dupid=TRUE) {
	trms <- get_groupvars(group)
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
	
	suppress_some_warnings(answ, group)
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


# Called from .onLoad
load_required_variables <- function() {
	f <- system.file("terms/required_variables.csv", package="carobiner")
	if (!nzchar(f) || !file.exists(f)) {
		.carob_environment$required_variables <- NULL
		return(invisible(NULL))
	}
	x <- try(utils::read.csv(f, stringsAsFactors=FALSE, colClasses="character"), silent=TRUE)
	if (inherits(x, "try-error")) x <- NULL
	.carob_environment$required_variables <- x
	invisible(x)
}


# Return the required variables that are missing from vars
missing_required_variables <- function(answ, vars, group="") {
	rv <- carobiner:::.carob_environment$required_variables
	m <- grepl("meta", rv$file)
	if (group == "metadata") {
		rv <- rv[m, ]
	} else {
		rv <- rv[!m, ]	
	}
	if (is.null(rv) || nrow(rv) == 0) return(character(0))
	if (is.data.frame(vars)) vars <- names(vars)
	req <- trimws(rv$required)
	needed <- vapply(req, function(r) {
		if (r == "yes") TRUE
		else if (r == "" || r == "no") FALSE
		else if (startsWith(r, "!")) !grepl(substring(r, 2), group, fixed=TRUE)
		else grepl(r, group, fixed=TRUE)}, logical(1))
	miss <- setdiff(unique(rv$name[needed]), vars)
	if (length(miss) > 0) {
		if (group == "metadata") {
			answ[nrow(answ)+1, ] <- c("missing metadata", paste(miss, collapse=", "))		
		} else {
			answ[nrow(answ)+1, ] <- c("missing variables", paste(miss, collapse=", "))
		}
	}
	answ
}


# Variables flagged NAok=="no" should not contain missing values. 
check_NAok_variables <- function(answ, x) {
	if (is.null(x)) return(answ)
	rv <- .carob_environment$required_variables
	if (is.null(rv) || nrow(rv) == 0 || !is.data.frame(x)) return(answ)
	noNA <- unique(rv$name[trimws(rv$NAok) == "no"])
	
	bad <- NULL
	for (v in intersect(noNA, names(x))) {
		if (any(is.na(x[[v]]))) {
			bad <- c(bad, v)
		}
	}
	if (!is.null(bad)) {
		bad <- paste(v, collapse=", ")
		answ[nrow(answ)+1, ] <- c("NA detected", bad)
	}
	answ
}


check_terms <- function(records=NULL, metadata=NULL, longrecs=NULL, wth=NULL, soil=NULL, group="", check="all") {
	
	vocal::set_vocabulary(carob_vocabulary(), quiet=TRUE)
	vocal::check_vocabulary(delay=4, quiet=FALSE)
	
	answ <- data.frame(check="", msg="")[0,]
	if (check == "none") {
		return(answ)
	}
	recnms <- c(names(records), names(longrecs))

	if (!is.null(metadata)) {
		answ <- check_metadata(metadata, answ)
		answ <- missing_required_variables(answ, names(meta), "metadata")
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
	
	answ <- missing_required_variables(answ, recnms, group)
	answ <- check_NAok_variables(answ, records)
	answ <- check_NAok_variables(answ, longrecs)
	
	if (!is.null(wth)) {
		answ <- check_weather(wth, answ)
	}
	if (!is.null(soil)) {
		answ <- check_soil(soil, answ)
	}

	answ
}

