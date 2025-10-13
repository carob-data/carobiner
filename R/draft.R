
quotes <- function(x) {
	paste0("\"", x, "\"") 
}

grepaste <- function(pattern, x, ignore.case=TRUE, oper=NULL, value) {
	v <- grep(pattern, x, ignore.case=ignore.case, value=TRUE)
	if (!is.null(oper)) {
		if (oper == "and") {
			v <- grep(value, v, ignore.case=ignore.case, value=TRUE)
		} else { # not 
			i <- grepl(value, v, ignore.case=ignore.case)
			v <- v[!i]
		}
	}
	#if (length(v) > 1) {
	#	v <- paste0(v[1], "\"]], #", paste(v[-1], collapse=", "))
	#} else if (length(v) > 0) {
	#	v <- paste0(v, "\"]],")
	#}
	v
}	

grepr <- function(x) {
	r <- list(
		hhid = grepaste("farmer", x),
		plot_id = grepaste("plot", x),
		country = grepaste("country", x),
		adm1 = grepaste("adm.*1|admin.*1|region|state|estado", x),
		adm2 = grepaste("adm.*2|admin.*2|munic|provin|distri", x),
		adm3 = grepaste("adm.*3|admin.*3|ward|commun", x),
		adm4 = grepaste("adm.*4|admin.*4", x),
		location = grepaste("locat|village|site", x),
		site = grepaste("hamlet", x),
		latitude = grepaste("latitude|^lat", x),
		longitude = grepaste("longitude|^long", x),
		elevation = grepaste("^elev|altitude", x),
		treatment = grepaste("treat", x),
		crop = grepaste("crop", x),
		variety = grepaste("variety|variedad|cultivar|clone", x),
		year = grepaste("year", x),
		season = grepaste("season", x),
		planting_date = grepaste("plant.*date|sow.*date", x),
		harvest_date = grepaste("harv.*date", x),
		maturity_date = grepaste("mat.*date", x),
		flowering_date = grepaste("flow.*date", x),
		transplanting_date = grepaste("trans.*date", x),
		N_fertilizer = grepaste("^N$|^N_|^Nfert|nitrogen", x, oper="not", value="soil|%"),
		P_fertilizer = grepaste("^P$|^P_|^Pfert|phosph", x, oper="not", value="soil|mg/kg"),
		K_fertilizer = grepaste("^K$|^K_|^Kfert|potas", x, oper="not", value="soil|mg/kg"),
		S_fertilizer = grepaste("^S$|^S_|^Sfert|sulf", x, oper="not", value="soil|mg/kg"),
		B_fertilizer = grepaste("^B$|^B_|^Bfert|boron", x, oper="not", value="soil|mg/kg"),
		Mg_fertilizer = grepaste("^Mg$|^Mgfert|^Mg_|magnesium", x, oper="not", value="soil|mg/kg"),
		Zn_fertilizer = grepaste("^Zn$|^Znfert|^Zn_|zinc", x, oper="not", value="soil|mg/kg"),
		
		soil_N = grepaste("^N$|^N_|nitrogen", x, oper="and", value="soil|%"),
		soil_P = grepaste("^P$|^P_|phosph", x, oper="and", value="soil|mg/kg"),
		soil_K = grepaste("^K$|^K_|potas", x, oper="and", value="soil|mg/kg"),
		soil_S = grepaste("^S$|^S_|sulf", x, oper="and", value="soil|mg/kg"),
		soil_B = grepaste("^B$|^B_|boron", x, oper="and", value="soil|mg/kg"),
		soil_Mg = grepaste("^Mg$|^Mg_|magnesium", x, oper="and", value="soil|mg/kg"),
		soil_Zn = grepaste("^Zn$|^Zn_|zinc", x, oper="and", value="soil|mg/kg"),
		
		soil_texture =  grepaste("texture", x),
		yield = grepaste("yield", x)
	)
	empty <- character(0)
	e <- sapply(r, \(i) identical(i, empty))
	r[!e]
}

wdist <- function(x, trms) {
	d <- utils::adist(x, trms)
	a <- apply(d, 1, which.min)
	z <- data.frame(from=x, to=trms[a], dist=d[cbind(1:nrow(d), a)])
}



read_format <- function(files, format, fun) { 
	is_fmt <- grepl(paste0("\\.", format, "$"), files, ignore.case=TRUE)
	if (any(is_fmt)) {
		ff <- files[is_fmt]
		out <- lapply(ff, \(f) try(fun(f)))
		names(out) <- basename(ff)
	} else {
		out <- NULL
	}
	out
}
	

get_raw_data <- function(ff) {

	is_xls <- grepl("\\.xlsx$|\\.xls$", ff)
	out <- list()
	if (any(is_xls)) {
		fxls <- ff[is_xls]
		xls <- vector("list", length(fxls))
		names(xls) <- basename(fxls)
		for (i in 1:length(fxls)) {
			sheets <- readxl::excel_sheets(fxls[i])
			ds <- lapply(sheets, \(s) suppressWarnings(carobiner::read.excel(fxls[i], sheet=s)))
			names(ds) <- sheets
			xls[[i]] <- ds
		}
		out$xls <- xls
	}


	out$csv <- read_format(ff, "csv", utils::read.csv) 
	out$rds <- read_format(ff, "rds", readRDS) 
	out$dta <- read_format(ff, "dta", haven::read_dta) 

	is_known <- is_xls | grepl("\\.csv$|\\.rds$|\\.dta$", ff, ignore.case=TRUE)
	is_other <- !is_known
	if (any(is_other)) {
		foth <- ff[is_other]
		ido <- grepl("\\.do$", foth)
		if (any(!ido)) {
			out$other <- basename(foth[!ido])
		}
		if (any(ido)) {
			out$do <- basename(foth[ido])
		}
	}
	out
}

get_filenames <- function(d) {
	f <- NULL
	n <- 0
	if (!is.null(d$xls)) {
		fxls <- names(d$xls)
		n <- length(fxls)
		f <- paste0(paste0("\tf", 1:n), ' <- ff[basename(ff) == "', fxls, '"]')
	}
	if (!is.null(d$csv)) {
		fcsv <- names(d$csv)
		n <- n+length(fcsv)
		f <- c(f, paste0(paste0("\tf", (length(f)+1):n), ' <- ff[basename(ff) == "', fcsv, '"]'))
	}
	if (!is.null(d$rds)) {
		frds <- names(d$rds)
		n <- n+length(frds)
		f <- c(f, paste0(paste0("\tf", (length(f)+1):n), ' <- ff[basename(ff) == "', basename(frds), '"]'))
	}
	if (!is.null(d$dta)) {
		fdta <- names(d$dta)
		n <- n+length(fdta)
		f <- c(f, paste0(paste0("\tf", (length(f)+1):n), ' <- ff[basename(ff) == "', basename(fdta), '"]'))
	}
	if (!is.null(d$other)) {
		foth <- d$other
		n <- n+length(foth)
		f <- c(f, paste0(paste0("\tf", (length(f)+1):n), ' <- ff[basename(ff) == "', basename(foth), '"]'))
	}

	if (!is.null(d$do)) {
		f <- c(f, paste("#\tthere are", length(d$do), "stata 'do' files that can be consulted"))
	}
	
	paste(f, collapse="\n")
}


read_files <- function(d) {
	r <- NULL
	n <- 1
	if (!is.null(d$xls)) {
	# if there is only one xls, and it has multiple sheets, these should be evaluated. 
		for (i in 1:length(d$xls)) {
			sheets <- names(d$xls[[i]])
			if (length(sheets) == 1) {	
				r <- c(r, paste0(paste0("\tr", n), " <- carobiner::read.excel(f", n, ")"))
			} else {
				for (j in 1:length(sheets)) {
					r <- c(r, paste0(paste0("\tr", n, letters[j]), " <- carobiner::read.excel(f", n, ", sheet=\"", sheets[j], "\")"))
				}
			}
			n <- n + 1
		}
	}

	if (any(!is.null(d$csv))) {
		nn <- length(d$csv)
		sq <- n:(n+nn-1)
		r <- c(r, paste0(paste0("\tr", sq), " <- read.csv(f", sq, ")"))
		n <- n + nn
	}

	if (any(!is.null(d$rds))) {
		nn <- length(d$rds)
		sq <- n:(n+nn-1)
		r <- c(r, paste0(paste0("\tr", sq), " <- readRDS(f", sq, ")"))
		n <- n + nn
	}

	if (any(!is.null(d$dta))) {
		nn <- length(d$dta)
		sq <- n:(n+nn-1)
		r <- c(r, paste0(paste0("\tr", sq), " <- haven::read_dta(f", sq, ")"))
		n <- n + nn
	}
	
	if (any(!is.null(d$other))) {
		nn <- length(d$other)
		sq <- n:(n+nn-1)
		r <- c(r, paste0(paste0("\t#r", sq), " <- read.???(f", sq, ")"))	
	}
	
	paste(r, collapse="\n")
}

make_unique <- function(x) {
	n <- unique(x$new)
	
	x <- lapply(n, \(new) {
		y <- x[x$new == new, ]
		if (nrow(y) > 1) {
			y <- y[which.min(utils::adist(y$new, y$old[1])), ]
		}
		y
	})
	do.call(rbind, x)
}

match_names <- function(r, n) {
	nms <- names(r)
	g <- lapply(nms, grepr)
	x <- sapply(g, length)
	gg <- unlist(g[x>0])
	txt <- NULL
	if (length(gg) > 0) {
		gg <- data.frame(new=names(gg), old=gg)
		gg <- make_unique(gg)
		bod <- unlist(lapply(1:nrow(gg), \(i) paste0("\t\t", gg[i, 1], " = r", n, "[[\"", gg[i,2], "\"]]")))
		j <- grep("crop|soil_texture", gg$new)
		bod[j] <- gsub("]]", "]])", gsub("= ", "= tolower(", bod[j]))
		j <- grep("^adm", gg$new)
		bod[j] <- gsub("]]", "]], \"title\")", gsub("= ", "= carobiner::fix_name(", bod[j]))
				
		bod <- paste(bod, collapse= ",\n")
		txt <- paste0("\td", n, " <- data.frame(\n", bod, "\n\t)\n")
		nms <- nms[!(nms %in% gg[,2])]
	} else {
		txt <- paste0("\td", n, " <- data.frame()\n")	
	}
	if (length(nms) > 0) {
		txt <- paste0(txt, "##r", n, ": \"", paste0(nms, collapse="\", \""), "\"\n")
	}
	txt
}	

match_org <- function(x, xd=.25) {
	if (length(x) == 0) {
		return(x)
	}
	v <- vocal::accepted_values("organization")
	x <- trimws(unlist(strsplit(x, ";")))
	for (i in 1:length(x)) {
		if (x[i] == "") next
		d <- adist(x[i], v$longname)
		if ((min(d, na.rm=TRUE) / nchar(x[i])) < xd) {
			x[i] <- v[which.min(d), 1]
		}
	}
	paste(x, collapse="; ")
}


draft <- function(uri, path, group="draft", overwrite=FALSE) {

#uri <- "hdl:11529/10548230"
#group <- "survey"
#overwrite <- TRUE

	if (file.exists(fscript) && (!overwrite)) {
		stop(paste(fscript, "exists. Use 'overwrite=TRUE' to overwrite it"))
	}

	gh <- try(carobiner::on_github(uri), silent=TRUE)
	if (NCOL(gh) > 1) {
		warning("this dataset is already in Carob")
		print(gh)
	}

	voc <- carobiner::carob_vocabulary()
	vocal::set_vocabulary(voc)

	did <- yuri::simpleURI(uri)
	## check on_carob ...
	
	fscript <- file.path(path, "scripts/_draft", group, paste0(did, ".R"))
	dir.create(dirname(fscript), FALSE, TRUE)

	ff  <- carobiner::get_data(uri, path, group)

	meta <-	carobiner::get_metadata(uri, path, group, major=0, minor=0, draft=TRUE)
	v <- c(unlist(strsplit(meta$version, "\\.")), NA, NA)
	v <- as.character(v)
	v[is.na(v)] <- "NA"

	s <- readLines(system.file("tmp/tmp", package="carobiner"))

	s <- gsub("_title_", gsub("\"", "'", meta$title), s)
	s <- gsub("_description_", gsub("\"", "'", meta$description), s)
	s <- gsub("_major_", v[1], s)
	s <- gsub("_minor_", v[2], s)
	s <- gsub("_uri_", uri, s)
	s <- gsub("_group_", group, s)
	s <- gsub("_dataorg_", match_org(meta$data_organization), s)
	s <- gsub("_pub_", ifelse(is.na(meta$publication), "NA", quotes(meta$publication)), s)
	s <- gsub("_today_", as.character(as.Date(Sys.time())), s)
	s <- gsub("_design_", ifelse(is.na(meta$design), "NA", quotes(meta$design)), s)


	d <- try(get_raw_data(ff))
	
	if (inherits(d, "try-error")) {
		writeLines(s, fscript)
		message(fscript)
		return(invisible(fscript))
	}
	
	wf <- get_filenames(d)

	s <- gsub("_filename_", wf, s)

	r <- read_files(d)
	s <- gsub("_read_", r, s)

	can_read <- grepl("\\.xlsx$|\\.xls$|\\.csv$|\\.dta|\\.rds", ff)
	
	if (!any(can_read)) {
		writeLines(s, fscript)
		message(fscript)
		return(invisible(fscript))
	}
	d$other <- NULL
	
	nms <- NULL
	if (!is.null(d$xls)) {
		for (i in 1:length(d$xls)) {
			if (length(d$xls[[i]]) == 1) {
				nms <- c(nms, match_names(d$xls[[i]][[1]], i))
			} else {
				for (j in 1:length(d$xls[[i]])) {
					nms <- c(nms, match_names(d$xls[[i]][[j]], paste0(i, letters[j])))
				}
			}
		}
	}
	if (!is.null(d$csv)) {
		for (i in 1:length(d$csv)) {
			nms <- c(nms, match_names(d$csv[[i]], i))
		}
	}
	if (!is.null(d$dta)) {
		for (i in 1:length(d$dta)) {
			nms <- c(nms, match_names(d$dta[[i]], i))
		}
	}
	
	
	if (is.null(nms)) {
		s <- gsub("_replacements_", "", s)
	} else {
		nms <- paste(nms, collapse="\n\n")
		s <- gsub("_replacements_", nms, s)	
	}

	#trms <- vocal::accepted_variables()	
	#trms <- trms[!(trms$group %in% c("all", "metadata")), "name"]
	
	
	writeLines(s, fscript)
	message(fscript)
	invisible(fscript)
}

#draft("hdl:11529/10548230", path, "survey", overwrite=TRUE)


