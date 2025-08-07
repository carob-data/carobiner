# Author: Robert Hijmans
# May 2021
# License GPL3


get_function <- function(name, path, group="") {
	f <- file.path(path, "scripts", group, "_functions.R")
	source(f, local=TRUE)
	get(name)
}


sort_by_terms <- function(x, type, group) {
	if (is.null(x)) return(x)
	if (type == "metadata") {
		trms <- vocal::accepted_variables("metadata")
	} else if (type == "weather") {
		trms <- vocal::accepted_variables("weather")
	} else {
		vars <- get_groupvars(group)
		trms <- vocal::accepted_variables(vars)
	}
	trms <- trms$name[trms$name %in% names(x)]
	x[, trms]
}


zip_clean <- function(path) {

	pzip <- Sys.getenv("R_ZIPCMD")
	if (pzip == "") {pzip <- "zip"}
	zipflags <- "-jq9"		

	zpath <- file.path(path, "data", "clean", "zip")
	dir.create(zpath, FALSE, FALSE)

	ff <- list.files(file.path(path, "data", "clean"), pattern="_meta.csv$", recursive=TRUE, full.names=TRUE)
	zz <- file.path(zpath, basename(gsub("_meta.csv", ".zip", ff)))
	e <- file.exists(zz)

	if (sum(e) > 0) {
		i <- which(e)
		zzi <- zz[i]
		ffinfo <- file.info(ff[i])$mtime
		zzinfo <- file.info(zzi)$mtime
		j <- zzinfo < ffinfo
		if (any(j)) {
			rz <- zzi[j]
			file.remove(rz)
		}
	}
	
	for (i in 1:length(ff)) {
		if (file.exists(zz[i])) next
		d <- utils::read.csv(ff[i])
		if (grepl("CC|ETALAB", d$license)) {
			pat <- paste0(gsub("_meta.csv", "", basename(ff[i])), ".*.csv")
			fd <- list.files(dirname(ff[i]), pattern=pat, full.names=TRUE, recursive=TRUE)
			utils::zip(zz[i], fd, zipflags, zip=pzip)
		}
	}
}


combine_compiled <- function(path, zip=TRUE, ...) {

	fff <- list.files(file.path(path, "data", "clean"), pattern=".csv$", recursive=TRUE)
	grps <- unique(sapply(strsplit(fff, "/"), function(i) ifelse(length(i) > 1, i[1], "doi")))
	cpath <- file.path(path, "data", "compiled")

	pzip <- Sys.getenv("R_ZIPCMD")
	if (pzip == "") {pzip <- "zip"}
	zipflags <- "-jq9"		

	ff <- list.files(cpath, pattern = "^carob_all.*.csv$|^carob_all.*.zip$", full.names=TRUE)
	# |^carob_all.*.xlsx$
	file.remove(ff)

	ff <- list.files(cpath, pattern = "\\.csv$", full.names=TRUE)

	i <- grep("terms.csv$", ff)
	gf <- ff[i]
	ff <- ff[-i]
	cterms <- unique(lapply(gf, utils::read.csv))
	cterms <- do.call(bindr, cterms)
	fterms <- file.path(cpath, paste0("carob_all_terms.csv"))
	data.table::fwrite(cterms, fterms, row.names=FALSE)
	fg <- c("metadata", "warnings", "long", "")

	for (add in c("-cc", "")) {
		j <- grepl(paste0("-cc.csv$"), ff)		
		if (add == "") {
			j <- !j
		}
		fi <- ff[j]
		d <- list()
		fcsv <- NULL
		for (x in fg) {
			name <- ifelse(x=="", "data", x)
			i <- grep(paste0(x, add, ".csv$"), fi)
			gf <- fi[i]
			fi <- fi[-i]
			y <- lapply(gf, utils::read.csv)
			d[[name]] <- do.call(bindr, y)
			outf <- file.path(cpath, paste0("carob_all_", x, add, ".csv"))
			outf <- gsub("_-cc.csv", "-cc.csv", outf)
			outf <- gsub("_.csv", ".csv", outf)
			data.table::fwrite(d[[name]], outf, row.names=FALSE)
			fcsv <- c(fcsv, outf)
		}

		excel=FALSE
		if (excel) {
			fxls <- gsub(".csv$", ".xlsx", outf)
			d$terms = cterms
			d <- d[c(1,5,4,3,2)]
			writexl::write_xlsx(d, fxls)
			rm(d)
		}

		if (zip) {
			fzip <- gsub("\\.csv$", ".zip", outf)
			zf <- c(fcsv, fterms)
			utils::zip(fzip, zf, zipflags, zip=pzip)
		}
	}
}


compile_carob <- function(path, group="", split_license=FALSE, zip=FALSE, excel=FALSE, cache=FALSE) {
	warn <- options("warn")
	if (warn$warn < 1) {
		on.exit(options(warn=warn$warn))
		options(warn=1)
	}
	dir.create(file.path(path, "data", "compiled"), showWarnings = FALSE, recursive = TRUE)
	fff <- list.files(file.path(path, "data", "clean", group), pattern=".csv$", recursive=TRUE)

	voc <- carob_vocabulary()
	vocal::set_vocabulary(voc)
	vocal::check_vocabulary(quiet=FALSE)


	if (group == "") {
		grps <- unique(sapply(strsplit(fff, "/"), function(i) ifelse(length(i) > 1, i[1], "doi")))
		grps <- grps[grps != "draft"]
	} else {
		fff <- file.path(group, fff)
		grps <- group
	}
		
	ret <- NULL
	if (zip) {
		pzip <- Sys.getenv("R_ZIPCMD")
		if (pzip == "") {
			pzip <- "zip"
		}
		zipflags <- "-jq9"		
	}
	
	fgrp <- dirname(fff)

	for (grp in grps) {

		wgroup <- ifelse(grp == "doi", "", paste0("_", grp))

		ff <- file.path(path, "data", "clean", fff[fgrp == grp])
		outft <- file.path(path, "data", "compiled", paste0("carob", wgroup, "_terms.csv"))
		if (file.exists(outft) && cache) {
			ft <- file.info(outft)$mtime
			fftime <- file.info(ff)$mtime
			if (all(fftime < ft)) next
		}

		print(grp)

		mi <- grepl("_meta.csv$", ff)
		li <- grepl("_long.csv$", ff)
		x <- sort_by_terms(.binder(ff[mi]), "metadata", grp)
		x[is.na(x)] <- ""
		x[] <- sapply(x, function(i) gsub("\n", " ", i))
		x[] <- sapply(x, function(i) gsub("\t", " ", i))
		y <- sort_by_terms(.binder(ff[!(mi|li)]), "records", grp)
# should be dealt with in source file
#		if ("reference" %in% colnames(y)) {
#			y$reference <- gsub("\n", " ", y$reference)
#			y$reference <- gsub("\t", " ", y$reference)
#		}
		z <- sort_by_terms(.binder(ff[li]), "records", grp)
		
		wf <- list.files(file.path(path, "data", "messages", grp), full.names=TRUE)
		if (length(wf) > 0) {
			wrn <- .binder(wf)
			have_warnings <- TRUE
		} else {
			have_warnings <- FALSE
		}
		
		gvars <- get_groupvars(grp)
		gterms <- vocal::accepted_variables(gvars)
		gterms <- gterms[, c("name", "type", "unit", "description")]

#		utils::write.csv(gterms, outft, row.names=FALSE)
		data.table::fwrite(gterms, outft, row.names=FALSE)
		if (split_license) {
			xx <- x[grepl("CC|ETALAB", x[,"license"]), ]
			yy <- y[y$dataset_id %in% xx[, "dataset_id"], ]

			if (have_warnings) wwrn <- wrn[wrn$dataset_id %in% xx[, "dataset_id"], ]
			if (nrow(xx) > 0) {
				outmf <- file.path(path, "data", "compiled", paste0("carob", wgroup, "_metadata-cc.csv"))
				#utils::write.csv(xx, outmf, row.names=FALSE)
				data.table::fwrite(xx, outmf, row.names=FALSE)
				outff <- file.path(path, "data", "compiled", paste0("carob", wgroup, "-cc.csv"))
				data.table::fwrite(yy, outff, row.names=FALSE)
				outwf <- file.path(path, "data", "compiled", paste0("carob", wgroup, "_warnings-cc.csv"))
				if (have_warnings) data.table::fwrite(wwrn, outwf, row.names=FALSE)
				
				if (length(z) > 0) {
					zz <- z[z$dataset_id %in% xx[, "dataset_id"], ]
					if (nrow(zz) > 0) {
						outlf <- file.path(path, "data", "compiled", paste0("carob", wgroup, "_long-cc.csv"))
						data.table::fwrite(zz, outlf, row.names=FALSE)
					} else {
						outlf <- NULL
					}
				} else {
					outlf <- NULL
				}
				if (zip) {
					fzip <- gsub(".csv$", ".zip", outff)
					if (file.exists(fzip)) file.remove(fzip)
					utils::zip(fzip, c(outft, outmf, outff, outlf, outwf), zipflags, zip=pzip)
				} 
				if (excel) {
					fxls <- gsub(".csv$", ".xlsx", outff)
					if (have_warnings) {
						dx <- list(sources=xx, terms=gterms, data=yy, warnings=wwrn)
					} else {
						dx <- list(sources=xx, terms=gterms, data=yy) # warnings=NULL)?
					}
					writexl::write_xlsx(dx, fxls)
				}
			}
		}
		if (have_warnings) {
			outwf <- file.path(path, "data", "compiled", paste0("carob", wgroup, "_warnings.csv"))
			data.table::fwrite(wrn, outwf, row.names=FALSE)
		}
		outmf <- file.path(path, "data", "compiled", paste0("carob", wgroup, "_metadata.csv"))
		data.table::fwrite(x, outmf, row.names=FALSE)
		outff <- file.path(path, "data", "compiled", paste0("carob", wgroup, ".csv"))
		data.table::fwrite(y, outff, row.names=FALSE)
		if (length(z) > 0) {
			outlf <- file.path(path, "data", "compiled", paste0("carob", wgroup, "_long.csv"))
			data.table::fwrite(z, outlf, row.names=FALSE)
		} else {
			outlf <- NULL
		}
		if (zip) {
			fzip <- gsub(".csv$", ".zip", outff)
			if (file.exists(fzip)) file.remove(fzip)
			utils::zip(fzip, c(outft, outmf, outff, outlf, outwf), flags=zipflags, zip=pzip)
		}
		if (excel) {
			fxls <- gsub(".csv$", ".xlsx", outff)
			dx <- list(sources=x, terms=gterms, data=y, warnings=wrn)
			writexl::write_xlsx(dx, fxls)
		}
		
		ret <- c(ret, outmf, outff)
	}
	utils::flush.console()
	
	ret
}


run_carob <- function(cleanuri, path, group="", quiet=FALSE) {
	w <- options("warn")
	if (w$warn < 1) {
		on.exit(options(warn=w$warn))
		options(warn=1)
	}

	ff <- list.files(file.path(path, "scripts", group), pattern="R$", full.names=TRUE, recursive=TRUE)
	f <- grep(cleanuri, ff, value=TRUE)
	carob_script <- function() {FALSE}
	rm(carob_script)
	if (!quiet) cat(basename(f), "\n"); utils::flush.console()
	source(f, local=TRUE)
	if (!exists("carob_script")) {
		stop(" ", basename(f), " does not have a 'carob_script' function", call.=FALSE)
	}
	if (!carob_script(path)) {
		cat(paste(" ", basename(f), " failed\n"))
	}
	invisible(TRUE)
}



process_carob <- function(path, group="", quiet=FALSE, check=NULL, cache=TRUE) {

	if (!file.exists(path)) {
		stop("path does not exist")
	}

	options(carobiner_check=check)
	on.exit(options(carobiner_check=NULL))

	w <- options("warn")
	if (w$warn < 1) {
		on.exit(options(warn=w$warn), add=TRUE)
		options(warn=1)
	}

#	if (group != "") {
#		check_group(group)
#	}

	base <- file.path(path, "scripts")
	ffR <- list.files(file.path(base, group), pattern="\\.R$", full.names=TRUE, recursive=TRUE)
	ff_un <- grepl("^_", basename(ffR))
	ffR <- ffR[!ff_un]
	ffR <- ffR[!grepl("/_pending/|/_draft/|/_rejected/", ffR)]
	#ffR <- ffR[!grepl("/_removed/", ffR)]
	#ffR <- ffR[basename(ffR) != "template.R"]

	if (length(ffR) == 0) {
		return(invisible(TRUE))
	}

	fcsv <- list.files(file.path(path, "data", "clean", group), pattern="_meta.csv$", full.names=TRUE, recursive=TRUE)
	fcsv <- gsub("_nodata", "", fcsv)
	
	if (length(fcsv) == 0) cache = FALSE
	
	if (cache) {
		have_csv <- data.frame(
			group = basename(dirname(fcsv)), 
			URI=tolower(gsub("_meta.csv$", "", basename(fcsv), ignore.case = TRUE)),
			csvfile = fcsv,
			csvtime = file.mtime(fcsv),
			data = TRUE) 
					
		have_R <- data.frame(
			group = basename(dirname(ffR)), 
			URI= tolower(gsub("\\.R$", "", basename(ffR), ignore.case = TRUE)),
			Rfile = ffR,
			Rtime = file.mtime(ffR),
			script = TRUE) 

		have <- merge(have_csv, have_R, by=c("group", "URI"), all=TRUE)

		noscript <- which(is.na(have$script))
		if (length(noscript) > 0) {
			# remove compiled data for which there is no matching script
			for (i in noscript) {
				dff <- list.files(dirname(have$csvfile[i]), pattern=gsub("_meta.csv", "", basename(have$csvfile[i])), full.names=TRUE)
				file.remove(dff)
			}
			have <- have[-noscript, ,drop=FALSE]
		}
		
		keep <- which(is.na(have$data) | (have$Rtime > have$csvtime))
		ffR <- have$Rfile[keep]
		
		if (length(ffR) == 0) {
			return(invisible(TRUE))
		}
		
	} else {
		fcsv <- list.files(file.path(path, "data", "clean", group), pattern=".csv$", full.names=TRUE, recursive=TRUE)
		file.remove(fcsv)
		fmsg <- list.files(file.path(path, "data", "messages", group), pattern="\\.csv$", recursive=TRUE, full.names=TRUE)
		file.remove(fmsg)
	}

	ffR <- sort(ffR)
	
	tab <- table(basename(ffR))
	if (any(tab > 1)) {
		dups <- names(tab[tab>1])
		message(paste("duplicate files: ", paste(dups, collapse=", ")))
	}

	carob_script <- function() {FALSE}
	#rm(list=ls(globalenv()))
	for (f in ffR) {
		rm(carob_script)
		if (!quiet) cat(gsub(base, "", f), "\n"); utils::flush.console()
		source(f, local=TRUE)
		if (!exists("carob_script")) {
			stop(basename(f), " does not have a 'carob_script' function", call.=FALSE)
		}
		ok <- FALSE
		try(ok <- carob_script(path))
		if (!ok) {
			message(paste("  processing failed for:\n", basename(f)))
		}
		utils::flush.console()
	}
	
	ffm <- list.files(file.path(path, "data", "messages"), pattern=".csv$", full.names=TRUE, recursive=TRUE)
	msg <- lapply(ffm, utils::read.csv)
	msg <- do.call(rbind, msg)
	utils::write.csv(msg, file.path(path, "data", "messages.csv"), row.names=FALSE)

	update_todo(path)
	invisible(TRUE)
}


make_carob <- function(path, group="", quiet=FALSE, check="all", report=FALSE, combine=FALSE, cache=TRUE, ...) {
	get_packages(group)
	message(" === process ==="); utils::flush.console()
	process_carob(path, group=group, quiet=quiet, check=check, cache=cache)
	message(" === compile ==="); utils::flush.console()
	out <- compile_carob(path, group=group, cache=cache, ...)
	if (report) {
		message(" === report ==="); utils::flush.console()
		make_reports(path, group="", cache=TRUE)
	}
	if (!is.null(out)) {
		if (combine) {
			message(" === combine ==="); utils::flush.console()
			combine_compiled(path, ...)
		}
	}
	message(" === done ===")
}


