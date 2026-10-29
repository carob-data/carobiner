

.carob_skip_script_re <- "/(_pending|_draft|_rejected|_AI|_todo)/"


.carob_unpublished_script <- function(path, id) {
	if (is.null(path) || !nzchar(path) || is.null(id) || !nzchar(id)) return(FALSE)
	pat <- paste0("^", gsub(".", "\\.", id, fixed = TRUE), "\\.R$")
	ff <- list.files(file.path(path, "scripts"), pattern = pat, recursive = TRUE, full.names = TRUE)
	if (length(ff) == 0L) return(FALSE)
	skip <- grepl(.carob_skip_script_re, ff)
	any(skip) && !any(!skip)
}


.carob_compiled_stem <- function(filename) {
	s <- sub("(_meta_variable|_meta|_long|_wth)?\\.csv$", "", basename(filename), ignore.case = TRUE)
	sub("_nodata$", "", s, ignore.case = TRUE)
}


.carob_remove_compiled <- function(path, id) {
	if (is.null(path) || is.null(id) || !nzchar(id)) return(invisible())
	idre <- paste0("^", gsub(".", "\\.", id, fixed = TRUE), "(\\.csv|_)")
	for (kind in c("clean", "messages", "evaluation")) {
		base <- file.path(path, "data", kind)
		if (!dir.exists(base)) next
		ff <- list.files(base, pattern = idre, full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
		if (length(ff) > 0L) file.remove(ff)
	}
	invisible()
}


.carob_drop_unpublished_compiled <- function(path) {
	ffR <- list.files(file.path(path, "scripts"), pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
	ffR <- ffR[!grepl(.carob_skip_script_re, ffR)]
	keep <- tolower(gsub("\\.R$", "", basename(ffR)))
	for (kind in c("clean", "messages", "evaluation")) {
		base <- file.path(path, "data", kind)
		if (!dir.exists(base)) next
		ff <- list.files(base, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
		if (length(ff) == 0L) next
		stem <- tolower(.carob_compiled_stem(ff))
		drop <- !stem %in% keep
		if (any(drop)) file.remove(ff[drop])
	}
	invisible()
}


write_files <- function(path=NULL, metadata, wide, long=NULL, wth=NULL, var_meta=NULL, options=NULL) {

	group <- metadata$carob_group
#	if (length(group) == 0 || is.na(group)) group <- metadata$group
#	check_group(group)
	cleanuri <- metadata$dataset_id
	stopifnot(nrow(metadata) == 1)
	if (!is.null(long)) {
		long$dataset_id <- cleanuri
	}
	if (!is.null(wth)) {
		wth$dataset_id <- cleanuri
	}
	if (!is.null(var_meta)) {
		var_meta$dataset_id <- cleanuri
	}

	to_mem <- FALSE
	if (is.null(path)) {
		path <- file.path(tempdir(), "carob")
		to_mem <- TRUE
	}

	# Scripts under _todo/_draft/_rejected/_pending/_AI must not write compiled output.
	unpublished <- !to_mem && .carob_unpublished_script(path, cleanuri)

	if (!to_mem) {
		if (unpublished) {
			.carob_remove_compiled(path, cleanuri)
		} else {
			dir.create(file.path(path, "data", "clean"), FALSE, FALSE)
			file.remove(list.files(file.path(path, "data", "clean", group), cleanuri, full.names=TRUE))
		}

		if (missing(wide)) {
			if (!grepl("_nodata$", cleanuri)) {
				stop("wide records missing")
			}
			if (unpublished) return(TRUE)
			d <- data.frame(ignore=TRUE)
			outf <- file.path(path, "data", "clean", group, paste0(cleanuri, ".csv"))
			utils::write.csv(d, outf, row.names=FALSE)	
			mf <- gsub(".csv$", "_meta.csv", outf)
			utils::write.csv(metadata, mf, row.names=FALSE)
			return(TRUE)
		}
	}

	if (nrow(wide) > 0) {
		wide$dataset_id <- metadata$dataset_id
		opt <- options("carobiner_check")
		answ <- check_terms(wide, metadata, long, wth, var_meta, group, check=opt)

		if (!unpublished) {
			dir.create(file.path(path, "data", "messages", group), FALSE, TRUE)
			dir.create(file.path(path, "data", "evaluation", group), FALSE, TRUE)
			fmsg <- file.path(path, "data", "messages", group, paste0(cleanuri, ".csv"))
			if (file.exists(fmsg)) file.remove(fmsg)
			feval <- file.path(path, "data", "evaluation", group, paste0(cleanuri, ".csv"))
			if (file.exists(feval)) file.remove(feval)
			e <- evaluate_quality(wide, group)
			data.table::fwrite(e, feval, row.names=FALSE)
		}

		if (!to_mem) {
			answ <- check_pubs(metadata, path, answ)
		}

		if (nrow(answ) > 0) {
			answ$group <- group
			answ$dataset_id <- cleanuri
			cc <- metadata$carob_contributor
			if (is.null(cc) || length(cc) == 0L) cc <- NA_character_
			answ$contributor <- cc

			fign <- file.path(path, "scripts", group, "ignore.csv")
			if (file.exists(fign)) {
				ign <- utils::read.csv(fign)
				ign <- apply(ign, 1, function(i) paste(i, collapse="#"))
				ans <- apply(answ, 1, function(i) paste(i, collapse="#"))
				m <- stats::na.omit(match(ign, ans))
				if (length(m)  > 0) {
					answ <- answ[-m, ]
				}
			}
			if (nrow(answ) > 0) {
				if (!unpublished) {
					data.table::fwrite(answ, fmsg, row.names=FALSE)
				}
				for (i in 1:nrow(answ)) {
					message(paste0("    ", answ$check[i], ": ", answ$msg[i]))
				}
				message(paste("    contributor:", metadata$carob_contributor))
			}
		} 
	}

	if (is.null(wide$record_id) && (nrow(wide) > 0)) {
		wide$record_id <- 1:nrow(wide)
	}
	
	wide <- sort_by_terms(wide, "records", group, keep_extra=TRUE)
	metadata <- sort_by_terms(metadata, "metadata", group, keep_extra=TRUE)
	long <- sort_by_terms(long, "longrecs", group, keep_extra=TRUE)
	wth <- sort_by_terms(wth, "weather", group, keep_extra=TRUE)
	var_meta <- sort_by_terms(var_meta, "soil", group, keep_extra=TRUE)

	if (nrow(wide) > 0) {
		metadata$crops <- paste(sort(unique(wide$crop)), collapse=";")
		metadata$countries <- paste(sort(unique(wide$country)), collapse=";")
	}
	
	if (to_mem) {
		return(list(meta=metadata, wide=wide, long=long))
	}
	if (unpublished) {
		return(TRUE)
	}
	
	outf <- file.path(path, "data", "clean", group, paste0(cleanuri, ".csv"))
	dir.create(dirname(outf), FALSE, FALSE)
	data.table::fwrite(wide, outf, row.names=FALSE)

	if (!is.null(long)) {
		outfw <- file.path(path, "data", "clean", group, paste0(cleanuri, "_long.csv"))
		data.table::fwrite(long, outfw, row.names=FALSE)
	}

	if (!is.null(wth)) {
		wthf <- file.path(path, "data", "clean", group, paste0(cleanuri, "_wth.csv"))
		data.table::fwrite(wth, wthf, row.names=FALSE)
	}
	if (!is.null(var_meta)) {
		soilf <- file.path(path, "data", "clean", group, paste0(cleanuri, "_meta_variable.csv"))
		data.table::fwrite(var_meta, soilf, row.names=FALSE)
	}

	mf <- gsub(".csv$", "_meta.csv", outf)
	data.table::fwrite(metadata, mf, row.names=FALSE)
	
	TRUE
}

