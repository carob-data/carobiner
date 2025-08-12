

write_files <- function(path=NULL, metadata, wide, long=NULL, wth=NULL, soil_meta=NULL, options=NULL) {

	group <- metadata$group
#	check_group(group)
	cleanuri <- metadata$dataset_id
	stopifnot(nrow(metadata) == 1)
	if (!is.null(long)) {
		long$dataset_id <- cleanuri
	}
	if (!is.null(wth)) {
		wth$dataset_id <- cleanuri
	}
	if (!is.null(soil_meta)) {
		soil_meta$dataset_id <- cleanuri
	}

	to_mem <- FALSE
	if (is.null(path)) {
		path <- file.path(tempdir(), "carob")
		to_mem <- TRUE
	}

	if (!to_mem) {
		dir.create(file.path(path, "data", "clean"), FALSE, FALSE)
		file.remove(list.files(file.path(path, "data", "clean", group), cleanuri, full.names=TRUE))

		if (missing(wide)) {
			if (!grepl("_nodata$", cleanuri)) {
				stop("wide records missing")
			}
			d <- data.frame(ignore=TRUE)
			outf <- file.path(path, "data", "clean", group, paste0(cleanuri, ".csv"))
			utils::write.csv(d, outf, row.names=FALSE)	
			mf <- gsub(".csv$", "_meta.csv", outf)
			utils::write.csv(metadata, mf, row.names=FALSE)
			return(TRUE)
		}
	}

	if (nrow(wide) > 0) {
		dir.create(file.path(path, "data", "messages", group), FALSE, TRUE)
		dir.create(file.path(path, "data", "evaluation", group), FALSE, TRUE)
		wide$dataset_id <- metadata$dataset_id
		opt <- options("carobiner_check")
		answ <- check_terms(metadata, wide, long, wth, soil_meta, group, check=opt)	
		fmsg <- file.path(path, "data", "messages", group, paste0(cleanuri, ".csv"))
		if (file.exists(fmsg)) file.remove(fmsg)
		
		feval <- file.path(path, "data", "evaluation", group, paste0(cleanuri, ".csv"))
		if (file.exists(feval)) file.remove(feval)
		e <- evaluate_quality(wide, group)	
		data.table::fwrite(e, feval, row.names=FALSE)		
		
		if (!to_mem) {
			answ <- check_pubs(metadata, path, answ)
		}

		if (nrow(answ) > 0) {
			answ$group <- group
			answ$dataset_id <- cleanuri
			answ$contributor <- metadata$carob_contributor

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
				data.table::fwrite(answ, fmsg, row.names=FALSE)
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
	
	wide <- sort_by_terms(wide, "records", group)
	metadata <- sort_by_terms(metadata, "metadata", group)
	long <- sort_by_terms(long, "longrecs", group)
	wth <- sort_by_terms(wth, "weather", group)
	soil_meta <- sort_by_terms(soil_meta, "soil", group)

	if (nrow(wide) > 0) {
		metadata$crops <- paste(sort(unique(wide$crop)), collapse=";")
		metadata$countries <- paste(sort(unique(wide$country)), collapse=";")
	}
	
	if (to_mem) {
		return(list(meta=metadata, wide=wide, long=long))
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
	if (!is.null(soil_meta)) {
		soilf <- file.path(path, "data", "clean", group, paste0(cleanuri, "_meta_soil.csv"))
		data.table::fwrite(soil_meta, soilf, row.names=FALSE)
	}

	mf <- gsub(".csv$", "_meta.csv", outf)
	data.table::fwrite(metadata, mf, row.names=FALSE)
	
	TRUE
}

