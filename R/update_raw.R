

is_closed_version <- function(lv) {
	if (isTRUE(lv$versionState == "DEACCESSIONED")) {
		return("deaccessioned")
	}
	fjs <- lv$files
	if (is.null(fjs) || NROW(fjs) == 0) {
		return("no files")
	}
	n <- NROW(fjs)
	closed <- rep(FALSE, n)
	if (!is.null(fjs$restricted) && length(fjs$restricted) == n) {
		closed <- closed | vapply(fjs$restricted, function(v) isTRUE(as.logical(v)), logical(1))
	}
	if (is.data.frame(fjs$dataFile) && !is.null(fjs$dataFile$restricted) && length(fjs$dataFile$restricted) == n) {
		closed <- closed | vapply(fjs$dataFile$restricted, function(v) isTRUE(as.logical(v)), logical(1))
	}
	if (is.data.frame(fjs$dataFile) && is.data.frame(fjs$dataFile$embargo) && !is.null(fjs$dataFile$embargo$dateAvailable) && length(fjs$dataFile$embargo$dateAvailable) == n) {
		ed <- as.Date(fjs$dataFile$embargo$dateAvailable, optional = TRUE)
		closed <- closed | (!is.na(ed) & (ed > Sys.Date()))
	}
	if (any(closed)) {
		paste0(sum(closed), "/", n, " files closed")
	} else {
		""
	}
}


is_current <- function(jf ) {
	
#	uname <- yuri::simpleURI(uri)
#	jf <- file.path(path, "data/raw", group, uname, paste0(uname, ".json"))
	if (!file.exists(jf)) {
		warning("json does not exist")
		return(NA)
	}
	x <- jsonlite::fromJSON(jf)
	old_major <- x$data$latestVersion$versionNumber
	if (is.null(old_major)) {
		return(NA)
	}
	old_minor <- x$data$latestVersion$versionMinorNumber
	uri <- x$data$latestVersion$datasetPersistentId
	url <- x$data$persistentUrl
	
	httr::set_config(httr::config(ssl_verifypeer = 0L))

	g <- httr::GET(url)
	if (g$status_code %in% c(200,202)) {
		u <- g$url
	} else {
		warning("bad response")
		return(NA)
	}

	domain <- yuri:::.getdomain(u)
	protocol <- yuri:::.getprotocol(u)
	baseu <- paste0(protocol, domain)

	pid <- unlist(strsplit(u, "\\?"))[2]
	uu <- paste0(baseu, "/api/datasets/:persistentId?", pid)
	
	# the nice way
	#r <- httr::GET(uu)
	#httr::stop_for_status(r)
	#js <- httr::content(r, as = "text", encoding = "UTF-8")
	# but for cimmyt...
	tmpf <- tempfile()
	on.exit(unlink(tmpf), add = TRUE)
	if (grepl("worldagroforestry", uu) || grepl("cirad.fr", uu) || grepl("cipotato", uu)) {
		# temporary fix because WorldAgroFor https cert has expired
		# not sure why for CIP on Ubuntu (cert expired)
		utils::download.file(uu, tmpf, quiet=TRUE, method="curl", extra="-k", mode="wb")
	} else {
		utils::download.file(uu, tmpf, quiet=TRUE, mode="wb")
	}
	njs <- readLines(tmpf, encoding = "UTF-8", warn=FALSE)
	njs <- jsonlite::fromJSON(njs)

	new_major <- njs$data$latestVersion$versionNumber
	new_minor <- njs$data$latestVersion$versionMinorNumber
	if (is.null(new_major) || is.null(new_minor)) {
		return(NA)
	}

	if ((new_major == old_major) && (new_minor == old_minor)) {
		return(TRUE)
	} else {
		closed <- is_closed_version(njs$data$latestVersion)
		return(list(
			old = paste0("old_", old_major, ".", old_minor),
			new = paste0(new_major, ".", new_minor),
			major = new_major,
			minor = new_minor,
			uri = if (length(uri) > 0) uri[1] else NA_character_,
			closed = closed
		))
	}

}	


raw_data_filenames <- function(dir, id, exclude_clean = FALSE) {
	skip <- c("ok.txt", paste0(id, ".json"), paste0(id, "_files.txt"), paste0(id, ".zip"))
	rel <- list.files(dir, recursive = TRUE, full.names = FALSE)
	if (exclude_clean) {
		rel <- rel[!grepl("^clean(/|$)", rel)]
	} else {
		rel <- rel[!grepl("^old_", rel)]
	}
	setdiff(rel, skip)
}


archive_old_version <- function(d, vold, dd) {
	old <- list.files(d, full.names = TRUE, recursive = FALSE)
	old <- old[!grepl("^old_", basename(old))]
	p <- file.path(d, vold)
	dir.create(p, FALSE, FALSE)
	if (length(old) > 0) {
		file.rename(old, file.path(p, basename(old)))
	}
	pat <- basename(d)
	id_re <- gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", pat, perl = TRUE)
	ff <- list.files(dd, pattern = paste0("^", id_re, "([._]|$)"), full.names = TRUE)
	pc <- file.path(p, "clean")
	if (length(ff) > 0) {
		dir.create(pc, FALSE, FALSE)
		ok <- file.copy(ff, pc, overwrite = TRUE)
		if (any(ok)) {
			file.remove(ff[ok])
		}
	}
	p
}


restore_archived_version <- function(d, p, dd) {
	partial <- list.files(d, full.names = TRUE, recursive = FALSE)
	partial <- partial[!grepl("^old_", basename(partial))]
	if (length(partial) > 0) unlink(partial, recursive = TRUE)
	pc <- file.path(p, "clean")
	if (dir.exists(pc)) {
		cf <- list.files(pc, full.names = TRUE, recursive = FALSE)
		if (length(cf) > 0) {
			dir.create(dd, FALSE, TRUE)
			ok <- file.copy(cf, dd, overwrite = TRUE)
			if (any(ok)) unlink(pc, recursive = TRUE)
		} else {
			unlink(pc, recursive = TRUE)
		}
	}
	old_back <- list.files(p, full.names = TRUE, recursive = FALSE)
	if (length(old_back) > 0) {
		file.rename(old_back, file.path(d, basename(old_back)))
	}
	if (length(list.files(p, all.files = TRUE, no.. = TRUE)) == 0) {
		unlink(p, recursive = TRUE)
	}
	invisible(TRUE)
}


compare_raw_files <- function(old_dir, new_dir, id, tol = 0.05) {
	old_rel <- raw_data_filenames(old_dir, id, exclude_clean = TRUE)
	new_rel <- raw_data_filenames(new_dir, id, exclude_clean = FALSE)

	added <- setdiff(new_rel, old_rel)
	missing <- setdiff(old_rel, new_rel)
	common <- intersect(old_rel, new_rel)

	changed_sz <- character()
	if (length(common) > 0) {
		old_sz <- file.info(file.path(old_dir, common))$size
		new_sz <- file.info(file.path(new_dir, common))$size
		for (i in seq_along(common)) {
			os <- old_sz[i]
			ns <- new_sz[i]
			if (is.na(os) || is.na(ns)) next
			if (os == 0 && ns > 0) {
				changed_sz <- c(changed_sz, sprintf("%s (0 -> %.0f B)", common[i], ns))
			} else if (os > 0) {
				rel <- (ns - os) / os
				if (abs(rel) > tol) {
					changed_sz <- c(changed_sz, sprintf("%s (%+.1f%%: %.0f -> %.0f B)", common[i], 100 * rel, os, ns))
				}
			}
		}
	}

	msgs <- character()
	if (length(added) > 0) {
		msgs <- c(msgs, paste("  raw new files:", paste(added, collapse = ", ")))
	}
	if (length(missing) > 0) {
		msgs <- c(msgs, paste("  raw missing files:", paste(missing, collapse = ", ")))
	}
	if (length(changed_sz) > 0) {
		msgs <- c(msgs, paste0("  raw size change (>", round(100 * tol), "%): ", paste(changed_sz, collapse = "; ")))
	}
	if (length(msgs) == 0) {
		msgs <- paste0("  raw: unchanged (same files, size diff <= ", round(100 * tol), "%)")
	}
	msgs
}


update_script_version <- function(script_file, major, minor) {
	if (!file.exists(script_file)) return(FALSE)
	x <- readLines(script_file, warn = FALSE)
	idx <- grep("get_metadata\\s*\\(", x)
	if (length(idx) == 0) return(FALSE)
	win <- idx[1]:min(length(x), idx[1] + 10L)

	maj_re <- "(\\bmajor\\s*=\\s*)([0-9]+|NA)\\b"
	min_re <- "(\\bminor\\s*=\\s*)([0-9]+|NA)\\b"

	imaj <- win[grep(maj_re, x[win])]
	imin <- win[grep(min_re, x[win])]
	if (length(imaj) == 0 || length(imin) == 0) return(FALSE)

	x[imaj[1]] <- sub(maj_re, paste0("\\1", major), x[imaj[1]])
	x[imin[1]] <- sub(min_re, paste0("\\1", minor), x[imin[1]])
	writeLines(x, script_file)
	TRUE
}


col_diff_count <- function(a, b) {
	na_a <- is.na(a) | a == ""
	na_b <- is.na(b) | b == ""
	diff_na <- na_a != na_b
	both <- !na_a & !na_b
	if (!any(both)) return(sum(diff_na))
	sa <- a[both]
	sb <- b[both]
	diff_val <- sa != sb
	if (any(diff_val)) {
		num_a <- suppressWarnings(as.numeric(sa[diff_val]))
		num_b <- suppressWarnings(as.numeric(sb[diff_val]))
		if (!anyNA(num_a) && !anyNA(num_b)) {
			tol <- 1e-8 * pmax(1, abs(num_a), abs(num_b))
			diff_val[diff_val] <- abs(num_a - num_b) > tol
		}
	}
	sum(diff_na) + sum(diff_val)
}


col_set_changed <- function(a, b) {
	ua <- sort(unique(a[!(is.na(a) | a == "")]))
	ub <- sort(unique(b[!(is.na(b) | b == "")]))
	if (identical(ua, ub)) return(FALSE)
	if (length(ua) == length(ub) && length(ua) > 0) {
		na <- suppressWarnings(as.numeric(ua))
		nb <- suppressWarnings(as.numeric(ub))
		if (!anyNA(na) && !anyNA(nb)) {
			na <- sort(na)
			nb <- sort(nb)
			tol <- 1e-8 * pmax(1, abs(na), abs(nb))
			return(any(abs(na - nb) > tol))
		}
	}
	TRUE
}


compare_clean_files <- function(old_clean_dir, new_clean_dir, id) {
	id_re <- gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", id, perl = TRUE)
	pat_csv <- paste0("^", id_re, "(_.*)?\\.csv$")
	new_csv <- list.files(new_clean_dir, pattern = pat_csv, full.names = FALSE)

	if (!dir.exists(old_clean_dir)) {
		return(paste0("  clean: no old clean files to compare (", length(new_csv), " new csv files)"))
	}
	old_csv <- list.files(old_clean_dir, pattern = "\\.csv$", full.names = FALSE)

	msgs <- character()
	added_csv <- setdiff(new_csv, old_csv)
	missing_csv <- setdiff(old_csv, new_csv)
	if (length(added_csv) > 0) {
		msgs <- c(msgs, paste("  clean new files:", paste(added_csv, collapse = ", ")))
	}
	if (length(missing_csv) > 0) {
		msgs <- c(msgs, paste("  clean missing files:", paste(missing_csv, collapse = ", ")))
	}

	common_csv <- intersect(old_csv, new_csv)
	for (fn in common_csv) {
		d_old <- utils::read.csv(file.path(old_clean_dir, fn), colClasses = "character", stringsAsFactors = FALSE)
		d_new <- utils::read.csv(file.path(new_clean_dir, fn), colClasses = "character", stringsAsFactors = FALSE)
		parts <- character()
		if (nrow(d_old) != nrow(d_new)) {
			parts <- c(parts, paste0("rows ", nrow(d_old), " -> ", nrow(d_new)))
		}
		added_cols <- setdiff(names(d_new), names(d_old))
		removed_cols <- setdiff(names(d_old), names(d_new))
		if (length(added_cols) > 0) {
			parts <- c(parts, paste("added vars:", paste(added_cols, collapse = ", ")))
		}
		if (length(removed_cols) > 0) {
			parts <- c(parts, paste("removed vars:", paste(removed_cols, collapse = ", ")))
		}
		common_cols <- intersect(names(d_old), names(d_new))
		if (nrow(d_old) == nrow(d_new)) {
			ch <- character()
			for (col in common_cols) {
				nd <- col_diff_count(d_old[[col]], d_new[[col]])
				if (nd > 0) ch <- c(ch, paste0(col, " (", nd, ")"))
			}
			if (length(ch) > 0) {
				parts <- c(parts, paste("changed vars:", paste(ch, collapse = ", ")))
			}
		} else {
			ch <- character()
			for (col in common_cols) {
				if (col_set_changed(d_old[[col]], d_new[[col]])) ch <- c(ch, col)
			}
			if (length(ch) > 0) {
				parts <- c(parts, paste("changed value sets:", paste(ch, collapse = ", ")))
			}
		}
		if (length(parts) == 0) {
			msgs <- c(msgs, paste0("  clean ", fn, ": unchanged"))
		} else {
			msgs <- c(msgs, paste0("  clean ", fn, ": ", paste(parts, collapse = "; ")))
		}
	}
	msgs
}


update_carob <- function(path, group="") {

	jff <- list.files(file.path(path, "data/raw/", group), pattern=".json$", recursive=TRUE, full.names=TRUE)
	jff <- grep("/old_", jff, value=TRUE, invert=TRUE)
	jff <- jff[file.exists(file.path(dirname(jff), "ok.txt"))]
	
	for (jf in jff) {
		test <- try(is_current(jf))
		if (inherits(test, "try-error")) next
		if (is.list(test)) {
			d <- dirname(jf)
			grp <- basename(dirname(d))
			pat <- basename(d)

			vold <- test$old
			vnew <- test$new
			closed <- test$closed
			if (!is.null(closed) && !is.na(closed) && nzchar(closed)) {
				message(paste0(grp, " ", pat, " ", vold, " -> ", vnew, " (", closed, "; keeping old files)"))
				utils::flush.console()
				next
			}
			message(paste(grp, pat, vold, "->", vnew)); utils::flush.console()

			dd <- gsub("data/raw/", "data/clean/", dirname(d))
			p <- archive_old_version(d, vold, dd)
			pc <- file.path(p, "clean")

			# 1) download new version
			uri <- test$uri
			if (is.null(uri) || is.na(uri) || !nzchar(uri)) {
				uri <- yuri::simpleURI(pat, reverse=TRUE, warn=FALSE)
			}
			dl <- try(get_data(uri, path, grp, cache=FALSE, filter=FALSE), silent=TRUE)
			if (inherits(dl, "try-error") || !file.exists(file.path(d, "ok.txt"))) {
				err <- if (inherits(dl, "try-error")) trimws(as.character(dl)) else "download did not complete"
				message(paste0("  download failed (", err, "); restoring old files"))
				utils::flush.console()
				restore_archived_version(d, p, dd)
				next
			}

			# 2) compare old and new raw files
			for (m in compare_raw_files(p, d, pat)) {
				message(m)
			}
			utils::flush.console()

			# 3) update major / minor in script
			sf <- file.path(path, "scripts", grp, paste0(pat, ".R"))
			if (!file.exists(sf)) {
				message(paste("  no script found:", sf))
				utils::flush.console()
				next
			}
			if (update_script_version(sf, test$major, test$minor)) {
				message(paste0("  script updated: major=", test$major, ", minor=", test$minor))
			} else {
				message(paste("  could not update major/minor in script:", sf))
				utils::flush.console()
				next
			}
			utils::flush.console()

			# 4) run updated script and compare clean data
			run_res <- try({
				e <- new.env(parent = globalenv())
				source(sf, local = e)
				e$carob_script(path)
			}, silent = TRUE)
			if (inherits(run_res, "try-error")) {
				message(paste("  script failed:", trimws(as.character(run_res))))
				utils::flush.console()
				next
			}
			for (m in compare_clean_files(pc, dd, pat)) {
				message(m)
			}
			utils::flush.console()
		}
	}
}
