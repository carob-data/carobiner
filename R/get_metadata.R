
.old_check_version <- function(x, cleanuri, major=1, minor=0) {
	jmajor <- x$data$latestVersion$versionNumber 
	if (!is.null(jmajor)) {
		jminor <- x$data$latestVersion$versionMinorNumber 
		if (jmajor > major) {
		  warning(paste("using newer major version", jmajor, "for", cleanuri), call.=FALSE)
		} else if (jmajor < major) {
		  warning(paste("wrong major version in script", jmajor, "for", cleanuri), call.=FALSE)
		}
		if (jminor != minor) warning(paste("different minor version", jminor, "for", cleanuri), call.=FALSE)
	} else { # ckan
		v <- x$result$version
		if (!is.null(v)) {
			if (v != major) stop(paste("different version", v, "for", cleanuri), call.=FALSE)
		}
	}
}

checkVersion <- function(vmeta, major, minor, fpath, uri=NULL, path=NULL, group=NULL) {
	if (is.na(vmeta)) {
		if (!(is.na(major) && is.na(minor))) {
			stop(paste("version supplied where there is none? You can use 'NA'"), call.=FALSE)			
		}
		return(FALSE)
	}
	v <- c(unlist(strsplit(vmeta, "\\.")), NA)
	if (identical(v[1:2], as.character(c(major, minor)))) {
		return(FALSE)
	}

	req_v <- if (!is.na(minor)) paste0(major, ".", minor) else as.character(major)

	if (!is.null(uri) && !is.null(path) && nzchar(path) && !is.null(group)) {
		pat <- basename(fpath)
		jf <- file.path(fpath, paste0(pat, ".json"))
		if (file.exists(jf)) {
			test <- try(is_current(jf), silent = TRUE)
			if (is.list(test)) {
				if (identical(as.character(c(test$major, test$minor)), as.character(c(major, minor)))) {
					closed <- test$closed
					if (!is.null(closed) && !is.na(closed) && nzchar(closed)) {
						stop(paste0("version ", req_v, " in script is closed on server (", closed, "); keeping cached version ", vmeta), call.=FALSE)
					}
					dd <- file.path(path, "data", "clean", group)
					p <- archive_old_version(fpath, test$old, dd)
					dl <- try(get_data(uri, path, group, cache=FALSE, filter=FALSE), silent=TRUE)
					if (inherits(dl, "try-error") || !file.exists(file.path(fpath, "ok.txt"))) {
						err <- if (inherits(dl, "try-error")) trimws(as.character(dl)) else "download did not complete"
						restore_archived_version(fpath, p, dd)
						stop(paste0("version ", req_v, " exists on server, but download failed (", err, "); kept cached version ", vmeta), call.=FALSE)
					}
					message(paste0("updated ", pat, " ", test$old, " -> ", test$new, " (old files archived in ", test$old, ")"))
					utils::flush.console()
					old_rel <- raw_data_filenames(p, pat, exclude_clean = TRUE)
					new_rel <- raw_data_filenames(fpath, pat, exclude_clean = FALSE)
					if (!setequal(old_rel, new_rel)) {
						return("files_changed")
					}
					return(TRUE)
				} else {
					stop(paste0("version ", req_v, " in script, ", vmeta, " in cached data, and ", test$new, " on server. Fix script."), call.=FALSE)
				}
			} else if (isTRUE(test)) {
				stop(paste0("version ", req_v, " in script does not match latest version on server (", vmeta, "). Fix script."), call.=FALSE)
			}
		}
	}

	if (isTRUE(.carob_environment$purge)) {
		if (as.numeric(req_v) > as.numeric(vmeta)) {
			unlink(fpath)
			stop(paste0("version ", req_v, " in script but version ", vmeta, " reported by data. Cache was cleared, run again"), call.=FALSE)	
		} else {
			stop(paste0("version ", req_v, " in script but version ", vmeta, " reported by data. Fix script."), call.=FALSE)					
		}
	}
	stop(paste0("version ", req_v, " in script but version ", vmeta, " reported by data. Fix script or remove files with `get_data(, cache=FALSE`"), call.=FALSE)	
}

get_metadata <- function(uri, path, group, major, minor, ...) {
	if (isTRUE(grepl("doi:10.48529", uri))) {
		m <- yuri:::LSMS_metadata(uri, group, path, major, minor, ...)
		m$carob_group=group
		m$treatment_vars = "none"
		m$response_vars = "none"
		return(m)
	}
	dataset_id <- yuri::simpleURI(uri, warn=FALSE) #extract_metadata will warn
	jpath <- file.path(path, "data", "raw", group)
	m <- yuri::extract_metadata(uri, jpath)
	m$carob_group <- group
	d <- data.frame(list(...))
	
	draft <- isTRUE(d$draft)	
	if (!draft) {
		old_v <- m$version
		fpath <- file.path(jpath, dataset_id)
		upd <- checkVersion(old_v, major=major, minor=minor, fpath=fpath, uri=uri, path=path, group=group)
		if (isTRUE(upd) || identical(upd, "files_changed")) {
			m <- yuri::extract_metadata(uri, jpath)
			m$carob_group <- group
			checkVersion(m$version, major=major, minor=minor, fpath=fpath)
			if (identical(upd, "files_changed")) {
				cond <- structure(
					list(message = paste0("updated ", dataset_id, " from ", old_v, " to ", m$version,
						" (old files archived in old_", old_v, "); filenames changed, run script again")),
					class = c("carob_version_updated", "error", "condition")
				)
				stop(cond)
			}
		}
	}
	d$draft <- NULL
	
	if (nrow(d) == 1) {
		m[names(d)] <- d
	} else if (nrow(d) > 1) {
		warning("additional arguments must all have length 1")
	}

	m
}

