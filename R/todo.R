
.todo_script_difficulty <- function(path, script, group) {
	f <- file.path(path, script)
	if (is.na(script) || script == "" || !file.exists(f)) return(NA)
	x <- readLines(f, warn=FALSE)

	cmp <- regmatches(x, regexpr("carob_completion\\s*=\\s*[0-9]+", x))
	cmp <- if (length(cmp) > 0) as.numeric(sub(".*=\\s*", "", cmp[1])) else 0

	nis <- 0
	s <- grep("^## ISSUES", x)
	if (length(s) > 0) {
		e <- grep("^## |^[^#]", x)
		e <- c(e[e > s[1]], length(x) + 1)[1]
		if (e > s[1] + 1) {
			b <- x[(s[1]+1):(e-1)]
			nis <- sum(grepl("^#\\s?\\S", b) & !grepl("^#\\s{2,}", b))
		}
	}

	rd <- file.path(path, "data", "raw", group, gsub("\\.R$", "", basename(f)))
	nf <- 1
	if (dir.exists(rd)) {
		nf <- list.files(rd, recursive=TRUE)
		nf <- length(grep("\\.(pdf|docx?|txt|png|jpe?g|R)$", nf, invert=TRUE, ignore.case=TRUE))
	}

	# 0-5 points: work left (3), open issues (1), number of data files (1)
	p <- 3 * (100 - min(cmp, 100)) / 100 + min(nis, 15) / 15 + min(log10(max(nf, 1)), 2) / 2
	as.integer(max(1, min(5, round(p))))
}


update_todo <- function(path) {

	ftodo <- file.path(path, "misc", "todo", "to-do.csv")
	if (!file.exists(ftodo)) return(invisible(FALSE))
	
	todo <- utils::read.csv(ftodo)
	todo$uri <- trimws(todo$uri)
	uri <- gsub("https://doi.org/", "doi:", tolower(todo$uri))
	uri <- gsub("https://hdl.handle.net/", "hdl:", uri)
	id <- tolower(sapply(uri, yuri::simpleURI, warn=FALSE, USE.NAMES=FALSE))

	done <- list.files(file.path(path, "scripts"), pattern="^doi|^hdl.*\\.R$", recursive=TRUE, ignore.case=TRUE)
	# Match the holding folders only. A bare "_AI" also matches DOI stems such as AIS9XM.
	done <- grep("(^|/)_(draft|todo|pending|AI)(/|$)", done, invert=TRUE, value=TRUE)
	done <- gsub("\\.r$", "", tolower(basename(done)))
	done <- gsub(" ", "", done)
	id <- gsub(" ", "", id)

	fpr <- file.path(path, "misc", "todo", "last_pr.txt")
	last_pr <- 0
	if (file.exists(fpr)) {
		last_pr <- suppressWarnings(as.integer(readLines(fpr, n=1, warn=FALSE)[1]))
		if (is.na(last_pr)) last_pr <- 0
	}

	pr <- character()
	out <- try(.github_open_pr_ids(last_pr=last_pr), silent=TRUE)
	if (!inherits(out, "try-error")) {
		pr <- out$ids
		if (out$last_pr > last_pr) {
			writeLines(as.character(out$last_pr), fpr)
		}
	}

	i <- which(id %in% unique(c(done, pr)))
	changed <- length(i) > 0
	if (changed) {
		todo <- todo[-i,]
	}

	if (!is.null(todo$script)) {
		old <- todo$difficulty
		dif <- mapply(\(s, g) .todo_script_difficulty(path, s, g), todo$script, todo$group, USE.NAMES=FALSE)
		if (!is.null(old)) {
			keep <- is.na(dif) & !is.na(old)
			dif[keep] <- old[keep]
		}
		todo$difficulty <- as.integer(dif)
		if (is.null(old) || !identical(as.integer(old), todo$difficulty)) changed <- TRUE
	}

	if (changed) {
		utils::write.csv(todo, ftodo, row.names=FALSE)
	}

	ff <- list.files(file.path(path, "scripts", "_todo"), pattern="^doi|^hdl.*\\.R$", recursive=TRUE, ignore.case=TRUE, full.names=TRUE)
	if (length(ff) > 0) {
		fid <- gsub("\\.r$", "", tolower(basename(ff)))
		fid <- gsub(" ", "", fid)
		j <- fid %in% done
		if (any(j)) {
			file.remove(ff[j])
		}
	}

	invisible(TRUE)
}

